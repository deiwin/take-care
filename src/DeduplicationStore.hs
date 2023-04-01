module DeduplicationStore
  ( DeduplicationStore (..),
    runDeduplicationStore,
    DeduplicationContext (..),
    isAlreadyApplied,
    storeAppliedContext,
    DBCtx,
    runDBCtx,
  )
where

import Data.Function ((&))
import Data.Text (Text, pack)
import Database.SQLite.Simple (Connection, Only (Only), ToRow (toRow), changes, execute, execute_, open, query)
import IO (Env)
import IO qualified as Env (lookup)
import Polysemy (Embed, InterpreterFor, Member, Sem, embed, interpret, makeSem)
import Polysemy.Error (Error)
import Polysemy.Fail (failToError)
import Polysemy.Input (Input, input, runInputConst)
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log (info)
import Text.Printf (printf)

data DeduplicationContext = DeduplicationContext
  { effectsHash :: Text,
    inputHash :: Text,
    outputHash :: Text
  }
  deriving (Eq, Show)

instance ToRow DeduplicationContext where
  toRow (DeduplicationContext {..}) = toRow (effectsHash, inputHash, outputHash)

data DeduplicationStore m a where
  IsAlreadyApplied :: DeduplicationContext -> DeduplicationStore m Bool
  StoreAppliedContext :: DeduplicationContext -> DeduplicationStore m ()

makeSem ''DeduplicationStore

runDeduplicationStore ::
  ( Member (Embed IO) r,
    Member (Input DBCtx) r,
    Member (Error Text) r,
    Member Log r
  ) =>
  InterpreterFor DeduplicationStore r
runDeduplicationStore = interpret \case
  IsAlreadyApplied ctx@(DeduplicationContext {..}) -> failToError pack $ do
    dbCtxM <- input
    case dbCtxM of
      Nothing -> return False
      Just (DBCtx' {..}) -> do
        Log.info (pack (printf "Checking DB whether we have already applied %s .." (show ctx)))
        [Only count] <-
          embed $
            query
              connection
              "SELECT COUNT(1) FROM deduplication_store\
              \ WHERE effects_hash = ?\
              \ AND input_hash = ?\
              \ AND output_hash = ?"
              (effectsHash, inputHash, outputHash)
        Log.info (pack (printf "Found %d match(es)" count))
        Log.info "Updating last_accessed_at for given effects_hash to avoid it being garbage collected"
        embed $
          execute
            connection
            "UPDATE deduplication_store\
            \ SET last_accessed_at = datetime()\
            \ WHERE effects_hash = ?"
            (Only effectsHash)
        return (count >= (1 :: Int))
  StoreAppliedContext ctx -> do
    dbCtxM <- input
    case dbCtxM of
      Nothing -> return ()
      Just (DBCtx' {..}) -> do
        Log.info (pack (printf "Upserting to DB: %s .." (show ctx)))
        embed $
          execute
            connection
            "INSERT INTO deduplication_store\
            \   (effects_hash, input_hash, output_hash, last_accessed_at)\
            \   VALUES (?,?,?,datetime())\
            \ ON CONFLICT(effects_hash) DO UPDATE SET\
            \   input_hash = excluded.input_hash,\
            \   output_hash = excluded.output_hash,\
            \   last_accessed_at = excluded.last_accessed_at"
            ctx

type DBCtx = Maybe DBCtx'

newtype DBCtx' = DBCtx'
  { connection :: Connection
  }

runDBCtx ::
  ( Member (Embed IO) r,
    Member Env r,
    Member Log r
  ) =>
  InterpreterFor (Input DBCtx) r
runDBCtx program = do
  let dbFileName = "deduplication.db"
  persistentPathM <- Env.lookup "PERSISTENT_FOLDER_PATH"
  dbCtx <-
    case persistentPathM of
      Nothing -> do
        Log.info "Env variable PERSISTENT_FOLDER_PATH not set. Will not use DeduplicationStore."
        return Nothing
      Just persistentPath -> do
        let dbPath =
              persistentPath
                & (\s -> if s == "" then s else s <> "/")
                & (<> dbFileName)
        Log.info (pack (printf "Opening DB on path %s .." dbPath))
        connection <- embed $ open dbPath
        initializeDB connection
        return (Just DBCtx' {..})
  runInputConst dbCtx program

initializeDB ::
  ( Member (Embed IO) r,
    Member Log r
  ) =>
  Connection ->
  Sem r ()
initializeDB conn = do
  Log.info "Creating DB table if it doesn't exist .."
  embed $
    execute_
      conn
      "CREATE TABLE IF NOT EXISTS deduplication_store (\
      \  effects_hash TEXT PRIMARY KEY,\
      \  input_hash TEXT NOT NULL,\
      \  output_hash TEXT NOT NULL,\
      \  last_accessed_at TEXT NOT NULL\
      \)"

  Log.info "Deleting old, unused rows to avoid unbounded DB growth .."
  embed $
    execute_
      conn
      "DELETE FROM deduplication_store WHERE unixepoch(last_accessed_at) < unixepoch('now', '-14 days')"
  deletedCount <- embed $ changes conn
  Log.info (pack (printf "Deleted %d rows .." deletedCount))
