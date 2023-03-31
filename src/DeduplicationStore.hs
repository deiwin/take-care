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

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime (..))
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

type EffectsHash = Text

type InputHash = Text

data DeduplicationContext = DeduplicationContext
  { effectsHash :: EffectsHash,
    inputHash :: InputHash,
    validUntil :: UTCTime
  }
  deriving (Show)

instance ToRow DeduplicationContext where
  toRow (DeduplicationContext {..}) = toRow (effectsHash, inputHash, validUntil)

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
    conn <- connection <$> input @DBCtx
    Log.info (pack (printf "Checking DB whether we have already applied %s .." (show ctx)))
    [Only count] <-
      embed $
        query
          conn
          "SELECT COUNT(1) FROM deduplication_store\
          \ WHERE effects_hash = ?\
          \ AND input_hash = ?\
          \ AND unixepoch(valid_until) > unixepoch()"
          (effectsHash, inputHash)
    Log.info (pack (printf "Found %d match(es)" count))
    return (count >= (1 :: Int))
  StoreAppliedContext ctx -> do
    conn <- connection <$> input @DBCtx
    Log.info (pack (printf "Upserting to DB: %s .." (show ctx)))
    embed $
      execute
        conn
        "INSERT INTO deduplication_store\
        \   (effects_hash, input_hash, valid_until)\
        \   VALUES (?,?,?)\
        \ ON CONFLICT(effects_hash) DO UPDATE SET\
        \   input_hash = excluded.input_hash,\
        \   valid_until = excluded.valid_until"
        ctx

newtype DBCtx = DBCtx
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
  dbPath <-
    Env.lookup "PERSISTENT_FOLDER_PATH"
      <&> fromMaybe "" -- Default to empty value, referring to the current folder
      <&> (\s -> if s == "" then s else s <> "/")
      <&> (<> dbFileName)
  Log.info (pack (printf "Opening DB on path %s .." dbPath))
  connection <- embed $ open dbPath
  initializeDB connection
  runInputConst (DBCtx {..}) program

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
      \  valid_until TEXT NOT NULL\
      \)"

  Log.info "Deleting expired rows to avoid unbounded DB growth .."
  embed $ execute_ conn "DELETE FROM deduplication_store WHERE unixepoch(valid_until) < unixepoch()"
  deletedCount <- embed $ changes conn
  Log.info (pack (printf "Deleted %d rows .." deletedCount))
