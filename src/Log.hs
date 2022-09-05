module Log
  ( Effects,
    runLog,
  )
where

import Control.Category ((>>>))
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Polysemy (Embed, InterpretersFor, Members)
import Polysemy.Log (Log, LogEntry (..), interpretLogStderrWith)
import Polysemy.Log.Data.LogMessage (LogMessage (..))
import Polysemy.Log.Format (formatCaller, formatSeverity)
import Polysemy.Time (GhcTime, interpretTimeGhc)

type Effects =
  '[ Log,
     GhcTime
   ]

runLog :: Members '[Embed IO] r => InterpretersFor Effects r
runLog =
  interpretLogStderrWith formatLogEntry
    >>> interpretTimeGhc

formatLogEntry :: LogEntry LogMessage -> Text
formatLogEntry (LogEntry LogMessage {..} time source) =
  formatSeverity severity
    <> wrapMetadata (formatTime time)
    <> wrapMetadata (formatCaller source)
    <> message

formatTime :: UTCTime -> Text
formatTime time = pack (show time)

wrapMetadata :: Text -> Text
wrapMetadata text = "[" <> text <> "] "
