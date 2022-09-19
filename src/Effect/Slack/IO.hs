module Effect.Slack.IO
  ( apply
  )
where

import Control.Lens ((^.))
import Control.Monad (unless)
import Data.Functor ((<&>))
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, filter, pack)
import Polysemy (Member, Members, Sem)
import Polysemy.Error (Error, note)
import Polysemy.Log (Log)
import qualified Polysemy.Log as Log (info)
import Slack.Channel as Channel
  ( Channel,
    Channels,
    id,
  )
import qualified Slack.Channel as Channel (topic)
import qualified Slack.Channel as Channels (create, find, setTopic)
import Slack.Group as Group
  ( Groups,
    channelIDs,
    id,
  )
import qualified Slack.Group as Groups
  ( create,
    find,
    getMembers,
    setChannels,
    setMembers,
  )
import Slack.User as User
  ( Users,
    displayName,
  )
import qualified Slack.User as Users (find)
import Text.Printf (printf)
import Text.Show.Functions ()
import Prelude hiding (filter, unlines)
import Effect.Slack (SlackEffect (..))

apply ::
  ( Member (Error Text) r,
    Member Groups r,
    Member Channels r,
    Member Users r,
    Member Log r
  ) =>
  Set Text ->
  SlackEffect ->
  Sem r ()
apply members = \case
  SetGroup {handle, name, channels} -> do
    Log.info (pack (printf "Finding or creating the following channels: %s .." (show channels)))
    existingGroup <- Groups.find handle
    defaultChannelIDs <- (^. Channel.id) <<$>> traverse findOrCreateChannel channels

    Log.info (pack (printf "Finding or creating the group @%s .." handle))
    slackGroup <- maybe (createNew defaultChannelIDs) return existingGroup
    let groupID = slackGroup ^. Group.id

    currentMembers <- Set.fromList <$> Groups.getMembers groupID
    Log.info (pack (printf "Updating group members if changed from %s to %s .." (show currentMembers) (show members)))
    unless (members == currentMembers) $ Groups.setMembers groupID $ Set.toList members

    let currentChannels = slackGroup ^. channelIDs
    Log.info (pack (printf "Updating default channel IDs if changed from %s to %s .." (show currentChannels) (show defaultChannelIDs)))
    unless (same defaultChannelIDs currentChannels) $ Groups.setChannels groupID defaultChannelIDs
    where
      createNew = Groups.create handle name
      same a b = null (a \\ b) && null (b \\ a)
  SetChannelTopic {name, topic} -> do
    Log.info (pack (printf "Finding or creating channel #%s .." name))
    channel <- findOrCreateChannel name

    newTopic <- topic <$> traverse getDisplayName (Set.toList members)
    Log.info (pack (printf "Updating topic if changed from \"%s\" to \"%s\" .." (channel ^. Channel.topic) newTopic))
    unless
      (same (channel ^. Channel.topic) newTopic)
      (Channels.setTopic (channel ^. Channel.id) newTopic)
    where
      same oldTopic newTopic = oldTopic == newTopic || clean oldTopic == clean newTopic
      clean = filter (not . potentialAddedChar)
      potentialAddedChar c = c `elem` ['<', '>']

getDisplayName :: Members '[Users, Error Text] r => Text -> Sem r Text
getDisplayName id =
  Users.find id
    >>= note (pack (printf "Could not find user with ID: %s" id))
    <&> (^. User.displayName)

findOrCreateChannel ::
  Member Channels r =>
  Text ->
  Sem r Channel
findOrCreateChannel name = Channels.find name >>= maybe (Channels.create name) return

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap