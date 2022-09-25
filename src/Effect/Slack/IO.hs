module Effect.Slack.IO
  ( apply,
    showDryRun,
  )
where

import Control.Lens ((^.))
import Control.Monad (unless)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, filter, pack)
import Effect.Slack (SlackEffect (..))
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
    id,
    displayName,
  )
import qualified Slack.User as Users (find)
import Text.Printf (printf)
import Text.Show.Functions ()
import Prelude hiding (filter, unlines)

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
    members
      & Set.toList
      & traverse getUserID
      >>= Groups.setMembers groupID
      & unless (members == currentMembers)

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

showDryRun ::
  ( Member (Error Text) r,
    Member Users r
  ) =>
  Set Text ->
  SlackEffect ->
  Sem r Text
showDryRun members = fmap (pack . ("Slack." <>)) . \case
  SetChannelTopic {name, topic} ->
    members
      & Set.toList
      & traverse getDisplayName
      <&> topic
      <&> printf "SetChannelTopic #%s: %s" name
  SetGroup {handle, name, channels} ->
    return $ printf "SetGroup: @%s {name = \"%s\", channels = %s}" handle name (show channels)

getUserID :: Members '[Users, Error Text] r => Text -> Sem r Text
getUserID email =
  Users.find email
    >>= note (pack (printf "Could not find user with email: %s" email))
    <&> (^. User.id)

getDisplayName :: Members '[Users, Error Text] r => Text -> Sem r Text
getDisplayName email =
  Users.find email
    >>= note (pack (printf "Could not find user with email: %s" email))
    <&> (^. User.displayName)

findOrCreateChannel ::
  Member Channels r =>
  Text ->
  Sem r Channel
findOrCreateChannel name = Channels.find name >>= maybe (Channels.create name) return

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
