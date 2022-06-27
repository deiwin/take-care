{-# LANGUAGE AllowAmbiguousTypes #-}

module Slack.TestUtils
  ( runSlackWith,
    nullSlackMatch,
    SlackResponse (..),
    runSlackWithExpectations,
  )
where

import Control.Category ((>>>))
import Control.Lens ((&))
import Control.Monad (void)
import Data.Aeson (Value (Null), decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Polysemy (Embed, InterpreterFor, Member, Sem, embed, interpret, run, runM)
import Polysemy.Error (Error, note, runError)
import Polysemy.Internal (Append)
import Slack.Util (Slack (..))
import Test.Hspec (Expectation)

runSlackWith ::
  (a -> Sem '[Slack, Error Text] b) ->
  SlackResponse ->
  a ->
  Either Text b
runSlackWith runToSlack slackResponse =
  runToSlack
    >>> runSlack slackResponse
    >>> runError
    >>> run

runSlackWithExpectations ::
  (Sem (Append slackEffs '[Slack, Error Text, Embed IO]) a -> Sem '[Slack, Error Text, Embed IO] a) ->
  (forall rInitial x. (Slack (Sem rInitial) x -> IO ())) ->
  Sem (Append slackEffs '[Slack, Error Text, Embed IO]) a ->
  Expectation
runSlackWithExpectations runToSlack expectations =
  runToSlack
    >>> runSlackWithExpectations'
    >>> runError
    >>> runM
    >>> void
  where
    runSlackWithExpectations' :: Member (Embed IO) r => Sem (Slack ': r) a -> Sem r a
    runSlackWithExpectations' = interpret (embed . expecationWithNull)
    -- Required to match the type expectation of interpret
    expecationWithNull :: Slack (Sem rInitial) x -> IO x
    expecationWithNull slackQuery =
      case slackQuery of
        Get _ _ -> expectations slackQuery $> Null
        GetPaginated _ _ -> expectations slackQuery $> []
        Post _ _ -> expectations slackQuery $> Null

runSlack :: Member (Error Text) r => SlackResponse -> InterpreterFor Slack r
runSlack slackResponse = interpret \case
  Get _ _ ->
    getResponse slackResponse
      & note "No response provided for Get"
      & (>>= json)
  GetPaginated _ _ ->
    getPaginatedResponse slackResponse
      & note "No response provided for GetPaginated"
      & (>>= traverse json)
  Post _ _ ->
    postResponse slackResponse
      & note "No response provided for Post"
      & (>>= json)

data SlackResponse = SlackResponse
  { getResponse :: Maybe Text,
    getPaginatedResponse :: Maybe [Text],
    postResponse :: Maybe Text
  }

nullSlackMatch :: SlackResponse
nullSlackMatch =
  SlackResponse
    { getResponse = Nothing,
      getPaginatedResponse = Nothing,
      postResponse = Nothing
    }

json :: Member (Error Text) r => Text -> Sem r Value
json =
  encodeUtf8
    >>> fromStrict
    >>> decode
    >>> note "failed to parse test JSON"
