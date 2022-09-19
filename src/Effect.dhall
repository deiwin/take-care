let Effect =
      < SlackSetChannelTopic : { name : Text, topic : List Text -> Text }
      | SlackSetGroup : { handle : Text, name : Text, channels : List Text}
      >
 in Effect
