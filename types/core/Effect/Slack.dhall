let SlackEffect =
      < SetChannelTopic : { name : Text, topic : List Text -> Text }
      | SetGroup : { handle : Text, name : Text, channels : List Text}
      >
 in SlackEffect
