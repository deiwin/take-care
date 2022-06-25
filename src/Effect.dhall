let Effect =
      < SetSlackChannelTopic : { name : Text, topic : List Text -> Text }
      | SetSlackGroup : { handle : Text, name : Text, channels : List Text}
      >
 in Effect
