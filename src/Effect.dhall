let Effect =
      < SetSlackChannelTopic : { name : Text, topic : List Text -> Text }
      | InviteToSlackChannel : Text
      | SetSlackGroup : { handle : Text, name : Text}
      >
 in Effect
