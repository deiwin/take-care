let Effect =
      < SetSlackChannelTopic : { name : Text, topic : List Text -> Text }
      | InviteToSlackChannel : Text
      | SetSlackGroup : Text
      >
 in Effect
