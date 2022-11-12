let SlackEffect = ../core/Effect/Slack.dhall
let SetGroup = SlackEffect.SetGroup
let SetChannelTopic = SlackEffect.SetChannelTopic

let Effect = ../core/Effect.dhall
let Slack = Effect.Slack

let Rotation = ../core/Rotation.dhall
let Conf = ../core/Conf.dhall

let concat = https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/List/concat.dhall
let concatMap = https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/List/concatMap.dhall
let concatSep = https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Text/concatSep.dhall

let TeamArgs =
      { name : Text
      , members : { caretakers : List (List Text), others : List Text }
      , topic : Text -> Text
      }

let team =
      \(args : TeamArgs) ->
        let channelName = "tm-${args.name}"

        let allMembers =
              concat
                Text
                [ concat Text args.members.caretakers, args.members.others ]

        in    [ { rotation = Rotation.Weekly args.members.caretakers
                , effects =
                  [ Slack (SetChannelTopic
                      { name = channelName
                      , topic =
                          \(caretakers : List Text) ->
                            args.topic (concatSep ", " caretakers)
                      })
                  , Slack (SetGroup
                      { handle = "${args.name}-caretaker"
                      , name = "Team ${args.name} caretaker(s)"
                      , channels = [] : List Text
                      })
                  ]
                }
              , { rotation = Rotation.Const allMembers
                , effects =
                  [ Slack (SetGroup
                      { handle = "${args.name}-team"
                      , name = "Team ${args.name}"
                      , channels = [channelName]
                      })
                  ]
                }
              ]
            : List Conf

let teams = concatMap TeamArgs Conf team
 in teams
