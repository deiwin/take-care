let SlackEffect = ../core/Effect/Slack.dhall

let SetGroup = SlackEffect.SetGroup

let SetChannelTopic = SlackEffect.SetChannelTopic

let Effect = ../core/Effect.dhall

let Slack = Effect.Slack

let Rotation = ../core/Rotation.dhall

let Conf = ../core/Conf.dhall

let concat =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/List/concat.dhall
        sha256:54e43278be13276e03bd1afa89e562e94a0a006377ebea7db14c7562b0de292b

let concatMap =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/List/concatMap.dhall
        sha256:3b2167061d11fda1e4f6de0522cbe83e0d5ac4ef5ddf6bb0b2064470c5d3fb64

let concatSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Text/concatSep.dhall
        sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let TeamArgs =
      { name : Text
      , members : { caretakers : List (List Text), others : List Text }
      , topic : Text -> Text
      }

let team =
      \(args : TeamArgs) ->
        let channelName = "tm-${args.name}"

        let allMembers =
              concat Text args.members.caretakers # args.members.others

        in    [ { rotation = Rotation.Weekly args.members.caretakers
                , effects =
                  [ Slack
                      ( SetChannelTopic
                          { name = channelName
                          , topic =
                              \(caretakers : List Text) ->
                                args.topic (concatSep ", " caretakers)
                          }
                      )
                  , Slack
                      ( SetGroup
                          { handle = "${args.name}-caretaker"
                          , name = "Team ${args.name} caretaker(s)"
                          , channels = [] : List Text
                          }
                      )
                  ]
                }
              , { rotation = Rotation.Const allMembers
                , effects =
                  [ Slack
                      ( SetGroup
                          { handle = "${args.name}-team"
                          , name = "Team ${args.name}"
                          , channels = [ channelName ]
                          }
                      )
                  ]
                }
              ]
            : List Conf

let teams = concatMap TeamArgs Conf team

in  teams
