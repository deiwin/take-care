let Rotation = ./src/Rotation.dhall

let Effect = ./src/Effect.dhall

let Conf = ./src/Conf.dhall

let concat =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/List/concat.dhall

let concatMap =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/List/concatMap.dhall

let concatSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Text/concatSep.dhall

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
                  [ Effect.SetSlackChannelTopic
                      { name = channelName
                      , topic =
                          \(caretakers : List Text) ->
                            args.topic (concatSep ", " caretakers)
                      }
                  , Effect.SetSlackGroup "${args.name}-caretaker"
                  ]
                }
              , { rotation = Rotation.Const allMembers
                , effects =
                  [ Effect.InviteToSlackChannel channelName
                  , Effect.SetSlackGroup "${args.name}-team"
                  ]
                }
              ]
            : List Conf

let teams = concatMap TeamArgs Conf team

in    teams
        [ { members =
            { caretakers = [ [ "U111ALICE", "U22222BOB", "U333CAROL" ] ]
            , others = [ "U4444DAVE" ]
            }
          , name = "design"
          , topic =
              \(caretaker : Text) ->
                let standup = "Stand-up *9:30*"

                let board = "Board :incoming_envelope: https://team.board/url"

                let separator = ":paw_prints:"

                in  "${standup} ${separator} ${board} ${separator} Caretaker ${caretaker}"
          }
        , { members =
            { caretakers =
              [ [ "U55555EVE", "U6666FAYE" ], [ "U77777GIL", "U88888HAL" ] ]
            , others = [] : List Text
            }
          , name = "dev"
          , topic = \(caretakers : Text) -> "${caretakers} are the caretakers"
          }
        ]
    : List Conf
