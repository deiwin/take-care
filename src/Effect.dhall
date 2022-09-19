let SlackEffect = ./Effect/Slack.dhall
let Effect =
      < Slack : SlackEffect
      | NoOp
      >
 in Effect
