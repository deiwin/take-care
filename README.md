# take-care

**take-care** is a Slack team and chore ownership manager.

## Goal

**take-care** aims to 1) combat [diffusion of responsibility][1] and 2)
optimize team behavior regarding necessary distractions. It does so by managing
a weekly _caretaker_ rotation for teams.

_Caretakers_ are specific individuals in a team who are responsible for dealing
with unattractive but important tasks, such as chores. It's easy for everyone
on the team to fall into thinking "This is problematic, surely _somebody else_
will deal with this". However, if this week it's been agreed that John is
explicitly responsible for such problems, then John knows that _he_ has to deal
with the problem when he sees it and others know that they don't have to worry.

Additionally, other teams and individuals know that instead of using `@channel`
somewhere and notifying the entire team about issues and questions, their first
point of contact should be the caretaker. This minimizes distractions for the
rest of the team, allowing them to focus on planned work.

**take-care** manages the caretaker rotation for you. It makes the team's
current caretaker easily reachable via `@<team-name>-caretaker` Slack alias
(via [User Groups][2]). It also provides a simple way to include the current
caretaker's Slack handle in the topic of the team's channel.

Learn more by completing the **Getting started** tutorial below.

## Getting started

This section assumes that you have a Slack API token and you use Docker for
running **take-care**. If you don't have a token yet, complete the [Getting
a Slack API token](#getting-a-slack-api-token) section first. And if you don't
want to use Docker, then see the [Installation](#installation) section.

First, save your API token to an environment variable. This allows the rest of
the commands in this guide to work with simple copy-paste.

```
export API_TOKEN=<token>
```

The configuration requires referring to users by their Slack user IDs. Use the
`list-users` command to find these IDs.

```
$ docker run --rm -i -e "API_TOKEN=$API_TOKEN" deiwin/take-care:latest list-users
USLACKBOT: @slackbot
U111ALICE: @alice
U22222BOB: @bob
U333CAROL: @carol
U4444DAVE: @dave
U55555EVE: @eve
U6666FAYE: @faye
U77777GIL: @gil
U88888HAL: @hal
```

With the list of user IDs at hand, you're ready to create the configuration for
your teams. Save the following to a file called `teams.dhall` and edit
according to your needs. The configuration is written in [Dhall][3],
a programmable configuration language with Haskell-like syntax.

```haskell
let teams = ./types/zoo/teams.dhall
 in teams
      [ { members =
          { caretakers = [ [ "alice@example.com", "bob@example.com", "carol@example.com" ] ]
          , others = [ "dave@example.com" ]
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
            [ [ "eve@example.com", "faye@example.com" ], [ "gil@example.com", "hal@example.com" ] ]
          , others = [] : List Text
          }
        , name = "dev"
        , topic = \(caretakers : Text) -> "${caretakers} are the caretakers"
        }
      ]
```

Verify the configuration with the `--dry-run` flag for the `ensure`
command.

```dryRunExample
$ docker run --rm -i -e "API_TOKEN=$API_TOKEN" deiwin/take-care:latest ensure --dry-run < teams.dhall
For bob@example.com:
  Slack.SetChannelTopic #tm-design: Stand-up *9:30* :paw_prints: Board :incoming_envelope: https://team.board/url :paw_prints: Caretaker @bob
  Slack.SetGroup: @design-caretaker {name = "Team design caretaker(s)", channels = []}

For alice@example.com, bob@example.com, carol@example.com, dave@example.com:
  Slack.SetGroup: @design-team {name = "Team design", channels = ["tm-design"]}

For eve@example.com, gil@example.com:
  Slack.SetChannelTopic #tm-dev: @eve, @gil are the caretakers
  Slack.SetGroup: @dev-caretaker {name = "Team dev caretaker(s)", channels = []}

For eve@example.com, faye@example.com, gil@example.com, hal@example.com:
  Slack.SetGroup: @dev-team {name = "Team dev", channels = ["tm-dev"]}
```

And finally, run the `ensure` command.

```
$ docker run --rm -i -e "API_TOKEN=$API_TOKEN" deiwin/take-care:latest ensure < teams.dhall
Team design: success!
Team dev: success!
```

The `ensure` command _ensures_ that

- channels `#tm-design` and `#tm-dev`
  - exist,
  - include the configured team members, and
  - have their topic set per configuration;
- user groups `@design-team` and `@dev-team`
  - exist and
  - consist of the configured team members; and
- user groups `@design-caretaker` and `@dev-caretaker`
  - exist and
  - consist of the current caretakers (one per caretaker list per team).

## Getting a Slack API token

- Follow the [Creating apps Slack guide][4] to create a Slack app. You can call
  it **Take Care**, for example. The app will only live in your workspace.
- Go to the **OAuth & Permissions** section of your app's management page, to
  [add permissions for the following scopes][5]:
  - `channels:read`
  - `channels:write` for creating channels and setting channel topics
  - `usergroups:read`
  - `usergroups:write` for managing user groups
  - `users:read` for finding individual users' Slack handles
  - `users:read.email` for finding user info based on their email
- Save the permission changes.
- In the same **OAuth & Permissions** section, scroll to the top and click on
  the **Install App to Workspace** button and authorize the installation.
- You will then see a **OAuth Access Token** and a **Bot User OAuth Access
  Token**. The scopes above are configured separately for both. You can use
  either token, but the bot token will only work if you've enabled
  usergroup modifications for non-admins in your workspace. It is disabled
  by default, but can be enabled from **Administration** -> **Settings
  & Permissions** -> **Permissions** -> **User Groups**, or from
  https://<your-domain>.slack.com/admin/settings#user_group_restrictions

## Installation

The **Getting started** tutorial uses a `deiwin/take-care:latest` Docker image
for running **take-care**. Docker is used so you don't have to worry about
platform compatibility, but if you don't want to use Docker, then there's a few
alternative's as well.

### Downloading macOS binary

If you're using mac, you can find the latest [release from GitHub][6] and
download its binary. At the moment only binaries for macOS will be published.

### Building from source

Of course, you can also build the program from source:

```
git clone git@github.com:deiwin/take-care.git
cd take-care
stack install
```

[1]: https://en.wikipedia.org/wiki/Diffusion_of_responsibility
[2]: https://get.slack.help/hc/en-us/articles/212906697-User-Groups
[3]: https://github.com/dhall-lang/dhall-lang
[4]: https://api.slack.com/slack-apps#creating_apps
[5]: https://api.slack.com/slack-apps#oauth__amp__permissions
[6]: https://github.com/deiwin/take-care/releases
