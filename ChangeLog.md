# Changelog for take-care

## Unreleased changes

* Redesign configuration interface to be more flexible. Previously the
  configuration had specific constraints on Slack channel naming patterns etc.
  This is no longer the case. The old behavior is still expressable with the
  new interface, but the new interface allows doing many things which weren't
  possible previously. For a simple example, it's now possible to call the role
  "on-duty", or anything else, instead of the previously enforced "caretaker".
* Added logging. The program now outputs logs to stderr. Users who were
  previously using the programs entire output (stdout + stderr), should now
  distinguish between stdout and stderr.
* The configuration now uses emails instead of Slack IDs for identifying users.
  This allows referring to the same user across different systems.
* The Dhall types that make up the new, flexible configuration interface (API)
  of the application are now available in the `./types/core` of the repository
  and the available Docker images.

## v0.5.0

* Change `list-caretakers` command to be `ensure --dry-run` instead. Also
  update the output to show everything that the `ensure` command would
  _ensure_.
* Remove global caretaker group, consisting of caretakers from all teams.
  If you were using take-care previously, then you will have to disable
  the existing "caretakers" user group manually on Slack.
