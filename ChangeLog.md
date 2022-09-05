# Changelog for take-care

## Unreleased changes

* Added logging. The program now outputs logs to stderr. Users who were
  previously using the programs entire output (stdout + stderr), should now
  distinguish between stdout and stderr.

## v0.5.0

* Change `list-caretakers` command to be `ensure --dry-run` instead. Also
  update the output to show everything that the `ensure` command would
  _ensure_.
* Remove global caretaker group, consisting of caretakers from all teams.
  If you were using take-care previously, then you will have to disable
  the existing "caretakers" user group manually on Slack.
