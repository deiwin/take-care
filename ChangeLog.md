# Changelog for take-care

## Unreleased changes

## v0.7.1

* Use `/app/.cache` as the cache folder by default, falling back to the
  previous `/root/.cache` if necessary. For this, the Dhall cache is copied to
  both folders and then the `XDG_CACHE_HOME` env variable in the image is set
  explicitly to `/app/.cache`. If this env variable is unset, then it will fall
  back to using `$HOME/.cache` (`/root/.cache`) as it did previously. This
  change allows the cache to still be used even if the home folder is
  overwritten by a volume mount, for example.
* Retry Slack requests when hitting rate limits. All requests will be tried
  again (up to a handful of times) after the duration specificied in the
  `Retry-After` header. This can make the bot slower in case it starts hitting
  rate limits but it avoids unnecessary failures. The amount of requests is
  already kept to a minimum with different caching and deduplication
  mechanisms, but multiple invocations can still trigger rate limits.

## v0.7.0

* Add deduplication. If deduplication is enabled, the bot "remembers" the
  configurations it has already applied and will not apply the same
  configuration again unless something changes. The feature is disabled by
  default. Set the `PERSISTENT_FOLDER_PATH` environment variable to a folder
  for storing the DB to enable this feature.
* Add a `OpsgenieScheduleID` rotation. The new rotation pulls from Opsgenie the
  list of emails of whoever is on call currently for the specified schedule.
  Requires `OPSGENIE_API_TOKEN` env variable.
* Rename `API_TOKEN` env variable to `SLACK_API_TOKEN`.

## v0.6.2

* Fix caching in Docker builds.

## v0.6.1

* Freeze and cache in the Docker image the dependencies used in the type zoo
  (e.g. `types/zoo/teams.dhall`). This way the Dhall prelude functions used
  within those files don't need to be downloaded when the files are used during
  runtime.

## v0.6.0

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
* There's also a folder `./types/zoo` for reusable Dhall functions that build
  on top of the main, flexible API. For example, the previous interface is
  still usable through the `teams` Dhall function provided by
  `./types/zoo/teams.dhall` (with the small change of the `team` key being
  renamed to `name`).

## v0.5.0

* Change `list-caretakers` command to be `ensure --dry-run` instead. Also
  update the output to show everything that the `ensure` command would
  _ensure_.
* Remove global caretaker group, consisting of caretakers from all teams.
  If you were using take-care previously, then you will have to disable
  the existing "caretakers" user group manually on Slack.
