#!/bin/bash
set -euo pipefail
cd "${0%/*}/.."

main() {
  local version
  version=$(validate_input "$@")
  validate_git_state

  update_changelog "$version"
  update_package_file "$version"
  update_readme "$version"

  log_next_steps "$version"
}

update_changelog() {
  local version
  version=$1
  sed -i '' -e "s/## Unreleased changes/## Unreleased changes\\n\\n## v${version}/" ChangeLog.md
}

update_package_file() {
  local version
  version=$1
  sed -i '' -E "s/^(version:[[:space:]]*)[^[:space:]]+$/\1${version}/" package.yaml
}

update_readme() {
  local version
  version=$1
  sed -i '' -E "s|(https://github.com/deiwin/take-care/raw/v)[^/]+/|\1${version}/|" README.md
}

log_next_steps() {
local version
version=$1

log << EOF
Validate the changes. If everything looks OK, run the following commands to create the release:

git add .
git commit -m 'Update version to v${version}'
git tag v${version}
git push origin v${version}
git push
EOF
}

validate_input() {
  local version
  if [ $# -eq 1 ] && grep -Eq '^\d+\.\d+\.\d+$' <<< "$1"; then
    version="$1"
  else
    log_usage
    exit 1
  fi
  printf '%s' "$version"
}

validate_git_state() {
  if [ "$(git rev-parse --abbrev-ref HEAD)" != 'master' ]; then
    log 'The script should only be run from the master branch. Please check out the master branch and try again.'
    exit 1
  elif git fetch && ! git merge-base --is-ancestor origin/master master; then
    log 'origin/master has changed. Please rebase and try again.'
    exit 1
  elif [ -n "$(git status --porcelain)" ]; then
    log 'The working directory is not clean. Please commit or revert any changes and try again.'
    exit 1
  fi
}

log_usage() {
  log 'Usage: script/release.sh X.Y.Z'
}

log() {
  if [ $# -eq 0 ]; then
    cat 1>&2
  else
    echo "$@" 1>&2
  fi
}

main "$@"; exit
