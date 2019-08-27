#!/bin/bash

set -o pipefail
set -eux
source infra/scripts/variables.sh

function _goal_build() {
  stack build
}

function _goal_test() {
  stack test
}

function _goal_dist() {
  mkdir -p ./infra/api/dist/ && rm -rf ./infra/api/dist/*
  cp "$(stack path --local-install-root)/bin/unverified-email-api" "./infra/api/dist/"
  strip ./infra/api/dist/*
}

function _goal_containers() {
  infra/scripts/build-containers.sh
}

function _goal_deploy() {
  infra/scripts/deploy.sh
}

function _goal_help() {
  cat <<EOHELP
  usage do <target>

  Targets:
  build                - build the project
  test                 - build and test the project

  dist                 - copy the artifacts into ./infra/api/dist

  containers           - build containers
  deploy               - deploy the containers
EOHELP
}

for target in "${@}"
do
    eval "_goal_${target}"
done
