#!/bin/bash

set -o pipefail
set -eux

source infra/variables.sh

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
  docker build infra/smtpd -t "${IMAGE_SMTPD}"
  docker build infra/api -t "${IMAGE_API}"
}

function _goal_deploy() {
  docker save "${IMAGE_SMTPD}" | bzip2 -9 | ssh -oStrictHostKeyChecking=no "${REMOTE}" 'mkdir -p /opt/unverified.email/; bunzip2 | docker load'
  docker save "${IMAGE_API}" | bzip2 -9 | ssh -oStrictHostKeyChecking=no "${REMOTE}" 'mkdir -p /opt/unverified.email/; bunzip2 | docker load'

  envsubst < infra/docker-compose.yaml | ssh -oStrictHostKeyChecking=no "${REMOTE}" "cat > /opt/unverified.email/docker-compose.yaml"
  ssh -oStrictHostKeyChecking=no "${REMOTE}" 'cd /opt/unverified.email/ && docker-compose up -d --force-recreate'

  sleep 5
  ssh -oStrictHostKeyChecking=no "${REMOTE}" 'docker ps -a'
}

function _goal_linter-sh() {
  shellcheck -x ./do
  find . | grep '.sh$' | xargs shellcheck -x
}

function _goal_linters() {
  _goal_linter-sh
}

function _goal_help() {
  cat <<EOHELP
  usage do <target>

  Targets:
  build                - build the project
  test                 - build and test the project

  linters              - run all linters

  dist                 - copy the artifacts into ./infra/api/dist

  containers           - build containers
  deploy               - deploy the containers
EOHELP
}

for target in "${@}"
do
    eval "_goal_${target}"
done
