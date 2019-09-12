#!/bin/bash

set -o pipefail
set -e
set -u

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
  SSH="ssh -oStrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null ${REMOTE}"
  docker save "${IMAGE_SMTPD}" | bzip2 -9 | ${SSH} 'bunzip2 | docker load'
  docker save "${IMAGE_API}" | bzip2 -9 | ${SSH} 'bunzip2 | docker load'

  ${SSH} 'mkdir -p /opt/unverified.email/'
  envsubst < infra/nomad-definitions.hcl | ${SSH} ' \
    cat > /opt/unverified.email/nomad-definitions.hcl && \
    mkdir -p /opt/unverified.email/traefik/letsencrypt/ && \
    touch /opt/unverified.email/traefik/letsencrypt/acme.json && \
    chmod 700 /opt/unverified.email/traefik/letsencrypt/ && \
    chmod 600 /opt/unverified.email/traefik/letsencrypt/* && \
    nomad job run -verbose /opt/unverified.email/nomad-definitions.hcl'
}

function _goal_linter-sh() {
  shellcheck -x ./do
  find . | grep '.sh$' | xargs shellcheck -x
}

function _goal_linter-hs() {
  find . | grep '.hs$' | xargs stack exec hlint -- -XQuasiQuotes
}

function _goal_linters() {
  _goal_linter-sh
  _goal_linter-hs
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

  exit 1
}

set +u
# shellcheck disable=SC2198
if [ -z "${@}" ]; then _goal_help; fi
set -u
for target in "${@}"
do
  if [ "$(type -t "_goal_${target}")" = function ]
  then
    set -x
    eval "_goal_${target}"
  else
    _goal_help
  fi
done
