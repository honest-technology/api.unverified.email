#!/bin/bash

set -o pipefail
set -e
set -u

source infra/variables.sh

function _goal_build() {
  stack build
}

function _goal_build_in_docker() {
  docker_stack build
}

function _goal_test() {
  stack test
}

function docker_stack {
  docker run -v "$(pwd):$(pwd)" -v ${HOME}/.stack:/root/.stack --workdir "$(pwd)" haskell:8.6.5 stack --system-ghc "$@"
}

function _goal_dist() {
  mkdir -p ./infra/api/dist/ && rm -rf ./infra/api/dist/*
  if [ "$(uname)" = "Linux" ]; then
    _goal_build
    cp "$(stack path --local-install-root)/bin/unverified-email-api" "./infra/api/dist/"
  else
    _goal_build_in_docker
    cp "$(docker_stack path --local-install-root)/bin/unverified-email-api" "./infra/api/dist/"
  fi
}

function _goal_containers() {
  docker build infra/smtpd -t "${ENVSUBST_IMAGE_SMTPD}"
  docker build infra/api -t "${ENVSUBST_IMAGE_API}"
}

function _goal_deploy() {
  SSH="ssh -oStrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null ${REMOTE}"
  docker save "${ENVSUBST_IMAGE_SMTPD}" | bzip2 -9 | ${SSH} 'bunzip2 | docker load'
  docker save "${ENVSUBST_IMAGE_API}" | bzip2 -9 | ${SSH} 'bunzip2 | docker load'

  ${SSH} 'mkdir -p /opt/unverified.email/'
  local envs_to_substitute
  envs_to_substitute=$(set | grep ENVSUBST_ | awk -F'[=]' '{ print "$" $1 }')
  envsubst "${envs_to_substitute}" < infra/nomad-definitions.hcl | ${SSH} \
    'cat > /opt/unverified.email/nomad-definitions.hcl && '\
    'mkdir -p /opt/unverified.email/traefik/letsencrypt/ && '\
    'touch /opt/unverified.email/traefik/letsencrypt/acme.json && '\
    'chmod 700 /opt/unverified.email/traefik/letsencrypt/ && '\
    'chmod 600 /opt/unverified.email/traefik/letsencrypt/* && '\
    'nomad job run -verbose /opt/unverified.email/nomad-definitions.hcl'

  sleep 5
  ${SSH} "nomad deployment list -json | jq -r '.[0] .ID' | xargs nomad deployment status -json | jq '.Status'"
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
[[ -z "${*}" ]] && _goal_help
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
