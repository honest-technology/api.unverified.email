#!/bin/bash
set -e
crond
# shellcheck disable=SC2068
exec tini -g -- /opt/bin/unverified-email-api ${@}
