#!/bin/sh
set -e
crond
# shellcheck disable=SC2068
exec /opt/bin/unverified-email-api ${@}
