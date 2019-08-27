#!/bin/bash

set -e
set -u
set -o pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# shellcheck source=infra/scripts/lib.shs
source "${DIR}/variables.sh"

docker save ${IMAGE_SMTPD} | bzip2 -9 | pv | ssh -oStrictHostKeyChecking=no ${REMOTE} 'mkdir -p /opt/unverified.email/; bunzip2 | docker load'
docker save ${IMAGE_API} | bzip2 -9 | pv | ssh -oStrictHostKeyChecking=no ${REMOTE} 'mkdir -p /opt/unverified.email/; bunzip2 | docker load'
scp -oStrictHostKeyChecking=no "infra/nomad-definitions.hcl" "${REMOTE}:/opt/unverified.email/nomad-definitions.hcl"
ssh -oStrictHostKeyChecking=no "${REMOTE}" 'nomad job run /opt/unverified.email/nomad-definitions.hcl'

