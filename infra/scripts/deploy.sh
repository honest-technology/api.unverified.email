#!/bin/bash

set -e
set -u
set -o pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# shellcheck source=infra/scripts/variables.sh
source "${DIR}/variables.sh"

docker save "${IMAGE_SMTPD}" | bzip2 -9 | ssh -oStrictHostKeyChecking=no "${REMOTE}" 'mkdir -p /opt/unverified.email/; bunzip2 | docker load'
docker save "${IMAGE_API}" | bzip2 -9 | ssh -oStrictHostKeyChecking=no  'mkdir -p /opt/unverified.email/; bunzip2 | docker load'

cat infra/nomad-definitions.hcl | envsubst | ssh -oStrictHostKeyChecking=no "${REMOTE}" "cat > /opt/unverified.email/nomad-definitions.hcl"
ssh -oStrictHostKeyChecking=no "${REMOTE}" 'nomad job run /opt/unverified.email/nomad-definitions.hcl'

sleep 5
ssh -oStrictHostKeyChecking=no "${REMOTE}" '(nomad status unverified.email | grep -E "Status\s+= running" > /dev/null && echo "OK") || (nomad status unverified.email; exit 1)'

