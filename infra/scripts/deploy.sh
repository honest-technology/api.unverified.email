#!/bin/bash

set -e
set -u
set -o pipefail
set -x

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# shellcheck source=infra/variables.sh
source "${DIR}/../variables.sh"

docker save "${IMAGE_SMTPD}" | bzip2 -9 | ssh -oStrictHostKeyChecking=no "${REMOTE}" 'mkdir -p /opt/unverified.email/; bunzip2 | docker load'
docker save "${IMAGE_API}" | bzip2 -9 | ssh -oStrictHostKeyChecking=no "${REMOTE}" 'mkdir -p /opt/unverified.email/; bunzip2 | docker load'

envsubst < infra/docker-compose.yaml | ssh -oStrictHostKeyChecking=no "${REMOTE}" "cat > /opt/unverified.email/docker-compose.yaml"
ssh -oStrictHostKeyChecking=no "${REMOTE}" 'cd /opt/unverified.email/ && docker-compose up -d'

sleep 5
ssh -oStrictHostKeyChecking=no "${REMOTE}" 'docker ps -a'

