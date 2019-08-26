set -e
set -u
set -o pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# shellcheck source=infra/scripts/lib.shs
source "${DIR}/variables.sh"

docker build infra/smtpd -t ${IMAGE_SMTPD}
docker build infra/api -t ${IMAGE_API}
