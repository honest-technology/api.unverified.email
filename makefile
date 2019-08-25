REMOTE="root@api.unverified.email"
IMAGE_SMTPD="unverified.email/smtpd:unversioned"
IMAGE_API="unverified.email/api:unversioned"
SSH_PRIVATE_KEY="./secrets/unverified.email-deployment-ed25519"
SHELL=/bin/bash -o pipefail -o errexit -o nounset

.PHONY: all
all: build

.PHONY: build
build:
	stack build

.PHONY: test
test:
	stack test

.PHONY: dist
dist:
	mkdir -p ./infra/api/dist/ && rm -rf ./infra/api/dist/*
	cp "$(shell stack path --local-install-root)/bin/unverified-email-api" "./infra/api/dist/"
	strip ./infra/api/dist/*

.PHONY: containers
containers:
	docker build infra/smtpd -t ${IMAGE_SMTPD}
	docker build infra/api -t ${IMAGE_API}

# .PHONY: deploy
# deploy:
# 	ssh -i ${SSH_PRIVATE_KEY} -oStrictHostKeyChecking=no ${REMOTE} 'mkdir -p /opt/unverified.email/'
# 	docker save ${IMAGE_SMTPD} | bzip2 -9 | pv | ssh -i ${SSH_PRIVATE_KEY} -oStrictHostKeyChecking=no ${REMOTE} 'bunzip2 | docker load'
# 	docker save ${IMAGE_API} | bzip2 -9 | pv | ssh -i ${SSH_PRIVATE_KEY} -oStrictHostKeyChecking=no ${REMOTE} 'bunzip2 | docker load'
#     cat infra/nomad-definitions.hcl | ssh -i ${SSH_PRIVATE_KEY} -oStrictHostKeyChecking=no ${REMOTE} 'cat > /opt/unverified.email/nomad-definitions.hcl'
# 	ssh -i ${SSH_PRIVATE_KEY} -oStrictHostKeyChecking=no ${REMOTE} 'nomad job run /opt/unverified.email/nomad-definitions.hcl'
