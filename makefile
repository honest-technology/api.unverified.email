REMOTE="root@api.unverified.email"
IMAGE_SMTP="unverified.email/smtp:unversioned"
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

.PHONY: containers
containers:
	docker build infra/container -t ${IMAGE_SMTP}

.PHONY: deploy
deploy:
	ssh -i ${SSH_PRIVATE_KEY} -oStrictHostKeyChecking=no ${REMOTE} 'mkdir -p /opt/unverified.email/'
	docker save ${IMAGE_SMTP} | bzip2 -9 | pv | ssh -i ${SSH_PRIVATE_KEY} -oStrictHostKeyChecking=no ${REMOTE} 'bunzip2 | docker load'
	cat infra/nomad-definitions.hcl | ssh -i ${SSH_PRIVATE_KEY} -oStrictHostKeyChecking=no ${REMOTE} 'cat > /opt/unverified.email/nomad-definitions.hcl'
	ssh -i ${SSH_PRIVATE_KEY} -oStrictHostKeyChecking=no ${REMOTE} 'nomad job run /opt/unverified.email/nomad-definitions.hcl'
