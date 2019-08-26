SHELL=/bin/bash -o pipefail -o errexit -o nounset
include infra/scripts/variables.sh

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
	infra/scripts/build-containers.sh

.PHONY: deploy
deploy:
	infra/scripts/deploy.sh
