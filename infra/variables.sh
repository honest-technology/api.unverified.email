#!/bin/bash
export REMOTE="root@api.unverified.email"
COMMIT_ID="$(git rev-parse HEAD)" && export COMMIT_ID
export IMAGE_SMTPD="unverified.email/smtpd:${COMMIT_ID}"
export IMAGE_API="unverified.email/api:${COMMIT_ID}"

