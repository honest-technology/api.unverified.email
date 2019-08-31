FROM frolvlad/alpine-glibc:latest

RUN apk --no-cache add \
        ca-certificates \
        gmp \
        tini

COPY dist/unverified-email-api /opt/bin/unverified-email-api
ENV PORT=80 API_URL="api.unverified.email" INFO_PAGE_URL="unverified.email" MAILDIR="/mailbox/unverified/"

EXPOSE 80/tcp

ENTRYPOINT ["tini", "-g", "--", "/opt/bin/unverified-email-api"]
