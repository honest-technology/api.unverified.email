FROM ubuntu:rolling

RUN apt-get update && \
    apt-get install -y ca-certificates wget libgmp10 zlib1g

COPY dist/unverified-email-api /opt/bin/unverified-email-api
ENV PORT=80 API_URL="api.unverified.email" INFO_PAGE_URL="unverified.email" MAILDIR="/maildir/unverified.email"

EXPOSE 80/tcp
ENTRYPOINT ["/opt/bin/unverified-email-api"]
