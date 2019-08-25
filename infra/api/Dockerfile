FROM alpine:3.10.1

RUN apk --no-cache add \
    ca-certificates \
    bash \
    curl \
    libc6-compat \
    gmp

COPY dist/unverified-email-api /opt/bin/unverified-email-api
ENV PORT=80 API_URL="api.unverified.email" INFO_PAGE_URL="unverified.email" MAILDIR="/maildir/unverified.email"

EXPOSE 80/tcp
ENTRYPOINT ["/opt/bin/unverified-email-api"]
