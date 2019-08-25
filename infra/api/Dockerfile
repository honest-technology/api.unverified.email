FROM alpine:3.10.1

RUN apk --no-cache add ca-certificates wget \
    wget -q -O /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub \
    wget https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.30-r0/glibc-2.30-r0.apk \
    apk add glibc-2.30-r0.apk gmp

COPY dist/unverified-email-api /opt/bin/unverified-email-api
ENV PORT=80 API_URL="api.unverified.email" INFO_PAGE_URL="unverified.email" MAILDIR="/maildir/unverified.email"

EXPOSE 80/tcp
ENTRYPOINT ["/opt/bin/unverified-email-api"]
