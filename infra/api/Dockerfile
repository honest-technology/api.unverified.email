FROM frolvlad/alpine-glibc:latest

RUN apk --no-cache add \
        ca-certificates \
        gmp \
        tini

COPY entrypoint.sh /entrypoint.sh
ENV PORT=80 API_URL="api.unverified.email" INFO_PAGE_URL="https://github.com/ptek/api.unverified.email" MAILDIR="/mailbox/unverified/"
RUN echo '* * * * * find ${MAILDIR} -type f -mmin +5 -print | xargs -I{} sh -c "rm -f {}"' >> /etc/crontabs/root

COPY dist/unverified-email-api /opt/bin/unverified-email-api

EXPOSE 80/tcp

ENTRYPOINT ["tini", "-g", "--", "/entrypoint.sh"]
