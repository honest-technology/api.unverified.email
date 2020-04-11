FROM frolvlad/alpine-glibc:alpine-3.11_glibc-2.30

RUN apk --no-cache add \
        ca-certificates \
        gmp \
        tini

COPY entrypoint.sh /entrypoint.sh
RUN echo '* * * * * find ${MAILDIR} -type f -mmin +5 -print | xargs -I{} sh -c "rm -f {}"' >> /etc/crontabs/root
ENV PORT=80 \
    PORT_PROMETHEUS=8081 \
    API_URL="api.unverified.email" \
    SMTP_URL="smtp.unverified.email:25" \
    INFO_PAGE_URL="https://github.com/ptek/api.unverified.email" \
    MAILDIR="/mailbox/unverified/"

COPY dist/unverified-email-api /opt/bin/unverified-email-api

EXPOSE 80/tcp
EXPOSE 8081/tcp

ENTRYPOINT ["tini", "-g", "--", "/entrypoint.sh"]
