FROM alpine:3.11

RUN apk --no-cache add \
    ca-certificates \
    bash \
    curl \
    opensmtpd \
    tini
RUN adduser -D -H unverified && \
    mkdir -p /mailbox/unverified && \
    chown unverified:unverified /mailbox/unverified

COPY ./smtpd.conf /etc/smtpd

EXPOSE 25/tcp

ENTRYPOINT ["tini", "-g", "--"]
CMD ["/usr/sbin/smtpd", "-d"]
