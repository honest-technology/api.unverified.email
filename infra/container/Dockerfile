FROM alpine:3.10.1

RUN apk --no-cache add \
    ca-certificates \
    bash \
    curl \
    opensmtpd
RUN adduser -D -H unverified && \
    mkdir -p /mailbox/unverified && \
    chown unverified:unverified /mailbox/unverified


COPY ./smtpd.conf /etc/smtpd

EXPOSE 25
CMD ["/usr/sbin/smtpd", "-d"]


