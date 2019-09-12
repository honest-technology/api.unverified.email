job "unverified-email" {
  datacenters = ["dc1"]

  type = "service"

  update {
    max_parallel = 1
    min_healthy_time = "10s"
    healthy_deadline = "3m"
    auto_revert = false
    canary = 0
  }

  group "cluster-1" {

    task "traefik" {
      driver = "docker"

      config {
        image = "traefik:v2.0"
        args = [
          "--global.sendanonymoususage=false",
          "--api=true",
          "--providers.docker.exposedbydefault=false",
          "--entrypoints.web.address=:80",
          "--entrypoints.websecure=true",
          "--entrypoints.websecure.address=:443",
          "--certificatesresolvers.challenge=true",
          "--certificatesresolvers.challenge.acme.email=pavlo@kerestey.net",
          "--certificatesresolvers.challenge.acme.storage=/letsencrypt/acme.json",
          "--certificatesresolvers.challenge.acme.httpchallenge=true",
          "--certificatesresolvers.challenge.acme.httpchallenge.entryPoint=web",
        ]
        mounts = [
          {
            type = "bind"
            target = "/var/run/docker.sock"
            source = "/var/run/docker.sock"
            readonly = false
          },
          {
            type = "bind"
            target = "/letsencrypt"
            source = "/opt/unverified.email/traefik/letsencrypt"
            readonly = false
          }
        ]
        network_mode = "host"
      }

      resources {
        network {
          port "http" {
            static = 80
          }
          port "https" {
            static = 443
          }
        }
      }
    }

    task "api-1" {
      driver = "docker"

      config {
        image = "${IMAGE_API}"
        force_pull = "false"
        port_map {
          http = 80
        }
        mounts = [
          {
            type = "volume"
            target = "/mailbox/unverified/"
            source = "mailbox-unverified"
            readonly = false
          }
        ]
        labels = {
          "traefik.enable" = "true",
          "traefik.http.routers.api-1.rule" = "Host(`api.unverified.email`)",
          "traefik.http.routers.api-1.entrypoints" = "websecure",
          "traefik.http.routers.api-1.tls.certresolver" = "challenge"
        }
      }

      resources {
        network {
          port "http" {}
        }
      }
    }

    task "smtpd-1" {
      driver = "docker"

      config {
        image = "${IMAGE_SMTPD}"
        network_mode = "host"
        force_pull = "false"
        mounts = [
          {
            type = "volume"
            target = "/mailbox/unverified/"
            source = "mailbox-unverified"
            readonly = false
          }]
      }

      resources {
        network {
          port "smtp" {
            static = "25"
          }
        }
      }
    }
  }
}
