job "unverified-email" {
  datacenters = ["dc1"]

  update {
    max_parallel = 1
    min_healthy_time = "10s"
    healthy_deadline = "3m"
    auto_revert = false
    canary = 0
  }

  group "cluster-1" {

    task "reverse-proxy-1" {
      driver = "docker"

      config {
        image = "traefik:v2.0"
        args = [
          "--api=true",
          "--providers.docker=true",
          "--providers.docker.exposedbydefault=false",
          "--providers.docker.endpoint='unix:///var/run/docker.sock'",
          "--entryPoints.web.address=':80'",
          "--entryPoints.websecure.address=':443'",
          "--certificatesResolvers.challenge.acme.email: pavlo@kerestey.net",
          "--certificatesResolvers.challenge.acme.storage: /letsencrypt/acme.json",
          "--certificatesResolvers.challenge.acme.httpChallenge=true",
          "--certificatesResolvers.challenge.acme.httpChallenge.entryPoint: web",
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
            readonly = true
          }
        ]
      }

      resources {
        network {
          port "http" {}
        }
      }

      service {
        tags = [
          "traefik.enable=true",
          "traefik.http.routers.api-1.rule=Host(`api.unverified.email`)",
          "traefik.http.routers.api-1.entrypoints=websecure",
          "traefik.http.routers.api-1.tls.certresolver=challenge"
        ]
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
