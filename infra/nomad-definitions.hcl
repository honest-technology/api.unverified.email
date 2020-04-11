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

  group "services" {

    task "traefik" {
      driver = "docker"

      config {
        image = "traefik:v2.2"
        args = [
          "--global.sendanonymoususage=false",
          "--api=true",
          "--api.insecure=true",
          "--accesslog=true",
          "--log.level=DEBUG",

          "--providers.docker.exposedbydefault=false",
          "--metrics.prometheus=true",
          "--metrics.prometheus.entryPoint=metrics-traefik",
          "--entryPoints.web.address=:80",
          "--entryPoints.websecure.address=:443",
          "--entryPoints.metrics-api.address=:8081",
          "--entryPoints.metrics-traefik.address=:8082",

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
          port "http" { static = "80" }
          port "https" { static = "443" }
          port "traefik_api" { static = "8080" }
          port "metrics_api" { static = "8081" }
          port "metrics_traefik" { static = "8082" }
        }
      }
    }

    task "api" {
      driver = "docker"

      config {
        image = "${ENVSUBST_IMAGE_API}"
        force_pull = "false"
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

          "traefik.http.routers.api.service"="api",
          "traefik.http.routers.api.rule" = "Host(`api.unverified.email`)",
          "traefik.http.routers.api.entrypoints" = "websecure",
          "traefik.http.routers.api.tls.certresolver" = "challenge",

          "traefik.http.routers.metrics-api.service"="metrics-api",
          "traefik.http.routers.metrics-api.rule" = "Host(`api.unverified.email`)",
          "traefik.http.routers.metrics-api.entrypoints" = "metrics-api",
          "traefik.http.routers.metrics-api.tls.certresolver" = "challenge",

          "traefik.http.services.api.loadBalancer.server.port" = "80",
          "traefik.http.services.metrics-api.loadBalancer.server.port" = "8081",
        }
        network_mode = "bridge"
        port_map {
          http = 80
          metrics = 8081
        }
      }

      resources {
        network {
          port "http" {}
          port "metrics" {}
        }
      }
    }

    task "smtpd-1" {
      driver = "docker"

      config {
        image = "${ENVSUBST_IMAGE_SMTPD}"
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
