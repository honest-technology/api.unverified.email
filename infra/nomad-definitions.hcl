job "unverified.email" {
  datacenters = ["dc1"]

  group "cluster-1" {
    task "api-1" {
      driver = "docker"

      config {
        image = "hashicorp/http-echo:latest"
        args = [
          "-listen", ":80",
          "-text", "This is api.unverified.email",
        ]
        network_mode = "host"
      }

      resources {
        network {
          port "http" {
            static = "80"
          }
        }
      }
    }

    task "smtpd-1" {
      driver = "docker"

      config {
        image = "unverified.email/smtp:unversioned"
        network_mode = "host"
        force_pull = "false"
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
