job "unverified.email" {
  datacenters = ["dc1"]

  group "cluster-1" {
    task "api-1" {
      driver = "docker"

      config {
        image = "unverified.email/api:unversioned"
        network_mode = "host"
        force_pull = "false"
        mounts = [{
            type = "volume"
            target = "/mailbox/unverified/"
            source = "mailbox-unverified"
            readonly = true
        }]
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
        mounts = [{
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
