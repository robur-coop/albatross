{ config, lib, pkgs, ... }:

let
  parent_conf = config.services.albatross;
  conf = parent_conf.endpoint;
  enabled = parent_conf.enable && conf.enable;
  inherit (parent_conf) package cacert;

in {
  options = with lib;
    with types; {
      services.albatross.endpoint = {
        enable = mkEnableOption "albatross tls endpoint";

        port = mkOption {
          type = port;
          default = 1025;
        };

        cert = mkOption {
          description =
            "TLS certificate used to authenticate the endpoint. Should be signed by the certificate passed to 'services.albatross.ca'.";
          type = path;
        };

        private_key = mkOption {
          description = "Private key corresponding to the 'cert'.";
          type = path;
        };

      };
    };

  config = lib.mkIf enabled {
    systemd.services.albatross-tls-endpoint = {
      description = "Albatross tls endpoint";
      requires = [ "albatrossd.socket" "albatross-tls-endpoint.socket" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        User = "albatross";
        Group = "albatross";
        ExecStart = ''
          ${package}/bin/albatross-tls-endpoint --systemd-socket-activation --tmpdir="%t/albatross/" ${cacert} ${conf.cert} ${conf.private_key}
        '';
      };
    };

    systemd.sockets.albatross-tls-endpoint = {
      description = "Albatross tls endpoint listening for requests";
      partOf = [ "albatross-tls-endpoint.service" ];
      socketConfig = {
        ListenStream = conf.port;
        SocketUser = "albatross";
        SocketMode = "0660";
      };
    };

    networking.firewall.allowedTCPPorts = [ conf.port ];
  };
}
