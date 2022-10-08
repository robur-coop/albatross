{
  inputs.nixpkgs.url = "nixpkgs";
  inputs.opam-nix = {
    url = "github:tweag/opam-nix";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };
  inputs.flake-utils = {
    url = "github:numtide/flake-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, opam-nix, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (opam-nix.lib.${system}) buildOpamProject;
        scope = buildOpamProject { } "albatross" ./. { ocaml-system = "*"; };

        # Prevent unnecessary dependencies on the resulting derivation
        albatross = scope.albatross.overrideAttrs (_: {
          removeOcamlReferences = true;
          doNixSupport = false;
        });
      in {
        packages = { inherit albatross; };
        defaultPackage = albatross;

      }) // {
        nixosModules.albatross = { pkgs, ... }:
          let albatross = self.packages.${pkgs.system}.albatross;
          in {
            imports = [
              (import packaging/nixos/albatross_service.nix albatross)
              packaging/nixos/albatross_tls_endpoint.nix
            ];

          };
      };
}
