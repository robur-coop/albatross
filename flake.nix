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
    flake-utils.lib.eachDefaultSystem (system: {
      legacyPackages = let
        inherit (opam-nix.lib.${system}) buildOpamProject;
        scope = buildOpamProject { } "albatross" ./. { ocaml-system = "*"; };

      in scope.overrideScope' (self: super: {
        # Prevent unnecessary dependencies on the resulting derivation
        albatross = super.albatross.overrideAttrs (_: {
          removeOcamlReferences = true;
          doNixSupport = false;
        });
      });

      defaultPackage = self.legacyPackages.${system}.albatross;

    });
}
