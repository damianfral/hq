{
  description = "Haskell CLI flake template";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    feedback.url = "github:NorfairKing/feedback";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    pre-commit-hooks,
    feedback,
    ...
  }: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [self.overlays.default];
      };
    filteredSrc = nix-filter.lib {
      root = ./.;
      include = [
        "app/"
        "src/"
        "test/"
        "package.yaml"
        "LICENSE"
      ];
    };
  in
    {
      overlays.default = final: prev: {
        hq = final.haskell.lib.justStaticExecutables (
          final.haskellPackages.hq.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ ["--ghc-options=-O2"];
          })
        );
        haskellPackages = prev.haskellPackages.override (old: {
          overrides =
            final.lib.composeExtensions
            (old.overrides or (_: _: {}))
            (self: super: {
              hq =
                self.generateOptparseApplicativeCompletions
                ["hq"]
                (self.callCabal2nix "hq" filteredSrc {});
            });
        });
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = pkgsFor system;
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            actionlint.enable = true;
            alejandra.enable = true;
            beautysh.enable = true;
            check-merge-conflicts.enable = true;
            hlint.enable = true;
            hpack.enable = true;
            markdownlint.enable = true;
            nil.enable = true;
            ormolu.enable = true;
            ripsecrets.enable = true;
            shellcheck.enable = true;
            statix.enable = true;
          };
        };
      in rec {
        packages.hq = pkgs.hq;
        packages.default = packages.hq;

        apps.hq = flake-utils.lib.mkApp {drv = pkgs.hq;};
        apps.default = apps.hq;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [p.hq];
          buildInputs = with pkgs;
          with pkgs.haskellPackages; [
            actionlint
            alejandra
            cabal-install
            ghcid
            haskell-language-server
            hlint
            feedback.packages.${system}.default
            nil
            ormolu
            statix
          ];
          inherit (precommitCheck) shellHook;
        };

        checks = {pre-commit-check = precommitCheck;};
      }
    );
  nixConfig = {
    extra-substituters = [
      "https://opensource.cachix.org"
      "https://haskell-language-server.cachix.org"
      "https://feedback.cachix.org"
    ];
    extra-trusted-public-keys = [
      "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc="
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "feedback.cachix.org-1:8PNDEJ4GTCbsFUwxVWE/ulyoBMDqqL23JA44yB0j1jI="
    ];
  };
}
