{
  description = "Haskell CLI flake template";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    feedback.url = "github:NorfairKing/feedback";
    json-data = {
      url = "https://github.com/antonmedv/json-examples/raw/master/data_100mb.json";
      flake = false;
    };
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.inputs = {
      nixpkgs.follows = "nixpkgs";
      pre-commit-hooks.follows = "pre-commit-hooks";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    pre-commit-hooks,
    feedback,
    json-data,
    weeder-nix,
    ...
  }: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [self.overlays.default];
      };
    filteredSrc = nix-filter.lib {
      root = ./.;
      include = ["app" "src/" "test/" "package.yaml" "LICENSE"];
    };
  in
    {
      overlays.default = final: prev: let
        # Haskell package set with profiling enabled throughout the
        # dependency closure, so executables can be linked for profiling.
        profHaskellPackages = prev.haskell.packages.ghc9124.override (old: {
          overrides =
            final.lib.composeExtensions
            (old.overrides or (_: _: {}))
            (pself: psuper: {
              mkDerivation = args:
                psuper.mkDerivation (args
                  // {
                    enableLibraryProfiling = true;
                    enableExecutableProfiling = true;
                    # Test suites (and benchmarks) of dependencies are not
                    # needed here, and some fail to link when built with
                    # profiling objects only (e.g. Template Haskell tests
                    # needing .dyn_o files).
                    doCheck = false;
                    doBenchmark = false;
                    doHaddock = false;
                  });
              hq =
                pself.generateOptparseApplicativeCompletions
                ["hq"]
                (pself.callCabal2nix "hq" filteredSrc {});
            });
        });
      in {
        hq = final.haskell.lib.justStaticExecutables (
          final.haskellPackages.hq.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ ["--ghc-options=-O2"];
            # jq on PATH for the HQ.JQParitySpec suite (native input only:
            # it must not ship with the hq closure).
            nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [final.jq];
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
        # Profiling build of hq: dependencies come with profiling
        # libraries from profHaskellPackages and hq itself gets cost
        # centers, so running it with +RTS -p writes usable .prof files.
        # (-rtsopts -threaded already come from package.yaml.)
        hq-prof = final.haskell.lib.justStaticExecutables (
          profHaskellPackages.hq.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ ["--ghc-options=-O2" "--ghc-options=-fprof-auto"];
          })
        );
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = pkgsFor system;
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            # actionlint.enable = true;
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
            # statix.enable = true;
          };
        };
      in rec {
        packages.hq = pkgs.hq;
        packages.default = packages.hq;
        packages.hq-prof = pkgs.hq-prof;
        packages.hq-bench-profile = pkgs.stdenv.mkDerivation {
          name = "hq-bench-profile";
          pname = "hq-bench-profile";
          version = "0.0.0.1";
          dontUnpack = true;
          buildInputs = [packages.hq-bench pkgs.hq-prof];
          # Run the hq benchmark commands (same list as hq-bench-data
          # uses) under the profiling RTS; jq commands are skipped since
          # only the Haskell binary is built for profiling. Each run
          # writes hq.prof, renamed per command to avoid clobbering.
          buildPhase = ''
            set -xue
            hq-bench --list | grep '^hq ' > commands.txt
            i=0
            while IFS= read -r cmd; do
              i=$((i + 1))
              prof=$(printf 'hq-%02d.prof' "$i")
              echo "profiling ($i): $cmd"
              bash -c "${pkgs.hq-prof}/bin/hq +RTS -p -RTS ''${cmd#hq }" <${json-data} >/dev/null
              mv hq.prof "./$prof"
            done < commands.txt
          '';
          installPhase = "mkdir $out && cp -t $out commands.txt hq-*.prof";
        };
        packages.hq-bench = pkgs.stdenv.mkDerivation {
          name = "hq-bench";
          pname = "hq-bench";
          version = "0.0.0.1";
          src = ./bench;
          dontUnpack = true;
          nativeBuildInputs = [pkgs.makeWrapper];
          buildInputs = [pkgs.hq pkgs.jq pkgs.time];
          buildPhase = ''
            set -xue
            mkdir -p "$out/bin"
            cp "$src/bench.sh" "$out/bin/hq-bench"
            chmod +x "$out/bin/hq-bench"
            wrapProgram "$out/bin/hq-bench" \
              --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.bash pkgs.time pkgs.hq pkgs.jq pkgs.coreutils]}
          '';
        };
        packages.hq-bench-data = pkgs.stdenv.mkDerivation {
          name = "hq-bench-data";
          pname = "hq-bench-data";
          src = json-data;
          version = "0.0.0.1";
          dontUnpack = true;
          buildInputs = [packages.hq-bench];
          buildPhase = ''
            set -xue
            hq-bench $src hq-bench.csv 3
          '';
          installPhase = "mkdir $out && cp hq-bench.csv $out/";
        };
        packages.hq-bench-charts = pkgs.stdenv.mkDerivation {
          name = "hq-bench-charts";
          pname = "hq-bench-charts";
          version = "0.0.0.1";
          dontUnpack = true;
          nativeBuildInputs = [pkgs.vega-lite];
          buildPhase = ''
            set -xue
            cp ${packages.hq-bench-data}/hq-bench.csv hq-bench.csv
            cp ${./bench/bench_runtime.vl.json} bench_runtime.vl.json
            cp ${./bench/bench_memory.vl.json} bench_memory.vl.json
            vl2svg bench_runtime.vl.json bench_runtime.svg
            vl2svg bench_memory.vl.json bench_memory.svg
          '';
          installPhase = "mkdir $out && cp -t $out bench_runtime.svg bench_memory.svg";
        };

        apps.hq = flake-utils.lib.mkApp {drv = pkgs.hq;};
        apps.hq-bench = flake-utils.lib.mkApp {drv = packages.hq-bench;};
        apps.default = apps.hq;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [p.hq];
          buildInputs = with pkgs;
          with pkgs.haskellPackages; [
            # actionlint
            alejandra
            cabal-install
            ghcid
            haskell-language-server
            hlint
            feedback.packages.${system}.default
            nil
            ormolu
            statix
            # jq on PATH for local `cabal test` (HQ.JQParitySpec suite).
            pkgs.jq
          ];
          inherit (precommitCheck) shellHook;
        };

        checks = {
          pre-commit-check = precommitCheck;
          weeder-check = weeder-nix.lib.${system}.makeWeederCheck {
            haskellPackages = pkgs.haskellPackages;
            packages = ["hq"];
            reportOnly = true;
          };
        };
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
