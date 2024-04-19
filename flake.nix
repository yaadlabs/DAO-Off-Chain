{
  description = "triphut-dao-offchain";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    ctl = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      rev = "f2e0f90d269815448f2fb7c6741b9ad25d095d7d";
    };

    nixpkgs.follows = "ctl/nixpkgs";

    lbf.url = "github:mlabs-haskell/lambda-buffers";

    dao-onchain.url = "github:yaadlabs/DAO";
  };

  outputs = { self, nixpkgs, ctl, lbf, ... }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      spago-2-nix-overlay = final: prev: {
        dao-onchain-ctl-types = final.stdenv.mkDerivation {
          name = "dao-onchain-ctl-types";
          version = inputs.dao-onchain.shortRev;
          src = "inputs.dao-onchain.packages.x86_64-linux.dao-lb-plutus-types-purescript";
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };
      };
      
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          ctl.overlays.purescript
          ctl.overlays.runtime
          ctl.overlays.spago
          spago-2-nix-overlay
        ];
      };

      runtimeConfig = { };
      
      psProjectFor = pkgs:
        pkgs.purescriptProject rec {
          inherit pkgs;
          projectName = "triphut-dao-offchain";
          packageJson = ./package.json;
          packageLock = ./package-lock.json;
          src = builtins.path {
            path = ./.;
            name = "${projectName}-src";
            # Adjust the `filter` as necessary
            filter = path: ftype: !(pkgs.lib.hasSuffix ".md" path);
          };
          shell = {
            withRuntime = true;
            packageLockOnly = true;
            packages = with pkgs; [
              fd
              nodePackages.eslint
              nodePackages.prettier
            ];

          };
        };
    in
    {
      packages = perSystem (system:
        let
          nixPkgs = nixpkgsFor system;

          pkgs = nixPkgs;
        in
        {
          default = self.packages.${system}.dao-bundle-web;

          dao-bundle-web-esbuild = (psProjectFor pkgs).bundlePursProjectEsbuild {
            main = "Dao.Main";
          };

          dao-bundle-web-webpack = (psProjectFor pkgs).bundlePursProjectWebpack {
            main = "Dao.Main";
          };

          dao-offchain-bundle-web = {
            mainModule = "Dao.Web.Api";
            entrypointJs = "index.ts";
            webpackConfig = "webpack.config.lib.cjs";
            bundledModuleName = "library.js";
            enableCheck = true;
          };

          dao-runtime = pkgs.buildCtlRuntime runtimeConfig;
        });


      apps = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          default = self.apps.${system}.dao-runtime;
          dao-runtime = pkgs.launchCtlRuntime runtimeConfig;
          docs = (psProjectFor pkgs).launchSearchablePursDocs { };
        });

      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          dao-plutip-test = (psProjectFor pkgs).runPlutipTest {
            testMain = "Test.Dao.Main";
          };

          formatting-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = with pkgs; [
                fd
                easy-ps.purs-tidy
                nixpkgs-fmt
                nodePackages.prettier
              ];
            }
            ''
              cd ${self}
              purs-tidy check $(fd -epurs)
              nixpkgs-fmt --check $(fd -enix --exclude='spago*')
              prettier -c $(fd -ejs)
              touch $out
            '';

          js-lint-check = pkgs.runCommand "js-lint-check"
            {
              nativeBuildInputs = [ pkgs.nodePackages.eslint pkgs.fd ];
            }
            ''
              cd ${self}
              eslint $(fd -ejs)
              touch $out
            '';
        });

      devShells = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          default = (psProjectFor pkgs).devShell;
        });
    };

  nixConfig.bash-prompt = "[\\[\\e[0;1m\\]\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
}
