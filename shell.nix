{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, containers, esqueleto
      , extensible-exceptions, monad-logger, persistent
      , persistent-sqlite, persistent-template, resourcet, servant
      , servant-server, stdenv, text, time, transformers, wai, wai-extra
      , warp, X11
      }:
      mkDerivation {
        pname = "TimeLogger";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson async base containers esqueleto extensible-exceptions
          monad-logger persistent persistent-sqlite persistent-template
          resourcet servant servant-server text time transformers wai
          wai-extra warp X11
        ];
        description = "Time logger";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
