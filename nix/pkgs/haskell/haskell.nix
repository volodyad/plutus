############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ pkgs
, lib
, stdenv
, rPackages
, haskell-nix
, agdaWithStdlib
, buildPackages
, gitignore-nix
, z3
, R
, checkMaterialization
, compiler-nix-name
, enableHaskellProfiling
  # Whether to set the `defer-plugin-errors` flag on those packages that need
  # it. If set to true, we will also build the haddocks for those packages.
, deferPluginErrors
, nativePlutus ? null
}:
let
  r-packages = with rPackages; [ R tidyverse dplyr stringr MASS plotly shiny shinyjs purrr ];
  project = haskell-nix.cabalProject' {
    inherit compiler-nix-name;

    cabalProjectLocal = if (nativePlutus == null)
    then ''
      -- packages: ${pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.configured-src}


      source-repository-package
        type: git
        location: https://github.com/ghcjs/ghcjs.git
        tag: 6f20f45e384e4907cbf11ec7c258e456c4f0f4d7
        --sha256: 098n3nabc9dgsfh0mznpkaxhbwmsp5rx5wcvx4411k631lglkyk2

      allow-newer: ghcjs:base16-bytestring
                 , ghcjs:aeson
                 , stm:base
                 , cardano-binary:recursion-schemes
                 , jsaddle:ghcjs-base
                 , ghcjs-base:primitive
                 , ghcjs-base:time
                 , ghcjs-base:hashable
                 , ghcjs-base:aeson
                 , servant-foreign:lens
                 , servant-client:http-client
      constraints: plutus-tx +ghcjs-plugin


      -- The following is needed because Nix is doing something crazy.
      package byron-spec-ledger
        tests: False

      package marlowe
        tests: False

      package plutus-doc
        tests: False

      package plutus-metatheory
        tests: False

      package prettyprinter-configurable
        tests: False

      package small-steps
        tests: False

      package small-steps-test
        tests: False

      package byron-spec-chain
        tests: False

    '' else ''
      constraints: plutus-tx +ghcjs-plugin
    '';

    # This is incredibly difficult to get right, almost everything goes wrong, see https://github.com/input-output-hk/haskell.nix/issues/496
    src = let root = ../../../.; in
      haskell-nix.haskellLib.cleanSourceWith {
        filter = gitignore-nix.gitignoreFilter root;
        src = root;
        # Otherwise this depends on the name in the parent directory, which reduces caching, and is
        # particularly bad on Hercules, see https://github.com/hercules-ci/support/issues/40
        name = "plutus";
      };
    # These files need to be regenerated when you change the cabal files.
    # See ../CONTRIBUTING.doc for more information.
    # Unfortuntely, they are *not* constant across all possible systems, so in some circumstances we need different sets of files
    # At the moment, we only need one but conceivably we might need one for darwin in future.
    # See https://github.com/input-output-hk/nix-tools/issues/97
    # materialized =
    #   if stdenv.hostPlatform.isUnix then ./materialized-unix
    #   else __trace "Don't have materialized files for this platform ${stdenv.hostPlatform.config}" null;
    # If true, we check that the generated files are correct. Set in the CI so we don't make mistakes.
    inherit checkMaterialization;
    sha256map = {
      "https://github.com/shmish111/purescript-bridge.git"."6a92d7853ea514be8b70bab5e72077bf5a510596" = "13j64vv116in3c204qsl1v0ajphac9fqvsjp7x3zzfr7n7g61drb";
      "https://github.com/shmish111/servant-purescript.git"."a76104490499aa72d40c2790d10e9383e0dbde63" = "11nxxmi5bw66va7psvrgrw7b7n85fvqgfp58yva99w3v9q3a50v9";
      "https://github.com/input-output-hk/cardano-base"."4f627b3797ba4c852e4c86fb5383d35600423205" = "0xmbk89fzji07y9i05xbiczv8ppy1ffpnm6ym1pypl6cf3kl2b7r";
      "https://github.com/input-output-hk/cardano-crypto.git"."f73079303f663e028288f9f4a9e08bcca39a923e" = "1n87i15x54s0cjkh3nsxs4r1x016cdw1fypwmr68936n3xxsjn6q";
      "https://github.com/input-output-hk/cardano-ledger-specs"."097890495cbb0e8b62106bcd090a5721c3f4b36f" = "0i3y9n0rsyarvhfqzzzjccqnjgwb9fbmbs6b7vj40afjhimf5hcj";
      "https://github.com/input-output-hk/cardano-prelude"."ee4e7b547a991876e6b05ba542f4e62909f4a571" = "0dg6ihgrn5mgqp95c4f11l6kh9k3y75lwfqf47hdp554w7wyvaw6";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/input-output-hk/iohk-monitoring-framework"."34abfb7f4f5610cabb45396e0496472446a0b2ca" = "1fdc0a02ipa385dnwa6r6jyc8jlg537i12hflfglkhjs2b7i92gs";
      "https://github.com/input-output-hk/ouroboros-network"."6cb9052bde39472a0555d19ade8a42da63d3e904" = "0rz4acz15wda6yfc7nls6g94gcwg2an5zibv0irkxk297n76gkmg";
    };
    modules = [
        # { nonReinstallablePkgs = [
        #   "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
        #   "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
        #   # ghcjs custom packages
        #   "ghcjs-prim" "ghcjs-th"
        #   "ghc-boot"
        #   "ghc" "Win32" "array" "binary" "bytestring" "containers"
        #   "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
        #   # "ghci" "haskeline"
        #   "hpc"
        #   "mtl" "parsec" "process" "text" "time" "transformers"
        #   "unix" "xhtml"
        #   # "stm" "terminfo"
        # ]; }
      {
        reinstallableLibGhc = false;
        packages = {

          ghcjs.components.library.build-tools = let alex = pkgs.haskell-nix.tool compiler-nix-name "alex" {
            index-state = pkgs.haskell-nix.internalHackageIndexState;
            version = "3.2.5"; }; in [ alex ];
          ghcjs.flags.use-host-template-haskell = true;

          plutus-use-cases.ghcOptions = if (nativePlutus != null)
                                        then (let attr = nativePlutus.haskell.projectPackages.plutus-tx-plugin.components.library;
                                         in [ "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                                              "-host-package-db ${attr}/package.conf.d"
                                              "-Werror" ])
                                        else __trace "nativePlutus is null" [];

          plutus-tx-plugin.ghcOptions = if (nativePlutus != null)
                                        then (let attr = nativePlutus.haskell.projectPackages.plutus-tx-plugin.components.library;
                                         in [ "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                                              "-host-package-db ${attr}/package.conf.d"
#                                              "-Werror"
                                            ])
                                        else __trace "nativePlutus is null" [];

          plutus-tx-tests.ghcOptions = if (nativePlutus != null)
                                        then (let attr = nativePlutus.haskell.projectPackages.plutus-tx-plugin.components.library;
                                         in [ "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                                              "-host-package-db ${attr}/package.conf.d"
#                                              "-Werror"
                                            ])
                                        else __trace "nativePlutus is null" [];

          plutus-errors.ghcOptions = if (nativePlutus != null)
                                        then (let attr = nativePlutus.haskell.projectPackages.plutus-tx-plugin.components.library;
                                         in [ "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                                              "-host-package-db ${attr}/package.conf.d"
                                              "-Werror" ])
                                        else __trace "nativePlutus is null" [];

          plutus-benchmark.ghcOptions = if (nativePlutus != null)
                                        then (let attr = nativePlutus.haskell.projectPackages.plutus-tx-plugin.components.library;
                                         in [ "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                                              "-host-package-db ${attr}/package.conf.d"
                                              "-Werror" ])
                                        else __trace "nativePlutus is null" [];


          plutus-ledger.components.library.build-tools = if (nativePlutus != null) then [ pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.buildGHC ] else [];
          plutus-ledger.ghcOptions = if (nativePlutus != null)
                                        then (let attr = nativePlutus.haskell.projectPackages.plutus-tx-plugin.components.library;
                                         in [ "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                                              "-host-package-db ${attr}/package.conf.d"
                                              "-Werror" ])
                                        else __trace "nativePlutus is null" [];

          plutus-ledger-test.ghcOptions = if (nativePlutus != null)
                                        then (let attr = nativePlutus.haskell.projectPackages.plutus-tx-plugin.components.library;
                                         in [ "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                                              "-host-package-db ${attr}/package.conf.d"
                                              "-Werror" ])
                                        else __trace "nativePlutus is null" [];

          Cabal.patches = [ ../../patches/cabal.patch ];
          # See https://github.com/input-output-hk/plutus/issues/1213 and
          # https://github.com/input-output-hk/plutus/pull/2865.
          marlowe.doHaddock = deferPluginErrors;
          marlowe.flags.defer-plugin-errors = deferPluginErrors;

          plutus-use-cases.doHaddock = deferPluginErrors;
          plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

          plutus-ledger.doHaddock = deferPluginErrors;
          plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

          # Packages we just don't want docs for
          plutus-benchmark.doHaddock = false;
          # FIXME: Haddock mysteriously gives a spurious missing-home-modules warning
          plutus-tx-plugin.doHaddock = false;

          # Fix missing executables on the paths of the test runners. This is arguably
          # a bug, and the fix is a bit of a hack.
          marlowe.components.tests.marlowe-test.preCheck = ''
            PATH=${lib.makeBinPath [ z3 ]}:$PATH
          '';
          # In this case we can just propagate the native dependencies for the build of the test executable,
          # which are actually set up right (we have a build-tool-depends on the executable we need)
          # I'm slightly surprised this works, hooray for laziness!
          plutus-metatheory.components.tests.test1.preCheck = ''
            PATH=${lib.makeBinPath project.hsPkgs.plutus-metatheory.components.tests.test1.executableToolDepends }:$PATH
          '';
          # FIXME: Somehow this is broken even with setting the path up as above
          plutus-metatheory.components.tests.test2.doCheck = false;
          # plutus-metatheory needs agda with the stdlib around for the custom setup
          # I can't figure out a way to apply this as a blanket change for all the components in the package, oh well
          plutus-metatheory.components.library.build-tools = [ agdaWithStdlib ];
          plutus-metatheory.components.exes.plc-agda.build-tools = [ agdaWithStdlib ];
          plutus-metatheory.components.tests.test1.build-tools = [ agdaWithStdlib ];
          plutus-metatheory.components.tests.test2.build-tools = [ agdaWithStdlib ];
          plutus-metatheory.components.tests.test3.build-tools = [ agdaWithStdlib ];

          # Relies on cabal-doctest, just turn it off in the Nix build
          prettyprinter-configurable.components.tests.prettyprinter-configurable-doctest.buildable = lib.mkForce false;

          plutus-core.components.benchmarks.update-cost-model = {
            build-tools = r-packages;
            # Seems to be broken on darwin for some reason
            platforms = lib.platforms.linux;
          };

          plutus-core.components.benchmarks.cost-model-test = {
            build-tools = r-packages;
            # Seems to be broken on darwin for some reason
            platforms = lib.platforms.linux;
          };

          marlowe-actus.components.exes.marlowe-shiny = {
            build-tools = r-packages;
            # Seems to be broken on darwin for some reason
            platforms = lib.platforms.linux;
          };

          # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
          iohk-monitoring.doHaddock = false;

          # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
          plutus-core.ghcOptions = [ "-Werror" ];
          marlowe.ghcOptions = [ "-Werror" ];
          marlowe-symbolic.ghcOptions = [ "-Werror" ];
          marlowe-actus.ghcOptions = [ "-Werror" ];
          marlowe-playground-server.ghcOptions = [ "-Werror" ];
          marlowe-dashboard-server.ghcOptions = [ "-Werror" ];
          playground-common.ghcOptions = [ "-Werror" ];
          # FIXME: has warnings
          #plutus-metatheory.package.ghcOptions = "-Werror";
          plutus-contract.ghcOptions = [ "-Werror" ];
          # plutus-ledger.ghcOptions = [ "-Werror" ];
          plutus-ledger-api.ghcOptions = [ "-Werror" ];
          plutus-playground-server.ghcOptions = [ "-Werror" ];
          plutus-pab.ghcOptions = [ "-Werror" ];
          plutus-tx.ghcOptions = [ "-Werror" ];
          # plutus-tx-plugin.ghcOptions = [ "-Werror" ];
          plutus-doc.ghcOptions = [ "-Werror" ];
          # plutus-use-cases.ghcOptions = [ "-Werror" ];

          # External package settings

          inline-r.ghcOptions = [ "-XStandaloneKindSignatures" ];

          # Haddock doesn't work for some reason
          eventful-sql-common.doHaddock = false;
          # Needs some extra options to work with newer persistent
          eventful-sql-common.ghcOptions = [ "-XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses" ];

          # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
          # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
          ieee.components.library.libs = lib.mkForce [ ];
        };
      }
    ] ++ lib.optional enableHaskellProfiling {
      enableLibraryProfiling = true;
      enableExecutableProfiling = true;
    };
  };

in
project
