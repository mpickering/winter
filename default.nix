{ compiler ? "ghc881"

, rev    ? "e1ad1a0aa2ce6f9fd951d18181ba850ca8e74133"
, sha256 ? "0vk5sjmbq52xfrinrhvqry53asl6ppwbly1l7ymj8g0j4pkjf7p1"

, pkgs   ? import <nixpkgs> {}

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation   ? null
}:

let

haskellPackages = pkgs.haskell.packages.${compiler};

wasm = pkgs.ocamlPackages.wasm.overrideAttrs (attrs: {
  src = pkgs.fetchFromGitHub {
    owner  = "WebAssembly";
    repo   = "spec";
    rev    = "a56cf2ec042da382f0196fe14dcbd7ff2e973466";
    sha256 = "02c7bpmjmq3bxp4w0g8gkg87ixh4x67d9hz4g25i4snxnc7bj0g7";
    # date = 2018-10-31T18:30:05+01:00;
  };
  postInstall = attrs.postInstall + ''
    mkdir -p $out/test
    cp -pR test/core $out/test
  '';
});

drv = haskellPackages.developPackage {
  name = "winter";
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
  };

  source-overrides = {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.wabt wasm
    ];

    passthru = {
      nixpkgs = pkgs;
    };
  });

  inherit returnShellEnv;
};

in drv
