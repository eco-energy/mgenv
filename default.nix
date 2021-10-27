{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
# , overlays ? []
}:

let
  sources = import ./nix/sources.nix { inherit pkgs; }
    // sourcesOverride;
  iohKNix = import sources.iohk-nix {};
  haskellNix = import sources."haskell.nix" {
    inherit system;
    sourcesOverride = {
      hackage = sources.hackage-nix;
      stackage = sources.stackage-nix;
    };
  };
  # use our own nixpkgs if it exist in our sources,
  # otherwise use iohkNix default nixpkgs.
  nixpkgs = haskellNix.sources.nixpkgs-2009 or
    (builtins.trace "Using IOHK default nixpkgs" iohKNix.nixpkgs);

  pkgs = import nixpkgs (haskellNix.nixpkgsArgs // {
    inherit system crossSystem; # overlays;
  });
in
pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "mgenv";
    src = ./.;
  };
  projectFileName = "stack.yaml";
  #stack-sha256="166491vz810yrhgk09yjk3yfznmfmr8nkchg24cr5p5zxhvq3nfk";
  #materialized=./mgenv.materialized;
}

