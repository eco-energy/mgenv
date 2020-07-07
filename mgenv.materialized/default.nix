{
  extras = hackage:
    {
      packages = {
        "geodetics" = (((hackage.geodetics)."0.1.0").revisions)."699c10cd8d69222b125d054cdc8e2bcee7cb0558c78fa1b4fa06b2d3ecf29f5c";
        "hgeometry" = (((hackage.hgeometry)."0.9.0.0").revisions)."43043574d2ce39c543dfadb629ab3a41fe65d7bf68a1426d8d01312818bc6e4e";
        "hgeometry-combinatorial" = (((hackage.hgeometry-combinatorial)."0.9.0.0").revisions)."a03b48302d8c542ff2ff90ba5d45f2742cc21ed1a676c8fa4870cd461a273354";
        "monad-bayes" = (((hackage.monad-bayes)."0.1.1.0").revisions)."57d96f530a37a1c8efed0830c7f2bcf86da05d0d53324aebdd5b5aa165d72829";
        mgenv = ./mgenv.nix;
        concat-inline = ./.stack-to-nix.cache.0;
        concat-known = ./.stack-to-nix.cache.1;
        concat-satisfy = ./.stack-to-nix.cache.2;
        concat-classes = ./.stack-to-nix.cache.3;
        concat-plugin = ./.stack-to-nix.cache.4;
        concat-examples = ./.stack-to-nix.cache.5;
        concat-graphics = ./.stack-to-nix.cache.6;
        concat-hardware = ./.stack-to-nix.cache.7;
        };
      };
  resolver = "lts-14.17";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }