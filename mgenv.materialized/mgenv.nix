{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "mgenv"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2019 Faez Shakil";
      maintainer = "faez.shakil@gmail.com";
      author = "Faez Shakil";
      homepage = "https://github.com/faezs/mgenv#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on Github at <https://github.com/faezs/mgenv#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."MemoTrie" or (errorHandler.buildDepError "MemoTrie"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
          (hsPkgs."astro" or (errorHandler.buildDepError "astro"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."checkers" or (errorHandler.buildDepError "checkers"))
          (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
          (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
          (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."diagrams" or (errorHandler.buildDepError "diagrams"))
          (hsPkgs."diagrams-contrib" or (errorHandler.buildDepError "diagrams-contrib"))
          (hsPkgs."diagrams-core" or (errorHandler.buildDepError "diagrams-core"))
          (hsPkgs."diagrams-lib" or (errorHandler.buildDepError "diagrams-lib"))
          (hsPkgs."diagrams-svg" or (errorHandler.buildDepError "diagrams-svg"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
          (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
          (hsPkgs."geodetics" or (errorHandler.buildDepError "geodetics"))
          (hsPkgs."hgeometry" or (errorHandler.buildDepError "hgeometry"))
          (hsPkgs."hgeometry-combinatorial" or (errorHandler.buildDepError "hgeometry-combinatorial"))
          (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
          (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."monad-bayes" or (errorHandler.buildDepError "monad-bayes"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."non-empty" or (errorHandler.buildDepError "non-empty"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector-space" or (errorHandler.buildDepError "vector-space"))
          (hsPkgs."wai-middleware-static" or (errorHandler.buildDepError "wai-middleware-static"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          ];
        buildable = true;
        modules = [
          "Paths_mgenv"
          "ConCat/CircAff"
          "ConCat/Hypergraph"
          "ConCat/Prop"
          "Env"
          "ET"
          "Geometry/EMST"
          "Grid"
          "HH"
          "Physics/Consumption"
          "Physics/Converters"
          "Physics/Field"
          "Physics/Generation"
          "Physics/PV"
          "Physics/Storage"
          "Physics/Transmission"
          "Physics/Units"
          "Prob/GenerativeModel"
          "Scan"
          "Semanticsv1"
          "Server"
          "Viz"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "mgenv-exe" = {
          depends = [
            (hsPkgs."MemoTrie" or (errorHandler.buildDepError "MemoTrie"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
            (hsPkgs."astro" or (errorHandler.buildDepError "astro"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."checkers" or (errorHandler.buildDepError "checkers"))
            (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
            (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
            (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."diagrams" or (errorHandler.buildDepError "diagrams"))
            (hsPkgs."diagrams-contrib" or (errorHandler.buildDepError "diagrams-contrib"))
            (hsPkgs."diagrams-core" or (errorHandler.buildDepError "diagrams-core"))
            (hsPkgs."diagrams-lib" or (errorHandler.buildDepError "diagrams-lib"))
            (hsPkgs."diagrams-svg" or (errorHandler.buildDepError "diagrams-svg"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
            (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
            (hsPkgs."geodetics" or (errorHandler.buildDepError "geodetics"))
            (hsPkgs."hgeometry" or (errorHandler.buildDepError "hgeometry"))
            (hsPkgs."hgeometry-combinatorial" or (errorHandler.buildDepError "hgeometry-combinatorial"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mgenv" or (errorHandler.buildDepError "mgenv"))
            (hsPkgs."monad-bayes" or (errorHandler.buildDepError "monad-bayes"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."non-empty" or (errorHandler.buildDepError "non-empty"))
            (hsPkgs."optparse-simple" or (errorHandler.buildDepError "optparse-simple"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."vector-space" or (errorHandler.buildDepError "vector-space"))
            (hsPkgs."wai-middleware-static" or (errorHandler.buildDepError "wai-middleware-static"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          buildable = true;
          modules = [ "Paths_mgenv" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "mgenv-test" = {
          depends = [
            (hsPkgs."MemoTrie" or (errorHandler.buildDepError "MemoTrie"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
            (hsPkgs."astro" or (errorHandler.buildDepError "astro"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."checkers" or (errorHandler.buildDepError "checkers"))
            (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
            (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
            (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."diagrams" or (errorHandler.buildDepError "diagrams"))
            (hsPkgs."diagrams-contrib" or (errorHandler.buildDepError "diagrams-contrib"))
            (hsPkgs."diagrams-core" or (errorHandler.buildDepError "diagrams-core"))
            (hsPkgs."diagrams-lib" or (errorHandler.buildDepError "diagrams-lib"))
            (hsPkgs."diagrams-svg" or (errorHandler.buildDepError "diagrams-svg"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
            (hsPkgs."fused-effects" or (errorHandler.buildDepError "fused-effects"))
            (hsPkgs."geodetics" or (errorHandler.buildDepError "geodetics"))
            (hsPkgs."hgeometry" or (errorHandler.buildDepError "hgeometry"))
            (hsPkgs."hgeometry-combinatorial" or (errorHandler.buildDepError "hgeometry-combinatorial"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mgenv" or (errorHandler.buildDepError "mgenv"))
            (hsPkgs."monad-bayes" or (errorHandler.buildDepError "monad-bayes"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."non-empty" or (errorHandler.buildDepError "non-empty"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."vector-space" or (errorHandler.buildDepError "vector-space"))
            (hsPkgs."wai-middleware-static" or (errorHandler.buildDepError "wai-middleware-static"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          buildable = true;
          modules = [
            "CircAffSpec"
            "Common"
            "GeometrySpec"
            "GridSpec"
            "HHSpec"
            "PVSpec"
            "SolarSpec"
            "StorageSpec"
            "Paths_mgenv"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./.;
    }) // { cabal-generator = "hpack"; }