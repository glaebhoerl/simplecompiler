with (import <unstable> {});

haskell.lib.buildStackProject {
    name              = "simplecompiler";
    nativeBuildInputs = [ haskell.packages.ghc862.ghc ];
    buildInputs       = [ llvm_7 ];
}
