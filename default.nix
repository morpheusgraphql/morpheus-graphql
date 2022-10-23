with (import <nixpkgs> { });

haskell.lib.buildStackProject {
  name = "hello";
  src = ./.;
}