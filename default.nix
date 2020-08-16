reflex-platform-args:

let
  reflexPlatformSrc = builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/df0bdcca5eb2a3236ec0496e4430d91876b29cf5.tar.gz";
  };

  ghcjsIgnoreOverrides = self: super:
    let
      ignore = {
        haskeline = null;
      };
    in if (super.ghc.isGhcjs or false) then ignore else {};

haskellOverlaysPre = (reflex-platform-args.haskellOverlaysPre or [ ])
    ++ [ ];

  haskellOverlaysPost = (reflex-platform-args.haskellOverlaysPost or [ ])
    ++ [ ghcjsIgnoreOverrides ];

  config = { allowBroken = true; };

  reflex-platform = import reflexPlatformSrc (reflex-platform-args // {
    inherit config haskellOverlaysPre haskellOverlaysPost;
  });

in reflex-platform.project ({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;

  packages = {
    cubicaltt = ./.;
  };

  shells = {
    ghc = ["cubicaltt"];
    ghcjs = ["cubicaltt"];
  };

  shellToolOverrides = ghc: super: {
    closure-compiler = null;
    haskell-ide-engine = null;
    hdevtools = null;
    hlint = null;
    stylish-haskell = null;
    ghcid = if pkgs.stdenv.isDarwin then null else super.ghcid;
    cabal-install = null;
  };
})