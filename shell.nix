{ pkgs ? import <nixpkgs> {} } : let
  idris2-ear7h = (pkgs.idris2.unwrapped.overrideAttrs (old: {
    src = pkgs.fetchgit {
      url  = "https://github.com/ear7h/idris2";
      rev  = "f38c0ad";
      hash = "sha256-2NStzz8xpOR9SeuVHvxSWVor9YNTbDGfJMnDd/UV04c=";
    };
  })).withPackages (x: [ ]);

  supportLib = pkgs.stdenv.mkDerivation {
    name = "website_support";

    nativeBuildInputs = with pkgs; [
      pkg-config
    ];

    buildInputs = with pkgs; [
      libsodium.dev
    ];

    src = ./support.c;

    dontUnpack = true;

    CFLAGS =
      if pkgs.stdenv.hostPlatform.isDarwin
      then "-dynamiclib"
      else "-shared -fPIC";

    LIB_SUFFIX =
      if pkgs.stdenv.hostPlatform.isDarwin
      then "dylib"
      else "so";

    buildPhase = ''
      mkdir -p $out/lib
      cc $CFLAGS -lsodium -o $out/lib/libwebsite_support.$LIB_SUFFIX $src
    '';
  };

  libPath = with pkgs; lib.makeLibraryPath [
    supportLib
    libsodium
  ];

in pkgs.mkShell {
  IDRIS2_SH =
    "${pkgs.coreutils}/bin/env DYLD_LIBRARY_PATH=${libPath} ${pkgs.bash}/bin/sh";

  buildInputs = with pkgs; [
    idris2-ear7h
    idris2Packages.pack
    chez
    cowsay
  ] ++ (pkgs.lib.optional pkgs.stdenv.hostPlatform.isLinux pkgs.valgrind);
}
