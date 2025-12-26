{
  description = "Karmarkdown Markdown LSP (Zig)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        zig = if pkgs ? zig_0_15 then pkgs.zig_0_15 else pkgs.zig;
        src = pkgs.lib.cleanSourceWith {
          src = ./.;
          filter = path: type:
            let
              base = builtins.baseNameOf path;
            in
              base != ".git" && base != ".jj" && base != ".zig-cache" &&
              base != "zig-cache" && base != "zig-out";
        };
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "karmarkdown";
          version = "git-${self.shortRev or "dirty"}";
          src = src;
          nativeBuildInputs = [ zig ];
          dontConfigure = true;
          buildPhase = ''
            export HOME="$TMPDIR"
            export ZIG_GLOBAL_CACHE_DIR="$TMPDIR/zig-cache"
            export ZIG_LOCAL_CACHE_DIR="$TMPDIR/zig-cache-local"
            zig build -Doptimize=ReleaseSafe --prefix $out
          '';
          installPhase = "true";
          meta.mainProgram = "karmarkdown";
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/karmarkdown";
        };
      });
}
