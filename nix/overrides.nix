{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  ring-buffers = (
    with rec {
      ring-buffersSource = pkgs.lib.cleanSource ../.;
      ring-buffersBasic  = self.callCabal2nix "ring-buffers" ring-buffersSource { };
    };
    overrideCabal ring-buffersBasic (old: {
    })
  );
}
