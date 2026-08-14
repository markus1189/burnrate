{
  description = "burnrate — month-to-date LLM spend and projected burn, for a status bar";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      packages = forAllSystems (pkgs: rec {
        burnrate = pkgs.haskellPackages.callCabal2nix "burnrate" ./. { };
        default = burnrate;
      });

      apps = forAllSystems (pkgs: rec {
        burnrate = {
          type = "app";
          program = "${self.packages.${pkgs.system}.burnrate}/bin/burnrate";
        };
        default = burnrate;
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ self.packages.${pkgs.system}.burnrate ];
          withHoogle = false;
          nativeBuildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            hlint
            ormolu
          ];
          # `pass` is how keys are resolved by default; keep it on PATH so the
          # dev shell can run the thing for real.
          buildInputs = [ pkgs.pass ];
        };
      });

      checks = forAllSystems (pkgs: {
        inherit (self.packages.${pkgs.system}) burnrate;
      });
    };
}
