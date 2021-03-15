{
  description = "A script to convert DSSAT-pythia per-pixel files to GGCMI NetCDF files";

  inputs.nixpkgs.url= "github:frostbytten/nixpkgs/R-4.0.4-issues";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
    let 
      pkgs = nixpkgs.legacyPackages.${system}; 
      rEnv = pkgs.rWrapper.override {
        packages = with pkgs.rPackages; [
          data_table
          docopt
          ncdf4
          udunits2
          jsonlite
        ];
      };
    in
      rec {
        packages = flake-utils.lib.flattenTree {
          rEnv = rEnv;
        };
        defaultPackage = packages.rEnv;
      }
    );
}
