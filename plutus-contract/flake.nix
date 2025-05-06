{
  description = "A simple Nix flake for Cardano NFT Store project";
  
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11"; # Choose the appropriate Nixpkgs version
  
  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.callPackage ./default.nix {};
  };
}
