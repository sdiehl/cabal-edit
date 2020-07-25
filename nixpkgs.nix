let
  # Pin of nixos-20.03 on 2020-07-24
  rev = "69af91469be26a8d28052f3d8de6d48c0b5e05bc";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "02cxlm12jvbbiky1awfm6pxfp346p3hgvbnhlpfzb3sgwm53r8qd";
  };

in
  import nixpkgs
