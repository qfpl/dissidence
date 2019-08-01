{ nixpkgs ? import <nixpkgs> {} }: 
let
  backend-static = nixpkgs.haskell.lib.justStaticExecutables (import ./. {});
  config = nixpkgs.pkgs.writeText "config" ''
    {
      port = 8001,
      dbPath = "dissidence.db",
      jwtKey = "vt82gwBJLSHUNXO0D6B6FciWQYBgkWJhttqELfjpiIAsdzWGKaNfwFAtPkMWQTj5uL3uiowNX4QeccI3ieXNdv6Sj8CqZF8hJ60skG4HgdC3beAkeMdA8juQPWYLTuqoHxNp0FD8jTMQvN8Z46Bi2KlygHVKPo4hqv19yXzspyBuaj4OsIe4byl6sQk3pbhUQClon36POlFrQ1rVGJdtN7cRXgZw4ejZRLt8wUe6tbUE0KSlQjoFVwNAWFiO4rDF"
    }
  '';
  dockerImage =
    nixpkgs.pkgs.dockerTools.buildImage {
      name = "benkolera/dissidence-backend";
      tag = "latest";
      fromImage = nixpkgs.pkgs.dockerTools.pullImage {
          imageName = "alpine";
          imageDigest = "sha256:e1871801d30885a610511c867de0d6baca7ed4e6a2573d506bbec7fd3b03873f";
          sha256 = "05wcg38vsygjzf59cspfbb7cq98c7x18kz2yym6rbdgx960a0kyq";
        };
      contents = "${backend-static}/bin";
      config = {
        Cmd = [
          "${backend-static}/bin/backend"
          config  
        ];
      };
    };
in dockerImage
