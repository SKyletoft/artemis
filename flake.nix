{
	description = "A compiler for the Artemis language";

	inputs = {
		nixpkgs.url     = "github:nixos/nixpkgs/nixpkgs-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem(system:
			let pkgs = nixpkgs.legacyPackages.${system};
			in rec {
				artemis = pkgs.rustPlatform.buildRustPackage {
					pname              = "artemis";
					version            = "0.0.1";
					src                = self;
					cargoSha256        = "sha256-ah8IjShmivS6IWL3ku/4/j+WNr/LdUnh1YJnPdaFdcM=";
					cargoLock.lockFile = "${self}/Cargo.lock";
					nativeBuildInputs  = with pkgs; [ pkg-config ];
					buildInputs        = with pkgs; [];
				};
				defaultPackage = artemis;
				devShell = pkgs.mkShell {
					shellHook         = ''PS1="\e[32;1mnix-flake: \e[34m\w \[\033[00m\]\n↳ "'';
					buildInputs       = artemis.buildInputs;
					nativeBuildInputs = with pkgs; [ rustup ] ++ artemis.nativeBuildInputs;
				};
			}
		);
}

