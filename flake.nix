{
	description = "A compiler for the Artemis language";

	inputs = {
		nixpkgs.url     = "github:nixos/nixpkgs/nixpkgs-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem(system:
			let
				pkgs = nixpkgs.legacyPackages.${system};
			in rec {
				artemis = pkgs.rustPlatform.buildRustPackage {
					pname              = "artemis";
					version            = "0.0.1";
					src                = self;
					cargoSha256        = "sha256-ah8IjShmivS6IWL3ku/4/j+WNr/LdUnh1YJnPdaFdcM=";
					cargoLock.lockFile = "${self}/Cargo.lock";
					buildInputs        = with pkgs; [ nasm mold musl ];
					nativeBuildInputs  = with pkgs; [ gcc ];
					
					MUSL = "${pkgs.musl}/lib";
					NASM = "${pkgs.nasm}/bin/nasm";
					MOLD = "${pkgs.mold}/bin/mold";
				};
				defaultPackage = artemis;
				devShell = pkgs.mkShell {
					shellHook = ''
						PS1="\e[32;1mnix-flake: \e[34m\w \[\033[00m\]\n↳ "
						MUSL="${pkgs.musl}/lib"
						NASM="${pkgs.nasm}/bin/nasm"
						MOLD="${pkgs.mold}/bin/mold"
					'';
					buildInputs       = artemis.buildInputs;
					nativeBuildInputs = with pkgs; [
						rustup
						valgrind
						rr
						tokei
						lldb

						llvmPackages_14.libcxxStdenv
						llvmPackages_14.libunwind
						llvmPackages_14.libcxx
						llvmPackages_14.libcxxabi
						clang-tools_14
						gnumake
					] ++ artemis.nativeBuildInputs;
				};
			}
		);
}
