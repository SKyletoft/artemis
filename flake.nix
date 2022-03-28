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
				LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
				BINDGEN_EXTRA_CLANG_ARGS = "-isystem ${pkgs.llvmPackages.libclang.lib}/lib/clang/${pkgs.lib.getVersion pkgs.llvmPackages.clang}/include";
			in rec {
				artemis = pkgs.rustPlatform.buildRustPackage {
					pname              = "artemis";
					version            = "0.0.1";
					src                = self;
					cargoSha256        = "sha256-ah8IjShmivS6IWL3ku/4/j+WNr/LdUnh1YJnPdaFdcM=";
					cargoLock.lockFile = "${self}/Cargo.lock";
					buildInputs        = with pkgs; [ ];
					nativeBuildInputs  = with pkgs; [
						haskellPackages.BNFC
						llvmPackages.clang
						llvmPackages.libclang.lib
						llvmPackages.libclang.dev
						stdenv.cc.libc
					];

					inherit LIBCLANG_PATH;
					inherit BINDGEN_EXTRA_CLANG_ARGS;
				};
				defaultPackage = artemis;
				devShell = pkgs.mkShell {
					shellHook = ''
						PS1="\e[32;1mnix-flake: \e[34m\w \[\033[00m\]\n↳ "
					'';
					buildInputs       = artemis.buildInputs;
					nativeBuildInputs = with pkgs; [ rustup ] ++ artemis.nativeBuildInputs;

					inherit LIBCLANG_PATH;
					inherit BINDGEN_EXTRA_CLANG_ARGS;
				};
			}
		);
}

