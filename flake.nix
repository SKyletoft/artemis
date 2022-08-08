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
				artemis-unwrapped = pkgs.rustPlatform.buildRustPackage {
					pname              = "artemis";
					version            = "0.0.1";
					src                = self;
					cargoSha256        = "sha256-ah8IjShmivS6IWL3ku/4/j+WNr/LdUnh1YJnPdaFdcM=";
					cargoLock.lockFile = "${self}/Cargo.lock";
					buildInputs        = with pkgs; [ nasm mold ];
					nativeBuildInputs  = with pkgs; [];
				};
				artemis-runtime = pkgs.stdenv.mkDerivation rec {
					pname        = "artemis-runtime";
					version      = artemis-unwrapped.version;
					src          = self;
					buildPhase   = "make -C c_working_files";
					installPhase = ''
						mkdir -p $out
						cp c_working_files/*.o $out
					'';
				};
				artemis-wrapped = pkgs.writeShellScriptBin "artemis" ''
					export MUSL="${pkgs.musl}/lib"
					export NASM="${pkgs.nasm}/bin/nasm"
					export MOLD="${pkgs.mold}/bin/mold"
					export ARTEMIS_RUNTIME="${artemis-runtime}"
					${artemis-unwrapped}/bin/artemis $@
				'';
				defaultPackage = artemis-wrapped;
				devShell = pkgs.mkShell {
					shellHook = ''
						PS1="\e[32;1mnix-flake: \e[34m\w \[\033[00m\]\n↳ "
						export MUSL="${pkgs.musl}/lib"
						export NASM="${pkgs.nasm}/bin/nasm"
						export MOLD="${pkgs.mold}/bin/mold"
						export ARTEMIS_RUNTIME="c_working_files"
					'';
					buildInputs       = artemis-unwrapped.buildInputs;
					nativeBuildInputs = with pkgs; [
						rustup
						tokei

						llvmPackages_14.libcxxStdenv
						llvmPackages_14.libunwind
						llvmPackages_14.libcxx
						llvmPackages_14.libcxxabi
						clang-tools_14
						gnumake
					] ++ artemis-unwrapped.nativeBuildInputs;
				};
			}
		);
}
