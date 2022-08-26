{
	description = "A compiler for the Artemis language";

	inputs = {
		nixpkgs.url     = "github:nixos/nixpkgs/nixpkgs-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem(system:
			let
				pkgs     = nixpkgs.legacyPackages.${system};
				pkgs_x86 = nixpkgs.legacyPackages.x86_64-linux;
				pkgs_arm = nixpkgs.legacyPackages.aarch64-linux;
				version  = "0.0.1";
				src      = self;
			in rec {
				packages = {
					artemis-unwrapped = pkgs.rustPlatform.buildRustPackage {
						inherit src version;
						pname              = "artemis";
						cargoSha256        = "sha256-ah8IjShmivS6IWL3ku/4/j+WNr/LdUnh1YJnPdaFdcM=";
						cargoLock.lockFile = "${self}/Cargo.lock";
					};

					artemis-runtime = pkgs.stdenv.mkDerivation rec {
						inherit src version;
						pname        = "artemis-runtime";
						buildPhase   = "make -C c_working_files";
						installPhase = ''
							mkdir -p $out
							cp c_working_files/*.o $out
						'';
					};

					artemis-wrapped = pkgs.writeShellScriptBin "artemis" ''
						export MOLD="${pkgs.mold}/bin/mold"
						export MUSL_x86="${pkgs_x86.musl}/lib"
						export MUSL_ARM="${pkgs_arm.musl}/lib"
						export NASM="${pkgs.nasm}/bin/nasm"
						export GNU_AS="${pkgs_arm.gcc}/bin/as"
						export ARTEMIS_RUNTIME_x86="${self.packages.x86_64-linux.artemis-runtime}"
						export ARTEMIS_RUNTIME_ARM="${self.packages.aarch64-linux.artemis-runtime}"
						${packages.artemis-unwrapped}/bin/artemis $@
					'';

					default = packages.artemis-wrapped;
				};

				devShell = pkgs.mkShell {
					shellHook = ''
						PS1="\e[32;1mnix-flake: \e[34m\w \[\033[00m\]\n↳ "
						export MOLD="${pkgs.mold}/bin/mold"
						export MUSL_x86="${pkgs_x86.musl}/lib"
						export MUSL_ARM="${pkgs_arm.musl}/lib"
						export NASM="${pkgs.nasm}/bin/nasm"
						export GNU_AS="${pkgs_arm.gcc}/bin/as"
						export ARTEMIS_RUNTIME_x86="c_working_files"
						export ARTEMIS_RUNTIME_ARM="${self.packages.aarch64-linux.artemis-runtime}"
					'';
					nativeBuildInputs = with pkgs; [
						rustup
						tokei
						gdb
						mold
						gcc12

						llvmPackages_14.libcxxStdenv
						llvmPackages_14.libunwind
						llvmPackages_14.libcxx
						llvmPackages_14.libcxxabi
						clang-tools_14
						gnumake
					];
				};
			}
		);
}
