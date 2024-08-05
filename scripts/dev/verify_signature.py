import os
import subprocess
from enum import Enum
from pathlib import Path
from typing import List
import argparse
import platform

class Generator(Enum):
    IFW = 1
    TGZ = 2


BUNDLED_APPS = [
    "PreProcess/EP-Launch-Lite.app",
    "PreProcess/IDFVersionUpdater/IDFVersionUpdater.app",
    "PostProcess/EP-Compare/EP-Compare.app",
]


def get_cmake_install_prefix_for_generator(build_dir: Path, generator: Generator) -> Path:
    cpack_dir = build_dir / f"_CPack_Packages/Darwin/{generator.name}"
    if not cpack_dir.exists():
        print(f"Could not find a _CPack_Packages directory for {generator.name}")
        return None
    cmake_install_root = next(x for x in cpack_dir.glob("*") if x.is_dir() and x.suffix != ".app")

    cmake_install_prefix = next(cmake_install_root.glob("**/energyplus-24.2.0")).parent
    return cmake_install_prefix


def find_executable_files(root_dir: Path) -> List[Path]:
    bundled_apps = [root_dir / x for x in BUNDLED_APPS]

    files = list(
        [
            x
            for x in root_dir.glob("**/*")
            if x.is_file()
            and not x.is_symlink()
            and os.access(x, os.X_OK)
            and not any([x.is_relative_to(bundled_p) for bundled_p in bundled_apps])
        ]
    )

    dylibs = list(
        [
            x
            for x in root_dir.glob("**/*.dylib")
            if x.is_file()
            and not x.is_symlink()
            and not any([x.is_relative_to(bundled_p) for bundled_p in bundled_apps])
        ]
    )
    print(f"In {root_dir} found {len(files)} executable files and {len(dylibs)} dylibs")

    files = list(set(files + dylibs))

    files = sorted(sorted(files), key=lambda x: len(x.parts))

    return files


def verify_signature(p):
    info = subprocess.run(["codesign", "-dvvv", "--strict", str(p)], stderr=subprocess.PIPE, encoding="utf-8").stderr
    if "K7JYVQJL7" not in info:
        raise ValueError(f"{p} is not codesigned properly")


def compare_executables():
    cmake_ifw_prefix = get_cmake_install_prefix_for_generator(generator=Generator.IFW)
    ifw_files = find_executable_files(cmake_ifw_prefix)
    ifw_files_rel = [x.relative_to(cmake_ifw_prefix) for x in ifw_files]
    cmake_tgz_prefix = get_cmake_install_prefix_for_generator(generator=Generator.TGZ)
    tgz_files = find_executable_files(cmake_tgz_prefix)
    tgz_files_rel = [x.relative_to(cmake_tgz_prefix) for x in tgz_files]
    extra_ifw_files = set(ifw_files_rel) - set(tgz_files_rel)
    if extra_ifw_files:
        print(f"Extra IFW files: {extra_ifw_files}")

    extra_tgz_files = set(tgz_files_rel) - set(ifw_files_rel)
    if extra_tgz_files:
        print(f"Extra TGZ files: {extra_tgz_files}")


if __name__ == "__main__":

    if platform.system() != 'Darwin':
        raise OSError("Only supported on Darwin")

    parser = argparse.ArgumentParser(description="Verify codesigning on macOS")
    parser.add_argument(
        "build_dir", type=Path,
        help="Root of the Build directory where CMakeCache.txt can be found (or the install dir if --install is passed"
    )
    parser.add_argument("--install", action="store_true", default=False,
                        help="This is an install dir, not the build_dir")
    args = parser.parse_args()

    build_dir = args.build_dir.resolve()
    if not (build_dir.exists() and build_dir.is_dir()):
        raise IOError(f"{build_dir} is not a valid directory")

    if args.install:
        if not (build_dir / 'energyplus').is_file():
            raise ValueError(f"{build_dir} does not contain energyplus exe")
        executable_files = find_executable_files(root_dir=build_dir)
        excludes = ["runenergyplus", "runreadvars", "runepmacro"]
        executable_files = [x for x in executable_files
                            if not any([n in x.name for n in excludes])]
        for p in executable_files:
            verify_signature(p)
    else:
        if not (build_dir / 'CMakeCache.txt').is_file():
            raise ValueError(f"{build_dir} does not contain CMakeCache.txt, did you forget to build?")

        if not (build_dir / '_CPack_Packages').is_dir():
            raise ValueError(
                f"{build_dir} does not contain a _CPack_Packages subfolder, did you forget to build the package?"
            )

        for generator in Generator:
            cmake_install_prefix = get_cmake_install_prefix_for_generator(build_dir=build_dir, generator=generator)
            if cmake_install_prefix is None:
                continue
            executable_files = find_executable_files(root_dir=cmake_install_prefix)
            for p in executable_files:
                verify_signature(p)
