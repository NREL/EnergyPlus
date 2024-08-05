# EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University
# of Illinois, The Regents of the University of California, through Lawrence
# Berkeley National Laboratory (subject to receipt of any required approvals
# from the U.S. Dept. of Energy), Oak Ridge National Laboratory, managed by UT-
# Battelle, Alliance for Sustainable Energy, LLC, and other contributors. All
# rights reserved.
#
# NOTICE: This Software was developed under funding from the U.S. Department of
# Energy and the U.S. Government consequently retains certain rights. As such,
# the U.S. Government has been granted for itself and others acting on its
# behalf a paid-up, nonexclusive, irrevocable, worldwide license in the
# Software to reproduce, distribute copies to the public, prepare derivative
# works, and perform publicly and display publicly, and to permit others to do
# so.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# (1) Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#
# (2) Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#
# (3) Neither the name of the University of California, Lawrence Berkeley
#     National Laboratory, the University of Illinois, U.S. Dept. of Energy nor
#     the names of its contributors may be used to endorse or promote products
#     derived from this software without specific prior written permission.
#
# (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in
#     stand-alone form without changes from the version obtained under this
#     License, or (ii) Licensee makes a reference solely to the software
#     portion of its product, Licensee must refer to the software as
#     "EnergyPlus version X" software, where "X" is the version number Licensee
#     obtained under this License and may not use a different name for the
#     software. Except as specifically required in this Section (4), Licensee
#     shall not use in a company name, a product name, in advertising,
#     publicity, or other promotional activities any name, trade name,
#     trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or
#     confusingly similar designation, without the U.S. Department of Energy's
#     prior written consent.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

import argparse
import os
import platform
import subprocess
from enum import Enum
from pathlib import Path
from typing import List


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


def verify_signature(p, verbose=False, root_dir=None):
    info = subprocess.run(["codesign", "-dvvv", "--strict", str(p)], stderr=subprocess.PIPE, encoding="utf-8").stderr
    if "K7JYVQJL7" not in info:
        raise ValueError(f"{p} is not codesigned properly")
    if verbose:
        if root_dir is not None:
            p = p.relative_to(root_dir)
        print(f"- {p}: OK")


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

    if platform.system() != "Darwin":
        raise OSError("Only supported on Darwin")

    parser = argparse.ArgumentParser(description="Verify codesigning on macOS")
    parser.add_argument(
        "build_dir",
        type=Path,
        help="Root of the Build directory where CMakeCache.txt can be found (or the install dir if --install is passed",
    )
    parser.add_argument(
        "--install", action="store_true", default=False, help="This is an install dir, not the build_dir"
    )
    parser.add_argument(
        "--verbose", action="store_true", default=False, help="This is an install dir, not the build_dir"
    )
    args = parser.parse_args()

    build_dir = args.build_dir.resolve()
    if not (build_dir.exists() and build_dir.is_dir()):
        raise IOError(f"{build_dir} is not a valid directory")

    if args.install:
        if not (build_dir / "energyplus").is_file():
            raise ValueError(f"{build_dir} does not contain energyplus exe")
        print(f"Checking Install dir {build_dir}")
        executable_files = find_executable_files(root_dir=build_dir)
        excludes = ["runenergyplus", "runreadvars", "runepmacro"]
        executable_files = [x for x in executable_files if not any([n in x.name for n in excludes])]
        for p in executable_files:
            verify_signature(p, verbose=args.verbose, root_dir=build_dir)
        print("Everything is signed correctly")
    else:
        if not (build_dir / "CMakeCache.txt").is_file():
            raise ValueError(f"{build_dir} does not contain CMakeCache.txt, did you forget to build?")

        if not (build_dir / "_CPack_Packages").is_dir():
            raise ValueError(
                f"{build_dir} does not contain a _CPack_Packages subfolder, did you forget to build the package?"
            )

        for generator in Generator:
            print(f"Checking {generator.name} Generator")
            cmake_install_prefix = get_cmake_install_prefix_for_generator(build_dir=build_dir, generator=generator)
            if cmake_install_prefix is None:
                continue
            executable_files = find_executable_files(root_dir=cmake_install_prefix)
            for p in executable_files:
                verify_signature(p, verbose=args.verbose, root_dir=cmake_install_prefix)
            print("Everything is signed correctly")
            print("=" * 80)
