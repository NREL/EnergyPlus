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
from os import chdir, devnull, scandir
from pathlib import Path
from re import search
from shutil import rmtree
from subprocess import check_call, CalledProcessError, STDOUT
from sys import exit, path

this_file_path = Path(__file__).resolve()
package_test_root_dir = this_file_path.parent
path.insert(0, str(package_test_root_dir))

from ep_testing.config import CONFIGURATIONS, TestConfiguration, OS
from ep_testing.tester import Tester


def get_version_info() -> tuple:
    repo_root = package_test_root_dir.parent.parent
    version_file = repo_root / 'cmake' / 'Version.cmake'
    contents = version_file.read_text()
    major = search(r'CMAKE_VERSION_MAJOR [0-9]+', contents).group(0).split(' ')[1]
    minor = search(r'CMAKE_VERSION_MINOR [0-9]+', contents).group(0).split(' ')[1]
    last_major = search(r'PREV_RELEASE_MAJOR [0-9]+', contents).group(0).split(' ')[1]
    last_minor = search(r'PREV_RELEASE_MINOR [0-9]+', contents).group(0).split(' ')[1]
    last_patch = search(r'PREV_RELEASE_PATCH [0-9]+', contents).group(0).split(' ')[1]
    this_version = f"{major}.{minor}"
    last_version = f"{last_major}.{last_minor}"
    last_tag = f"v{last_major}.{last_minor}.{last_patch}"
    return this_version, last_version, last_tag


def find_and_extract_package(config: TestConfiguration, artifact_folder: Path) -> str:
    extract_path = Path.cwd() / 'ep_package'
    saved_working_directory = Path.cwd()
    chdir(artifact_folder)
    package_file_name = str(list(artifact_folder.rglob('*'))[0])
    if config.os == OS.Linux:
        # tar -xzf ep.tar.gz -C ep_package
        extract_command = ['tar', '-xzf', package_file_name, '-C', str(extract_path)]
    elif config.os == OS.Mac:
        # tar -xzf ep.tar.gz -C ep_package
        extract_command = ['tar', '-xzf', package_file_name, '-C', str(extract_path)]
    else:  # if config.os == OS.Windows:
        # 7z x ep.zip -oep_package
        extract_command = ['7z.exe', 'x', package_file_name, '-o' + str(extract_path)]
    if extract_path.exists():
        rmtree(extract_path)
    try:
        extract_path.mkdir(parents=True)
    except Exception as e:
        raise Exception('Could not create extraction path at %s; error: %s' % (extract_path, str(e)))
    try:
        print("Extracting asset...")
        dev_null = open(devnull, 'w')
        check_call(extract_command, stdout=dev_null, stderr=STDOUT)
        print(" ...Extraction Complete")
    except CalledProcessError as e:
        raise Exception("Extraction failed with this error: " + str(e))
    # should result in a single new directory inside the extract path, like: /extract/path/EnergyPlus-V1-abc-Linux
    all_sub_folders = [f.path for f in scandir(extract_path) if f.is_dir()]
    if len(all_sub_folders) > 1:
        raise Exception('Extracted EnergyPlus package has more than one directory, problem.')
    chdir(saved_working_directory)
    return all_sub_folders[0]


def run(config: str, this: str, last: str, last_tag: str, package_dir: Path, msvc: str, verbose: bool) -> int:
    c = TestConfiguration(config, this, last, last_tag, msvc)
    extracted_package_dir = find_and_extract_package(c, package_dir)
    t = Tester(c, Path(extracted_package_dir), verbose)
    t.run()
    return 0  # TODO: be better about the exit code


def main() -> int:
    parser = argparse.ArgumentParser(description="Run Package Tests on EnergyPlus")
    arg = parser.add_argument  # readability
    arg('config', choices=CONFIGURATIONS.keys(), help="Specify the run configuration")
    arg('package_dir', help="Path to the extracted EnergyPlus package directory")
    arg('--msvc', type=str, default='', help="For MSVC builds, this is the VS 'year', like '2022'")
    arg('--verbose', action='store_true', help="If specified, get verbose output")
    args = parser.parse_args()
    this, last, last_tag = get_version_info()
    tentative_path = Path(args.package_dir)
    p = tentative_path if tentative_path.is_absolute() else Path(Path.cwd()) / tentative_path
    r = run(args.config, this, last, last_tag, p, args.msvc, args.verbose)
    return r


if __name__ == "__main__":
    exit(main())
