#!/usr/bin/env python3
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

r"""
Script/module for versioning related activities.

Versions are stored in Git tags.
Valid "versioning" tags are prefixed with 'v' and MUST be PEP 440 compliant.

See also: https://peps.python.org/pep-0440
"""

import argparse
import subprocess
try: from packaging.version import Version
except ModuleNotFoundError:
    raise ModuleNotFoundError('Install via `python3 -m pip install packaging`.')


class VersionManager:
    def __init__(self, repository):
        self.repository = repository

    def get(self) -> Version:
        version_str = subprocess.check_output(
            ['git', 'describe', '--tags', '--abbr=0', '--match=v*'],
            cwd=self.repository,
            text=True,
        )
        return Version(version_str)

    def set(self, version) -> Version:
        version_ = (
            version 
            if isinstance(version, Version) else
            Version(version)
        )
        subprocess.run(
            ['git', 'tag', f'v{version_!s}'], 
            cwd=self.repository,
            check=True, 
        )
        return version_


def main(args=None):
    parser = argparse.ArgumentParser(
        description="Manage versions of a Git repository, PEP 440 compliant.",
    )
    parser.add_argument(
        '-c', '--cwd', 
        default='.', 
        help="Path to the Git repository.",
    )

    subparsers = parser.add_subparsers(dest='command', required=True)
    # command: get
    parser_get = subparsers.add_parser(
        'get', 
        help="Get the version attached to current commit.",
    )
    # command: set
    parser_set = subparsers.add_parser(
        'set', 
        help="Tag current commit to a new version.",
    )
    parser_set.add_argument(
        'version', 
        help="New version to set.",
    )
    
    args = parser.parse_args(args=args)
    vm = VersionManager(args.cwd)
    if args.command == 'get':
        print(str(vm.get()))
    if args.command == 'set':
        print(str(vm.set(args.version)))


if __name__ == '__main__':
    main()


__all__ = [
    'VersionManager',
    'main',
]