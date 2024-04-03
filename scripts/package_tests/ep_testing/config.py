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


class OS:
    Windows = 1
    Linux = 2
    Mac = 3


CONFIGURATIONS = {
    'ubuntu2004': {
        'os': OS.Linux, 'bitness': 'x64', 'asset_pattern': 'Linux-Ubuntu20.04-x86_64.tar.gz', 'os_version': '20.04'
    },
    'ubuntu2204': {
        'os': OS.Linux, 'bitness': 'x64', 'asset_pattern': 'Linux-Ubuntu22.04-x86_64.tar.gz', 'os_version': '22.04'
    },
    'mac11': {
        'os': OS.Mac, 'bitness': 'x64', 'asset_pattern': 'Darwin-macOS11.6-x86_64.tar.gz', 'os_version': '11.6'
    },
    'mac12': {
        'os': OS.Mac, 'bitness': 'x64', 'asset_pattern': 'Darwin-macOS12.1-x86_64.tar.gz', 'os_version': '12.1'
    },
    'mac12-arm64': {
        'os': OS.Mac, 'bitness': 'arm64', 'asset_pattern': 'Darwin-macOS12.1-arm64.tar.gz', 'os_version': '12'
    },
    'mac13-arm64': {
        'os': OS.Mac, 'bitness': 'arm64', 'asset_pattern': 'Darwin-macOS13-arm64.tar.gz', 'os_version': '13'
    },
    'win32': {
        'os': OS.Windows, 'bitness': 'x32', 'asset_pattern': 'Windows-i386.zip', 'os_version': '10'
    },
    'win64': {
        'os': OS.Windows, 'bitness': 'x64', 'asset_pattern': 'Windows-x86_64.zip', 'os_version': '10'
    },
    'win64-2022server': {
        'os': OS.Windows, 'bitness': 'x64', 'asset_pattern': 'Windows-x86_64.zip', 'os_version': '2022'
    },
}


class TestConfiguration:

    def __init__(self, run_config_key: str, this_version: str, last_version: str, last_tag: str, msvc_version=None):

        # invalid keys are protected in the command's finalize_options method
        this_config = CONFIGURATIONS[run_config_key]
        self.os_version = this_config['os_version']
        self.os = this_config['os']
        self.msvc_version = None
        if msvc_version in ['16', '17', '18']:
            self.msvc_version = msvc_version
        elif self.os == OS.Windows and self.os_version == '2022':
            self.msvc_version = 17
        elif self.os == OS.Windows:
            self.msvc_version = 16
        self.asset_pattern = this_config['asset_pattern']
        self.bitness = this_config['bitness']

        self.this_version = this_version
        # self.tag_this_version = 'v24.1.0-RC2'
        self.last_version = last_version
        self.tag_last_version = last_tag
