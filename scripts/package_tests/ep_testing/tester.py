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


import os
from pathlib import Path

from ep_testing.config import TestConfiguration, OS
from ep_testing.tests.api import TestPythonAPIAccess, TestCAPIAccess, TestCppAPIDelayedAccess
from ep_testing.tests.energyplus import TestPlainDDRunEPlusFile
from ep_testing.tests.expand_objects import TestExpandObjectsAndRun
from ep_testing.tests.hvacdiagram import HVACDiagram
from ep_testing.tests.transition import TransitionOldFile


class Tester:

    def __init__(self, config: TestConfiguration, install_path: Path, verbose: bool):
        self.install_path = str(install_path)
        self.config = config
        self.verbose = verbose

    def run(self):
        saved_path = os.getcwd()
        TestPlainDDRunEPlusFile().run(
            self.install_path, self.verbose, {'test_file': '1ZoneUncontrolled.idf'}
        )
        TestPlainDDRunEPlusFile().run(
            self.install_path, self.verbose, {'test_file': 'PythonPluginCustomOutputVariable.idf'}
        )
        TestExpandObjectsAndRun().run(
            self.install_path, self.verbose, {'test_file': 'HVACTemplate-5ZoneFanCoil.idf'}
        )
        TransitionOldFile().run(
            self.install_path, self.verbose, {'last_version': self.config.tag_last_version}
        )
        HVACDiagram().run(
            self.install_path, self.verbose, {}
        )
        if self.config.os == OS.Windows:
            print("Windows Symlink runs are not testable on Travis, I think the user needs symlink privilege.")
        else:
            TestPlainDDRunEPlusFile().run(
                self.install_path, self.verbose, {'test_file': '1ZoneUncontrolled.idf', 'binary_sym_link': True}
            )
        TestCAPIAccess().run(
            self.install_path, self.verbose,
            {'os': self.config.os, 'bitness': self.config.bitness, 'msvc_version': self.config.msvc_version}
        )
        TestCppAPIDelayedAccess().run(
            self.install_path, self.verbose,
            {'os': self.config.os, 'bitness': self.config.bitness, 'msvc_version': self.config.msvc_version}
        )
        if self.config.bitness == 'x32':
            print("Travis does not have a 32-bit Python package readily available, so not testing Python API")
        else:
            TestPythonAPIAccess().run(
                self.install_path, self.verbose, {'os': self.config.os}
            )
        os.chdir(saved_path)
