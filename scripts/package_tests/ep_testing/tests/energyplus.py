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
import subprocess

from ep_testing.tests.base import BaseTest


class TestPlainDDRunEPlusFile(BaseTest):

    def name(self):
        return 'Test running IDF and make sure it exits OK'

    def run(self, install_root: str, verbose: bool, kwargs: dict):
        if 'test_file' not in kwargs:
            raise Exception('Bad call to %s -- must pass test_file in kwargs' % self.__class__.__name__)
        test_file = kwargs['test_file']
        print('* Running test class "%s" on file "%s"... ' % (self.__class__.__name__, test_file), end='')
        eplus_binary = os.path.join(install_root, 'energyplus')
        idf_path = os.path.join(install_root, 'ExampleFiles', test_file)
        if 'binary_sym_link' in kwargs:
            eplus_binary_to_use = os.path.join(os.getcwd(), 'ep_symlink')
            if verbose:
                print(f' [SYM-LINKED at {eplus_binary_to_use}]', end='')
            else:
                print(' [SYM-LINKED]', end='')
            os.symlink(eplus_binary, eplus_binary_to_use)
        else:
            eplus_binary_to_use = eplus_binary

        # print(f"Attempting to execute the EnergyPlus binary located at: {eplus_binary}")
        # print(f"Does it exist? {os.path.exists(eplus_binary)}")
        cmd = [eplus_binary_to_use, '-D', idf_path]
        r = subprocess.run(cmd,
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        if r.returncode == 0:
            print(' [DONE]!')
        else:
            raise Exception(
                'EnergyPlus failed!\n'
                f'Command {cmd} failed with exit status {r.returncode}!\n'
                'stderr:\n'
                f'{r.stderr.decode().strip()}'
                '\n\n'
                'stdout:\n'
                f'{r.stdout.decode().strip()}')
