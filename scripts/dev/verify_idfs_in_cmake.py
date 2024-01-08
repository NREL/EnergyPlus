#!/usr/bin/env python
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

import json
import os
import re
import sys


current_script_dir = os.path.dirname(os.path.realpath(__file__))
test_files_dir = os.path.join(current_script_dir, '..', '..', 'testfiles')

cmake_lists_file = os.path.join(test_files_dir, "CMakeLists.txt")
cmake_list_idf_files = set()
with open(cmake_lists_file) as f:
    contents = f.read()
    matches = re.findall(r'\(([^)]+)\)', contents, re.MULTILINE)
    for match in matches:
        if 'IDF_FILE' not in match:
            continue
        cleaned_match = match.replace('\n', '')
        tokens = cleaned_match.split()  # special case that allows multiple whitespace delimiters
        filename = tokens[1]
        cmake_list_idf_files.add(filename)

found_idf_files = set()
for root, dirs, files in os.walk(test_files_dir):
    for sfile in files:
        if sfile.endswith('.idf') or sfile.endswith('.imf'):
            if root == test_files_dir:
                found_idf_files.add(sfile)
            else:
                folder = os.path.basename(os.path.normpath(root))
                found_idf_files.add(os.path.join(folder, sfile))

# there are a few files we purposely skip
files_to_skip = {"_1a-Long0.0.idf", "_ExternalInterface-actuator.idf", "_ExternalInterface-schedule.idf",
                 "_ExternalInterface-variable.idf", "HVAC3Zone-IntGains-Def.imf", "HVAC3ZoneChillerSpec.imf",
                 "HVAC3ZoneGeometry.imf", "HVAC3ZoneMat-Const.imf"}
found_idf_files_trimmed = found_idf_files - files_to_skip

# the CMakeLists file will always have forward slashes
# on Linux and Mac, the found_idfs will also have forward slashes
# but on Windows, the path delimiter will be a backslash
# so replace all backslashes here before comparing anything.
found_idf_files_refined = set()
for fil in found_idf_files_trimmed:
    found_idf_files_refined.add(fil.replace("\\", "/"))

# check if any are missing in cmake
need_to_add_to_cmake = found_idf_files_refined.difference(cmake_list_idf_files)
if len(need_to_add_to_cmake) > 0:
    for this_file in sorted(need_to_add_to_cmake):
        print(json.dumps({
            'tool': 'verify_idfs_in_cmake',
            'filename': this_file,
            'file': this_file,
            'line': 0,
            'messagetype': 'error',
            'message': 'File missing from testfiles/CMakeLists.txt'
        }))
    sys.exit(1)
