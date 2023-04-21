#!/usr/bin/env python
# EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University
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
#
# Use this script to update the license text
#
# Directions: These directions assume you're working on a branch. First, check
# that the "dryrun" parameter below is set to True. This will run the script
# and check the files but will not actually make any changes. There are large
# number of files, so it's best to make sure that everything makes sense before
# actually making the change. Next, execute the script from the root of the
# EnergyPlus source tree:
#
#   $> python ./scripts/dev/license-update.py
#   Skipping writing out LICENSE.txt
#   Checked 809 files
#   Would have replaced text in 0 files
#
# The number of files will likely change, this is as of 1/2/2019. Note that it
# says that 0 files would have been modified. This is because it isn't looking
# for the right text.
#
# In the file "licensetext.py", find the variable named "_previous_year" and
# change it to the year that is to be replaced. In 2019, the variable was set
# to '2018'. Note that it should be a string. Run the script again:
#
#   $> python ./scripts/dev/license-update.py
#   Skipping writing out LICENSE.txt
#   Checked 809 files
#   Would have replaced text in 809 files
#
# We're ready to go. Change the "dryrun" parameter below to "False" and run
# the script again:
#
#   $> python ./scripts/dev/license-update.py
#   Writing out LICENSE.txt
#   Checked 809 files
#   Replaced text in 809 files
#
# Finally, change the "dryrun" parameter back to "True". Commit your changes
# and you're done.
#
# Keep in mind that this was originally written to handle C++ alone within the
# context of decent_ci. Python support came much later, so there are some
# things (like how files and exclusions are handled) that are a little less
# than uniform.
#
# For 2023, used the following to find some missed files:
#
#   $ ls -lR | grep -e '\.cpp$' -e '\.hpp$' -e '\.hh$' -e '\.cc$' -e '\.c$' -e '\.h$' |  wc -l
#
# and
#
#   $  find ./ | grep -e '\.py$' | grep -v third_party | grep -v readthedocs | grep -v bin | grep -v build | wc -l
#
# These are both pretty awful and need to be improved, and it would help if the
# exclusions were uniform.

import licensetext
import argparse

parser = argparse.ArgumentParser(description='Update the E+ license year.')
parser.add_argument('-v', '--verbose', dest='verbose', action='store_true',
                    default=False, help='operate verbosely')
# Maybe next year
#parser.add_argument('--update', dest='dryrun', action='store_false',
#                    default=True, help='update the license year')

args = parser.parse_args()

TOOL_NAME = 'license-update'
dryrun = True

#
# Directories to check
#
cpp_dirs = ["./src/",
            "./tst/EnergyPlus/"]
python_dirs = ["./"]

# Get the C++ current text
current = licensetext.current()
previous = licensetext.previous()

# Create LICENSE.txt
licensetxt = licensetext.merge_paragraphs(current)

if not dryrun:
    print('Writing out LICENSE.txt')
    filename = "LICENSE.txt"
    fp = open(filename, 'w')
    fp.write(licensetxt)
    fp.close()
else:
    print('Skipping writing out LICENSE.txt')

full_count = 1

# Create C++ Replacer object
replacer = licensetext.Replacer(previous, current, dryrun=dryrun)

# Check C++ files
for base in cpp_dirs:
    replacer.visit(base)

print('\nC++ Summary')
print(replacer.summary())
full_count += len(replacer.replaced)

# Get the Python current text
current = licensetext.current_python()
previous = licensetext.previous_python()

# Create Python Replacer object
replacer = licensetext.Replacer(previous, current, extensions=['py'],
                                dryrun=dryrun)

# Check Python files
patterns = [r'.*third_party.*', r'^\.(\\|/)build.*',
            r'^\.(\\|/)bin.*', r'.*readthedocs.*',
            r'.*venv.*', r'.*cmake-build-.*']
for base in python_dirs:
    replacer.visit(base, exclude_patterns=patterns)

print('\nPython Summary')
print(replacer.summary(full_report=args.verbose))
full_count += len(replacer.replaced)
print('\nFull count of files: %d' % full_count)
