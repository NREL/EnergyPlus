#!/usr/bin/env python

import os
import glob
import datetime
import json

TOOL_NAME = 'license-check'

#
# Directories to check
#
dirs = ["./src/EnergyPlus/",
        "./tst/EnergyPlus/unit/"]

#
# From file "EnergyPlus License DRAFT 112015 100 fixed.txt"
#
# This is the license text EXACTLY as agreed upon with LBL IPO, so don't
# ever change it. EVER. YES I MEAN EVER.
preamble = """// EnergyPlus, Copyright (c) 1996-2015, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.
"""

def error(dictionary):
    print(json.dumps(dictionary))

def checkLicense(filename,possible,correct,offset=0):
    if possible == correct:
        return
    try:
        possibleYear = possible[offset+31:offset+35]
        correctYear = correct[offset+31:offset+35]
    except IndexError:
        error({'tool':TOOL_NAME,
               'filename':filename,
               'line':1,
               'messagetype':'error',
               'message':'License text cannot be matched, check entire license'})
        return
    try:
        int(possibleYear)
        if possibleYear != correctYear:
            error({'tool':TOOL_NAME,
                   'filename':filename,
                   'line':1,
                   'messagetype':'error',
                   'message':'License year is incorrect'})
            corrected = possible[:offset+31]+correctYear+possible[offset+35:]
            if corrected == correct:
                return
    except:
        error({'tool':TOOL_NAME,
               'filename':filename,
               'line':1,
               'messagetype':'error',
               'message':'License text cannot be matched, check entire license'})
        return
    error({'tool':TOOL_NAME,
           'filename':filename,
           'line':1,
           'messagetype':'error',
           'message':'Non-year differences in license text, check entire license'})

class Checker:
    def __init__(self, boilerplate):
        lines = boilerplate.splitlines()
        self.n = len(lines)
        self.text = boilerplate
    def check(self, path):
        srcs = glob.glob(path+'*.cc')
        hdrs = glob.glob(path+'*.hh')
        hdrs.extend(glob.glob(path+'*.h'))
        for file in srcs + hdrs:
            fp = open(file,'r')
            txt = fp.read()
            fp.close()
            n = txt.count(self.text)
            if n == 0:
                lines = txt.splitlines()[:self.n]
                shortened = '\n'.join(lines)+'\n'
                checkLicense(file,shortened,preamble,offset=3)
            else:
                if n > 1:
                    error({'tool':TOOL_NAME,
                           'filename':file,
                           'line':1,
                           'messagetype':'error',
                           'message':'Multiple instances of license text'})
                if not txt.startswith(self.text):
                    error({'tool':TOOL_NAME,
                           'filename':file,
                           'line':1,
                           'messagetype':'error',
                           'message':'License text is not at top of file'})                                      

# Modify the year in the text
originalYear = '2015'
currentYear = '%d' % datetime.date.today().year
preamble = preamble.replace(originalYear, currentYear)

# Check LICENSE.txt
# Create the text as it should be
lines = []
current = ''
for line in preamble.splitlines():
    line = line.rstrip()
    if line == '//':
        lines.extend([current.lstrip(), ''])
        current = ''
    else:
        current += ' ' + line[2:].lstrip()
lines.append(current.lstrip())
licensetxt = '\n'.join(lines)+'\n'
# Load the text file
filename = "LICENSE.txt"
fp = open(filename)
filetxt = fp.read()
fp.close()
# Compare the two strings
checkLicense('LICENSE.txt',filetxt,licensetxt)

# Create Checker object
checker = Checker(preamble)

#checker.check(file)

# Check files
for base in dirs:
    subdirs = glob.glob(base + '*/')
    checker.check(base)
    for sub in subdirs:
        checker.check(sub)
