import datetime
import json
import glob
import os
#
# The previous year that is in the license. It should be a string
#
_previous_year = '2019'
#
# From file "EnergyPlus License DRAFT 112015 100 fixed.txt"
#
# This is the license text EXACTLY as agreed upon with LBL IPO, so don't
# ever change it. EVER. YES I MEAN EVER.
_original = """// EnergyPlus, Copyright (c) 1996-2015, The Board of Trustees of the University of Illinois and
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

def previous():
    '''Return the previous license text, last changed January 2, 2019.'''
    # Modify the year in the text
    originalYear = '2015'
    currentYear = _previous_year
    txt = _original.replace(originalYear, currentYear)
    # Modify and delete some lines with LBL IP permission
    # Keep in mind that the line numbering here starts with 0
    lines = txt.splitlines()
    # On line 37, replace LBNL with USDOE
    lines[37] = lines[37].replace('Lawrence Berkeley National Laboratory',
                                  'the U.S. Department of Energy')
    # Delete the last 9 lines
    lines = lines[:-9]
    # Delete lines 4-6
    lines = lines[:4] + lines[7:]
    # Modify the notice
    lines[0] = lines[0].replace(' and', ',')
    lines[2] = '// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge'
    lines.insert(3, '// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other')
    lines[4] = '// contributors. All rights reserved.'
    txt = '\n'.join(lines)+'\n'
    return txt

def current():
    '''Return the current license text, as of today.'''
    # Modify the year in the text
    originalYear = '2015'
    currentYear = '%d' % datetime.date.today().year
    txt = _original.replace(originalYear, currentYear)
    # Modify and delete some lines with LBL IP permission
    # Keep in mind that the line numbering here starts with 0
    lines = txt.splitlines()
    # On line 37, replace LBNL with USDOE
    lines[37] = lines[37].replace('Lawrence Berkeley National Laboratory',
                                  'the U.S. Department of Energy')
    # Delete the last 9 lines
    lines = lines[:-9]
    # Delete lines 4-6
    lines = lines[:4] + lines[7:]
    # Modify the notice
    lines[0] = lines[0].replace(' and', ',')
    lines[2] = '// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge'
    lines.insert(3, '// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other')
    lines[4] = '// contributors. All rights reserved.'
    txt = '\n'.join(lines)+'\n'
    
    return txt

def original():
    '''Return the original BSD3+ license text from 2015.'''
    return _original

def error(dictionary):
    '''Default method for output of JSON-style messages for decent_ci.'''
    print(json.dumps(dictionary))

def checkLicense(filename,possible,correct,offset=0,toolname='unspecified',
                 message=error):
    '''Check for a few of the usual issues with the license'''
    if possible == correct:
        return
    try:
        possibleYear = possible[offset+31:offset+35]
        correctYear = correct[offset+31:offset+35]
    except IndexError:
        message({'tool':toolname,
                 'filename':filename,
                 'file':filename,
                 'line':1,
                 'messagetype':'error',
                 'message':'License text cannot be matched, check entire license'})
        return
    try:
        int(possibleYear)
        if possibleYear != correctYear:
            message({'tool':toolname,
                     'filename':filename,
                     'file':filename,
                     'line':1,
                     'messagetype':'error',
                     'message':'License year is incorrect'})
            corrected = possible[:offset+31]+correctYear+possible[offset+35:]
            if corrected == correct:
                return
    except:
        message({'tool':toolname,
                 'filename':filename,
                 'file':filename,
                 'line':1,
                 'messagetype':'error',
                 'message':'License text cannot be matched, check entire license'})
        return
    message({'tool':toolname,
             'filename':filename,
             'file':filename,
             'line':1,
             'messagetype':'error',
             'message':'Non-year differences in license text, check entire license'})


def mergeParagraphs(text):
    '''Merge license text lines into a single line per paragraph.'''
    lines = []
    current = ''
    for line in text.splitlines():
        line = line.rstrip()
        if line == '//':
            lines.extend([current.lstrip(), ''])
            current = ''
        else:
            current += ' ' + line[2:].lstrip()
    lines.append(current.lstrip())
    return '\n'.join(lines)+'\n'


class FileVisitor:
    def __init__(self, extensions = None):
        self.visited_files = []
        if extensions == None:
            self.extensions = ['cc', 'cpp', 'c', 'hh', 'hpp', 'h']
        else:
            self.extensions = extensions

    def files(self, path):
        results = []
        for ext in self.extensions:
            results.extend(glob.glob(path+'**/*.'+ext, recursive=True))
        return results

    def visit_file(self, filepath):
        pass

    def error(self, file, line_number, mesg):
        pass

    def visit(self, path):
        for file in self.files(path):
            self.visit_file(file)
            self.visited_files.append(file)

    def readtext(self, filepath):
        fp = open(filepath, 'r', encoding='utf-8')
        try:
            txt = fp.read()
        except UnicodeDecodeError as exc:
            self.error(filepath, 0, 'UnicodeDecodeError: '+ str(exc))
            txt = None
        except Exception as exc:
            self.error(filepath, 0, 'Exception: '+ str(exc))
            txt = None
        fp.close()
        return txt


class Checker(FileVisitor):
    def __init__(self, boilerplate, toolname='unspecified'):
        super().__init__()
        lines = boilerplate.splitlines()
        self.n = len(lines)
        self.text = boilerplate
        self.toolname = toolname

    def error(self, file, line_number, mesg):
        dictionary = {'tool':self.toolname,
                      'filename':file,
                      'file':file,
                      'line':line_number,
                      'messagetype':'error',
                      'message':mesg}
        print(json.dumps(dictionary))

    def visit_file(self, filepath):
        txt = self.readtext(filepath)
        if txt != None:
            n = txt.count(self.text)
            if n == 0:
                lines = txt.splitlines()[:self.n]
                shortened = '\n'.join(lines)+'\n'
                checkLicense(filepath,shortened,self.text,offset=3,
                             toolname=self.toolname,message=error)
            else:
                if n > 1:
                    self.error(filepath, 1, 'Multiple instances of license text')
                if not txt.startswith(self.text):
                    self.error(filepath, 1, 'License text is not at top of file')


class Replacer(FileVisitor):
    def __init__(self, oldtext, newtext, dryrun=True):
        super().__init__()
        self.oldtxt = oldtext
        self.newtxt = newtext
        self.dryrun = dryrun
        self.replaced = []
        self.failures = []

    def error(self, file, line, mesg):
        self.failures.append(file + ', ' + mesg)

    def writetext(self, filepath, txt):
        fp = open(filepath, 'w', encoding='utf-8')
        fp.write(txt)
        fp.close()

    def visit_file(self,filepath):
        txt = self.readtext(filepath)
        if txt != None:
            if self.dryrun:
                if self.oldtxt in txt:
                    self.replaced.append(filepath)
            else:
                txt = txt.replace(self.oldtxt, self.newtxt)
                if self.newtxt in txt:
                    self.writetext(filepath, txt)
                    self.replaced.append(filepath)

    def summary(self):
        txt = ['Checked %d files' % len(self.visited_files)]
        if self.dryrun:
            txt.append('Would have replaced text in %d files' % len(self.replaced))
        else:
            txt.append('Replaced text in %d files' % len(self.replaced))
        if len(self.failures):
            txt.append('Failures in %d files' % len(self.failures))
            for message in self.failures:
                txt.append('\t' + message)
        return '\n'.join(txt)

    def report(self):
        remaining = self.visited_files[:]
        txt = ['Replaced text in the following files']
        for file in self.replaced:
            remaining.remove(file)
            txt.append('\t'+file)
        txt.append('No changes made to the following files')
        for file in remaining:
            txt.append('\t'+file)
        return self.summary() + '\n\n' + '\n'.join(txt)


if __name__ == '__main__':
    text = current()
    print(text)
    for number,line in enumerate(text.splitlines()):
        print('%2d' % number,line)
