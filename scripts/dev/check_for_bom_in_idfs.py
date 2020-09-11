#!/usr/bin/env python

import codecs
import io
import json
import os
import sys


def usage():
    print("""This script verifies the IDFs don\'t have byte order marks.""")


current_script_dir = os.path.dirname(os.path.realpath(__file__))
test_files_dir = os.path.join(current_script_dir, '..', '..', 'testfiles')

num_issues_found = 0

for root, dirs, files in os.walk(test_files_dir):
    for sfile in files:
        if sfile.endswith('.idf') or sfile.endswith('.imf'):
            if root == test_files_dir:
                relative_path = sfile
            else:
                folder = os.path.basename(os.path.normpath(root))
                relative_path = os.path.join(folder, sfile)
            abs_path = os.path.join(test_files_dir, relative_path)
            with io.open(abs_path, 'rb') as fd:
                for i, line in enumerate(fd):
                    if codecs.BOM_UTF8 in line:
                        print(json.dumps({
                            'tool': 'check_for_bom_in_idfs',
                            'filename': os.path.join('testfiles', relative_path),
                            'file': os.path.join('testfiles', relative_path),
                            'line': i + 1,
                            'messagetype': 'error',
                            'message': 'Byte-Order-Mark sequence detected in IDF, check editor'
                        }))
                        num_issues_found += 1

if num_issues_found > 0:
    sys.exit(1)
