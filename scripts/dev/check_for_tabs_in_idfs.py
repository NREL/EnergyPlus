#!/usr/bin/env python

import io
import json
import os


def usage():
    print("""This script verifies that the idf files in the testfiles directory don't have tab characters.""")


current_script_dir = os.path.dirname(os.path.realpath(__file__))
test_files_dir = os.path.join(current_script_dir, '..', '..', 'testfiles')

for root, dirs, files in os.walk(test_files_dir):
    for sfile in files:
        if sfile.endswith('.idf') or sfile.endswith('.imf'):
            if root == test_files_dir:
                relative_path = sfile
            else:
                folder = os.path.basename(os.path.normpath(root))
                relative_path = os.path.join(folder, sfile)
            abs_path = os.path.join(test_files_dir, relative_path)
            with io.open(abs_path, 'r', encoding='utf-8', errors='strict') as fd:
                for i, line in enumerate(fd):
                    if '\t' in line:
                        print(json.dumps({
                            'tool': 'check_for_tabs_in_idfs',
                            'filename': os.path.join('testfiles', relative_path),
                            'file': os.path.join('testfiles', relative_path),
                            'line': i + 1,
                            'messagetype': 'error',
                            'message': 'Tab character found in IDF, use spaces for indentation'
                        }))
