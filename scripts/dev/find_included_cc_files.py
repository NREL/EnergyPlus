#!/usr/bin/env python

import json
import os
import io  # For Python 2 compat
import sys

dirs_to_search = [os.path.join('src', 'EnergyPlus'), os.path.join('tst', 'EnergyPlus', 'unit')]
current_script_dir = os.path.dirname(os.path.realpath(__file__))
repo_root = os.path.abspath(os.path.join(current_script_dir, '..', '..'))
full_path_dirs_to_search = [os.path.join(repo_root, d) for d in dirs_to_search]

num_issues_found = 0

for this_root in full_path_dirs_to_search:
    for root, dirs, filenames in os.walk(this_root):
        for filename in filenames:
            if len(filename) < 3:
                continue
            if not filename[-3:] in ['.cc', '.hh']:
                continue
            file_path = os.path.join(root, filename)
            relative_file_path = os.path.relpath(file_path, repo_root)
            try:
                with io.open(file_path, encoding='utf-8', errors='strict') as f_idf:
                    idf_text = f_idf.read()
                    line_num = 0
                    for line in idf_text.split('\n'):
                        line_num += 1
                        line = line.strip()
                        if '#include' in line and '<' in line and '>' in line:
                            include_token = line.split('<')[1].split('>')[0].strip()
                            if '.cc' in include_token:
                                ci_msg = {'tool': 'find_included_cc_files',
                                          'filename': filename,
                                          'file': relative_file_path,
                                          'line': line_num,
                                          'messagetype': 'error',
                                          'message': ("Found included CC file: '{}'".format(line))
                                          }
                                print(json.dumps(ci_msg))
                                num_issues_found += 1
            except Exception:
                # don't do anything, there are reasons this is ok to fail
                print("XCE")

if num_issues_found > 0:
    sys.exit(1)
