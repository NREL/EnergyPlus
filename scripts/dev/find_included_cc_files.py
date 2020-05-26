#!/usr/bin/env python

import json
import os
import io  # For Python 2 compat

dirs_to_search = [os.path.join('src', 'EnergyPlus'), os.path.join('tst', 'EnergyPlus', 'unit')]
current_script_dir = os.path.dirname(os.path.realpath(__file__))
repo_root = os.path.abspath(os.path.join(current_script_dir, '..', '..'))
full_path_dirs_to_search = [os.path.join(repo_root, d) for d in dirs_to_search]

for this_root in full_path_dirs_to_search:
    for root, dirs, filenames in os.walk(this_root):
        for filename in filenames:
            if len(filename) < 3:
                continue
            if not filename[-3:] in ['.cc', '.hh']:
                continue
            file_path = os.path.join(root, filename)
            relative_file_path = os.path.relpath(file_path, repo_root)
            # print("processing file: " + relative_file_path)
            # if 'LowTempRadiantSystem.unit.cc' in relative_file_path:
            #     i = 1
            try:
                with io.open(file_path, encoding='utf-8', errors='strict') as f_idf:
                    idf_text = f_idf.read()
                    for line in idf_text.split('\n'):
                        line = line.strip()
                        if '#include' in line and '<' in line and '>' in line:
                            include_token = line.split('<')[1].split('>')[0].strip()
                            if '.cc' in include_token:
                                ci_msg = {'tool': 'find_included_cc_files',
                                          'filename': filename,
                                          'file': relative_file_path,
                                          'messagetype': 'error',
                                          'message': ("Found included CC file: '{}'".format(line))
                                          }
                                print(json.dumps(ci_msg))
            except Exception:
                # don't do anything, there are reasons this is ok to fail
                print("XCE")
