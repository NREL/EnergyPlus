#!/usr/bin/env python

import codecs
import json
import os

DIRS_TO_SKIP = ['.git', 'build', 'builds', 'cmake-build-debug', 'cmake-build-release', 'design', 'release', 'third_party']
FILE_PATTERNS = ['.cc', '.hh', '.tex', '.cpp', '.hpp']

current_script_dir = os.path.dirname(os.path.realpath(__file__))
repo_root = os.path.abspath(os.path.join(current_script_dir, '..', '..'))
full_path_dirs_to_skip = [os.path.join(repo_root, d) for d in DIRS_TO_SKIP]

# str.decode and str.encode function can be very useful
files_with_non_utf8 = []
for root, dirs, filenames in os.walk(repo_root):
    if any(root.startswith(f) for f in full_path_dirs_to_skip):
        continue
    for filename in filenames:
        if not any(filename.endswith(f) for f in FILE_PATTERNS):
            continue
        file_path = os.path.join(root, filename)
        relative_file_path = os.path.relpath(file_path, repo_root)
        # print("Trying to open file: " + relative_file_path)
        line_num = 0
        try:
            with codecs.open(file_path, encoding='utf-8', errors='ignore') as f_idf:
                idf_text = f_idf.readlines()  # EDWIN: Make sure this reads the IDF properly
                for line in idf_text:
                    line_num += 1
                    line.decode('UTF-8', 'strict')
        except (UnicodeDecodeError, UnicodeEncodeError):
            # line contains non-utf8 character
            files_with_non_utf8.append({
                'tool': 'check_non_utf8',
                'filename': relative_file_path,
                'line': line_num,
                'messagetype': 'warning',
                'message': 'File had invalid characters/encoding issues'
            })
if len(files_with_non_utf8) > 0:
    print(json.dumps(files_with_non_utf8))
