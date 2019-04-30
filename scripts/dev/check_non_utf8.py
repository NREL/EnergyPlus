#!/usr/bin/env python

import codecs
import json
import os

DIRS_TO_SKIP = [  # note I am skipping docs for right now; I want to do those files
    '.git', 'build', 'builds', 'cmake-build-debug', 'cmake-build-release', 'design', 'doc', 'release', 'third_party'
]
FILE_NAMES_TO_SKIP = [  # these CC files purposefully have bad characters - not sure what to do besides ignore them
    'InputProcessor.unit.cc', 'EconomicTariff.cc', 'OutputReportTabular.cc'
]
FILE_PATTERNS = [  # tex files are included here, but the docs folder is ignored, so it has no effect right now
    '.cc', '.hh', '.tex', '.cpp', '.hpp'
]

current_script_dir = os.path.dirname(os.path.realpath(__file__))
repo_root = os.path.abspath(os.path.join(current_script_dir, '..', '..'))
full_path_dirs_to_skip = [os.path.join(repo_root, d) for d in DIRS_TO_SKIP]

for root, dirs, filenames in os.walk(repo_root):
    if any(root.startswith(f) for f in full_path_dirs_to_skip):
        continue
    for filename in filenames:
        if not any(filename.endswith(f) for f in FILE_PATTERNS):
            continue
        if any(f in filename for f in FILE_NAMES_TO_SKIP):
            continue
        file_path = os.path.join(root, filename)
        relative_file_path = os.path.relpath(file_path, repo_root)
        with codecs.open(file_path, encoding='utf-8', errors='ignore') as f_idf:
            idf_text = f_idf.readlines()  # EDWIN: Make sure this reads the IDF properly
            line_num = 0
            for line in idf_text:
                try:
                    line_num += 1
                    line.decode('UTF-8', 'strict')
                except (UnicodeDecodeError, UnicodeEncodeError):
                    # line contains non-utf8 character
                    print(json.dumps({
                        'tool': 'check_non_utf8',
                        'filename': relative_file_path,
                        'file' : relative_file_path,
                        'line': line_num,
                        'messagetype': 'warning',
                        'message': 'File had invalid characters/encoding issues'
                    }))
