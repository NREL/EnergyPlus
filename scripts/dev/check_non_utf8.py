#!/usr/bin/env python

import json
import os
import io  # For Python 2 compat

# note I am skipping docs for right now; I want to do those files
DIRS_TO_SKIP = [
    '.git', 'build', 'builds', 'cmake-build-debug',
    'cmake-build-release', 'design', 'doc', 'release', 'third_party'
]

# these CC files purposefully have bad characters
# not sure what to do besides ignore them
FILE_NAMES_TO_SKIP = [
    # 'InputProcessor.unit.cc', 'EconomicTariff.cc',
    # 'OutputReportTabular.cc'
]

# tex files are included here, but the docs folder is ignored,
# so it has no effect right now
FILE_PATTERNS = [
    '.cc', '.hh', '.tex', '.cpp', '.hpp',
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

        # High level check: try to open the file as utf-8 in full
        try:
            with io.open(file_path, encoding='utf-8',
                         errors='strict') as f_idf:
                idf_text = f_idf.read()
        except UnicodeDecodeError:
            ci_msg = {'tool': 'verify_file_encodings',
                      'filename': filename,
                      'file': relative_file_path,
                      'messagetype': 'error',
                      'message': ("{} isn't UTF-8 encoded"
                                  "".format(relative_file_path))
                      }
            print(json.dumps(ci_msg))

            # Now you try to give a better error message by pointing at the
            # lines that are guilty. To do so, you open as binary, and try to
            # decode each line as utf-8
            with io.open(file_path, 'rb') as f_idf:
                binary_lines = f_idf.readlines()

            for line_num, line in enumerate(binary_lines):
                try:
                    line.decode(encoding='utf-8', errors='strict')
                    # codecs.decode(line, encoding='utf-8', errors='strict')
                except (UnicodeDecodeError, UnicodeEncodeError):
                    _l = line.decode(encoding='utf-8', errors='replace')

                    # line contains non-utf8 character
                    print(json.dumps({
                        'tool': 'check_non_utf8',
                        'filename': relative_file_path,
                        'file': relative_file_path,
                        'line': line_num,
                        'messagetype': 'warning',
                        # " {}".format(_l)) # only works python3
                        'message': ("Line has invalid characters/encoding: " +
                                    _l)
                    }))
