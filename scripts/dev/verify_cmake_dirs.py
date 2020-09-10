#!/usr/bin/env python3

import json
import os
import sys
from pathlib import Path

current_script_dir = Path(os.path.dirname(os.path.realpath(__file__)))
repo_root_dir = current_script_dir.parent.parent

common_build_folder_names = ['build', 'builds', 'cmake-build-debug', 'cmake-build-release']
common_cmake_file_names = ['CMakeLists.txt', '.cmake']
common_tokens_to_sanitize_out = ['ORIGINAL_CMAKE_SOURCE_DIR', 'ORIGINAL_CMAKE_BINARY_DIR']

DEBUG = False

num_issues_found = 0


def custom_check_output_line(relative_file_path: str, line_num: int, message: str) -> str:
    return json.dumps({
        'tool': 'verify_cmake_dirs',
        'file': relative_file_path,
        'line': line_num,
        'messagetype': 'error',
        'message': message
    })


for path in Path(repo_root_dir).rglob('*'):
    # only do files
    if not path.is_file():
        continue
    # get a nice relative path
    relative_path = path.relative_to(repo_root_dir)
    s_relative_path = str(relative_path)
    # skip any configured files in any known build folder
    in_build_folder = False
    for b in common_build_folder_names:
        if s_relative_path.startswith(b):
            in_build_folder = True
            break
    if in_build_folder:
        continue
    # ignore everything in third_party...right?
    if s_relative_path.startswith('third_party'):
        continue
    # try to match a common name or pattern
    for f in common_cmake_file_names:
        if s_relative_path.endswith(f):
            break
    else:
        continue
    if DEBUG:
        print('Processing %s ... ' % relative_path, end='')
    contents = path.read_text(encoding='utf-8', errors='ignore')
    lines = contents.split('\n')
    for i, line in enumerate(lines):
        # now sanitize the line just a bit
        for token in common_tokens_to_sanitize_out:
            line = line.replace(token, '')
        cmake_src = 'CMAKE_SOURCE_DIR' in line
        cmake_bin = 'CMAKE_BINARY_DIR' in line
        if cmake_src and cmake_bin:
            print(custom_check_output_line(
                s_relative_path, i+1, 'Found CMAKE_SOURCE_DIR and CMAKE_BINARY_DIR in file contents'
            ))
            num_issues_found += 1
        elif cmake_src:
            print(custom_check_output_line(
                s_relative_path, i+1, 'Found CMAKE_SOURCE_DIR in file contents'
            ))
            num_issues_found += 1
        elif cmake_bin:
            print(custom_check_output_line(
                s_relative_path, i+1, 'Found CMAKE_BINARY_DIR in file contents'
            ))
            num_issues_found += 1
        else:
            if DEBUG:
                print(' [DONE]')

if num_issues_found > 0:
    sys.exit(1)
