#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This script ensures that files are encoded in UTF-8.

cf: https://github.com/NREL/EnergyPlus/issues/7213
Winter in Winter 2019
"""

__author__ = "Julien Marrec, EffiBEM"
__email__ = "julien@effibem.com"

import os
import sys
import json
import io  # For Python 2 compat
import glob as gb

# Get a path that'll work if run directly from this folder (when running
# locally usually) or the root of the repo (decent_ci for eg)
ROOT_DIR = os.path.abspath(
    os.path.join(os.path.dirname(os.path.realpath(__file__)),
                 "../../"))

# If you want this script to fix the encoding problems it finds
DO_FIX = False


def check_file_encoding(idf_path):
    """
    Verify that the file can be loaded with strict UTF-8 encoding,
    print a json message to console otherwise for decent_ci to consume
    """
    try:
        with io.open(idf_path, 'r', encoding='utf-8', errors='strict') as f:
            f.read()
        return True
    except UnicodeDecodeError:
        rel_file = os.path.relpath(idf_path, start=ROOT_DIR)
        ci_msg = {'tool': 'verify_file_encodings',
                  'filename': idf_path,
                  'file': idf_path,
                  'messagetype': 'error',
                  'message': "{} isn't UTF-8 encoded".format(rel_file)
                  }
        print(json.dumps(ci_msg))
        return False


def fix_encoding(idf_path):
    try:
        with io.open(idf_path, 'r', encoding='latin-1',
                     errors='strict') as f_in:
            idf_text = f_in.read()
        with io.open(idf_path, 'w', encoding='utf-8') as f_out:
            f_out.write(idf_text)

    except ValueError:
        rel_file = os.path.relpath(idf_path, start=ROOT_DIR)
        raise ValueError("Cannot fix encoding for {}".format(rel_file))


if __name__ == '__main__':
    # Glob all .idf / .imf
    # Glob recursive Works in python3.4 and above only...
    if sys.version_info > (3, 4):
        test_files = gb.glob(os.path.join(ROOT_DIR, '**/*.i[d|m]f'),
                             recursive=True)
    else:
        import fnmatch
        test_files = []
        for root, dirnames, filenames in os.walk(ROOT_DIR):
            for filename in fnmatch.filter(filenames, '*.i[d|m]f'):
                test_files.append(os.path.join(root, filename))

    for test_file in test_files:
        success = check_file_encoding(test_file)
        if DO_FIX and not success:
            # Fix encoding
            fix_encoding(test_file)
