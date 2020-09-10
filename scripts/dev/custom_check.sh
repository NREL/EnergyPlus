#!/bin/bash

REPO_ROOT=$1

EXIT_STATUS=0
python3 "$REPO_ROOT"/scripts/dev/license-check.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/check_stray_fields_in_idd.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/verify_idfs_in_cmake.py || EXIT_STATUS=$?
exit $EXIT_STATUS

#    - ./scripts/dev/check_non_utf8.py
#    - ./scripts/dev/check_pr_labels.py -- use https://github.com/jesusvasquez333/verify-pr-label-action
#    - ./scripts/dev/verify_file_encodings.py
#    - ./scripts/dev/validate_idd_units.py
#    - ./scripts/dev/find_byref_bool_override.py
#    - ./scripts/dev/check_for_tabs_in_idfs.py
#    - ./scripts/dev/check_for_bom_in_idfs.py
#    - ./scripts/dev/verify_cmake_dirs.py
#    - ./scripts/dev/find_included_cc_files.py
