#!/bin/bash

REPO_ROOT=$1

EXIT_STATUS=0

python3 "$REPO_ROOT"/scripts/dev/license-check.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/check_stray_fields_in_idd.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/verify_idfs_in_cmake.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/check_non_utf8.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/verify_file_encodings.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/validate_idd_units.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/find_byref_bool_override.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/check_for_tabs_in_idfs.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/check_for_bom_in_idfs.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/verify_cmake_dirs.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/find_included_cc_files.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/analyze_state.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/check_for_c_style_comments.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/check_constexpr.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/check_for_switch_case_parentheses.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/check_for_malformed_enums.py || EXIT_STATUS=$?
python3 "$REPO_ROOT"/scripts/dev/check_format_strings.py || EXIT_STATUS=$?

exit $EXIT_STATUS
