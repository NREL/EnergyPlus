#!/bin/bash

REPO_ROOT=$1

EXIT_STATUS=0

function execute {
  echo "Running Custom Check: \"$*\""
  OUTPUT=$("$@" 2>&1)
  # shellcheck disable=SC2181
  if [ $? -ne 0 ]; then
    echo "**FAILURE**: $OUTPUT"
    EXIT_STATUS=1
  fi
}

execute python3 "$REPO_ROOT"/scripts/dev/license-check.py
execute python3 "$REPO_ROOT"/scripts/dev/check_stray_fields_in_idd.py
execute python3 "$REPO_ROOT"/scripts/dev/verify_idfs_in_cmake.py
execute python3 "$REPO_ROOT"/scripts/dev/check_non_utf8.py
execute python3 "$REPO_ROOT"/scripts/dev/verify_file_encodings.py
execute python3 "$REPO_ROOT"/scripts/dev/validate_idd_units.py
execute python3 "$REPO_ROOT"/scripts/dev/find_byref_bool_override.py
execute python3 "$REPO_ROOT"/scripts/dev/check_for_tabs_in_idfs.py
execute python3 "$REPO_ROOT"/scripts/dev/check_for_bom_in_idfs.py
execute python3 "$REPO_ROOT"/scripts/dev/verify_cmake_dirs.py
execute python3 "$REPO_ROOT"/scripts/dev/find_included_cc_files.py
execute python3 "$REPO_ROOT"/scripts/dev/analyze_state.py
execute python3 "$REPO_ROOT"/scripts/dev/check_for_c_style_comments.py
execute python3 "$REPO_ROOT"/scripts/dev/check_constexpr.py
execute python3 "$REPO_ROOT"/scripts/dev/check_for_switch_case_parentheses.py
execute python3 "$REPO_ROOT"/scripts/dev/check_for_malformed_enums.py

exit $EXIT_STATUS
