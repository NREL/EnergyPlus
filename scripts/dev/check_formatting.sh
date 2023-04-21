#!/bin/bash

# On Github, we use a separate clang-format action to handle Clang-Format checks
# This script is simply a convenient check that is not guaranteed to exactly match the one in the action.
# But it should be close, maybe close enough to create a pre-commit check or pre-push check

# The version of Clang-Format that we use on Github Actions is defined in the workflow file.
# At the time of this writing we are using 10, which is at /usr/bin/clang-format-10 on a Linux box if apt installed.
# I will make this script version agnostic but know that different versions may have slightly different behavior.

# Run this script from the root of the repo and it will check the files in src/EnergyPlus and tst/EnergyPlus/unit

exit_code=0
for dir in "src/EnergyPlus" "tst/EnergyPlus/unit"; do
  echo "***Processing files in directory: ${dir}"
  files=$(find "${dir}" | grep -E '\.((c|C)c?(pp|xx|\+\+)*$|(h|H)h?(pp|xx|\+\+)*$)')
  for file in $files; do
    if ! /usr/bin/clang-format-10 --dry-run --Werror --style=file "${file}";
    then
		  exit_code=1
		fi
  done
done
exit "$exit_code"
