#!/usr/bin/env python3

# OK, so when we are building on Github Actions, and we want to upload a package, we need to specify the file name
# we _could_ specify a pattern, but if you do that, even if it just finds one result, it will zip it up.
# We don't want zipped up artifacts, we want the raw installers.
# Since it will be very difficult to identify the package-name dynamically ahead of time, this script will simply
#  print the found name for the specific requested configuration
# The Github Action can then set the env variable for the package name and then it will have a single entry, unzipped

from pathlib import Path
import re
import sys


def usage():
    return """
# To use this, just pass in an argument for the build directory where the package files will live, and then a search key
# Example: python3 get_package_names.py /full/path/to/EnergyPlus/build mac_dmg
# Output, with no newline: EnergyPlus-9.4.0-4b487efdc8-Darwin-x86_64.dmg
# Exits with code 0 for success, 1 for bad arguments, 2 for no package found, and 3 for anything else
"""


class ExitCodes:
    Success = 0
    BadArguments = 1
    NoPackageFound = 2
    OtherError = 3


class ValidSearchKeys:
    Windows32Zip = 'win32_zip'
    Windows32Exe = 'win32_exe'
    Windows64Zip = 'win64_zip'
    Windows64Exe = 'win64_exe'
    MacTarball = 'mac_tar'
    MacDmg = 'mac_dmg'
    Ubuntu18Tarball = 'ubuntu18_tar'
    Ubuntu18Shell = 'ubuntu18_sh'
    Ubuntu18Run = 'ubuntu18_run'
    Ubuntu20Tarball = 'ubuntu20_tar'
    Ubuntu20Shell = 'ubuntu20_sh'
    Ubuntu20Run = 'ubuntu20_run'


ALL_VALID_SEARCH_KEYS = [
    ValidSearchKeys.Windows32Zip,
    ValidSearchKeys.Windows32Exe,
    ValidSearchKeys.Windows64Zip,
    ValidSearchKeys.Windows64Exe,
    ValidSearchKeys.MacTarball,
    ValidSearchKeys.MacDmg,
    ValidSearchKeys.Ubuntu18Tarball,
    ValidSearchKeys.Ubuntu18Shell,
    ValidSearchKeys.Ubuntu18Run,
    ValidSearchKeys.Ubuntu20Tarball,
    ValidSearchKeys.Ubuntu20Shell,
    ValidSearchKeys.Ubuntu20Run,
]

SEARCH_PATTERNS = {
    ValidSearchKeys.Windows32Zip: r'EnergyPlus-\d.\d.\d-.{10}-Windows-i386.zip',
    ValidSearchKeys.Windows32Exe: r'EnergyPlus-\d.\d.\d-.{10}-Windows-i386.exe',
    ValidSearchKeys.Windows64Zip: r'EnergyPlus-\d.\d.\d-.{10}-Windows-x86_64.zip',
    ValidSearchKeys.Windows64Exe: r'EnergyPlus-\d.\d.\d-.{10}-Windows-x86_64.exe',
    ValidSearchKeys.MacTarball: r'EnergyPlus-\d.\d.\d-.{10}-Darwin-x86_64.tar.gz',
    ValidSearchKeys.MacDmg: r'EnergyPlus-\d.\d.\d-.{10}-Darwin-x86_64.dmg',
    ValidSearchKeys.Ubuntu18Tarball: r'EnergyPlus-\d.\d.\d-.{10}-Linux-x86_64.tar.gz',
    ValidSearchKeys.Ubuntu18Shell: r'EnergyPlus-\d.\d.\d-.{10}-Linux-x86_64.sh',
    ValidSearchKeys.Ubuntu18Run: r'EnergyPlus-\d.\d.\d-.{10}-Linux-x86_64.run',
    ValidSearchKeys.Ubuntu20Tarball: r'EnergyPlus-\d.\d.\d-.{10}-Linux-x86_64.tar.gz',
    ValidSearchKeys.Ubuntu20Shell: r'EnergyPlus-\d.\d.\d-.{10}-Linux-x86_64.sh',
    ValidSearchKeys.Ubuntu20Run: r'EnergyPlus-\d.\d.\d-.{10}-Linux-x86_64.run',
}

if len(sys.argv) != 3:
    print(f"Invalid arguments, usage: {usage()}", file=sys.stderr)
    sys.exit(ExitCodes.BadArguments)

build_dir = Path(sys.argv[1])
if not build_dir.exists():
    print(f"Invalid argument, path to build dir (first argument) does not exist on disk", file=sys.stderr)
    sys.exit(ExitCodes.BadArguments)

search_key = sys.argv[2]
if search_key not in ALL_VALID_SEARCH_KEYS:
    print(f"Invalid argument, search key (second argument) is not in valid list", file=sys.stderr)
    sys.exit(ExitCodes.BadArguments)

regex_to_search = re.compile(SEARCH_PATTERNS[search_key])
found_matches = [x.name for x in build_dir.iterdir() if x.is_file() and regex_to_search.match(x.name)]
if len(found_matches) == 0:
    print(f"Invalid condition, did not find a package that matches the search key, check build", file=sys.stderr)
    sys.exit(ExitCodes.NoPackageFound)
elif len(found_matches) == 1:
    print(found_matches[0], end="")
    sys.exit(ExitCodes.Success)
if len(found_matches) > 1:
    print(f"Invalid condition, found more than one possible package name for this match, check build", file=sys.stderr)
    sys.exit(ExitCodes.OtherError)
