#! /usr/bin/env python3
# This script will build and update the C documentation.
# The C docs are pre-built because the hoops to get doxygen based builds running on RTD and then wedged into sphinx
#  was a bit too much at the time of writing this.  It is certainly possible (probable) that this will be better
#  automated later.

# Dependencies to this:
#  - You'll need doxygen installed and available on PATH

from pathlib import Path
from shutil import copytree, rmtree
from subprocess import check_call, CalledProcessError, DEVNULL

DOXYGEN_BINARY = 'doxygen'  # could alter this locally if you need to point to a specific binary

# set up some file paths for convenience
this_file_path = Path(__file__)
rtd_dir = this_file_path.parent
doxygen_dir = rtd_dir / 'doxygen'
doxygen_html_output_dir = doxygen_dir / '_build' / 'html'
sphinx_dir = rtd_dir / 'sphinx'
target_c_prebuilt_dir = sphinx_dir / 'static' / 'c_prebuilt'

# test a file path to make sure we are in the right spot before trying to run
if doxygen_dir.exists():
    print("* Directory validation completed successfully")
else:
    raise Exception(f"Expected doxygen config dir to exist at \"{doxygen_dir}\" but it does not; aborting!")

# now try to run doxygen:
try:
    check_call([DOXYGEN_BINARY], cwd=doxygen_dir, stdout=DEVNULL, stderr=DEVNULL)
    print("* Doxygen completed successfully")
except CalledProcessError as e:
    raise Exception(f"Doxygen failed! Exception string: {str(e)}") from None
except FileNotFoundError as e:
    raise Exception(
        f"Doxygen binary not found, was it on path?  Looked for it at: {DOXYGEN_BINARY}; error = {str(e)}"
    ) from None

# ok, so doxygen should've run, validate the output directory exists
if doxygen_html_output_dir.exists():
    print("* Doxygen html output directory existence verified")
else:
    raise Exception(
        f"Although Doxygen appeared to run, the output directory is missing at {doxygen_html_output_dir}"
    ) from None

# alright, it exists, time to clean up the previous version, it should be there in some form, but I guess it is possible
#  that it is missing from some previous run, so we'll still check before just deleting it
if target_c_prebuilt_dir.exists():
    try:
        rmtree(target_c_prebuilt_dir)
        print("* Successfully deleted previous c_prebuilt html directory")
    except Exception as e:
        raise Exception(f"Could not delete existing c_prebuilt html directory") from None
else:
    print("* No c_prebuilt directory to remove, skipping this step")

# ok, now just copy it
try:
    copytree(doxygen_html_output_dir, target_c_prebuilt_dir)
    print("* Successfully copied doxygen output directory to c_prebuilt directory")
except Exception as e:
    raise Exception(
        f"Could not copy doxygen output from '{doxygen_html_output_dir}' to '{target_c_prebuilt_dir}'"
    ) from None

print("* Complete!  Now when you run `make html` in the sphinx directory it will process and include new C API docs")
