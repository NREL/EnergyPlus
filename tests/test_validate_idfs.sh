#!/bin/bash

function purpose() {
	echo ""
	echo " ** Purpose: "
	echo "This compares the testfiles idf contents against the CMakeLists.txt file"
}

function call_signature() {
	echo ""
	echo " ** Usage: "
	echo "  test_validate_idfs.sh <EnergyPlusTeam_repository_root>"
	echo "     where:"
	echo "    <EnergyPlusTeam_repository_root> -- root of repository; should contain testfiles directory with idfs and the CMakeLists.txt file"
}

function prerequisites() {
	echo ""
	echo " ** Prerequisites: "
	echo "Application requires the following tools available in the running shell:"
	echo "  grep"
	echo "  echo"
}

function usage() {
	purpose
	call_signature
	prerequisites
}

## Output:
### 2 text lines in the following format
### <N> Files common -- good
### <M> IDF Files in testfiles but not in CMakeLists
### where <M> and <N> are integers, padded to four characters with leading zeroes
### Followed by a list of lines in the following form, one line per missing file:
###  Missing from CMakeList: ${FILENAME}

## Return Value:
EXIT_ALLGOOD=0               ### Returns 0 if all files are common
EXIT_WARNINGDIFFS=1          ### Returns 1 if there are any idfs missing in CMakeLists
EXIT_BADSIGNATURE=101        ### Returns >100 if other issue occurred
EXIT_MISSINGARGUMENTDIR=102  ### Returns >100 if other issue occurred
EXIT_MISSINGTESTFILESDIR=103 ### Returns >100 if other issue occurred
EXIT_MISSINGCMAKELIST=104    ### Returns >100 if other issue occurred

# Process command line signature
if [ $# -ne 1 ]; then
	usage
	exit $EXIT_BADSIGNATURE
fi

# Process command line argument
REPO_DIR=$1
if [ ! -d "${REPO_DIR}" ]; then
	echo "Argument directory not found.  Expected directory at \"${REPO_DIR}\""
	exit $EXIT_MISSINGARGUMENTDIR
fi
TESTFILES_DIR="${REPO_DIR}/testfiles"
if [ ! -d "${TESTFILES_DIR}" ]; then
	echo "Test files directory not found.  Expected directory at \"${TESTFILES_DIR}\""
	exit $EXIT_MISSINGTESTFILESDIR
fi
CMAKEFILE="${TESTFILES_DIR}/CMakeLists.txt"
if [ ! -f "${CMAKEFILE}" ]; then
	echo "CMakeLists.txt file not found.  Expected it at \"${CMAKEFILE}\""
	exit $EXIT_MISSINGCMAKELIST
fi

# OK, everything should be all set to process, let's initialize counters
NUM_MISSING_FROM_CMAKE_LIST=0
FILES_MISSING_FROM_CMAKE_LIST=""
NUM_COMMON_FILES=0

# This flag will allow us to do globbing from bash directly without worrying
#  about if there is no match (which would be default return the glob string itself)
shopt -s nullglob

# First grab the valid CMakeLists contents into a variable so we don't have to ping that file over and over
# Valid lines should *not* start with a CMake comment (octothorpe, #)
# The following grep will remove any spaces where the first non-space character is a comment
# We will *not* validate the actual CMake line.  CMake will do this on its own.
VALID_CMAKELIST_CONTENTS=`grep -v '^\s*\t*#' "${CMAKEFILE}"`

# Now loop over all the idfs and imfs in the directory.  
# For each file, grep the valid CMakeLists contents to find a line containing that file
# Note the whole glob should *not* be quoted or the resulting list will be treated like a single file
for idf in "${TESTFILES_DIR}"/*.idf "${TESTFILES_DIR}"/*.imf; do
	FILENAME=`basename ${idf}`
	echo "$VALID_CMAKELIST_CONTENTS" | grep -q "${FILENAME}"
	if [ $? -ne 0 ]; then
		let NUM_MISSING_FROM_CMAKE_LIST++
		FILES_MISSING_FROM_CMAKE_LIST="${FILES_MISSING_FROM_CMAKE_LIST} Missing from CMakeList: ${FILENAME}\n"
	else
		let NUM_COMMON_FILES++
	fi
done

# We're done, provide the results textually first:
S_NUM_COMMON_FILES=`printf "%04d\n" ${NUM_COMMON_FILES}`
S_NUM_MISSING_FROM_CMAKE_LIST=`printf "%04d\n" ${NUM_MISSING_FROM_CMAKE_LIST}`
echo "${S_NUM_COMMON_FILES} Common files"
echo "${S_NUM_MISSING_FROM_CMAKE_LIST} Files missing from CMakeLists"
echo -e "${FILES_MISSING_FROM_CMAKE_LIST}"

# Then provide the return code for the caller
if [ $S_NUM_MISSING_FROM_CMAKE_LIST -eq 0 ]; then
	exit $EXIT_ALLGOOD
else
	exit $EXIT_WARNINGDIFFS
fi
