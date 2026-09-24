#!/bin/bash

# Usage: run-format.sh [<module>]
# A module can optionally be specified as the first parameter, in
# which case only that module will be formatted. If no module is specified,
# all modules are formatted.

# Disable globbing. This is needed when defining patterns that have wildcards.
set -e -f

# Use a specific clang-format version, since formatting may differ between versions.
CLANG_FORMAT_BINARY=clang-format-21

#Directories that must be excluded from formatting. These paths are
#relative to SOURCE_DIR.
EXCLUDE_DIRS=(external build CMake)

#The patterns of the C++ source files, which clang-format should format.
CXX_SOURCES=(*.cc *.tcc *.h)

MODULES="$1"
if [[ "${MODULES}" == "" ]] ; then
    MODULES="casa coordinates derivedmscal fits images lattices meas measures mirlib ms msfits python scimath tables"
fi

REPO_DIR=$(dirname "$0")
RUN_DIR=`pwd`

for MODULE in ${MODULES} ; do
    cd ${RUN_DIR}
    
    #The directory that contains the source files.
    SOURCE_DIR=${REPO_DIR}/../${MODULE}

    if [[ ! -e ${SOURCE_DIR} ]] ; then
	echo "Invalid module specified: can't find ${SOURCE_DIR}."
	exit 1
    fi

    echo Running formatter for module ${MODULE}...
    
    source $(dirname "$0")/format-implementation.sh
done
