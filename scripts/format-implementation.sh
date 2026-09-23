# format-implementation.sh: Formats source code in a repository in accordance
# with .clang-format. The script only formats source code 
# that has been added to git. E.g. new untracked files are ignored and will only 
# be formatted once they are staged.
#
# This file was adapted from the scripts/run-format.sh file from the
# aocommon repository.
#
# This script uses the following variables:
# - SOURCE_DIR: The directory that contains the source files.
# - EXCLUDE_DIRS: (Optional) directories that must be excluded from formatting.
#                 These paths are relative to SOURCE_DIR.
# - CXX_SOURCES: Patterns of the C++ files, which clang-format should format.
#
# A repository that uses format.sh should define its own run-format.sh script
# that defines these variables and then sources this script.
# If you want to automatically check formatting in each commit, include the line
# "./scripts/run-format.sh" to .git/hooks/pre-commit
# and make sure pre-commit is an executable shell script.

# Disable globbing
set -e -f

# Check arguments
if [ -z "$SOURCE_DIR" ]; then
  echo "Please define SOURCE_DIR using $BASH_SOURCE"
  exit 1
fi
if [ -z "$CXX_SOURCES" ]; then CXX_SOURCES=(*.cc *.h *.tcc); fi

# Detect run environment.
if [ "$GITHUB_ACTIONS" = "true" ] && [ -f /.dockerenv ]; then
  echo " (dry run on Github)"
elif [ -n "$CI" ]; then
  DRYRUN=" (dry run on CI)"
elif [ -n "$GIT_AUTHOR_DATE" ]; then
  DRYRUN=" (dry run in git hook)"
fi

# Print in bold-face
if [[ "${DRYRUN}" != "" ]] ; then
    echo -e "\e[1mRunning formatters$DRYRUN...\e[0m"
fi

# Convert SOURCES into "-name ext1 -o -name ext2 -o -name ext3 ..."
CXX_FIND_NAMES="-name ${CXX_SOURCES[0]}"
for i in `seq 1 $((${#CXX_SOURCES[*]} - 1))`; do
  CXX_FIND_NAMES+=" -o -name ${CXX_SOURCES[$i]}"
done

# Convert EXCLUDE_DIRS into "-path ./dir1 -prune -o -path ./dir2 -prune -o ..."
FIND_EXCLUDES=
for e in ${EXCLUDE_DIRS[*]}; do
  FIND_EXCLUDES+="-path ./$e -prune -o "
done

# Use `find` to incorporate the EXCLUDE_DIRS
cd $SOURCE_DIR
CXX_FILES_EXCLUDEDIRS=$(find . $FIND_EXCLUDES -type f \( $CXX_FIND_NAMES \) -print)

# Only format files that are in the git repo
CXX_FILES=$(git ls-files $CXX_FILES_EXCLUDEDIRS)

if [[ "${CLANG_FORMAT_BINARY}" == "" ]] ; then
    CLANG_FORMAT_BINARY="clang-format"
fi

if [ -n "$DRYRUN" ]; then
  # If the clang-format xml has no replacement entries, all files are formatted.
  if !(${CLANG_FORMAT_BINARY} -style=file --output-replacements-xml $CXX_FILES |
       grep -q "<replacement ") ; then
    # Print in bold-face green
    echo -e "\e[1m\e[32mGreat job, all files are properly formatted!\e[0m"
  else
    # Print in bold-face red
    echo -e "\e[1m\e[31mAt least one file is not properly formatted!\e[0m"
    echo -e "\e[1m\e[31mRun scripts/run-format.sh for formatting all files!\e[0m"
    exit 1
  fi
else
  ${CLANG_FORMAT_BINARY} -i -style=file $CXX_FILES
  # Print in bold-face
  echo -e "\e[1mSuccessfully formatted all files.\e[0m"
fi
