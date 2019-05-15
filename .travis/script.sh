#!/bin/bash

set -e
set -x

if [ "$TRAVIS_OS_NAME" = osx ]; then
    cd ${TRAVIS_BUILD_DIR}/build
    ccache -s
    ccache -z
    make -j2
    # Skip tConvert on OSX; boost-python can't be found on non-standard location
    #env CTEST_OUTPUT_ON_FAILURE=1 ctest -E tConvert
    make test 
    make install
    ccache -s
else
    docker run casacore/${DIST}_${CC} ctest -E "(tBucketFile|tByteIO|tDirectory|tFile|tRegularFile|tSymLink)"
fi
