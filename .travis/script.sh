#!/usr/bin/bash

if [[ "$TRAVIS_OS_NAME" = osx ]]; do
    cd ${TRAVIS_BUILD_DIR}/build
    ccache -s
    ccache -z
    make -j2
    # Skip tConvert on OSX; boost-python can't be found on non-standard location
    env CTEST_OUTPUT_ON_FAILURE=1 ctest -E tConvert
    make install
    ccache -s
else
    docker run casacore/${BLA} make test
fi