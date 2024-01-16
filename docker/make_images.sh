#!/bin/bash
#
# Script to make python wheels for several versions

set -eu

SCRIPT_DIR=$(cd $(dirname $0) && pwd)
ROOT_DIR=$(git rev-parse --show-toplevel)

REPOSITORY=quay.io/casacore/casacore
GIT_TAG=$(git describe --tags --dirty)

py_major=3
for py_minor in $(seq 7 12); do
    echo -e "\n\n******** Building wheel for python ${py_major}.${py_minor} ********\n"
    py_version=${py_major}${py_minor}
    [ ${py_minor} -le 7 ] && py_unicode="m" || py_unicode=
    docker build \
        --build-arg PYMAJOR=${py_major} \
        --build-arg PYMINOR=${py_minor} \
        --build-arg PYUNICODE=${py_unicode} \
        --build-arg THREADS=16 \
        --file ${SCRIPT_DIR}/py_wheel.docker \
        --tag ${REPOSITORY}:py${py_version}_${GIT_TAG} \
        ${ROOT_DIR}
done
