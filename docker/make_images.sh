#!/bin/bash -xe

HERE=`dirname "$0"`
cd $HERE/..

tag=$(git describe --tags)

for i in 37 38 39 310 311 312; do
    docker build -f docker/py${i}_wheel.docker . -t quay.io/casacore/casacore:${tag}_wheel${i}
done
