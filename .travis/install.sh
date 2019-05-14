#!/bin/bash

set -e
set -x

if [ "$TRAVIS_OS_NAME" = osx ]; then
  MAJORPYTHONVERSION=$(echo $PYTHONVERSION | head -c 1)
  wget https://repo.continuum.io/miniconda/Miniconda${MAJORPYTHONVERSION}-latest-MacOSX-x86_64.sh -O miniconda.sh;

  brew update >/dev/null
  brew cask uninstall oclint | true
  brew install fftw hdf5 ccache

  bash miniconda.sh -b -p $HOME/miniconda
  export PATH="$HOME/miniconda/bin:$PATH"
  hash -r
  conda config --set always_yes yes --set changeps1 no
  conda update -q conda
  conda config --add channels conda-forge
  conda info -a
  conda create -q -n testenv python=${PYTHONVERSION} numpy wcslib cfitsio
  source activate testenv
  conda install -q -y -c meznom boost-python
fi
