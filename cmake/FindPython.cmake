# - Try to find the Python interpreter, Python header files and libraries. 
# This macro effectively wraps the FindPythonInterp and FindPythonLibs macros
# provided by CMake.
#
# In addition to the variables that are set by FindPythonInterp and
# FindPythonLibs, this will define:
#  PYTHON_FOUND        - system has Python interpreter, Python headers
#                        files and libraries
#  PYTHON_INCLUDE_DIRS - path to the Python header files
#  PYTHON_BUILD_DIR    - build directory for Python extensions (cached)
#  PYTHON_INSTALL_DIR  - installation directory for Python extensions (cached)

# Copyright (C) 2009
# ASTRON (Netherlands Institute for Radio Astronomy)
# P.O.Box 2, 7990 AA Dwingeloo, The Netherlands
#
# This file is part of the LOFAR software suite.
# The LOFAR software suite is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# The LOFAR software suite is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with the LOFAR software suite. If not, see <http://www.gnu.org/licenses/>.
#
# $Id: FindPython.cmake 22948 2012-11-23 08:54:47Z loose $

# Set options string to pass to the find_package() commands below.
set(_options ${Python_FIND_VERSION})
if(Python_FIND_VERSION_EXACT)
  list(APPEND _options EXACT)
endif(Python_FIND_VERSION_EXACT)
if(Python_FIND_QUIETLY)
  list(APPEND _options QUIET)
endif(Python_FIND_QUIETLY)
if(Python_FIND_REQUIRED)
  list(APPEND _options REQUIRED)
endif(Python_FIND_REQUIRED)

# Search for the Python interpreter.
find_package(PythonInterp ${_options})

# Search for the Python header files and libraries.
find_package(PythonLibs ${_options})

# Set PYTHON_INCLUDE_DIRS variable, because FindPythonLibs does not do it.
set(PYTHON_INCLUDE_DIRS "${PYTHON_INCLUDE_DIR}")

# PythonInstall sets PYTHON_BUILD_DIR and PYTHON_INSTALL_DIR
#include(PythonInstall)

# Set PYTHON_FOUND to TRUE if both Python interpreter and libraries are found.
set(PYTHON_FOUND FALSE)
if(PYTHONINTERP_FOUND AND PYTHONLIBS_FOUND)
  set(PYTHON_FOUND TRUE)
endif(PYTHONINTERP_FOUND AND PYTHONLIBS_FOUND)

