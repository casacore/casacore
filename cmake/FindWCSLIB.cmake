# - Try to find WCSLIB: the FITS "World Coordinate System" library
# Variables used by this module:
#  WCSLIB_ROOT_DIR     - WCSLIB root directory
# Variables defined by this module:
#  WCSLIB_FOUND        - system has WCSLIB
#  WCSLIB_INCLUDE_DIR  - the WCSLIB include directory (cached)
#  WCSLIB_INCLUDE_DIRS - the WCSLIB include directories
#                        (identical to WCSLIB_INCLUDE_DIR)
#  WCSLIB_LIBRARY      - the WCSLIB library (cached)
#  WCSLIB_LIBRARIES    - the WCSLIB libraries
#                        (identical to WCSLIB_LIBRARY)
#  WCSLIB_VERSION_STRING the found version of WCSLIB

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
# $Id$

if(NOT WCSLIB_FOUND)

  find_path(WCSLIB_INCLUDE_DIR wcslib/wcsconfig.h
    HINTS ${WCSLIB_ROOT_DIR} PATH_SUFFIXES include)

  if(WCSLIB_INCLUDE_DIR)
    FILE(READ "${WCSLIB_INCLUDE_DIR}/wcslib/wcsconfig.h" WCSLIB_H)
    set(WCSLIB_VERSION_REGEX ".*#define WCSLIB_VERSION[^0-9]*([0-9]+)\\.([0-9]+).*")
    if ("${WCSLIB_H}" MATCHES ${WCSLIB_VERSION_REGEX})
      STRING(REGEX REPLACE ${WCSLIB_VERSION_REGEX}
                           "\\1.\\2" WCSLIB_VERSION_STRING "${WCSLIB_H}")
      STRING(REGEX REPLACE "^([0-9]+)[.]([0-9]+)" "\\1" WCSLIB_VERSION_MAJOR ${WCSLIB_VERSION_STRING})
      STRING(REGEX REPLACE "^([0-9]+)[.]([0-9]+)" "\\2" WCSLIB_VERSION_MINOR ${WCSLIB_VERSION_STRING})
    else ()
      set(WCSLIB_VERSION_STRING "Unknown")
    endif ()
  endif(WCSLIB_INCLUDE_DIR)

  find_library(WCSLIB_LIBRARY wcs
    HINTS ${WCSLIB_ROOT_DIR} PATH_SUFFIXES lib)
  find_library(M_LIBRARY m)
  mark_as_advanced(WCSLIB_INCLUDE_DIR WCSLIB_LIBRARY M_LIBRARY)

  if(CMAKE_VERSION VERSION_LESS "2.8.3")
    find_package_handle_standard_args(WCSLIB DEFAULT_MSG
      WCSLIB_LIBRARY M_LIBRARY WCSLIB_INCLUDE_DIR)
  else ()
    include(FindPackageHandleStandardArgs)
    find_package_handle_standard_args(WCSLIB
      REQUIRED_VARS WCSLIB_LIBRARY M_LIBRARY WCSLIB_INCLUDE_DIR
      VERSION_VAR WCSLIB_VERSION_STRING)
  endif ()

  set(WCSLIB_INCLUDE_DIRS ${WCSLIB_INCLUDE_DIR})
  set(WCSLIB_LIBRARIES ${WCSLIB_LIBRARY} ${M_LIBRARY})

endif(NOT WCSLIB_FOUND)
