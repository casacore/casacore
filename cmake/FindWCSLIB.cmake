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

  find_path(WCSLIB_INCLUDE_DIR wcslib/wcs.h
    PATHS ${WCSLIB_ROOT_DIR} PATH_SUFFIXES include)
  find_library(WCSLIB_LIBRARY wcs
    PATHS ${WCSLIB_ROOT_DIR} PATH_SUFFIXES lib)
  find_library(M_LIBRARY m)
  mark_as_advanced(WCSLIB_INCLUDE_DIR WCSLIB_LIBRARY M_LIBRARY)

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(WCSLIB DEFAULT_MSG
    WCSLIB_LIBRARY M_LIBRARY WCSLIB_INCLUDE_DIR)

  set(WCSLIB_INCLUDE_DIRS ${WCSLIB_INCLUDE_DIR})
  set(WCSLIB_LIBRARIES ${WCSLIB_LIBRARY} ${M_LIBRARY})

endif(NOT WCSLIB_FOUND)
