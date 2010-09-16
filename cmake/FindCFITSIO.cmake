# - Try to find CFITSIO.
# Variables used by this module:
#  CFITSIO_ROOT_DIR     - CFITSIO root directory
# Variables defined by this module:
#  CFITSIO_FOUND        - system has CFITSIO
#  CFITSIO_INCLUDE_DIR  - the CFITSIO include directory (cached)
#  CFITSIO_INCLUDE_DIRS - the CFITSIO include directories
#                         (identical to CFITSIO_INCLUDE_DIR)
#  CFITSIO_LIBRARY      - the CFITSIO library (cached)
#  CFITSIO_LIBRARIES    - the CFITSIO libraries
#                         (identical to CFITSIO_LIBRARY)

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

if(NOT CFITSIO_FOUND)

  find_path(CFITSIO_INCLUDE_DIR fitsio.h
    PATHS ${CFITSIO_ROOT_DIR} PATH_SUFFIXES include)
  find_library(CFITSIO_LIBRARY cfitsio
    PATHS ${CFITSIO_ROOT_DIR} PATH_SUFFIXES lib)
  find_library(M_LIBRARY m)
  mark_as_advanced(CFITSIO_INCLUDE_DIR CFITSIO_LIBRARY M_LIBRARY)

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(CFITSIO DEFAULT_MSG
    CFITSIO_LIBRARY M_LIBRARY CFITSIO_INCLUDE_DIR)

  set(CFITSIO_INCLUDE_DIRS ${CFITSIO_INCLUDE_DIR})
  set(CFITSIO_LIBRARIES ${CFITSIO_LIBRARY} ${M_LIBRARY})

endif(NOT CFITSIO_FOUND)
