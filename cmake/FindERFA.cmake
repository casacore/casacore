# - Try to find ERFA: the liberfa reimplementation of the IAU SOFA routines
# Variables used by this module:
#  ERFA_ROOT_DIR     - ERFA root directory
# Variables defined by this module:
#  ERFA_FOUND        - system has ERFA
#  ERFA_INCLUDE_DIR  - the ERFA include directory
#  ERFA_LIBRARY      - the ERFA library (cached)
#  ERFA_LIBRARIES    - the ERFA libraries
#  ERFA_INCLUDE_DIRS - the ERFA include directories

if(NOT ERFA_FOUND)

  find_path(ERFA_INCLUDE_DIR erfa.h
    HINTS ${ERFA_ROOT_DIR}
    PATH_SUFFIXES include)
  mark_as_advanced(ERFA_INCLUDE_DIR)

  find_library(ERFA_LIBRARY NAMES erfa liberfa
    HINTS ${ERFA_ROOT_DIR}
    PATH_SUFFIXES lib)
  mark_as_advanced(ERFA_LIBRARY)

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(ERFA DEFAULT_MSG
    ERFA_LIBRARY ERFA_INCLUDE_DIR)

  set(ERFA_LIBRARIES ${ERFA_LIBRARY})
  set(ERFA_INCLUDE_DIRS ${ERFA_INCLUDE_DIR})

endif(NOT ERFA_FOUND)
