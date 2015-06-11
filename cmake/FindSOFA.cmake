# - Try to find SOFA: the IAU Standards of Fundamental Astronomy libraries
# Variables used by this module:
#  SOFA_ROOT_DIR     - SOFA root directory
# Variables defined by this module:
#  SOFA_FOUND        - system has SOFA
#  SOFA_LIBRARY      - the SOFA library (cached)
#  SOFA_LIBRARIES    - the SOFA libraries
#                        (identical to SOFA_LIBRARY)

if(NOT SOFA_FOUND)

  find_library(SOFA_LIBRARY sofa
    HINTS ${SOFA_ROOT_DIR} PATH_SUFFIXES lib)
  mark_as_advanced(SOFA_LIBRARY)

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(SOFA DEFAULT_MSG SOFA_LIBRARY)

  set(SOFA_LIBRARIES ${SOFA_LIBRARY})

endif(NOT SOFA_FOUND)
