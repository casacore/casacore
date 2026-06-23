# - Try to find libdeflate: the deflate compression library
# Variables used by this module:
#  LibDeflate_ROOT        - LibDeflate root directory (used as hint)
# Variables defined by this module:
#  LibDeflate_FOUND       - system has libdeflate
#  LibDeflate_INCLUDE_DIR - the libdeflate include directory (cached)
#  LibDeflate_LIBRARY     - the libdeflate library (cached)
#  LibDeflate_VERSION     - the version of libdeflate (cached)

if(NOT LibDeflate_FOUND)

  find_path(LibDeflate_INCLUDE_DIR NAMES libdeflate.h 
    HINTS ${LibDeflate_ROOT_DIR}/include)
  find_library(LibDeflate_LIBRARY NAMES deflate libdeflate 
    HINTS ${LibDeflate_ROOT_DIR}/lib)

  if(LibDeflate_INCLUDE_DIR AND EXISTS "${LibDeflate_INCLUDE_DIR}/libdeflate.h")
    file(STRINGS "${LibDeflate_INCLUDE_DIR}/libdeflate.h" _version_line 
      REGEX "^#define[\t ]+LIBDEFLATE_VERSION_STRING[\t ]+\".*\"")
    string(REGEX REPLACE "^.*LIBDEFLATE_VERSION_STRING[\t ]+\"([^\"]*)\".*$" "\\1" _version_string "${_version_line}")
  endif()

  set(LibDeflate_VERSION ${_version_string} CACHE STRING "Version of libdeflate found" FORCE)

  mark_as_advanced(LibDeflate_INCLUDE_DIR LibDeflate_LIBRARY LibDeflate_VERSION)

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(LibDeflate
      REQUIRED_VARS LibDeflate_LIBRARY LibDeflate_INCLUDE_DIR
      VERSION_VAR LibDeflate_VERSION
  )

endif(NOT LibDeflate_FOUND)
