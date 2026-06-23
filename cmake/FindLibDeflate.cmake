#[=======================================================================[.rst:
FindLibDeflate
--------------

Find the deflate compression library (libdeflate).

Result Variables
^^^^^^^^^^^^^^^^

This module will set the following variables::

 LibDeflate_FOUND - True if a matching version of libdeflate was found

Hints
^^^^^

Set ``LibDeflate_ROOT_DIR`` to a directory that contains a libdeflate
installation.

Cache variables
^^^^^^^^^^^^^^^

This module may set the following cache variables::

 LibDeflate_INCLUDE_DIR - Directory containing the libdeflate header file
 LibDeflate_LIBRARY     - Location of the libdeflate library
 LibDeflate_VERSION     - The version of the libdeflate that was found
#]=======================================================================]

if(NOT LibDeflate_FOUND)

  find_path(
    LibDeflate_INCLUDE_DIR
    NAMES libdeflate.h
    HINTS ${LibDeflate_ROOT_DIR}/include)
  find_library(
    LibDeflate_LIBRARY
    NAMES deflate libdeflate
    HINTS ${LibDeflate_ROOT_DIR}/lib)

  if(LibDeflate_INCLUDE_DIR AND EXISTS "${LibDeflate_INCLUDE_DIR}/libdeflate.h")
    file(STRINGS "${LibDeflate_INCLUDE_DIR}/libdeflate.h" _version_line
         REGEX "^#define[\t ]+LIBDEFLATE_VERSION_STRING[\t ]+\".*\"")
    string(REGEX REPLACE "^.*LIBDEFLATE_VERSION_STRING[\t ]+\"([^\"]*)\".*$"
                         "\\1" _version_string "${_version_line}")
  endif()
  set(LibDeflate_VERSION
      ${_version_string}
      CACHE STRING "Version of libdeflate found" FORCE)

  mark_as_advanced(LibDeflate_INCLUDE_DIR LibDeflate_LIBRARY LibDeflate_VERSION)

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(
    LibDeflate
    REQUIRED_VARS LibDeflate_LIBRARY LibDeflate_INCLUDE_DIR
    VERSION_VAR LibDeflate_VERSION)

endif(NOT LibDeflate_FOUND)
