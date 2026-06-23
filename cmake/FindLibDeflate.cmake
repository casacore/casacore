#[=======================================================================[.rst:
FindLibDeflate
--------------

Find the deflate compression library (libdeflate).

Result Variables
^^^^^^^^^^^^^^^^

This module defines the following variables:

``LibDeflate_FOUND``
  True if a matching version of libdeflate was found
``LibDeflate_VERSION``
  The version of libdeflate that was found

Hints
^^^^^

``LibDeflate_ROOT_DIR``
  The user may set this to the root directory of a libdeflate installation

Cache variables
^^^^^^^^^^^^^^^

The following cache variables may also be set:

``LibDeflate_INCLUDE_DIR``
  Directory containing the libdeflate header file
``LibDeflate_LIBRARY``
  Location of the libdeflate library

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

  mark_as_advanced(LibDeflate_INCLUDE_DIR LibDeflate_LIBRARY)

  if(LibDeflate_INCLUDE_DIR AND EXISTS "${LibDeflate_INCLUDE_DIR}/libdeflate.h")
    file(STRINGS "${LibDeflate_INCLUDE_DIR}/libdeflate.h" _version_line
         REGEX "^#define[\t ]+LIBDEFLATE_VERSION_STRING[\t ]+\".*\"")
    string(REGEX REPLACE "^.*LIBDEFLATE_VERSION_STRING[\t ]+\"([^\"]*)\".*$"
                         "\\1" LibDeflate_VERSION "${_version_line}")
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(
    LibDeflate
    REQUIRED_VARS LibDeflate_LIBRARY LibDeflate_INCLUDE_DIR
    VERSION_VAR LibDeflate_VERSION)

endif(NOT LibDeflate_FOUND)
