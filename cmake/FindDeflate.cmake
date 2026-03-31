# Find libdeflate

if(NOT DEFLATE_FOUND)

  find_path(DEFLATE_INCLUDE_DIR libdeflate.h)
  find_library(DEFLATE_LIBRARY NAMES deflate libdeflate)
  mark_as_advanced(WCSLIB_INCLUDE_DIR WCSLIB_LIBRARY M_LIBRARY)

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(DEFLATE
      REQUIRED_VARS DEFLATE_LIBRARY DEFLATE_INCLUDE_DIR
  )

endif(NOT DEFLATE_FOUND)
