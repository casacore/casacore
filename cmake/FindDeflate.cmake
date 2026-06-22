# Find libdeflate

if(NOT DEFLATE_FOUND)

  find_path(DEFLATE_INCLUDE_DIR libdeflate.h HINTS ${DEFLATE_ROOT_DIR}/include)
  find_library(DEFLATE_LIBRARY NAMES deflate libdeflate HINTS ${DEFLATE_ROOT_DIR}/lib)
  mark_as_advanced(DEFLATE_INCLUDE_DIR DEFLATE_LIBRARY)

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(DEFLATE
      REQUIRED_VARS DEFLATE_LIBRARY DEFLATE_INCLUDE_DIR
  )

endif(NOT DEFLATE_FOUND)
