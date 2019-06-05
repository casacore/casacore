# It would be nice to use CMAKE_VERSION instead but that requires CMake 2.6.3
if(${CMAKE_MAJOR_VERSION} GREATER 3 OR
  (${CMAKE_MAJOR_VERSION} EQUAL 3 AND ${CMAKE_MINOR_VERSION} GREATER 11))
    include (${CMAKE_CURRENT_LIST_DIR}/CMakeLists-cmake3.12.txt)
else()
    include (${CMAKE_CURRENT_LIST_DIR}/CMakeLists-older-cmake.txt)
endif()
