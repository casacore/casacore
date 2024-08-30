cmake_minimum_required(VERSION 3.6)

find_package(GSL REQUIRED)

find_package(Threads REQUIRED)

include_directories(${GSL_INCLUDE_DIRS})

add_compile_options(-O3 -Wall -DNDEBUG)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS OFF)

option(PORTABLE "Generate portable code" ON)
option(BUILD_PACKAGES "Build Debian packages" OFF)

if(NOT PORTABLE)
  if(USE_AVX512F)
    add_compile_options(-mavx512f)
  elseif(USE_AVX2)
    add_compile_options(-mavx2)
  elseif(USE_AVX)
    add_compile_options(-mavx)
  else()
    add_compile_options(-march=native)
  endif()
else()
  if(USE_AVX512F OR USE_AVX2 OR USE_AVX)
    message(FATAL_ERROR "Cannot enable AVX features when building portable code.")
  endif()
endif()

add_library(dyscostman-object OBJECT
	aftimeblockencoder.cc
	dyscostman.cc
	dyscodatacolumn.cc
	dyscoweightcolumn.cc
	stochasticencoder.cc
	threadeddyscocolumn.cc
	rftimeblockencoder.cc
	rowtimeblockencoder.cc)
set_property(TARGET dyscostman-object PROPERTY POSITION_INDEPENDENT_CODE 1) 

set(DYSCOSTMAN_SOURCES $<TARGET_OBJECTS:dyscostman-object> PARENT_SCOPE)
set(DYSCOSTMAN_LIBRARIES ${GSL_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} PARENT_SCOPE)
if(BUILD_TESTING)
  find_package(Boost 1.72 COMPONENTS system filesystem unit_test_framework)
  if(Boost_FOUND)
    include_directories(${Boost_INCLUDE_DIR})
    add_executable(tDysco
      $<TARGET_OBJECTS:dyscostman-object>
      tests/runtests.cc
      tests/testbytepacking.cc
      tests/testdyscostman.cc
      tests/testtimeblockencoder.cc
      )
    target_link_libraries(tDysco ${Boost_FILESYSTEM_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${GSL_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} casa_tables casa_casa)
    add_test(tDysco tDysco)
  else()
    message("Boost testing framework not found.")
  endif()
endif()
