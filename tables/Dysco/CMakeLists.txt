cmake_minimum_required(VERSION 3.6)

project(dysco)

find_package(GSL REQUIRED)

find_package(Threads REQUIRED)

include_directories(${GSL_INCLUDE_DIRS})

add_compile_options(-O3 -Wall -DNDEBUG)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS OFF)

option(PORTABLE "Generate portable code" OFF)
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
	aftimeblockencoder.cpp
	dyscostman.cpp
	dyscodatacolumn.cpp
	dyscoweightcolumn.cpp
	stochasticencoder.cpp
	threadeddyscocolumn.cpp
	rftimeblockencoder.cpp
	rowtimeblockencoder.cpp)
set_property(TARGET dyscostman-object PROPERTY POSITION_INDEPENDENT_CODE 1) 

add_library(dyscostman SHARED $<TARGET_OBJECTS:dyscostman-object>)
set_target_properties(dyscostman PROPERTIES SOVERSION 0)
target_link_libraries(dyscostman ${GSL_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

find_package(Boost COMPONENTS system filesystem unit_test_framework)
if(Boost_FOUND)
	include_directories(${Boost_INCLUDE_DIR})
  add_executable(tDysco
    $<TARGET_OBJECTS:dyscostman-object>
    tests/runtests.cpp 
    tests/testbytepacking.cpp
    tests/testdithering.cpp
    tests/testdyscostman.cpp
    tests/testtimeblockencoder.cpp
    )
  target_link_libraries(tDysco ${Boost_FILESYSTEM_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${GSL_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} casa_tables casa_casa)
  add_test(tDysco tDysco)
else()
  message("Boost testing framework not found.")
endif()

install (TARGETS dyscostman DESTINATION lib) 
