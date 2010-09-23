#
# Casacore
#
project(casacore)

cmake_minimum_required (VERSION 2.6)

enable_language (Fortran)

set (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
option (BUILD_SHARED_LIBS "" YES)

# Define the compiler flags to be used.
# Note: -Wshadow and -Wunreachable-code give many warnings.
# Casacore uses longlong, so no warnings for it.
##set (CMAKE_CXX_FLAGS "-W -Wall -Wpointer-arith -pedantic -Wno-long-long")
set (CMAKE_CXX_FLAGS "-W -Wall")
# Use debug mode if -DDEBUG=ON is given
if (DEBUG)
    set (CMAKE_BUILD_TYPE Debug)
else (DEBUG)
    # No -DDEBUG given; use debug mode if building in dbg or debug directory.
    get_filename_component(_cmpvar ${CMAKE_BINARY_DIR} NAME)
    if("_cmpvar" STREQUAL "dbg" OR "_cmpvar" STREQUAL "debug")
        set (CMAKE_BUILD_TYPE Debug)
    else("_cmpvar" STREQUAL "dbg" OR "_cmpvar" STREQUAL "debug")
        set (CMAKE_BUILD_TYPE Release)
    endif("_cmpvar" STREQUAL "dbg" OR "_cmpvar" STREQUAL "debug")
endif (DEBUG)


# For the sake of consistency
if (HDF5_ROOT_DIR)
    set (ENV{HDF5_ROOT} ${HDF5_ROOT_DIR})
endif (HDF5_ROOT_DIR)

include (CTest)
include (FindDL)
include (FindReadline)
include (FindFLEX REQUIRED)
include (FindBISON REQUIRED)
include (FindWCSLIB REQUIRED)
include (FindCFITSIO REQUIRED)
include (FindBLAS REQUIRED)
include (FindLAPACK REQUIRED)
include (FindHDF5)
if (FFTW3_DISABLE_THREADS)
    find_package (FFTW3 COMPONENTS single double)
else (FFTW3_DISABLE_THREADS)
    find_package (FFTW3 COMPONENTS single double threads)
endif (FFTW3_DISABLE_THREADS)

# Set the include directories and HAVE compiler variables
include_directories (${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
if (WCSLIB_FOUND)
    include_directories (${WCSLIB_INCLUDE_DIRS})
endif (WCSLIB_FOUND)
if (CFITSIO_FOUND)
    include_directories (${CFITSIO_INCLUDE_DIRS})
endif (CFITSIO_FOUND)
if (HDF5_FOUND)
    include_directories (${HDF5_INCLUDE_DIRS})
    add_definitions(-DHAVE_HDF5)
endif (HDF5_FOUND)
if (FFTW3_FOUND)
    include_directories (${FFTW3_INCLUDE_DIRS})
    add_definitions(-DHAVE_FFTW3)
    if (NOT FFTW3_DISABLE_THREADS)
        add_definitions(-DHAVE_FFTW3_THREADS)
    endif (NOT FFTW3_DISABLE_THREADS)
endif (FFTW3_FOUND)
if (DL_FOUND)
    add_definitions(-DHAVE_DL)
endif (DL_FOUND)
if (READLINE_FOUND)
    add_definitions(-DHAVE_READLINE)
endif (READLINE_FOUND)


add_subdirectory (casa)
add_subdirectory (tables)
add_subdirectory (mirlib)
add_subdirectory (scimath_f)
add_subdirectory (scimath)
add_subdirectory (measures)
add_subdirectory (lattices)
add_subdirectory (fits)
add_subdirectory (ms)
add_subdirectory (msfits)
add_subdirectory (derivedmscal)
add_subdirectory (coordinates)
add_subdirectory (components)
add_subdirectory (images)

if (APPLE)
    set_target_properties(casa_mirlib casa_scimath_f casa_fits casa_coordinates PROPERTIES LINK_FLAGS -single_module)
endif (APPLE)

message (STATUS "CMAKE_SYSTEM ............... = ${CMAKE_SYSTEM}")
message( STATUS "CMAKE_BUILD_TYPE ........... = ${CMAKE_BUILD_TYPE}")
message( STATUS "BUILD_SHARED_LIBS .......... = ${BUILD_SHARED_LIBS}")
message( STATUS "CMAKE_CXX_COMPILER ......... = ${CMAKE_CXX_COMPILER}")
message( STATUS "CMAKE_CXX_FLAGS ............ = ${CMAKE_CXX_FLAGS}")
message (STATUS "DL library?       .......... = ${DL_LIBRARIES}")
message (STATUS "Readline library? .......... = ${READLINE_LIBRARIES}")
message (STATUS "BLAS library? .............. = ${BLAS_LIBRARIES}")
message (STATUS "LAPACK library? ............ = ${LAPACK_LIBRARIES}")
message (STATUS "WCS library? ............... = ${WCSLIB_LIBRARIES}")
message (STATUS "CFitsio library? ........... = ${CFITSIO_LIBRARIES}")
message (STATUS "HDF5 library? .............. = ${HDF5_hdf5_LIBRARY}")
message (STATUS "FFTW3 library? ............. = ${FFTW3_LIBRARIES}")
