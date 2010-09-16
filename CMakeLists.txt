#
# Casacore
#
project(casacore)

cmake_minimum_required (VERSION 2.6)

enable_language (Fortran)

set (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
option (BUILD_SHARED_LIBS "" YES)


# For the sake of consistency
if (HDF5_ROOT_DIR)
    set (ENV{HDF5_ROOT} ${HDF5_ROOT_DIR})
endif (HDF5_ROOT_DIR)

include (CTest)
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

if (HDF5_FOUND)
    if (FFTW3_FOUND)
        include_directories (${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR} ${HDF5_INCLUDE_DIR} ${FFTW3_INCLUDE_DIR})
    else (FFTW3_FOUND)
        include_directories (${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR} ${HDF5_INCLUDE_DIR})
    endif (FFTW3_FOUND)
else (HDF5_FOUND)
    if (FFTW3_FOUND)
        include_directories (${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR} ${FFTW3_INCLUDE_DIR})
    else (FFTW3_FOUND)
        include_directories (${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
    endif (FFTW3_FOUND)
endif (HDF5_FOUND)


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
message (STATUS "Readline library? .......... = ${READLINE_LIBRARIES}")
message (STATUS "CFitsio library? ........... = ${CFITSIO_LIBRARIES}")
message (STATUS "HDF5 library? .............. = ${HDF5_hdf5_LIBRARY}")
message (STATUS "BLAS library? .............. = ${BLAS_LIBRARIES}")
message (STATUS "LAPACK library? ............ = ${LAPACK_LIBRARIES}")
message (STATUS "WCS library? ............... = ${WCSLIB_LIBRARIES}")
message (STATUS "FFTW3 library? ............. = ${FFTW3_LIBRARIES}")

