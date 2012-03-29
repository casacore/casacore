#
# Casacore
#
project(casacore)

SET(CPACK_PACKAGE_NAME "casacore")
SET(PROJECT_VERSION "1.4.0")
SET(PROJECT_VERSION_REVISION 1)
string(TOLOWER "${CPACK_PACKAGE_NAME}" CPACK_PACKAGE_NAME_LOWERCASE) 
find_program(DPKG_PROGRAM dpkg DOC "dpkg program of Debian-based systems") 
if(DPKG_PROGRAM) 
  execute_process( 
    COMMAND ${DPKG_PROGRAM} --print-architecture 
    OUTPUT_VARIABLE CPACK_DEBIAN_PACKAGE_ARCHITECTURE 
    OUTPUT_STRIP_TRAILING_WHITESPACE 
  ) 
  set(CPACK_PACKAGE_FILE_NAME 
"${CPACK_PACKAGE_NAME_LOWERCASE}_${PROJECT_VERSION}-${PROJECT_VERSION_REVISION}_${CPACK_DEBIAN_PACKAGE_ARCHITECTURE}")   

else(DPKG_PROGRAM) 
  set(CPACK_PACKAGE_FILE_NAME 
"${CPACK_PACKAGE_NAME_LOWERCASE}_${PROJECT_VERSION}-${PROJECT_VERSION_REVISION}_${CMAKE_SYSTEM_NAME}") 
endif(DPKG_PROGRAM) 

cmake_minimum_required (VERSION 2.6.1)
SET(CPACK_GENERATOR "DEB")
SET(CPACK_PACKAGE_VERSION "1.1.0")
SET(CPACK_PACKAGE_CONTACT "Malte Marquarding") #required
SET(CPACK_DEBIAN_PACKAGE_DEPENDS "libcfitsio3, libgfortran3")
INCLUDE(CPack)

enable_language (Fortran)

set (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# By default build shared libraries
option (ENABLE_SHARED "" YES)
if (ENABLE_SHARED)
    option (BUILD_SHARED_LIBS "" YES)
else (ENABLE_SHARED)
    option (BUILD_SHARED_LIBS "" NO)
endif (ENABLE_SHARED)

# By default do not use HDF5, FFTW3, threads
option (USE_HDF5 "" NO)
option (USE_FFTW3 "" NO)
option (USE_THREADS "" NO)
option (USE_OPENMP "" NO)

# Define the compiler flags to be used.
# Note: -Wshadow and -Wunreachable-code give (too) many warnings.
# Casacore uses longlong, so no warnings for it.
if (NOT CMAKE_CXX_FLAGS)
    set (CMAKE_CXX_FLAGS "-W -Wall -Wpointer-arith -pedantic -Wno-long-long")
##    set (CMAKE_CXX_FLAGS "-W -Wall")
endif (NOT CMAKE_CXX_FLAGS)

# Set build type if not given.
if (NOT CMAKE_BUILD_TYPE)
    # Use debug mode if building in dbg or debug directory.
    get_filename_component(_cmpvar ${CMAKE_BINARY_DIR} NAME)
    if("_cmpvar" STREQUAL "dbg" OR "_cmpvar" STREQUAL "debug")
        set (CMAKE_BUILD_TYPE Debug)
    else("_cmpvar" STREQUAL "dbg" OR "_cmpvar" STREQUAL "debug")
        set (CMAKE_BUILD_TYPE Release)
    endif("_cmpvar" STREQUAL "dbg" OR "_cmpvar" STREQUAL "debug")
endif (NOT CMAKE_BUILD_TYPE)

# FindHDF5 uses environment variables, so set it if needed.
if (HDF5_ROOT_DIR)
    set (ENV{HDF5_ROOT} ${HDF5_ROOT_DIR})
endif (HDF5_ROOT_DIR)

# Find out which modules to build.
if (NOT MODULE)
   set (MODULE all)
endif (NOT MODULE)
set (_usebison NO)
set (_uselapack NO)
set (_usefits NO)
set (_usewcs NO)
set (_modules casa)
if (NOT ${MODULE} STREQUAL "casa")
    set (_modules ${_modules} tables)
    set (_usebison YES)
    if (NOT ${MODULE} STREQUAL "tables")
        set (_modules ${_modules} scimath_f scimath measures meas lattices)
        set (_uselapack YES)
        if (NOT ${MODULE} STREQUAL "measures")
            set (_modules ${_modules} fits)
            set (_usefits YES)
	    if (${MODULE} STREQUAL "ms" OR ${MODULE} STREQUAL "all")
                set (_modules ${_modules} ms derivedmscal msfits)
	    endif (${MODULE} STREQUAL "ms" OR ${MODULE} STREQUAL "all")
	    if (${MODULE} STREQUAL "images" OR ${MODULE} STREQUAL "all")
                set (_modules ${_modules} mirlib coordinates components images)
                set (_usewcs YES)
	    endif (${MODULE} STREQUAL "images" OR ${MODULE} STREQUAL "all")
        endif (NOT ${MODULE} STREQUAL "measures")
    endif (NOT ${MODULE} STREQUAL "tables")
endif (NOT ${MODULE} STREQUAL "casa")

# Determine which external packages to use.
include (CTest)
find_package (DL)
find_package (Readline)
if (USE_HDF5)
    find_package (HDF5 REQUIRED)
endif (USE_HDF5)
if (_usebison STREQUAL YES)
    find_package (FLEX REQUIRED)
    find_package (BISON REQUIRED)
endif (_usebison STREQUAL YES)
if (_uselapack STREQUAL YES)
    find_package (BLAS REQUIRED)
    find_package (LAPACK REQUIRED)
    if (USE_FFTW3)
        if (FFTW3_DISABLE_THREADS)
            find_package (FFTW3 COMPONENTS single double REQUIRED)
        else (FFTW3_DISABLE_THREADS)
            find_package (FFTW3 COMPONENTS single double threads)
        endif (FFTW3_DISABLE_THREADS)
    endif (USE_FFTW3)
endif (_uselapack STREQUAL YES)
if (_usefits STREQUAL YES)
    find_package (CFITSIO REQUIRED)
endif (_usefits STREQUAL YES)
if (_usewcs STREQUAL YES)
    find_package (WCSLIB REQUIRED)
endif (_usewcs STREQUAL YES)
 
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

# Thread support.
set(_errmsg "FIXME: Don't know how to enable thread support for ")
find_package(Pthreads REQUIRED)
if(CMAKE_COMPILER_IS_GNUCC)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread")
else(CMAKE_COMPILER_IS_GNUCC)
    message(FATAL_ERROR "${_errmsg} ${CMAKE_C_COMPILER}")
endif(CMAKE_COMPILER_IS_GNUCC)
if(CMAKE_COMPILER_IS_GNUCXX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
else(CMAKE_COMPILER_IS_GNUCXX)
    message(FATAL_ERROR "${_errmsg} ${CMAKE_CXX_COMPILER}")
endif(CMAKE_COMPILER_IS_GNUCXX)
if(USE_THREADS)
    add_definitions(-DUSE_THREADS)
endif(USE_THREADS)
if(USE_OPENMP)
    add_definitions(-DUSE_MULTI_THREADING)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")
endif(USE_OPENMP)

# Add the modules to be built.
foreach (module ${_modules})
    add_subdirectory (${module})
    if (APPLE)
        if (${module} STREQUAL scimath_f OR ${module} STREQUAL fits OR ${module} STREQUAL mirlib OR ${module} STREQUAL coordinates)
            set_target_properties(casa_${module} PROPERTIES LINK_FLAGS -single_module)
        endif (${module} STREQUAL scimath_f OR ${module} STREQUAL fits OR ${module} STREQUAL mirlib OR ${module} STREQUAL coordinates)
    endif (APPLE)
endforeach (module)

# Show summary.
message (STATUS "CMAKE_SYSTEM .......... = ${CMAKE_SYSTEM}")
message (STATUS "CMAKE_BUILD_TYPE ...... = ${CMAKE_BUILD_TYPE}")
message (STATUS "Modules to be built.... = ${_modules}")
message (STATUS "BUILD_SHARED_LIBS ..... = ${BUILD_SHARED_LIBS}")
message (STATUS "CMAKE_CXX_COMPILER .... = ${CMAKE_CXX_COMPILER}")
message (STATUS "CMAKE_CXX_FLAGS ....... = ${CMAKE_CXX_FLAGS}")
message (STATUS "DL library?       ..... = ${DL_LIBRARIES}")
message (STATUS "Pthreads library? ..... = ${PTHREADS_LIBRARIES}")
message (STATUS "Readline library? ..... = ${READLINE_LIBRARIES}")
message (STATUS "BLAS library? ......... = ${BLAS_LIBRARIES}")
message (STATUS "LAPACK library? ....... = ${LAPACK_LIBRARIES}")
message (STATUS "WCS library? .......... = ${WCSLIB_LIBRARIES}")
message (STATUS "CFitsio library? ...... = ${CFITSIO_LIBRARIES}")
message (STATUS "HDF5 library? ......... = ${HDF5_hdf5_LIBRARY}")
message (STATUS "FFTW3 library? ........ = ${FFTW3_LIBRARIES}")
