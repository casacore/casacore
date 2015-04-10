#
# Casacore
#
project(casacore)

cmake_minimum_required (VERSION 2.6.0)

set(PROJECT_VERSION_MAJOR 2)
set(PROJECT_VERSION_MINOR 0)
set(PROJECT_VERSION_PATCH 1)
set(PROJECT_VERSION_REVISION 1)
set(PROJECT_VERSION 
  "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH}")

set(CPACK_GENERATOR "DEB")
set(CPACK_PACKAGE_NAME "casacore")
set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")

set(CPACK_PACKAGE_CONTACT "Malte Marquarding") #required
set(CPACK_PACKAGE_VENDOR "casacore.googlecode.com")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Astronomical data processing library")

string(TOLOWER "${CPACK_PACKAGE_NAME}" CPACK_PACKAGE_NAME_LOWERCASE) 
find_program(DPKG_PROGRAM dpkg DOC "dpkg program of Debian-based systems") 
if(DPKG_PROGRAM) 
  execute_process( 
    COMMAND ${DPKG_PROGRAM} --print-architecture 
    OUTPUT_VARIABLE CPACK_DEBIAN_PACKAGE_ARCHITECTURE 
    OUTPUT_STRIP_TRAILING_WHITESPACE 
  ) 
  set( CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME_LOWERCASE}-dev_${PROJECT_VERSION}-${PROJECT_VERSION_REVISION}_${CPACK_DEBIAN_PACKAGE_ARCHITECTURE}")   

else() 
  set(CPACK_PACKAGE_FILE_NAME 
"${CPACK_PACKAGE_NAME_LOWERCASE}_${PROJECT_VERSION}-${PROJECT_VERSION_REVISION}_${CMAKE_SYSTEM_NAME}") 
endif() 

# debian specific
set(CPACK_DEBIAN_PACKAGE_NAME "casacore-dev")
set(CPACK_DEBIAN_PACKAGE_SECTION "science")
set(CPACK_DEBIAN_PACKAGE_PRIORITY "extra")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "libcfitsio3, libgfortran3")
include(CPack)

enable_language (Fortran)

set (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# By default build shared libraries
option (BUILD_PYTHON "Build the python bindings" NO)
option (ENABLE_SHARED "Build shared libraries" YES)
option (ENABLE_RPATH "Include rpath in executables and shared libraries" YES)

# By default do not use HDF5, FFTW3, threads
option (CXX11 "Compile as C++11 if possible" NO)
option (ENABLE_TABLELOCKING "Make locking for concurrent table access possible" YES)
option (USE_HDF5 "Build HDF5 " NO)
option (USE_FFTW3 "Use FFTW instead of FFTPack" NO)
option (USE_THREADS "Use Mutex thread synchronization" NO)
option (USE_OPENMP "Use OpenMP threading" NO)
option (USE_STACKTRACE "Show stacktrace in case of exception" NO)

# Test if shared libraries have to be built.
if (ENABLE_SHARED)
    option (BUILD_SHARED_LIBS "" YES)
    if (ENABLE_RPATH)
        # Set RPATH to use for installed targets; append linker search path
        set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
        set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
    endif (ENABLE_RPATH)
else()
    option (BUILD_SHARED_LIBS "" NO)
endif (ENABLE_SHARED)

# Define the compiler flags to be used.
# Note: -Wshadow and -Wunreachable-code give (too) many warnings.
# Casacore uses longlong, so no warnings for it.
# Clang gives warning on bison generated code; disable unneeded-internal-declaration.
if (NOT CMAKE_CXX_FLAGS)
    set (CMAKE_CXX_FLAGS "-Wextra -Wall -W -Wpointer-arith -Woverloaded-virtual -Wwrite-strings -pedantic -Wno-long-long")
#SET(CMAKE_CXX_FLAGS="-g -O0 -Wall -Wextra -Wshadow -Wunused-variable
# -Wunused-parameter -Wunused-function -Wunused -Wno-system-headers
# -Wno-deprecated -Woverloaded-virtual -Wwrite-strings -fprofile-arcs
# -ftest-coverage -Wold-style-cast -Weffc++ -Wconversion")
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unneeded-internal-declaration")
    endif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
endif (NOT CMAKE_CXX_FLAGS)

# Set build type if not given.
if (NOT CMAKE_BUILD_TYPE)
    # Use debug mode if building in dbg or debug directory.
    get_filename_component(_cmpvar ${CMAKE_BINARY_DIR} NAME)
    if("_cmpvar" STREQUAL "dbg" OR "_cmpvar" STREQUAL "debug")
        set (CMAKE_BUILD_TYPE Debug)
    else()
        if("_cmpvar" STREQUAL "cov")
            set (CMAKE_BUILD_TYPE Debug)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0 --coverage")
            set(CMAKE_LD_FLAGS "${CMAKE_LD_FLAGS} --coverage")
        else()
            set (CMAKE_BUILD_TYPE Release)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DNDEBUG")
        endif("_cmpvar" STREQUAL "cov")
    endif("_cmpvar" STREQUAL "dbg" OR "_cmpvar" STREQUAL "debug")
endif (NOT CMAKE_BUILD_TYPE)

# Detect if the compiler supports C++11 if we want to use it.
if (CXX11)
    if ((CMAKE_COMPILER_IS_GNUCXX AND NOT ${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS 4.6) OR
       (CMAKE_CXX_COMPILER_ID STREQUAL "Clang" AND NOT ${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS 4.3))
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
    else()
        option(CXX11 "" NO)
    endif()
endif()

# use faster fortran rules for complex operations, removes restoring complex
# infinities if naive computation results in NAN + NAN * I
include(CheckCXXCompilerFlag)
include(CheckCCompilerFlag)
#Handling complex multiplication and division with correct treating of complex
#infinities (one element Inf regardless of the other) according to the C is
#complicated.
#E.g. a = NaN + 1e30 i; a * a is not NaN but a complex infinity (-Inf - NaN).
#
#Treating this situation correctly has large performance impact. In GCC's
#implementation it is about 4 times slower than the naive implementation, with
#vectorization enabled the impact is even larger.
#As correct treatment of complex infinities when NaN appear in results is seldom
#accounted for, or not required and most other languages do not have these
#rules, the correct treatmeant can be disabled with the -fcx-fortran-rules flag.
#This changes the semantics to those of the FORTRAN language which is removes
#the need for rescuing the result when NaN appear. Python also follows FORTRAN
#rules.
#Additionally the correct behavior is not implemented in all compilers,
#most notably clang which is the default compiler on MacOS. So turning off
#correct treatment with GCC does not only make our code faster but also behave
#the same on more compilers.
#
#This has measurable impact on e.g. applycal where the cpu performance improves
#by about 20%.
check_cxx_compiler_flag(-fcx-fortran-rules HAS_GXX_FORTRAN_RULES)
check_c_compiler_flag(-fcx-fortran-rules HAS_GCC_FORTRAN_RULES)
# added before cmake flags so it can be disabled with
# -fno-cx-fortran-rules for testing
if (HAS_GXX_FORTRAN_RULES)
    set(CMAKE_CXX_FLAGS "-fcx-fortran-rules ${CMAKE_CXX_FLAGS}")
endif()
if (HAS_GCC_FORTRAN_RULES)
    set(CMAKE_C_FLAGS "-fcx-fortran-rules ${CMAKE_C_FLAGS}")
endif()

# Ensure clang is not complaining about unused arguments.
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set(CMAKE_CXX_FLAGS "-Qunused-arguments ${CMAKE_CXX_FLAGS}")    
    set(CMAKE_C_FLAGS "-Qunused-arguments ${CMAKE_C_FLAGS}")    
endif()


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
                set (_modules ${_modules} mirlib coordinates images)
                set (_usewcs YES)
	    endif (${MODULE} STREQUAL "images" OR ${MODULE} STREQUAL "all")
        endif (NOT ${MODULE} STREQUAL "measures")
    endif (NOT ${MODULE} STREQUAL "tables")
endif (NOT ${MODULE} STREQUAL "casa")
if (BUILD_PYTHON)
    set (_modules ${_modules} python)
    find_package(Boost REQUIRED COMPONENTS python)
    find_package(Python REQUIRED)
    find_package (NUMPY REQUIRED)
endif (BUILD_PYTHON)

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
        else()
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
if (Boost_FOUND)
    include_directories (${Boost_INCLUDE_DIRS})
    add_definitions(-DHAVE_BOOST)
endif (Boost_FOUND)
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

if(USE_OPENMP)
    set (USE_THREADS YES)
    find_package (OpenMP REQUIRED)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif(USE_OPENMP)
# Thread support?
if(USE_THREADS)
    set(_errmsg "FIXME: Don't know how to enable thread support for ")
    find_package(Pthreads REQUIRED)
    add_definitions(-DUSE_THREADS)
    if(CMAKE_COMPILER_IS_GNUCC)
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread")
    else()
        if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
            set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread")
        else()
            message(FATAL_ERROR "${_errmsg} ${CMAKE_C_COMPILER}")
        endif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    endif(CMAKE_COMPILER_IS_GNUCC)
    if(CMAKE_COMPILER_IS_GNUCXX)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
    else()
        if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
        else()
            message(FATAL_ERROR "${_errmsg} ${CMAKE_CXX_COMPILER}")
        endif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    endif(CMAKE_COMPILER_IS_GNUCXX)
endif(USE_THREADS)

# Set compiler flag in no table locking.
if (NOT ENABLE_TABLELOCKING)
    add_definitions(-DAIPS_TABLE_NOLOCKING)
endif (NOT ENABLE_TABLELOCKING)

if(USE_STACKTRACE)
    add_definitions(-DUSE_STACKTRACE)
endif(USE_STACKTRACE)

# Add the modules to be built.
add_subdirectory (build-tools)
foreach (module ${_modules})
    add_subdirectory (${module})
    set_target_properties(
      casa_${module} 
      PROPERTIES 
          VERSION "${PROJECT_VERSION}"
          SOVERSION "${PROJECT_VERSION_MAJOR}"
    )
    if (APPLE)
        if (${module} STREQUAL scimath_f OR ${module} STREQUAL fits OR ${module} STREQUAL mirlib OR ${module} STREQUAL coordinates)
            set_target_properties(casa_${module} PROPERTIES LINK_FLAGS -single_module)
        endif (${module} STREQUAL scimath_f OR ${module} STREQUAL fits OR ${module} STREQUAL mirlib OR ${module} STREQUAL coordinates)
    endif (APPLE)
endforeach (module)

# Show summary.
message (STATUS "CMAKE_SYSTEM .......... = ${CMAKE_SYSTEM}")
message (STATUS "CMAKE_BUILD_TYPE ...... = ${CMAKE_BUILD_TYPE}")
message (STATUS "Modules to be built ... = ${_modules}")
message (STATUS "BUILD_SHARED_LIBS ..... = ${BUILD_SHARED_LIBS}")
message (STATUS "ENABLE_RPATH .......... = ${ENABLE_RPATH}")
message (STATUS "ENABLE_TABLELOCKING ... = ${ENABLE_TABLELOCKING}")
message (STATUS "USE_OPENMP ............ = ${USE_OPENMP}")
message (STATUS "USE_STACKTRACE ........ = ${USE_STACKTRACE}")
message (STATUS "CMAKE_CXX_COMPILER .... = ${CMAKE_CXX_COMPILER}")
message (STATUS "CMAKE_CXX_FLAGS ....... = ${CMAKE_CXX_FLAGS}")
message (STATUS "C++11 support ......... = ${CXX11}")
message (STATUS "DL library? ........... = ${DL_LIBRARIES}")
message (STATUS "Pthreads library? ..... = ${PTHREADS_LIBRARIES}")
message (STATUS "Readline library? ..... = ${READLINE_LIBRARIES}")
message (STATUS "BLAS library? ......... = ${BLAS_LIBRARIES}")
message (STATUS "LAPACK library? ....... = ${LAPACK_LIBRARIES}")
message (STATUS "WCS library? .......... = ${WCSLIB_LIBRARIES}")
message (STATUS "CFitsio library? ...... = ${CFITSIO_LIBRARIES}")
message (STATUS "HDF5 library? ......... = ${HDF5_hdf5_LIBRARY}")
message (STATUS "FFTW3 library? ........ = ${FFTW3_LIBRARIES}")
message (STATUS "BUILD_PYTHON .......... = ${BUILD_PYTHON}")
if (BUILD_PYTHON)
    message (STATUS "PYTHON library? ....... = ${PYTHON_LIBRARIES}")
    message (STATUS "NUMPY include? ........ = ${NUMPY_INCLUDE_DIRS}")
    message (STATUS "Boost library? ........ = ${Boost_LIBRARIES}")
    message (STATUS "Boost includes? ....... = ${Boost_INCLUDE_DIRS}")
endif (BUILD_PYTHON)


# List of build variables and defaults.
#  BUILD_PYTHON                  NO
#  ENABLE_SHARED                 YES
#  ENABLE_RPATH                  YES
#  CXX11                         NO
#  ENABLE_TABLELOCKING           YES
#  USE_HDF5                      NO
#  USE_FFTW                      NO
#  USE_THREADS                   NO
#  USE_OPENMP                    NO
#  USE_STACKTRACE                NO
#  DATA_DIR                      ""
# 
# List of possibly used external packages and where
#  CFITSIO      fits
#  WCSLIB       coordinates
#  DL           casa (optional)
#  READLINE     casa (optional)
#  HDF5         casa (optional)
#  BISON        tables,images
#  FLEX         tables,images
#  LAPACK       scimath
#  BLAS         scimath
#  FFTW         scimath (optional)
#  BOOST        python (Boost-Python only)
#  PYTHON       python
#  NUMPY        python

