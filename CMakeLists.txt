#
# Casacore
#
project(casacore)

cmake_minimum_required (VERSION 2.6.0)
include(CheckCXXCompilerFlag)
include(CheckCCompilerFlag)
include(CheckFunctionExists)

# fixes warnings on cmake 3.x+ For now we want the old behavior to be backwards compatible
if (POLICY CMP0048)
    cmake_policy (SET CMP0048 OLD)
endif()

set(PROJECT_VERSION_MAJOR 2)
set(PROJECT_VERSION_MINOR 1)
set(PROJECT_VERSION_PATCH 0)
set(PROJECT_VERSION_REVISION 1)
set(PROJECT_VERSION 
  "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH}")

SET(NO_SOVERSION FALSE CACHE BOOL "do not add version information to shared libraries")
set (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

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

# By default build only Python2 bindings
option (BUILD_PYTHON "Build the python bindings" YES)
option (BUILD_PYTHON3 "Build the python3 bindings" NO)

# By default build shared libraries
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
option (CASA_BUILD "Building the in the CASA (http://casa.nrao.edu) environment" NO)

set(CASA_DEFAULT_ALIGNMENT "32" CACHE STRING "Default alignment of casa::AlignedAllocator")

# basic setup for building within CASA environment
if( CASA_BUILD )
   if( NOT DATA_DIR )
      set(DATA_DIR "%CASAROOT%/data")
   endif( )
   set(USE_FFTW3 ON)
   set(USE_OPENMP ON)
   set(USE_THREADS ON)
   set(UseCasacoreNamespace 1) # Force casacore routines to live in casacore rather than casa namespace
   ###
   ### after CASA switches from ASAP to libsakura, CASA will no longer
   ### use casacore's python module and it will no longer use boost...
   ###
   set(BUILD_PYTHON ON)
   set(Boost_NO_BOOST_CMAKE 1)
   if (EXISTS "/opt/casa/01/include/python2.7")
      ### RHEL7
      set(PYTHON_LIBRARY "/opt/casa/01/lib/libpython2.7.so")
      set(PYTHON_INCLUDE_DIR "/opt/casa/01/include/python2.7")
      set(PYTHON_EXECUTABLE:FILEPATH "/opt/casa/01/bin/python")
      if (EXISTS "/usr/include/boost")
         set(BOOST_ROOT "/usr")
      endif( )
   elseif(EXISTS "/usr/lib64/casa/01/include/python2.7")
      ### RHEL5/RHEL6
      set(PYTHON_LIBRARY "/usr/lib64/casa/01/lib/libpython2.7.so")
      set(PYTHON_INCLUDE_DIR "/usr/lib64/casa/01/include/python2.7")
      set(PYTHON_EXECUTABLE:FILEPATH "/usr/lib64/casa/01/bin/python")
      if (EXISTS "/usr/lib64/casa/01/include/boost")
         ### RHEL5
         set(BOOST_ROOT "/usr/lib64/casa/01")
      elseif (EXISTS "/usr/include/boost")
         ### RHEL6
         set(BOOST_ROOT="/usr")
      endif( )
   endif( )
endif( )

# Test if shared libraries have to be built.
if (ENABLE_SHARED)
    option (BUILD_SHARED_LIBS "" YES)
    if (ENABLE_RPATH)
        # Set RPATH to use for installed targets; append linker search path
        set(CMAKE_INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib" )
        set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
        set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
        set(CMAKE_MACOSX_RPATH TRUE)
    endif (ENABLE_RPATH)
	if( CASA_BUILD )
		set(CXX11 ON)
		option(NO_SOVERSION "Build shared libraries without version information" NO)
		if( NOT NO_SOVERSION )
			set( epochdelta 1385614800 )
			execute_process( COMMAND perl -e "$t=time( )-${epochdelta};$z=$t & 0xff; $y=($t>>8)&0xff; $x=($t>>16)&0xffff; print \"$x.$y.$z\""
					         OUTPUT_VARIABLE __casa_soversion )
			set(casa_soversion ${__casa_soversion} CACHE STRING "version for shared objects")
			message( STATUS "Shared object version number ${casa_soversion}" )
			file( WRITE ${CMAKE_INSTALL_PREFIX}/casa_sover.txt
				  "# generated by casacore/CMakeList.txt... Do not edit\n"
				  "${casa_soversion}\n" )
		else( )
			message( STATUS "User disabled shared library versioning" )
		endif( )
	endif( )
else()
    option (BUILD_SHARED_LIBS "" NO)
endif (ENABLE_SHARED)

if (UseCasacoreNamespace)

    add_definitions (-DUseCasacoreNamespace)
    message (STATUS "Using namespace casacore.")

else ()

    message (STATUS "Namespace casacore 'merged' into namespace casa.")

endif ()

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
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsigned-char")

# Set build type if not given.
if (NOT CMAKE_BUILD_TYPE)
    # Use debug mode if building in dbg or debug directory.
    get_filename_component(_cmpvar ${CMAKE_BINARY_DIR} NAME)
    if(_cmpvar STREQUAL "dbg" OR _cmpvar STREQUAL "debug")
        set (CMAKE_BUILD_TYPE Debug)
    else()
        if(_cmpvar STREQUAL "cov")
            set (CMAKE_BUILD_TYPE Debug)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0 --coverage")
            set(CMAKE_LD_FLAGS "${CMAKE_LD_FLAGS} --coverage")
        else()
            set (CMAKE_BUILD_TYPE Release)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DNDEBUG")
        endif(_cmpvar STREQUAL "cov")
    endif(_cmpvar STREQUAL "dbg" OR _cmpvar STREQUAL "debug")
endif (NOT CMAKE_BUILD_TYPE)

# Detect if the compiler supports C++11 if we want to use it.
if (CXX11)
    check_cxx_compiler_flag(-std=c++11 HAS_CXX11)
    if (HAS_CXX11)
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
    else()
      if (CASA_BUILD)
        message(FATAL_ERROR "CASA_BUILD requires a c++11 compatible compiler")
      else()
        message(FATAL_ERROR "C++11 build was requested but the compiler does not support -std=c++11")
      endif()
    endif()
endif()

# use faster fortran rules for complex operations, removes restoring complex
# infinities if naive computation results in NAN + NAN * I
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

# Since 2015, we need pwrite and pwrite (POSIX 2001)
check_function_exists(pread HAVE_PREAD)
check_function_exists(pwrite HAVE_PWRITE)
if (NOT (HAVE_PREAD AND HAVE_PWRITE))
        message(FATAL_ERROR "Casacore requires pread and pwrite functionality" )
endif (NOT (HAVE_PREAD AND HAVE_PWRITE))

# FindHDF5 uses environment variables, so set it if needed.
if (HDF5_ROOT_DIR)
    set (ENV{HDF5_ROOT} ${HDF5_ROOT_DIR})
endif (HDF5_ROOT_DIR)

# Find out which modules to build.
if (NOT MODULE)
   set (MODULE all)
endif (NOT MODULE)
set (_usebison YES)
set (_uselapack YES)
set (_usefits NO)
set (_usewcs NO)
set (_modules casa)
set (_modules2 )
if (NOT ${MODULE} STREQUAL "casa")
    set (_modules ${_modules} tables)
    set (_usebison YES)
    if (NOT ${MODULE} STREQUAL "tables")
        set (_modules ${_modules} scimath_f scimath measures meas)
        set (_uselapack YES)
        if (NOT ${MODULE} STREQUAL "measures")
            if (${MODULE} STREQUAL "ms")
                set (_modules ${_modules} ms derivedmscal)
            endif()
            if (${MODULE} STREQUAL "msfits" OR ${MODULE} STREQUAL "all")
                set (_modules2 ${_modules2} ms msfits derivedmscal)
                set (_usefits YES)
            endif()
            if (${MODULE} STREQUAL "images" OR ${MODULE} STREQUAL "all")
                set (_modules2 ${_modules2} lattices mirlib coordinates images)
                set (_usewcs YES)
                set (_usefits YES)
            endif()
        endif()
    endif()
endif()

if (_usefits)
    set (_modules ${_modules} fits)
endif()
set (_modules ${_modules} ${_modules2})

if (BUILD_PYTHON)
    set (_modules ${_modules} python)
endif (BUILD_PYTHON)

if (BUILD_PYTHON3)
    set (_modules ${_modules} python3)
endif (BUILD_PYTHON3)


# Determine which external packages to use.
include (CTest)
find_package (DL)
find_package (Readline)
find_package (SOFA)
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
    find_package (CFITSIO 3.030 REQUIRED) # Should pad to three decimal digits
endif (_usefits STREQUAL YES)
if (_usewcs STREQUAL YES)
#    find_package (WCSLIB 4.20 REQUIRED)
    find_package (WCSLIB 4.7 REQUIRED)     # needed for CASA
    if (WCSLIB_VERSION_STRING STREQUAL "5.14")
      # WCSlib 5.14 throws segfaults on lots of tests, e.g. tTempImage
      message(WARNING "Casacore is not compatible with wcslib 5.14, see issue gh-384.")
    endif (WCSLIB_VERSION_STRING STREQUAL "5.14")
endif (_usewcs STREQUAL YES)
 
# Set the include directories and HAVE compiler variables
include_directories (${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
if (WCSLIB_FOUND)
    include_directories (${WCSLIB_INCLUDE_DIRS})
    add_definitions (-DWCSLIB_VERSION_MAJOR=${WCSLIB_VERSION_MAJOR} -DWCSLIB_VERSION_MINOR=${WCSLIB_VERSION_MINOR})
endif (WCSLIB_FOUND)
if (CFITSIO_FOUND)
    include_directories (${CFITSIO_INCLUDE_DIRS})
    add_definitions (-DCFITSIO_VERSION_MAJOR=${CFITSIO_VERSION_MAJOR} -DCFITSIO_VERSION_MINOR=${CFITSIO_VERSION_MINOR})
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
    find_package (OpenMP)
    if (OPENMP_FOUND)
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    else (OPENMP_FOUND)
        message(WARNING "Cannot fullfill USE_OPENMP, compiler does not support it")
    endif (OPENMP_FOUND)
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

# Set default DATA_DIR if undefined.
if (NOT DATA_DIR)
   set (DATA_DIR ${CMAKE_INSTALL_PREFIX}/share/casacore/data)
endif (NOT DATA_DIR)
# Let cmake cache DATA_DIR.
set (DATA_DIR "${DATA_DIR}" CACHE PATH "Measures tables root")


# Set compiler flag if no table locking.
if (NOT ENABLE_TABLELOCKING)
    add_definitions(-DAIPS_TABLE_NOLOCKING)
endif (NOT ENABLE_TABLELOCKING)
# Set compiler flag if stack tracing.
if(USE_STACKTRACE)
    add_definitions(-DUSE_STACKTRACE)
endif(USE_STACKTRACE)

# Enable cmake testing and add make check target that builds and runs the test
enable_testing()
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND})
# This legacy flag always builds the tests and runs them with make test. There
# seems to be no good way to make test executable depend on the test target
if (NOT BUILD_TESTING)
    set(EXCL_ALL EXCLUDE_FROM_ALL)
endif (NOT BUILD_TESTING)

# Add the modules to be built.
add_subdirectory (build-tools)
foreach (module ${_modules})
    add_subdirectory (${module})
    if (casa_soversion)
        set(CMAKE_INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib" )
        set_target_properties(
          casa_${module} 
          PROPERTIES 
              VERSION "${casa_soversion}"
              SOVERSION "${casa_soversion}"
        )
    else( )
        set_target_properties(
          casa_${module} 
          PROPERTIES 
              VERSION "${PROJECT_VERSION}"
              SOVERSION "${PROJECT_VERSION_MAJOR}"
        )
    endif( )
    if (CASA_BUILD)
        if (PYTHON_SHARED_LINKER_FLAGS AND ${module} STREQUAL python)
            set_target_properties(casa_python PROPERTIES LINK_FLAGS ${PYTHON_SHARED_LINKER_FLAGS})
        endif (PYTHON_SHARED_LINKER_FLAGS AND ${module} STREQUAL python)
    endif (CASA_BUILD)
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
message (STATUS "DATA directory ........ = ${DATA_DIR}")
message (STATUS "DL library? ........... = ${DL_LIBRARIES}")
message (STATUS "Pthreads library? ..... = ${PTHREADS_LIBRARIES}")
message (STATUS "Readline library? ..... = ${READLINE_LIBRARIES}")
message (STATUS "BLAS library? ......... = ${BLAS_LIBRARIES}")
message (STATUS "LAPACK library? ....... = ${LAPACK_LIBRARIES}")
message (STATUS "WCS library? .......... = ${WCSLIB_LIBRARIES}")
message (STATUS "SOFA library? ......... = ${SOFA_LIBRARIES}")
message (STATUS "CFitsio library? ...... = ${CFITSIO_LIBRARIES}")
message (STATUS "HDF5 library? ......... = ${HDF5_hdf5_LIBRARY}")
message (STATUS "FFTW3 library? ........ = ${FFTW3_LIBRARIES}")

message (STATUS "BUILD_PYTHON .......... = ${BUILD_PYTHON}")
message (STATUS "BUILD_PYTHON3 ......... = ${BUILD_PYTHON3}")

if (BUILD_PYTHON)
    message (STATUS "PYTHON2_EXECUTABLE ......... = ${PYTHON2_EXECUTABLE}")
    message (STATUS "PYTHON2_LIBRARIES........... = ${PYTHON2_LIBRARIES}")
    message (STATUS "PYTHON2_NUMPY_INCLUDE_DIRS . = ${PYTHON2_NUMPY_INCLUDE_DIRS}")
    message (STATUS "PYTHON2_Boost_LIBRARIES .... = ${PYTHON2_Boost_LIBRARIES}")
    message (STATUS "PYTHON2_Boost_INCLUDE_DIRS . = ${PYTHON2_Boost_INCLUDE_DIRS}")
endif (BUILD_PYTHON)

if (BUILD_PYTHON3)
    message (STATUS "PYTHON3_EXECUTABLE ......... = ${PYTHON3_EXECUTABLE}")
    message (STATUS "PYTHON3_LIBRARIES .......... = ${PYTHON3_LIBRARIES}")
    message (STATUS "PYTHON3_NUMPY_INCLUDE_DIRS . = ${PYTHON3_NUMPY_INCLUDE_DIRS}")
    message (STATUS "PYTHON3_Boost_LIBRARIES .... = ${PYTHON3_Boost_LIBRARIES}")
    message (STATUS "PYTHON3_Boost_INCLUDE_DIRS . = ${PYTHON3_Boost_INCLUDE_DIRS}")
endif (BUILD_PYTHON3)

# List of build variables and defaults.
#  BUILD_PYTHON                  NO
#  ENABLE_SHARED                 YES
#  ENABLE_RPATH                  YES
#  CXX11                         NO
#  ENABLE_TABLELOCKING           YES
#  USE_HDF5                      NO
#  USE_FFTW3                     NO
#  USE_THREADS                   NO
#  USE_OPENMP                    NO
#  USE_STACKTRACE                NO
#  DATA_DIR                      ${CMAKE_INSTALL_PREFIX}/share/casacore/data
#  MODULE                        all
#                                Possible value for MODULE (previous built too):
#                                - casa     (casa)
#                                - tables   (tables)
#                                - measures (scimath,scimath_f,measures,meas)
#                                -  ms      (ms,derivedmscal)
#                                or msfits  (fits,ms,msfits,derivedmscal)
#                                or images  (fits,lattices,mirlib,coordinates,images)
#                                - all
# 
# List of possibly used external packages and where
#  CFITSIO      fits
#  WCSLIB       coordinates
#  SOFA         measures (optional, only for testing)
#  DL           casa (optional)
#  READLINE     casa (optional)
#  HDF5         casa (optional)
#  BISON        casa,tables,images
#  FLEX         casa,tables,images
#  LAPACK       scimath
#  BLAS         scimath
#  FFTW         scimath (optional)
#  BOOST        python (Boost-Python only)
#  PYTHON       python
#  NUMPY        python

