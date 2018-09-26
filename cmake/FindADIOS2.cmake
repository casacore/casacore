# - Find ADIOS2 library, routines for scientific, parallel IO
#   https://www.olcf.ornl.gov/center-projects/adios2/
#
# Use this module by invoking find_package with the form:
#   find_package(ADIOS2
#     [version] [EXACT]     # Minimum or EXACT version, e.g. 1.6.0
#     [REQUIRED]            # Fail with an error if ADIOS2 or a required
#                           #   component is not found
#     [QUIET]               # ...
#     [COMPONENTS <...>]    # Compiled in components: fortran, readonly, 
                            # sequential (all are case insentative) 
#   )
#
# Module that finds the includes and libraries for a working ADIOS2 install.
# This module invokes the `adios2-config` script that should be installed with
# the other ADIOS2 tools.
#
# To provide a hint to the module where to find the ADIOS2 installation,
# set the ADIOS2_ROOT environment variable.
#
# If this variable is not set, make sure that at least the according `bin/`
# directory of ADIOS2 is in your PATH environment variable.
#
# Set the following CMake variables BEFORE calling find_packages to
# influence this module:
#   ADIOS2_USE_STATIC_LIBS - Set to ON to force the use of static
#                           libraries.  Default: OFF
#
# This module will define the following variables:
#   ADIOS2_INCLUDE_DIRS    - Include directories for the ADIOS2 headers.
#   ADIOS2_LIBRARIES       - ADIOS2 libraries.
#   ADIOS2_FOUND           - TRUE if FindADIOS2 found a working install
#   ADIOS2_VERSION         - Version in format Major.Minor.Patch
#
# Not used for now:
#   ADIOS2_DEFINITIONS     - Compiler definitions you should add with
#                           add_definitions(${ADIOS2_DEFINITIONS})
#
# Example to find ADIOS2 (default)
# find_package(ADIOS2)
# if(ADIOS2_FOUND)
#   include_directories(${ADIOS2_INCLUDE_DIRS})
#   add_executable(foo foo.c)
#   target_link_libraries(foo ${ADIOS2_LIBRARIES})
# endif()

# Example to find ADIOS2 using component
# find_package(ADIOS2 COMPONENTS fortran)
# if(ADIOS2_FOUND)
#   include_directories(${ADIOS2_INCLUDE_DIRS})
#   add_executable(foo foo.c)
#   target_link_libraries(foo ${ADIOS2_LIBRARIES})
# endif()
###############################################################################
#Copyright (c) 2014, Axel Huebl and Felix Schmitt from http://picongpu.hzdr.de
#All rights reserved.

#Redistribution and use in source and binary forms, with or without
#modification, are permitted provided that the following conditions are met:

#1. Redistributions of source code must retain the above copyright notice, this
#list of conditions and the following disclaimer.

#2. Redistributions in binary form must reproduce the above copyright notice,
#this list of conditions and the following disclaimer in the documentation
#and/or other materials provided with the distribution.

#3. Neither the name of the copyright holder nor the names of its contributors
#may be used to endorse or promote products derived from this software without
#specific prior written permission.

#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
#DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
#FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
#CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
#OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
###############################################################################


###############################################################################
# Required cmake version
###############################################################################

cmake_minimum_required(VERSION 2.8.5)


###############################################################################
# ADIOS2
###############################################################################
# get flags for adios2-config, -l is the default
#-f for fortran, -r for readonly, -s for sequential (nompi)
set(OPTLIST "--libs")
#if(ADIOS2_FIND_COMPONENTS)
#    foreach(COMP ${ADIOS2_FIND_COMPONENTS})
#        string(TOLOWER ${COMP} comp)
#        if(comp STREQUAL "fortran")
#            set(OPTLIST "${OPTLIST}f")
#        elseif(comp STREQUAL "readonly")
#            set(OPTLIST "${OPTLIST}r")
#        elseif(comp STREQUAL "sequential")
#            set(OPTLIST "${OPTLIST}s")
#        else()
#            message("ADIOS2 component ${COMP} is not supported. Please use fortran, readonly, or sequential")
#        endif()
#    endforeach()
#endif()

# we start by assuming we found ADIOS2 and falsify it if some
# dependencies are missing (or if we did not find ADIOS2 at all)
set(ADIOS2_FOUND TRUE)


# find `adios2-config` program #################################################
#   check the ADIOS2_ROOT hint and the normal PATH
find_file(ADIOS2_CONFIG
    NAME adios2-config
    PATHS $ENV{ADIOS2_ROOT}/bin $ENV{ADIOS2_DIR}/bin $ENV{INSTALL_PREFIX}/bin $ENV{PATH})

if(ADIOS2_CONFIG)
    message(STATUS "Found 'adios2-config': ${ADIOS2_CONFIG}")
else(ADIOS2_CONFIG)
    set(ADIOS2_FOUND FALSE)
    message(STATUS "Can NOT find 'adios2-config' - set ADIOS2_ROOT, ADIOS2_DIR or INSTALL_PREFIX, or check your PATH")
endif(ADIOS2_CONFIG)

# check `adios2-config` program ################################################
if(ADIOS2_FOUND)
    execute_process(COMMAND ${ADIOS2_CONFIG} ${OPTLIST}
                    OUTPUT_VARIABLE ADIOS2_LINKFLAGS
                    RESULT_VARIABLE ADIOS2_CONFIG_RETURN
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    if(NOT ADIOS2_CONFIG_RETURN EQUAL 0)
        set(ADIOS2_FOUND FALSE)
        message(STATUS "Can NOT execute 'adios2-config' - check file permissions")
    endif()

    # find ADIOS2_ROOT_DIR
    execute_process(COMMAND ${ADIOS2_CONFIG} --prefix
                    OUTPUT_VARIABLE ADIOS2_ROOT_DIR
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    if(NOT IS_DIRECTORY "${ADIOS2_ROOT_DIR}")
        set(ADIOS2_FOUND FALSE)
        message(STATUS "The directory provided by 'adios2-config --prefix' does not exist: ${ADIOS2_ROOT_DIR}")
    endif()
endif(ADIOS2_FOUND)

# option: use only static libs ################################################
if(ADIOS2_USE_STATIC_LIBS)
    # carfully: we have to restore the original path in the end
    set(_ORIG_CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})
    set(CMAKE_FIND_LIBRARY_SUFFIXES .a)
endif()


# we found something in ADIOS2_ROOT_DIR and adios2-config works #################
if(ADIOS2_FOUND)
    # ADIOS2 headers
    list(APPEND ADIOS2_INCLUDE_DIRS ${ADIOS2_ROOT_DIR}/include)

    # check for compiled in dependencies, recomve ";" in ADIOS2_LINKFLAGS (from cmake build)
    string(REGEX REPLACE ";" " " ADIOS2_LINKFLAGS "${ADIOS2_LINKFLAGS}")
    message(STATUS "ADIOS2 linker flags (unparsed): ${ADIOS2_LINKFLAGS}")

    # find all library paths -L
    #   note: this can cause trouble if some libs are specified twice from
    #         different sources (quite unlikely)
    #         http://www.cmake.org/pipermail/cmake/2008-November/025128.html
    set(ADIOS2_LIBRARY_DIRS "")
    string(REGEX MATCHALL " -L([A-Za-z_0-9/\\.-]+)" _ADIOS2_LIBDIRS " ${ADIOS2_LINKFLAGS}")
    foreach(_LIBDIR ${_ADIOS2_LIBDIRS})
        string(REPLACE " -L" "" _LIBDIR ${_LIBDIR})
        list(APPEND ADIOS2_LIBRARY_DIRS ${_LIBDIR})
    endforeach()
    # we could append ${CMAKE_PREFIX_PATH} now but that is not really necessary

    message(STATUS "ADIOS2 DIRS to look for libs: ${ADIOS2_LIBRARY_DIRS}")

    # parse all -lname libraries and find an absolute path for them
    string(REGEX MATCHALL " -l([A-Za-z_0-9\\.\\-\\+]+)" _ADIOS2_LIBS " ${ADIOS2_LINKFLAGS}")
    foreach(_LIB ${_ADIOS2_LIBS})
        string(REPLACE " -l" "" _LIB ${_LIB})

        # find static lib: absolute path in -L then default
        find_library(_LIB_DIR NAMES ${_LIB} PATHS ${ADIOS2_LIBRARY_DIRS} CMAKE_FIND_ROOT_PATH_BOTH)

        # found?
        if(_LIB_DIR)
            message(STATUS "Found ${_LIB} in ${_LIB_DIR}")
            list(APPEND ADIOS2_LIBRARIES "${_LIB_DIR}")
        else(_LIB_DIR)
            set(ADIOS2_FOUND FALSE)
            message(STATUS "ADIOS2: Could NOT find library '${_LIB}'")
        endif(_LIB_DIR)

        # clean cached var
        unset(_LIB_DIR CACHE)
        unset(_LIB_DIR)
    endforeach()

    #add libraries which are already using cmake format
    string(REGEX MATCHALL "/([A-Za-z_0-9/\\.\\-\\+]+)\\.([a|so]+)" _ADIOS2_LIBS_SUB "${ADIOS2_LINKFLAGS}")
    foreach(foo ${_ADIOS2_LIBS_SUB})
    if (EXISTS ${foo})
        message("Appending: ${foo}")
        list(APPEND ADIOS2_LIBRARIES "${foo}")
    endif()
    endforeach(foo)

    # add the version string
    execute_process(COMMAND ${ADIOS2_CONFIG} --version
                    OUTPUT_VARIABLE ADIOS2_VERSION
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    
endif(ADIOS2_FOUND)

# unset checked variables if not found
if(NOT ADIOS2_FOUND)
    unset(ADIOS2_INCLUDE_DIRS)
    unset(ADIOS2_LIBRARIES)
endif(NOT ADIOS2_FOUND)


# restore CMAKE_FIND_LIBRARY_SUFFIXES if manipulated by this module ###########
if(ADIOS2_USE_STATIC_LIBS)
    set(CMAKE_FIND_LIBRARY_SUFFIXES ${_ORIG_CMAKE_FIND_LIBRARY_SUFFIXES})
endif()


###############################################################################
# FindPackage Options
###############################################################################

# handles the REQUIRED, QUIET and version-related arguments for find_package
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ADIOS2
    REQUIRED_VARS ADIOS2_LIBRARIES ADIOS2_INCLUDE_DIRS
    VERSION_VAR ADIOS2_VERSION
)
