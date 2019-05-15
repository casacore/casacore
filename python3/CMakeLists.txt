message(STATUS "Looking for python3 specific environment...")

# tempororarly set variables used by findpython
if (PYTHON3_EXECUTABLE)
    set(PYTHON_EXECUTABLE ${PYTHON3_EXECUTABLE} CACHE FILEPATH "")
endif()

if (PYTHON3_LIBRARY)
    set(PYTHON_LIBRARY ${PYTHON3_LIBRARY} CACHE FILEPATH "")
endif()

if (PYTHON3_INCLUDE_DIR)
set(PYTHON_INCLUDE_DIR ${PYTHON3_INCLUDE_DIR} CACHE FILEPATH "")
endif()

if (PYTHON3_FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs)
    set(FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs ${PYTHON3_FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs} CACHE FILEPATH "")
endif()

# Detect the python properties
set(Python_ADDITIONAL_VERSIONS 3.7 3.6 3.5 3.4)

find_package(Python REQUIRED)

if (PYTHONINTERP_FOUND)
    find_package(Boost REQUIRED)
    if (${Boost_MAJOR_VERSION} STREQUAL 1 AND ${Boost_MINOR_VERSION} STRGREATER 66)
        # Boost>=1.67 Python components require a Python version suffix
	set(BOOST_PYTHON3_LIBRARY_NAME
	    python${PYTHON_VERSION_MAJOR}${PYTHON_VERSION_MINOR}
	    CACHE
	    STR
	    "The name of the boost python3 library to search for")
    else ()
        set(BOOST_PYTHON3_LIBRARY_NAME
	    python${PYTHON_VERSION_MAJOR}
	    CACHE                                                       
	    STR                                                    
	    "The name of the boost python library to search for")       
    endif ()
    find_package(Boost REQUIRED COMPONENTS ${BOOST_PYTHON3_LIBRARY_NAME})

    find_package (NUMPY REQUIRED)

    # copy the variables to their final destination
    set(PYTHON3_EXECUTABLE ${PYTHON_EXECUTABLE} CACHE FILEPATH "Path to Python3 interpreter")
    set(PYTHON3_LIBRARY ${PYTHON_LIBRARY} CACHE PATH "Python3 library")
    set(PYTHON3_INCLUDE_DIR ${PYTHON_INCLUDE_DIR} CACHE PATH "Python3 include folder")
    set(PYTHON3_FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs ${FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs} CACHE STRING "")

    set(PYTHON3_LIBRARIES ${PYTHON_LIBRARIES} PARENT_SCOPE)
    set(PYTHON3_NUMPY_INCLUDE_DIRS ${NUMPY_INCLUDE_DIRS} PARENT_SCOPE)
    set(PYTHON3_Boost_LIBRARIES ${Boost_LIBRARIES} PARENT_SCOPE)
    set(PYTHON3_Boost_INCLUDE_DIRS ${Boost_INCLUDE_DIRS} PARENT_SCOPE)
    set(PYTHON3_Boost_PYTHON_LIBRARY_RELEASE ${PYTHON3_Boost_LIBRARIES} PARENT_SCOPE)
    set(PYTHON3_INCLUDE_DIRS ${PYTHON_INCLUDE_DIRS} PARENT_SCOPE)

    # to access the variables here we also need to set them in the local scope
    set(PYTHON3_LIBRARIES ${PYTHON_LIBRARIES} )
    set(PYTHON3_NUMPY_INCLUDE_DIRS ${NUMPY_INCLUDE_DIRS} )
    set(PYTHON3_Boost_LIBRARIES ${Boost_LIBRARIES} )
    set(PYTHON3_Boost_INCLUDE_DIRS ${Boost_INCLUDE_DIRS} )
    set(PYTHON3_Boost_PYTHON_LIBRARY_RELEASE ${Boost_PYTHON_LIBRARY_RELEASE} )
    set(PYTHON3_INCLUDE_DIRS ${PYTHON_INCLUDE_DIRS} )

    # Remove cached variable to not confuse user
    unset(PYTHON_EXECUTABLE CACHE)
    unset(PYTHON_LIBRARY CACHE)
    unset(PYTHON_INCLUDE_DIR CACHE)
    unset(FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs CACHE)
endif(PYTHONINTERP_FOUND)



include_directories (${PYTHON3_Boost_INCLUDE_DIRS} ${PYTHON3_NUMPY_INCLUDE_DIRS} ${PYTHON3_INCLUDE_DIRS})

add_library (casa_python3
../python/Converters/PycArray.cc
../python/Converters/PycArrayNP.cc
../python/Converters/PycBasicData.cc
../python/Converters/PycExcp.cc
../python/Converters/PycImport.cc
../python/Converters/PycRecord.cc
../python/Converters/PycValueHolder.cc
)


target_link_libraries (casa_python3 casa_casa ${PYTHON3_Boost_LIBRARIES} ${PYTHON3_LIBRARIES})

install (TARGETS casa_python3
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

# don't install headers if python2 cmake logic already installs it
if (NOT BUILD_PYTHON)
    install (FILES
    ../python/Converters/PycArray.h
    ../python/Converters/PycArrayComCC.h
    ../python/Converters/PycArrayComH.h
    ../python/Converters/PycArrayNP.h
    ../python/Converters/PycBasicData.h
    ../python/Converters/PycExcp.h
    ../python/Converters/PycRecord.h
    ../python/Converters/PycValueHolder.h
    ../python/Converters/PycArray.tcc
    DESTINATION include/casacore/python/Converters
    )

    install (FILES
    ../python/Converters.h
    DESTINATION include/casacore/python
    )
endif (NOT BUILD_PYTHON)

