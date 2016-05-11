message(STATUS "Looking for python2 specific environment...")

# tempororarly set variables used by findpython
if (PYTHON2_EXECUTABLE)
    set(PYTHON_EXECUTABLE ${PYTHON2_EXECUTABLE} CACHE FILEPATH "")
endif()

if (PYTHON2_LIBRARY)
    set(PYTHON_LIBRARY ${PYTHON2_LIBRARY} CACHE FILEPATH "")
endif()

if (PYTHON2_INCLUDE_DIR)
set(PYTHON_INCLUDE_DIR ${PYTHON2_INCLUDE_DIR} CACHE FILEPATH "")
endif()

if (PYTHON2_FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs)
    set(FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs ${PYTHON2_FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs} CACHE FILEPATH "")
endif()

# Detect the python properties
set(Python_FIND_VERSION 2)
set(PythonInterp_FIND_VERSION_MAJOR 2)
find_package(Python REQUIRED)
if (PYTHONINTERP_FOUND)
    find_package(Boost REQUIRED COMPONENTS python)
    find_package (NUMPY REQUIRED)

    # copy the variables to their final destination
    set(PYTHON2_EXECUTABLE ${PYTHON_EXECUTABLE} CACHE FILEPATH "Path to Python2 interpreter")
    set(PYTHON2_LIBRARY ${PYTHON_LIBRARY} CACHE PATH "Python2 library")
    set(PYTHON2_INCLUDE_DIR ${PYTHON_INCLUDE_DIR} CACHE PATH "Python2 include folder")
    set(PYTHON2_FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs ${FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs} CACHE STRING "")

    set(PYTHON2_LIBRARIES ${PYTHON_LIBRARIES} PARENT_SCOPE)
    set(PYTHON2_NUMPY_INCLUDE_DIRS ${NUMPY_INCLUDE_DIRS} PARENT_SCOPE)
    set(PYTHON2_Boost_LIBRARIES ${Boost_LIBRARIES} PARENT_SCOPE)
    set(PYTHON2_Boost_INCLUDE_DIRS ${Boost_INCLUDE_DIRS} PARENT_SCOPE)
    set(PYTHON2_Boost_PYTHON_LIBRARY_RELEASE ${Boost_PYTHON_LIBRARY_RELEASE} PARENT_SCOPE)
    set(PYTHON2_INCLUDE_DIRS ${PYTHON_INCLUDE_DIRS} PARENT_SCOPE)

    # to access the variables here we also need to set them in the local scope
    set(PYTHON2_LIBRARIES ${PYTHON_LIBRARIES} )
    set(PYTHON2_NUMPY_INCLUDE_DIRS ${NUMPY_INCLUDE_DIRS} )
    set(PYTHON2_Boost_LIBRARIES ${Boost_LIBRARIES} )
    set(PYTHON2_Boost_INCLUDE_DIRS ${Boost_INCLUDE_DIRS} )
    set(PYTHON2_Boost_PYTHON_LIBRARY_RELEASE ${Boost_PYTHON_LIBRARY_RELEASE} )
    set(PYTHON2_INCLUDE_DIRS ${PYTHON_INCLUDE_DIRS} )

    # Remove cached variable to not confuse user
    unset(PYTHON_EXECUTABLE CACHE)
    unset(PYTHON_LIBRARY CACHE)
    unset(PYTHON_INCLUDE_DIR CACHE)
    unset(FIND_PACKAGE_MESSAGE_DETAILS_PythonLibs CACHE)
endif(PYTHONINTERP_FOUND)


include_directories (${PYTHON2_Boost_INCLUDE_DIRS} ${PYTHON2_NUMPY_INCLUDE_DIRS} ${PYTHON2_INCLUDE_DIRS})

add_library (casa_python
Converters/PycArray.cc
Converters/PycArrayNP.cc
Converters/PycBasicData.cc
Converters/PycExcp.cc
Converters/PycImport.cc
Converters/PycRecord.cc
Converters/PycValueHolder.cc
)

target_link_libraries (casa_python casa_casa ${PYTHON2_Boost_LIBRARIES} ${PYTHON2_LIBRARIES})

install (TARGETS casa_python
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
Converters/PycArray.h
Converters/PycArrayComCC.h
Converters/PycArrayComH.h
Converters/PycArrayNP.h
Converters/PycBasicData.h
Converters/PycExcp.h
Converters/PycRecord.h
Converters/PycValueHolder.h
Converters/PycArray.tcc
DESTINATION include/casacore/python/Converters
)

install (FILES
Converters.h
DESTINATION include/casacore/python
)

if (BUILD_TESTING)
    add_subdirectory (Converters/test)
endif (BUILD_TESTING)
