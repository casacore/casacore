#
# casacore python bindings
#
include_directories (${Boost_INCLUDE_DIRS} ${NUMPY_INCLUDE_DIRS} ${PYTHON_INCLUDE_DIRS})

add_library (casa_python
Converters/PycArray.cc
Converters/PycArrayNP.cc
Converters/PycBasicData.cc
Converters/PycExcp.cc
Converters/PycImport.cc
Converters/PycRecord.cc
Converters/PycValueHolder.cc
)

target_link_libraries (casa_python casa_casa ${Boost_PYTHON_LIBRARY_RELEASE} ${PYTHON_LIBRARIES})

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
