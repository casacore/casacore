set (tests
 tConvert
)
include_directories ("..")
foreach (test ${tests})
    add_library (${test}  ${test}.cc)
    SET_TARGET_PROPERTIES(${test} PROPERTIES PREFIX "_") 
    target_link_libraries (${test} casa_python ${Boost_PYTHON_LIBRARY_DEBUG} ${PYTHON_LIBRARIES})
    configure_file (${CMAKE_CURRENT_SOURCE_DIR}/${test}.py ${CMAKE_CURRENT_BINARY_DIR}/${test}.py COPYONLY)    
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
