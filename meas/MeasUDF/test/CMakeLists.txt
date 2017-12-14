set (tests
 tmeas
)

add_executable (azel azel.cpp)
target_link_libraries (azel casa_tables)
install(TARGETS azel DESTINATION bin)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_meas)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)

add_test (tAzel ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./tAzel)
