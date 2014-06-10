set (tests
 tmeas
)

foreach (test ${tests})
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
