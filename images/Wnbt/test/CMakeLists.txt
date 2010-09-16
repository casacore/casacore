set (tests
tComponentUpdate
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_images)
    add_test (NAME ${test} COMMAND ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
