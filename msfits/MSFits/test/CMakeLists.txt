set (tests
tfits2ms
tMSConcat
tMSSelection
)

foreach (test ${tests})
    add_executable (${test} ${test})
    target_link_libraries (${test} casa_msfits)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
