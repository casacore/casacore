set (tests
tfits2ms
tMSConcat
tMSFITSInput
tMSFITSOutput
tMSFitsSelection
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_msfits)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
