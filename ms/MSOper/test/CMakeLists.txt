set (tests
tMSDerivedValues
tMSKeys
tMSMetaData
tMSReader
tMSSummary
)

foreach (test ${tests})
	add_executable (${test} ${test}.cc)
	target_link_libraries (${test} casa_ms)
	add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
	add_dependencies(check ${test})
endforeach (test)
