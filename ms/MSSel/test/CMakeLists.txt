set (tests
tMSAntennaGram
tMSAntennaGram2
tMSAntennaGram3
tMSCorrGram
tMSFeedGram
tMSFieldGram
tMSScanGram
tMSSpwGram
tMSTimeGram
tMSUvDistGram
tMSSelection
)

foreach (test ${tests})
	add_executable (${test} ${test}.cc)
	target_link_libraries (${test} casa_ms)
	add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
	add_dependencies(check ${test})
endforeach (test)
