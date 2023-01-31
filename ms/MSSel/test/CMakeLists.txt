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

# Only test scripts, no test programs.
set (testscripts
tMSSelection2
)

foreach (test ${tests})
	add_executable (${test} ${test}.cc)
	add_pch_support(${test})
	target_link_libraries (${test} casa_ms)
	add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
	add_dependencies(check ${test})
endforeach (test)

foreach (test ${testscripts})
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
