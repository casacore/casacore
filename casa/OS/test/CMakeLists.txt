set (tests
tCanonicalConversion
tConversion
tConversionPerf
tDataConversion
tDirectory
tDirectoryIterator
tEnvVar
tFile
tHostInfo
tIBMConversion
tLECanonicalConversion
tMemoryTrace
tModcompConversion
tPath
tPrecTimer
tRegularFile
tSymLink
tTime
tTimer
tVAXConversion
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    add_pch_support(${test})
    target_link_libraries (${test} casa_casa)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
