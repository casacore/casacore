set (tests
tCanonicalConversion
tConversion
tDataConversion
tDirectory
tDirectoryIterator
tEnvVar
tFile
tHostInfo
tIBMConversion
tLECanonicalConversion
tMemory
tModcompConversion
tMutex
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
    target_link_libraries (${test} casa_casa)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
