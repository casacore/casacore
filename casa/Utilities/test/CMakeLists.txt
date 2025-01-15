set (tests
dCOWptr
dMUString
tAssert
tBinarySearch
tBitVector
tCompare
tCompositeNumber
tCopy
tCountedPtr
tCOWPtr
tDataType
tDefaultValue
tDynBuffer
tFallible
tGenSort
tLinearSearch
tPrecision
tRegex_1
tRegex2
tRegex
tSort_1
tSort
tStringDistance
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    add_pch_support(${test})
    target_link_libraries (${test} casa_casa)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
