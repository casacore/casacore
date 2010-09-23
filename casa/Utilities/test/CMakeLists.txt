set (tests
dCOWptr
dMUString
tBinarySearch
tBitVector
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
tPtrHolder
tRegex_1
tRegex2
tRegex
tRegister
tSort_1
tSort
tStringDistance
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_casa)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
