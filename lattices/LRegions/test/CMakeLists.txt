set (tests
tLatticeRegion
tLCComplement
tLCConcatenation
tLCDifference
tLCEllipsoid
tLCExtension
tLCIntersection
tLCLELMask
tLCMask
tLCPagedMask
tLCPixelSet
tLCPolygon
tLCPolygon2
tLCRegion
tLCSlicer
tLCStretch
tLCUnion
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    add_pch_support(${test})
    target_link_libraries (${test} casa_lattices)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
