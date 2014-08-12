set (tests
dM1950_2000
dMeasure
tEarthField
tEarthMagneticMachine
tMBaseline
tMDirection
tMEarthMagnetic
tMFrequency
tMeasComet
tMeasJPL
tMeasMath
tMeasure
tMeasureHolder
tMuvw
tParAngleMachine
tQuality
tRecordTransformable
tStokes
tUVWMachine
tVelocityMachine
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_measures)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)

foreach (test tIAU2000)
    add_executable (tIAU2000 tIAU2000.cc)
    target_link_libraries (tIAU2000 casa_measures sofa)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
