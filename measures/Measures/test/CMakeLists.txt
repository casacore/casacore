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
tMeasIERS
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

if (SOFA_FOUND)
    add_executable (tIAU2000 SofaTest.cc tIAU2000.cc)
    target_link_libraries (tIAU2000 casa_measures ${SOFA_LIBRARY})
    add_test (tIAU2000 ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./tIAU2000)
endif (SOFA_FOUND)
