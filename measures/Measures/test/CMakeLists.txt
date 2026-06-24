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
    add_pch_support(${test})
    target_link_libraries (${test} casa_measures)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)

find_package(Boost 1.73 COMPONENTS unit_test_framework)
if(Boost_FOUND)
    include_directories(${Boost_INCLUDE_DIR})

    add_executable (measurestest test.cc tCyclicPtr.cc)
    add_pch_support(measurestest)
    target_link_libraries(measurestest casa_measures ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})
    add_test (measurestest ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./measurestest)
    add_dependencies(check measurestest)
endif(Boost_FOUND)

if (NOT SOFA_FOUND)
    find_package(ERFA QUIET)
endif()

if (SOFA_FOUND)
    add_executable (tIAU2000 SofaTest.cc tIAU2000.cc)
    target_link_libraries (tIAU2000 casa_measures ${SOFA_LIBRARY})
    add_test (tIAU2000 ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./tIAU2000)
    add_dependencies(check tIAU2000)
endif (SOFA_FOUND)

if (SOFA_FOUND OR ERFA_FOUND)
    add_executable (tIAU2000App tIAU2000App.cc)
    target_link_libraries (tIAU2000App casa_measures)
    if (SOFA_FOUND)
        target_compile_definitions(tIAU2000App PRIVATE CASACORE_HAVE_SOFA)
        target_link_libraries (tIAU2000App ${SOFA_LIBRARY})
    else()
        target_compile_definitions(tIAU2000App PRIVATE CASACORE_HAVE_ERFA)
        target_include_directories(tIAU2000App PRIVATE ${ERFA_INCLUDE_DIRS})
        target_link_libraries (tIAU2000App ${ERFA_LIBRARIES})
    endif()
    add_test (tIAU2000App ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./tIAU2000App)
    add_dependencies(check tIAU2000App)
endif()
