set (tests
 tPositionEngine
 tEpochEngine
 tDirectionEngine
 tEarthMagneticEngine
 tFrequencyEngine
 tRadialVelocityEngine
 tDopplerEngine
 tmeas
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_meas)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
