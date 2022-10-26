set (tests
dCoordinates
dRemoveAxes
dWorldMap
tCoordinate
tCoordinateSystem
tCoordinateUtil
tDirectionCoordinate
tFrequencyAligner
tGaussianConvert
tLinearCoordinate
tLinearXform
tObsInfo
tProjection
tSpectralCoordinate
tStokesCoordinate
tQualityCoordinate
tTabularCoordinate
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    add_pch_support(${test})
    target_link_libraries (${test} casa_coordinates)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
