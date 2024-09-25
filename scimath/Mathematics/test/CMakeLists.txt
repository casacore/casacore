set (tests
dAutoDiff
dSparseDiff
tAutoDiff
tCombinatorics
tConvolver
tFFTServer
tFFTServer2
tGaussianBeam
tGeometry
tHistAcc
tInterpolateArray1D
tMathFunc
tMatrixMathLA
tMedianSlider
tSmooth
tSparseDiff
tStatAcc
tVanVleck
tVectorKernel
tInterpolate2D
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    add_pch_support(${test})
    target_link_libraries (${test} casa_scimath)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
