set (tests
dAutoDiff
dSparseDiff
tAutoDiff
tClassicalStatistics
tCombinatorics
tConvolver
tFFTServer
tFFTServer2
tFitToHalfStatistics
tGaussianBeam
tGeometry
tHingesFencesStatistics
tHistAcc
tInterpolateArray1D
tMathFunc
tMatrixMathLA
tMedianSlider
tSmooth
tSparseDiff
tStatAcc
tStatisticsUtilities
tVanVleck
tVectorKernel
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_scimath)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
