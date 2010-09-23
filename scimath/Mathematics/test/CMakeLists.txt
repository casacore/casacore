set (tests
dAutoDiff
dSparseDiff
tAutoDiff
tConvolver
tFFTServer
tHistAcc
tMathFunc
tMatrixMathLA
tMedianSlider
tSparseDiff
tStatAcc
tVanVleck
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_scimath)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
