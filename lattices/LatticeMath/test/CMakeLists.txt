set (tests
tFit2D
tLatticeAddNoise
tLatticeApply
tLatticeApply2
tLatticeConvolver
tLatticeFFT
tLatticeFit
tLatticeFractile
tLatticeHistograms
tLatticeMathUtil
tLatticeSlice1D
tLatticeStatistics
tLatticeStatsDataProvider
tLatticeTwoPtCorr
tLattStatsSpecialize
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_lattices)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
