set (tests
tBiweightStatistics
tChauvenetCriterionStatistics
tClassicalStatistics
tFitToHalfStatistics
tHingesFencesStatistics
tStatisticsAlgorithmFactory
tStatisticsTypes
tStatisticsUtilities
tStatsHistogram
tZScoreCalculator
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    add_pch_support(${test})
    target_link_libraries (${test} casa_scimath)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
