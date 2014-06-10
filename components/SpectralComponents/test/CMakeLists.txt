set (tests
tGaussianMultipletSpectralElement
tLogTransformedPolynomialSpectralElement
tLorentzianSpectralElement
tPowerLogPolynomialSpectralElement
tProfileFit1D
tSpectralFit
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_components)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
