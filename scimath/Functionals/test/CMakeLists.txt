set (tests
dFunction
dGaussianND
tChebyshev
tCombiFunction
tCompoundFunction
tConstantND
tFuncExpression
tFunctionHolder
tFunctionOrder
tFunctionWrapper
tGaussian1D
tGaussian2D
tGaussian3D
tGaussianND
tHyperPlane
tInterpolate1D
tPoisson
tPolynomial
tPowerLogarithmicPolynomial
tSampledFunctional
tSimButterworthBandpass
tSinusoid1D
tSPolynomial
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    add_pch_support(${test})
    target_link_libraries (${test} casa_scimath)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
