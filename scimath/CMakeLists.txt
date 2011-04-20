#
# CASA Scimath
#

add_library (casa_scimath
Fitting/LSQFit.cc
Fitting/LSQFit3.cc
Fitting/LSQMatrix2.cc
Fitting/FittingProxy.cc
Fitting/LSQMatrix.cc
Functionals/FuncExpression.cc
Functionals/FuncExprData.cc
Functionals/FunctionFactoryErrors.cc
Functionals/FunctionalProxy.cc
Functionals/SerialHelper.cc
Mathematics/Combinatorics.cc
Mathematics/MathFunc2.cc
Mathematics/Interpolate2D.cc
Mathematics/FFTPack.cc
Mathematics/FFTW.cc
Mathematics/FFTServer.cc
Mathematics/NNLSMatrixSolver.cc
Mathematics/MatrixSolver.cc
Mathematics/SquareMatrix2.cc
Mathematics/SCSL.cc
Mathematics/NumericTraits.cc
Mathematics/RigidVector2.cc
Mathematics/MedianSlider.cc
Mathematics/VectorKernel.cc
Mathematics/VanVleck.cc
)

if (FFTW3_FOUND)
    target_link_libraries (casa_scimath casa_scimath_f ${FFTW3_LIBRARIES})
else (FFTW3_FOUND)
    target_link_libraries (casa_scimath casa_scimath_f)
endif (FFTW3_FOUND)

install (TARGETS casa_scimath
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_WRITE GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
Fitting/FitGaussian.h
Fitting/FitGaussian.tcc
Fitting/FittingProxy.h
Fitting/GenericL2Fit.h
Fitting/GenericL2Fit.tcc
Fitting/LSQFit.h
Fitting/LSQFit2.tcc
Fitting/LSQMatrix.h
Fitting/LSQTraits.h
Fitting/LSQaips.h
Fitting/LSQaips.tcc
Fitting/LinearFit.h
Fitting/LinearFit.tcc
Fitting/LinearFitSVD.h
Fitting/LinearFitSVD.tcc
Fitting/NonLinearFit.h
Fitting/NonLinearFit.tcc
Fitting/NonLinearFitLM.h
Fitting/NonLinearFitLM.tcc
DESTINATION include/casacore/scimath/Fitting
)

install (FILES
Functionals/AbstractFunctionFactory.h
Functionals/ArraySampledFunctional.h
Functionals/ArraySampledFunctional.tcc
Functionals/Chebyshev.h
Functionals/Chebyshev.tcc
Functionals/ChebyshevParam.h
Functionals/ChebyshevParam.tcc
Functionals/Combi2Function.tcc
Functionals/CombiFunction.h
Functionals/CombiFunction.tcc
Functionals/CombiParam.h
Functionals/CombiParam.tcc
Functionals/CompiledFunction.h
Functionals/CompiledFunction.tcc
Functionals/CompiledParam.h
Functionals/CompiledParam.tcc
Functionals/Compound2Function.tcc
Functionals/CompoundFunction.h
Functionals/CompoundFunction.tcc
Functionals/CompoundParam.h
Functionals/CompoundParam.tcc
Functionals/DiracDFunction.h
Functionals/DiracDFunction.tcc
Functionals/DiracDParam.h
Functionals/DiracDParam.tcc
Functionals/EclecticFunctionFactory.h
Functionals/EclecticFunctionFactory.tcc
Functionals/EvenPolynomial.h
Functionals/EvenPolynomial.tcc
Functionals/EvenPolynomial2.tcc
Functionals/EvenPolynomialParam.h
Functionals/EvenPolynomialParam.tcc
Functionals/FuncExprData.h
Functionals/FuncExpression.h
Functionals/Function.h
Functionals/Function.tcc
Functionals/Function1D.h
Functionals/FunctionFactoryErrors.h
Functionals/FunctionHolder.h
Functionals/FunctionHolder.tcc
Functionals/FunctionMarshallable.h
Functionals/FunctionOrder.h
Functionals/FunctionOrder.tcc
Functionals/FunctionParam.h
Functionals/FunctionParam.tcc
Functionals/FunctionTraits.h
Functionals/FunctionWrapper.h
Functionals/FunctionWrapper.tcc
Functionals/FunctionalProxy.h
Functionals/GNoiseFunction.h
Functionals/GNoiseFunction.tcc
Functionals/GNoiseParam.h
Functionals/GNoiseParam.tcc
Functionals/Gaussian1D.h
Functionals/Gaussian1D.tcc
Functionals/Gaussian1D2.tcc
Functionals/Gaussian1DParam.h
Functionals/Gaussian1DParam.tcc
Functionals/Gaussian2D.h
Functionals/Gaussian2D.tcc
Functionals/Gaussian2D2.tcc
Functionals/Gaussian2DParam.h
Functionals/Gaussian2DParam.tcc
Functionals/Gaussian3D.h
Functionals/Gaussian3D.tcc
Functionals/Gaussian3D2.tcc
Functionals/Gaussian3DParam.h
Functionals/Gaussian3DParam.tcc
Functionals/GaussianND.h
Functionals/GaussianND.tcc
Functionals/GaussianNDParam.h
Functionals/GaussianNDParam.tcc
Functionals/HyperPlane.h
Functionals/HyperPlane.tcc
Functionals/HyperPlane2.tcc
Functionals/HyperPlaneParam.h
Functionals/HyperPlaneParam.tcc
Functionals/Interpolate1D.h
Functionals/Interpolate1D.tcc
Functionals/KaiserBFunction.h
Functionals/KaiserBFunction.tcc
Functionals/KaiserBParam.h
Functionals/KaiserBParam.tcc
Functionals/MarshButterworthBandpass.h
Functionals/MarshButterworthBandpass.tcc
Functionals/MarshallableChebyshev.h
Functionals/MarshallableChebyshev.tcc
Functionals/OddPolynomial.h
Functionals/OddPolynomial.tcc
Functionals/OddPolynomial2.tcc
Functionals/OddPolynomialParam.h
Functionals/OddPolynomialParam.tcc
Functionals/Polynomial.h
Functionals/Polynomial.tcc
Functionals/Polynomial2.tcc
Functionals/PolynomialParam.h
Functionals/PolynomialParam.tcc
Functionals/SPolynomial.h
Functionals/SPolynomial.tcc
Functionals/SPolynomialParam.h
Functionals/SPolynomialParam.tcc
Functionals/SampledFunctional.h
Functionals/ScalarSampledFunctional.h
Functionals/ScalarSampledFunctional.tcc
Functionals/SerialHelper.h
Functionals/SimButterworthBandpass.h
Functionals/SimButterworthBandpass.tcc
Functionals/SincFunction.h
Functionals/SincFunction.tcc
Functionals/SincParam.h
Functionals/SincParam.tcc
Functionals/Sinusoid1D.h
Functionals/Sinusoid1D.tcc
Functionals/Sinusoid1D2.tcc
Functionals/Sinusoid1DParam.h
Functionals/Sinusoid1DParam.tcc
Functionals/SpecificFunctionFactory.h
Functionals/UnaryFunction.h
Functionals/UnaryFunction.tcc
Functionals/UnaryParam.h
Functionals/UnaryParam.tcc
Functionals/WrapperBase.h
Functionals/WrapperData.h
Functionals/WrapperParam.h
Functionals/WrapperParam.tcc
DESTINATION include/casacore/scimath/Functionals
)

install (FILES
Mathematics/AutoDiff.h
Mathematics/AutoDiff.tcc
Mathematics/AutoDiffA.h
Mathematics/AutoDiffIO.h
Mathematics/AutoDiffIO.tcc
Mathematics/AutoDiffMath.h
Mathematics/AutoDiffMath.tcc
Mathematics/AutoDiffRep.h
Mathematics/AutoDiffRep.tcc
Mathematics/AutoDiffX.h
Mathematics/Combinatorics.h
Mathematics/ConvolveGridder.h
Mathematics/ConvolveGridder.tcc
Mathematics/Convolver.h
Mathematics/Convolver.tcc
Mathematics/DFTServer.h
Mathematics/DFTServer.tcc
Mathematics/FFTPack.h
Mathematics/FFTServer.h
Mathematics/FFTServer.tcc
Mathematics/FFTW.h
Mathematics/Gridder.h
Mathematics/Gridder.tcc
Mathematics/HistAcc.h
Mathematics/HistAcc.tcc
Mathematics/Interpolate2D.h
Mathematics/Interpolate2D2.tcc
Mathematics/InterpolateArray1D.h
Mathematics/InterpolateArray1D.tcc
Mathematics/MathFunc.h
Mathematics/MathFunc.tcc
Mathematics/MatrixMathLA.h
Mathematics/MatrixMathLA.tcc
Mathematics/MatrixSolver.h
Mathematics/MedianSlider.h
Mathematics/NNGridder.h
Mathematics/NNGridder.tcc
Mathematics/NNLSMatrixSolver.h
Mathematics/NumericTraits.h
Mathematics/NumericTraits2.h
Mathematics/RigidVector.h
Mathematics/RigidVector.tcc
Mathematics/SCSL.h
Mathematics/Smooth.h
Mathematics/Smooth.tcc
Mathematics/SparseDiff.h
Mathematics/SparseDiff.tcc
Mathematics/SparseDiffA.h
Mathematics/SparseDiffIO.h
Mathematics/SparseDiffIO.tcc
Mathematics/SparseDiffMath.h
Mathematics/SparseDiffMath.tcc
Mathematics/SparseDiffRep.h
Mathematics/SparseDiffRep.tcc
Mathematics/SparseDiffX.h
Mathematics/SquareMatrix.h
Mathematics/SquareMatrix.tcc
Mathematics/StatAcc.h
Mathematics/StatAcc.tcc
Mathematics/VanVleck.h
Mathematics/VectorKernel.h
DESTINATION include/casacore/scimath/Mathematics
)

install (FILES
Fitting.h
Functionals.h
Mathematics.h
DESTINATION include/casacore/scimath
)

if (BUILD_TESTING)
    add_subdirectory (Fitting/test)
    add_subdirectory (Functionals/test)
    add_subdirectory (Mathematics/test)
endif (BUILD_TESTING)

