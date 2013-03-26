set (tests
dPointShape
dSkyCompBase
dSpectralModel
dTwoSidedShape
tComponentList
tComponentType
tConstantSpectrum
tDiskShape
tLimbDarkenedDiskShape
tFluxStandard
tFluxStandard2
tFluxStandard3
tGaussianComponent
tGaussianShape
tGaussianBeam
tPointComponent
tPointShape
tSkyCompRep
tSpectralIndex
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_components)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
