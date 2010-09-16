#
# CASA Components
#

add_library (casa_components
SpectralComponents/SpectralFit.cc
SpectralComponents/SpectralEstimate.cc
SpectralComponents/Spectral2Element.cc
SpectralComponents/SpectralList.cc
SpectralComponents/SpectralElement.cc
ComponentModels/GaussianShape.cc
ComponentModels/TwoSidedShape.cc
ComponentModels/SpectralModel.cc
ComponentModels/SpectralIndex.cc
ComponentModels/ComponentType.cc
ComponentModels/PointShape.cc
ComponentModels/SkyCompRep.cc
ComponentModels/DiskShape.cc
ComponentModels/SkyComponent.cc
ComponentModels/ComponentShape.cc
ComponentModels/FluxStandard.cc
ComponentModels/SkyCompBase.cc
ComponentModels/ComponentList.cc
ComponentModels/ConstantSpectrum.cc
ComponentModels/ComponentType2.cc
)

target_link_libraries (casa_components casa_coordinates)

install (
TARGETS casa_components
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
ComponentModels/ConstantSpectrum.h
ComponentModels/ComponentList.h
ComponentModels/ComponentShape.h
ComponentModels/SkyComponent.h
ComponentModels/GaussianShape.h
ComponentModels/PointShape.h
ComponentModels/DiskShape.h
ComponentModels/ComponentType.h
ComponentModels/SpectralIndex.h
ComponentModels/SkyCompRep.h
ComponentModels/SkyCompBase.h
ComponentModels/TwoSidedShape.h
ComponentModels/SpectralModel.h
ComponentModels/FluxStandard.h
ComponentModels/Flux.h
ComponentModels/Flux.tcc
DESTINATION include/casacore/components/ComponentModels
)

install (FILES
SpectralComponents/ProfileFit1D.h
SpectralComponents/ProfileFit1D.tcc
SpectralComponents/SpectralEstimate.h
SpectralComponents/Spectral2Estimate.tcc
SpectralComponents/SpectralElement.h
SpectralComponents/Spectral4Element.tcc
SpectralComponents/SpectralFit.h
SpectralComponents/SpectralFit2.tcc
SpectralComponents/SpectralList.h
SpectralComponents/SpectralList2.tcc
DESTINATION include/casacore/components/SpectralComponents
)

install (FILES
ComponentModels.h
SpectralComponents.h
DESTINATION include/casacore/components
)

if (BUILD_TESTING)
    add_subdirectory (ComponentModels/test)
    add_subdirectory (SpectralComponents/test)
endif (BUILD_TESTING)

