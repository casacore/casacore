#
# CASA coordinates
#

add_library (casa_coordinates
Coordinates/Coordinate.cc
Coordinates/CoordinateSystem.cc
Coordinates/CoordinateUtil.cc
Coordinates/Direction2Coordinate.cc
Coordinates/DirectionCoordinate.cc
Coordinates/FITSCoordinateUtil.cc
Coordinates/GaussianConvert.cc
Coordinates/LinearCoordinate.cc
Coordinates/LinearXform2.cc
Coordinates/LinearXform.cc
Coordinates/ObsInfo.cc
Coordinates/Projection.cc
Coordinates/Spectral2Coordinate.cc
Coordinates/SpectralCoordinate.cc
Coordinates/QualityCoordinate.cc
Coordinates/StokesCoordinate.cc
Coordinates/TabularCoordinate.cc
)

target_link_libraries (casa_coordinates casa_fits ${WCSLIB_LIBRARIES})

install (TARGETS casa_coordinates
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
Coordinates/DirectionCoordinate.h
Coordinates/LinearXform.h
Coordinates/SpectralCoordinate.h
Coordinates/Coordinate.h
Coordinates/StokesCoordinate.h
Coordinates/QualityCoordinate.h
Coordinates/FITSCoordinateUtil.h
Coordinates/CoordinateSystem.h
Coordinates/FrequencyAligner.h
Coordinates/FrequencyAligner.tcc
Coordinates/Projection.h
Coordinates/ObsInfo.h
Coordinates/CoordinateUtil.h
Coordinates/GaussianConvert.h
Coordinates/LinearCoordinate.h
Coordinates/TabularCoordinate.h
DESTINATION include/casacore/coordinates/Coordinates
)

install (FILES
Coordinates.h
DESTINATION include/casacore/coordinates
)

if (BUILD_TESTING)
    add_subdirectory (Coordinates/test)
endif (BUILD_TESTING)

