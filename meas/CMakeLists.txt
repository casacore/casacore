#
# CASA meas
#

add_library (casa_meas
MeasUDF/BaseEngine.cc
MeasUDF/PositionEngine.cc
MeasUDF/PositionUDF.cc
MeasUDF/EpochEngine.cc
MeasUDF/EpochUDF.cc
MeasUDF/DirectionUDF.cc
MeasUDF/DirectionEngine.cc
MeasUDF/EarthMagneticUDF.cc
MeasUDF/EarthMagneticEngine.cc
MeasUDF/FrequencyUDF.cc
MeasUDF/FrequencyEngine.cc
MeasUDF/RadialVelocityUDF.cc
MeasUDF/RadialVelocityEngine.cc
MeasUDF/DopplerUDF.cc
MeasUDF/DopplerEngine.cc
MeasUDF/Register.cc
)

set (top_level_headers
MeasUDF.h
)

init_pch_support(casa_meas ${top_level_headers})

target_link_libraries (casa_meas casa_measures)

install (TARGETS casa_meas
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
MeasUDF/BaseEngine.h
MeasUDF/MeasEngine.h
MeasUDF/MeasEngine.tcc
MeasUDF/PositionUDF.h
MeasUDF/PositionEngine.h
MeasUDF/EpochUDF.h
MeasUDF/EpochEngine.h
MeasUDF/DirectionUDF.h
MeasUDF/DirectionEngine.h
MeasUDF/EarthMagneticUDF.h
MeasUDF/EarthMagneticEngine.h
MeasUDF/FrequencyUDF.h
MeasUDF/FrequencyEngine.h
MeasUDF/RadialVelocityUDF.h
MeasUDF/RadialVelocityEngine.h
MeasUDF/DopplerUDF.h
MeasUDF/DopplerEngine.h
MeasUDF/Register.h
DESTINATION include/casacore/meas/MeasUDF
)

install (FILES ${top_level_headers}
DESTINATION include/casacore/meas
)

add_subdirectory (MeasUDF/test ${EXCL_ALL})
