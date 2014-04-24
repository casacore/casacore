#
# CASA meas
#

add_library (casa_meas
MeasUDF/PositionEngine.cc
MeasUDF/PositionUDF.cc
MeasUDF/EpochEngine.cc
MeasUDF/EpochUDF.cc
MeasUDF/DirectionUDF.cc
MeasUDF/DirectionEngine.cc
MeasUDF/Register.cc
)

target_link_libraries (casa_meas casa_measures)

install (TARGETS casa_meas
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
MeasUDF/PositionUDF.h
MeasUDF/PositionEngine.h
MeasUDF/EpochUDF.h
MeasUDF/EpochEngine.h
MeasUDF/DirectionUDF.h
MeasUDF/DirectionEngine.h
MeasUDF/Register.h
DESTINATION include/casacore/meas/MeasUDF
)

install (FILES
MeasUDF.h
DESTINATION include/casacore/meas
)

if (BUILD_TESTING)
    add_subdirectory (MeasUDF/test)
endif (BUILD_TESTING)
