#
# CASA measures
#

# Check whether DATA_DIR contains data (only if measures is build)
get_filename_component(ABS_DATA_DIR "${DATA_DIR}" ABSOLUTE)
if (NOT EXISTS "${ABS_DATA_DIR}/ephemerides/")
    message(WARNING "No ephemerides data found in the specified DATA_DIR (\"${DATA_DIR}\"), so the location must be specified at runtime. If you have preinstalled casacore data, specify the location with -DDATA_DIR.")
endif()

# If given, add the data directory as compile variable.
# If it contains a single $, it needs to be doubled for make purposes.
if (DATA_DIR)
    string(REGEX REPLACE [$] $$ DATA_DIRX ${DATA_DIR})
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DCASADATA='\"${DATA_DIRX}\"'")
endif (DATA_DIR)

add_library (casa_measures
Measures/Aberration.cc
Measures/EarthField.cc
Measures/EarthMagneticMachine.cc
Measures/MBaseline.cc
Measures/MCBase.cc
Measures/MCBaseline.cc
Measures/MCDirection.cc
Measures/MCDoppler.cc
Measures/MCEarthMagnetic.cc
Measures/MCEpoch.cc
Measures/MCFrame.cc
Measures/MCFrequency.cc
Measures/MConvertBase.cc
Measures/MCPosition.cc
Measures/MCRadialVelocity.cc
Measures/MCuvw.cc
Measures/MDirection.cc
Measures/MDoppler.cc
Measures/MEarthMagnetic.cc
Measures/MeasComet.cc
Measures/MeasData.cc
Measures/MeasFrame.cc
Measures/MeasIERS.cc
Measures/MeasJPL.cc
Measures/MeasMath.cc
Measures/MeasTable.cc
Measures/MeasTableMul.cc
Measures/Measure.cc
Measures/MeasureHolder.cc
Measures/MeasuresProxy.cc
Measures/MEpoch.cc
Measures/MFrequency.cc
Measures/MPosition.cc
Measures/MRadialVelocity.cc
Measures/MRBase.cc
Measures/Muvw.cc
Measures/Nutation.cc
Measures/ParAngleMachine.cc
Measures/Precession.cc
Measures/Quality.cc
Measures/SolarPos.cc
Measures/Stokes.cc
Measures/UVWMachine.cc
Measures/VelocityMachine.cc
TableMeasures/TableMeasColumn.cc
TableMeasures/TableMeasDescBase.cc
TableMeasures/TableMeasOffsetDesc.cc
TableMeasures/TableMeasRefDesc.cc
TableMeasures/TableMeasType.cc
TableMeasures/TableMeasValueDesc.cc
TableMeasures/TableQuantumDesc.cc
)

target_link_libraries (casa_measures casa_tables casa_scimath)

add_subdirectory (apps)

install (
TARGETS casa_measures
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
Measures/Aberration.h
Measures/EarthField.h
Measures/EarthMagneticMachine.h
Measures/MBaseline.h
Measures/MCBase.h
Measures/MCBaseline.h
Measures/MCDirection.h
Measures/MCDoppler.h
Measures/MCEarthMagnetic.h
Measures/MCEpoch.h
Measures/MCFrame.h
Measures/MCFrequency.h
Measures/MConvertBase.h
Measures/MCPosition.h
Measures/MCRadialVelocity.h
Measures/MCuvw.h
Measures/MDirection.h
Measures/MDoppler.h
Measures/MEarthMagnetic.h
Measures/MeasBase.h
Measures/MeasBase.tcc
Measures/MeasComet.h
Measures/MeasConvert.h
Measures/MeasConvert.tcc
Measures/MeasData.h
Measures/MeasFrame.h
Measures/MeasIERS.h
Measures/MeasJPL.h
Measures/MeasMath.h
Measures/MeasRef.h
Measures/MeasRef.tcc
Measures/MeasTable.h
Measures/MeasTableMul.h
Measures/Measure.h
Measures/MeasureHolder.h
Measures/MeasuresProxy.h
Measures/MEpoch.h
Measures/MFrequency.h
Measures/MPosition.h
Measures/MRadialVelocity.h
Measures/MRBase.h
Measures/Muvw.h
Measures/Nutation.h
Measures/ParAngleMachine.h
Measures/Precession.h
Measures/Quality.h
Measures/SolarPos.h
Measures/Stokes.h
Measures/UVWMachine.h
Measures/VelocityMachine.h
DESTINATION include/casacore/measures/Measures
)

install (FILES
TableMeasures/ArrayMeasColumn.h
TableMeasures/ArrayMeasColumn.tcc
TableMeasures/ArrayQuantColumn.h
TableMeasures/ArrayQuantColumn.tcc
TableMeasures/ScalarMeasColumn.h
TableMeasures/ScalarMeasColumn.tcc
TableMeasures/ScalarQuantColumn.h
TableMeasures/ScalarQuantColumn.tcc
TableMeasures/TableMeasColumn.h
TableMeasures/TableMeasDescBase.h
TableMeasures/TableMeasDesc.h
TableMeasures/TableMeasDesc.tcc
TableMeasures/TableMeasOffsetDesc.h
TableMeasures/TableMeasRefDesc.h
TableMeasures/TableMeasType.h
TableMeasures/TableMeasValueDesc.h
TableMeasures/TableQuantumDesc.h
DESTINATION include/casacore/measures/TableMeasures
)

install (FILES
Measures.h
TableMeasures.h
DESTINATION include/casacore/measures
)

if (BUILD_TESTING)
    add_subdirectory (Measures/test)
    add_subdirectory (TableMeasures/test)
endif (BUILD_TESTING)

