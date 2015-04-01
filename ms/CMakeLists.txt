#
# CASA MeasurementSets 
#

set (
parser_inputs
MSAntennaGram
MSArrayGram
MSCorrGram
MSFieldGram
MSObservationGram
MSScanGram
MSSpwGram
MSTimeGram
MSUvDistGram
MSStateGram
)

foreach (src ${parser_inputs})
    BISON_TARGET (${src} MSSel/${src}.yy ${CMAKE_CURRENT_BINARY_DIR}/${src}.ycc COMPILE_FLAGS "-y -p ${src} --output-file=${src}.ycc")
    FLEX_TARGET (${src} MSSel/${src}.ll ${CMAKE_CURRENT_BINARY_DIR}/${src}.lcc COMPILE_FLAGS "-P${src}")
endforeach (src)

include_directories (${CMAKE_CURRENT_BINARY_DIR})

add_library (casa_ms
MeasurementSets/MSFreqOffset.cc
MeasurementSets/MSProcessorColumns.cc
MeasurementSets/MSHistory.cc
MeasurementSets/MSHistoryHandler.cc
MeasurementSets/MSSource.cc
MeasurementSets/MSFlagCmd.cc
MeasurementSets/MSPolColumns.cc
MeasurementSets/MSSysCal.cc
MeasurementSets/MSState.cc
MeasurementSets/MSTable2.cc
MeasurementSets/MSWeatherColumns.cc
MeasurementSets/MSDopplerUtil.cc
MeasurementSets/MSPointingColumns.cc
MeasurementSets/MSAntenna.cc
MeasurementSets/MSWeather.cc
MeasurementSets/MSTableImpl.cc
MeasurementSets/MSDopplerColumns.cc
MeasurementSets/MSFeedColumns.cc
MeasurementSets/MSField.cc
MeasurementSets/MSAntennaColumns.cc
MeasurementSets/MSObsColumns.cc
MeasurementSets/MSFeed.cc
MeasurementSets/MSColumns.cc
MeasurementSets/MSObservation.cc
MeasurementSets/MSDataDescColumns.cc
MeasurementSets/MSStateColumns.cc
MeasurementSets/MSFlagCmdColumns.cc
MeasurementSets/MeasurementSet.cc
MeasurementSets/MSMainColumns.cc
MeasurementSets/MSSourceColumns.cc
MeasurementSets/MSProcessor.cc
MeasurementSets/MSHistoryColumns.cc
MeasurementSets/MSPointing.cc
MeasurementSets/MSRange.cc
MeasurementSets/MSSysCalColumns.cc
MeasurementSets/MSDoppler.cc
MeasurementSets/StokesConverter.cc
MeasurementSets/MSDataDescription.cc
MeasurementSets/MSTileLayout.cc
MeasurementSets/MSFreqOffColumns.cc
MeasurementSets/MSFieldColumns.cc
MeasurementSets/MSSpectralWindow.cc
MeasurementSets/MSPolarization.cc
MeasurementSets/MSSpWindowColumns.cc
MeasurementSets/MSIter.cc
MeasurementSets/MSTable.cc
MSSel/MSAntennaGram.cc
MSSel/MSAntennaIndex.cc
MSSel/MSAntennaParse.cc
MSSel/MSArrayGram.cc
MSSel/MSArrayParse.cc
MSSel/MSCorrGram.cc
MSSel/MSCorrParse.cc
MSSel/MSDataDescIndex.cc
MSSel/MSDopplerIndex.cc
MSSel/MSFeedIndex.cc
MSSel/MSFieldGram.cc
MSSel/MSFieldIndex.cc
MSSel/MSFieldParse.cc
MSSel/MSFreqOffIndex.cc
MSSel/MSObservationGram.cc
MSSel/MSObservationParse.cc
MSSel/MSObsIndex.cc
MSSel/MSParse.cc
MSSel/MSPointingIndex.cc
MSSel/MSPolIndex.cc
MSSel/MSPolnGram.cc
MSSel/MSPolnParse.cc
MSSel/MSScanGram.cc
MSSel/MSScanParse.cc
MSSel/MSSelectableTable.cc
MSSel/MSSelection.cc
MSSel/MSSelectionError.cc
MSSel/MSSelectionErrorHandler.cc
MSSel/MSSelectionKeywords.cc
MSSel/MSSelectionTools.cc
MSSel/MSSelector.cc
MSSel/MSSourceIndex.cc
MSSel/MSSpwGram.cc
MSSel/MSSpwIndex.cc
MSSel/MSSpWindowIndex.cc
MSSel/MSSpwParse.cc
MSSel/MSStateGram.cc
MSSel/MSStateIndex.cc
MSSel/MSStateParse.cc
MSSel/MSSysCalIndex.cc
MSSel/MSTableIndex.cc
MSSel/MSTimeGram.cc
MSSel/MSTimeParse.cc
MSSel/MSUvDistGram.cc
MSSel/MSUvDistParse.cc
MSSel/MSWeatherIndex.cc
MSOper/MS1ToMS2Converter.cc
MSOper/MSConcat.cc
MSOper/MSDerivedValues.cc
MSOper/MSFlagger.cc
MSOper/MSKeys.cc
MSOper/MSLister.cc
MSOper/MSMetaData.cc
MSOper/MSReader.cc
MSOper/MSSummary.cc
MSOper/MSValidIds.cc
MSOper/NewMSSimulator.cc
${BISON_MSAntennaGram_OUTPUTS}
${FLEX_MSAntennaGram_OUTPUTS}
${BISON_MSArrayGram_OUTPUTS}
${FLEX_MSArrayGram_OUTPUTS}
${BISON_MSCorrGram_OUTPUTS}
${FLEX_MSCorrGram_OUTPUTS}
${BISON_MSFieldGram_OUTPUTS}
${FLEX_MSFieldGram_OUTPUTS}
${BISON_MSScanGram_OUTPUTS}
${FLEX_MSScanGram_OUTPUTS}
${BISON_MSObservationGram_OUTPUTS}
${FLEX_MSObservationGram_OUTPUTS}
${BISON_MSSpwGram_OUTPUTS}
${FLEX_MSSpwGram_OUTPUTS}
${BISON_MSTimeGram_OUTPUTS}
${FLEX_MSTimeGram_OUTPUTS}
${BISON_MSUvDistGram_OUTPUTS}
${FLEX_MSUvDistGram_OUTPUTS}
${BISON_MSStateGram_OUTPUTS}
${FLEX_MSStateGram_OUTPUTS}
)

target_link_libraries (
casa_ms casa_measures
)

add_subdirectory (apps)

install (TARGETS casa_ms
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
MeasurementSets/MSAntenna.h
MeasurementSets/MSAntennaColumns.h
MeasurementSets/MSAntennaEnums.h
MeasurementSets/MSColumns.h
MeasurementSets/MSDataDescColumns.h
MeasurementSets/MSDataDescEnums.h
MeasurementSets/MSDataDescription.h
MeasurementSets/MSDoppler.h
MeasurementSets/MSDopplerColumns.h
MeasurementSets/MSDopplerEnums.h
MeasurementSets/MSDopplerUtil.h
MeasurementSets/MSFeed.h
MeasurementSets/MSFeedColumns.h
MeasurementSets/MSFeedEnums.h
MeasurementSets/MSField.h
MeasurementSets/MSFieldColumns.h
MeasurementSets/MSFieldEnums.h
MeasurementSets/MSFlagCmd.h
MeasurementSets/MSFlagCmdColumns.h
MeasurementSets/MSFlagCmdEnums.h
MeasurementSets/MSFreqOffColumns.h
MeasurementSets/MSFreqOffEnums.h
MeasurementSets/MSFreqOffset.h
MeasurementSets/MSHistory.h
MeasurementSets/MSHistoryColumns.h
MeasurementSets/MSHistoryEnums.h
MeasurementSets/MSHistoryHandler.h
MeasurementSets/MSIter.h
MeasurementSets/MSMainColumns.h
MeasurementSets/MSMainEnums.h
MeasurementSets/MSObsColumns.h
MeasurementSets/MSObsEnums.h
MeasurementSets/MSObservation.h
MeasurementSets/MSPointing.h
MeasurementSets/MSPointingColumns.h
MeasurementSets/MSPointingEnums.h
MeasurementSets/MSPolColumns.h
MeasurementSets/MSPolEnums.h
MeasurementSets/MSPolarization.h
MeasurementSets/MSProcessor.h
MeasurementSets/MSProcessorColumns.h
MeasurementSets/MSProcessorEnums.h
MeasurementSets/MSRange.h
MeasurementSets/MSSource.h
MeasurementSets/MSSourceColumns.h
MeasurementSets/MSSourceEnums.h
MeasurementSets/MSSpWindowColumns.h
MeasurementSets/MSSpWindowEnums.h
MeasurementSets/MSSpectralWindow.h
MeasurementSets/MSState.h
MeasurementSets/MSStateColumns.h
MeasurementSets/MSStateEnums.h
MeasurementSets/MSSysCal.h
MeasurementSets/MSSysCalColumns.h
MeasurementSets/MSSysCalEnums.h
MeasurementSets/MSTable.h
MeasurementSets/MSTable.tcc
MeasurementSets/MSTableImpl.h
MeasurementSets/MSTileLayout.h
MeasurementSets/MSWeather.h
MeasurementSets/MSWeatherColumns.h
MeasurementSets/MSWeatherEnums.h
MeasurementSets/MeasurementSet.h
MeasurementSets/StokesConverter.h
DESTINATION include/casacore/ms/MeasurementSets
)

install (FILES
MSSel/MSAntennaGram.h
MSSel/MSAntennaIndex.h
MSSel/MSAntennaParse.h
MSSel/MSArrayGram.h
MSSel/MSArrayParse.h
MSSel/MSCorrGram.h
MSSel/MSCorrParse.h
MSSel/MSDataDescIndex.h
MSSel/MSDopplerIndex.h
MSSel/MSFeedIndex.h
MSSel/MSFieldGram.h
MSSel/MSFieldIndex.h
MSSel/MSFieldParse.h
MSSel/MSFreqOffIndex.h
MSSel/MSObservationGram.h
MSSel/MSObservationParse.h
MSSel/MSObsIndex.h
MSSel/MSParse.h
MSSel/MSPointingIndex.h
MSSel/MSPolIndex.h
MSSel/MSPolnGram.h
MSSel/MSPolnParse.h
MSSel/MSScanGram.h
MSSel/MSScanParse.h
MSSel/MSSelectableMainColumn.h
MSSel/MSSelectableTable.h
MSSel/MSSelection.h
MSSel/MSSelectionError.h
MSSel/MSSelectionErrorHandler.h
MSSel/MSSelectionKeywords.h
MSSel/MSSelectionTools.h
MSSel/MSSelector.h
MSSel/MSSelUtil.h
MSSel/MSSelUtil.tcc
MSSel/MSSelUtil2.h
MSSel/MSSelUtil2.tcc
MSSel/MSSourceIndex.h
MSSel/MSSpwGram.h
MSSel/MSSpwIndex.h
MSSel/MSSpWindowIndex.h
MSSel/MSSpwParse.h
MSSel/MSStateGram.h
MSSel/MSStateIndex.h
MSSel/MSStateParse.h
MSSel/MSSysCalIndex.h
MSSel/MSTableIndex.h
MSSel/MSTimeDefinitions.h
MSSel/MSTimeGram.h
MSSel/MSTimeParse.h
MSSel/MSUvDistGram.h
MSSel/MSUvDistParse.h
MSSel/MSWeatherIndex.h
DESTINATION include/casacore/ms/MSSel
)

install (FILES
MSOper/MS1ToMS2Converter.h
MSOper/MSConcat.h
MSOper/MSDerivedValues.h
MSOper/MSFlagger.h
MSOper/MSKeys.h
MSOper/MSLister.h
MSOper/MSMetaData.h
MSOper/MSReader.h
MSOper/MSSummary.h
MSOper/MSValidIds.h
MSOper/NewMSSimulator.h
DESTINATION include/casacore/ms/MSOper
)

install (FILES
MeasurementSets.h
MSSel.h
MSOper.h
DESTINATION include/casacore/ms
)

if (BUILD_TESTING)
    add_subdirectory (MeasurementSets/test)
    add_subdirectory (MSSel/test)
    add_subdirectory (MSOper/test)
endif (BUILD_TESTING)
