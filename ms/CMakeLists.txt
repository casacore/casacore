#
# CASA MeasurementSets 
#

set (
parser_inputs
MSAntennaGram
MSArrayGram
MSCorrGram
MSFieldGram
MSScanGram
MSSpwGram
MSTimeGram
MSUvDistGram
)

if (APPLE)
    foreach (src ${parser_inputs})
	    BISON_TARGET (${src} MeasurementSets/${src}.yy ${CMAKE_CURRENT_BINARY_DIR}/${src} COMPILE_FLAGS "-y -p ${src} --output-file=${src}.ycc")
        execute_process (COMMAND ${CMAKE_COMMAND} -E create_symlink ${src} ${src}.ycc WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
	    FLEX_TARGET (${src} MeasurementSets/${src}.ll ${CMAKE_CURRENT_BINARY_DIR}/${src}.lcc COMPILE_FLAGS "-P${src}")
    endforeach (src)
else (APPLE)
    foreach (src ${parser_inputs})
        BISON_TARGET (${src} MeasurementSets/${src}.yy ${CMAKE_CURRENT_BINARY_DIR}/${src}.ycc COMPILE_FLAGS "-y -p ${src} --output-file=${src}.ycc")
        FLEX_TARGET (${src} MeasurementSets/${src}.ll ${CMAKE_CURRENT_BINARY_DIR}/${src}.lcc COMPILE_FLAGS "-P${src}")
    endforeach (src)
endif (APPLE)

include_directories (${CMAKE_CURRENT_BINARY_DIR})

add_library (casa_ms
MeasurementSets/MSFlagger.cc
MeasurementSets/MSPolIndex.cc
MeasurementSets/MSValidIds.cc
MeasurementSets/MSFreqOffset.cc
MeasurementSets/MSProcessorColumns.cc
MeasurementSets/MSHistory.cc
MeasurementSets/MSHistoryHandler.cc
MeasurementSets/MSSource.cc
MeasurementSets/MSFlagCmd.cc
MeasurementSets/MSWeatherIndex.cc
MeasurementSets/MSDataDescIndex.cc
MeasurementSets/MSPolColumns.cc
MeasurementSets/MSSysCal.cc
MeasurementSets/MSAntennaGram.cc
MeasurementSets/MSScanGram.cc
MeasurementSets/MSState.cc
MeasurementSets/MSReader.cc
MeasurementSets/MSTable2.cc
MeasurementSets/MSWeatherColumns.cc
MeasurementSets/MSArrayParse.cc
MeasurementSets/MSDopplerUtil.cc
MeasurementSets/MSPointingColumns.cc
MeasurementSets/MSSummary.cc
MeasurementSets/MSSysCalIndex.cc
MeasurementSets/MSAntenna.cc
MeasurementSets/MSScanParse.cc
MeasurementSets/MSSpWindowIndex.cc
MeasurementSets/MSCorrGram.cc
MeasurementSets/MSPolnParse.cc
MeasurementSets/MSDerivedValues.cc
MeasurementSets/MSWeather.cc
MeasurementSets/MSTableImpl.cc
MeasurementSets/MSDopplerColumns.cc
MeasurementSets/MSFeedColumns.cc
MeasurementSets/MSField.cc
MeasurementSets/MSPointingIndex.cc
MeasurementSets/MSAntennaColumns.cc
MeasurementSets/MSObsColumns.cc
MeasurementSets/MSSourceIndex.cc
MeasurementSets/MSAntennaIndex.cc
MeasurementSets/NewMSSimulator.cc
MeasurementSets/MSFeed.cc
MeasurementSets/MSColumns.cc
MeasurementSets/MSSpwGram.cc
MeasurementSets/MSFeedIndex.cc
MeasurementSets/MSUvDistGram.cc
MeasurementSets/MSFieldIndex.cc
MeasurementSets/MSSelectionError.cc
MeasurementSets/MSObservation.cc
MeasurementSets/MSSelectionTools.cc
MeasurementSets/MSSelectionKeywords.cc
MeasurementSets/MSParse.cc
MeasurementSets/MSDataDescColumns.cc
MeasurementSets/MSTimeParse.cc
MeasurementSets/MSObsIndex.cc
MeasurementSets/MSStateColumns.cc
MeasurementSets/MSFlagCmdColumns.cc
MeasurementSets/MeasurementSet.cc
MeasurementSets/MS1ToMS2Converter.cc
MeasurementSets/MSUvDistParse.cc
MeasurementSets/MSAntennaParse.cc
MeasurementSets/MSMainColumns.cc
MeasurementSets/MSArrayGram.cc
MeasurementSets/MSConcat.cc
MeasurementSets/MSSourceColumns.cc
MeasurementSets/MSSpwParse.cc
MeasurementSets/MSProcessor.cc
MeasurementSets/MSFreqOffIndex.cc
MeasurementSets/MSHistoryColumns.cc
MeasurementSets/MSSelector.cc
MeasurementSets/MSPolnGram.cc
MeasurementSets/MSSelection.cc
MeasurementSets/MSPointing.cc
MeasurementSets/MSRange.cc
MeasurementSets/MSDopplerIndex.cc
MeasurementSets/MSSysCalColumns.cc
MeasurementSets/MSDoppler.cc
MeasurementSets/StokesConverter.cc
MeasurementSets/MSLister.cc
MeasurementSets/MSSpwIndex.cc
MeasurementSets/MSTimeGram.cc
MeasurementSets/MSDataDescription.cc
MeasurementSets/MSFieldParse.cc
MeasurementSets/MSTileLayout.cc
MeasurementSets/MSFreqOffColumns.cc
MeasurementSets/MSFieldColumns.cc
MeasurementSets/MSCorrParse.cc
MeasurementSets/MSSpectralWindow.cc
MeasurementSets/MSSimulator.cc
MeasurementSets/MSTableIndex.cc
MeasurementSets/MSPolarization.cc
MeasurementSets/MSSpWindowColumns.cc
MeasurementSets/MSIter.cc
MeasurementSets/MSFieldGram.cc
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
${BISON_MSSpwGram_OUTPUTS}
${FLEX_MSSpwGram_OUTPUTS}
${BISON_MSTimeGram_OUTPUTS}
${FLEX_MSTimeGram_OUTPUTS}
${BISON_MSUvDistGram_OUTPUTS}
${FLEX_MSUvDistGram_OUTPUTS}
)

target_link_libraries (casa_ms casa_measures)

add_subdirectory (apps)

install (TARGETS casa_ms
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
MeasurementSets/MS1ToMS2Converter.h
MeasurementSets/MSAntenna.h
MeasurementSets/MSAntennaColumns.h
MeasurementSets/MSAntennaEnums.h
MeasurementSets/MSAntennaGram.h
MeasurementSets/MSAntennaIndex.h
MeasurementSets/MSAntennaParse.h
MeasurementSets/MSArrayGram.h
MeasurementSets/MSArrayParse.h
MeasurementSets/MSColumns.h
MeasurementSets/MSConcat.h
MeasurementSets/MSCorrGram.h
MeasurementSets/MSCorrParse.h
MeasurementSets/MSDataDescColumns.h
MeasurementSets/MSDataDescEnums.h
MeasurementSets/MSDataDescIndex.h
MeasurementSets/MSDataDescription.h
MeasurementSets/MSDerivedValues.h
MeasurementSets/MSDoppler.h
MeasurementSets/MSDopplerColumns.h
MeasurementSets/MSDopplerEnums.h
MeasurementSets/MSDopplerIndex.h
MeasurementSets/MSDopplerUtil.h
MeasurementSets/MSFeed.h
MeasurementSets/MSFeedColumns.h
MeasurementSets/MSFeedEnums.h
MeasurementSets/MSFeedIndex.h
MeasurementSets/MSField.h
MeasurementSets/MSFieldColumns.h
MeasurementSets/MSFieldEnums.h
MeasurementSets/MSFieldGram.h
MeasurementSets/MSFieldIndex.h
MeasurementSets/MSFieldParse.h
MeasurementSets/MSFlagCmd.h
MeasurementSets/MSFlagCmdColumns.h
MeasurementSets/MSFlagCmdEnums.h
MeasurementSets/MSFlagger.h
MeasurementSets/MSFreqOffColumns.h
MeasurementSets/MSFreqOffEnums.h
MeasurementSets/MSFreqOffIndex.h
MeasurementSets/MSFreqOffset.h
MeasurementSets/MSHistory.h
MeasurementSets/MSHistoryColumns.h
MeasurementSets/MSHistoryEnums.h
MeasurementSets/MSHistoryHandler.h
MeasurementSets/MSIter.h
MeasurementSets/MSLister.h
MeasurementSets/MSMainColumns.h
MeasurementSets/MSMainEnums.h
MeasurementSets/MSObsColumns.h
MeasurementSets/MSObsEnums.h
MeasurementSets/MSObsIndex.h
MeasurementSets/MSObservation.h
MeasurementSets/MSParse.h
MeasurementSets/MSPointing.h
MeasurementSets/MSPointingColumns.h
MeasurementSets/MSPointingEnums.h
MeasurementSets/MSPointingIndex.h
MeasurementSets/MSPolColumns.h
MeasurementSets/MSPolEnums.h
MeasurementSets/MSPolIndex.h
MeasurementSets/MSPolarization.h
MeasurementSets/MSPolnGram.h
MeasurementSets/MSPolnParse.h
MeasurementSets/MSProcessor.h
MeasurementSets/MSProcessorColumns.h
MeasurementSets/MSProcessorEnums.h
MeasurementSets/MSRange.h
MeasurementSets/MSReader.h
MeasurementSets/MSScanGram.h
MeasurementSets/MSScanParse.h
MeasurementSets/MSSelUtil.h
MeasurementSets/MSSelUtil.tcc
MeasurementSets/MSSelUtil2.h
MeasurementSets/MSSelUtil2.tcc
MeasurementSets/MSSelection.h
MeasurementSets/MSSelectionError.h
MeasurementSets/MSSelectionKeywords.h
MeasurementSets/MSSelectionTools.h
MeasurementSets/MSSelector.h
MeasurementSets/MSSimulator.h
MeasurementSets/MSSource.h
MeasurementSets/MSSourceColumns.h
MeasurementSets/MSSourceEnums.h
MeasurementSets/MSSourceIndex.h
MeasurementSets/MSSpWindowColumns.h
MeasurementSets/MSSpWindowEnums.h
MeasurementSets/MSSpWindowIndex.h
MeasurementSets/MSSpectralWindow.h
MeasurementSets/MSSpwGram.h
MeasurementSets/MSSpwIndex.h
MeasurementSets/MSSpwParse.h
MeasurementSets/MSState.h
MeasurementSets/MSStateColumns.h
MeasurementSets/MSStateEnums.h
MeasurementSets/MSSummary.h
MeasurementSets/MSSysCal.h
MeasurementSets/MSSysCalColumns.h
MeasurementSets/MSSysCalEnums.h
MeasurementSets/MSSysCalIndex.h
MeasurementSets/MSTable.h
MeasurementSets/MSTable.tcc
MeasurementSets/MSTableImpl.h
MeasurementSets/MSTableIndex.h
MeasurementSets/MSTileLayout.h
MeasurementSets/MSTimeDefinitions.h
MeasurementSets/MSTimeGram.h
MeasurementSets/MSTimeParse.h
MeasurementSets/MSUvDistGram.h
MeasurementSets/MSUvDistParse.h
MeasurementSets/MSValidIds.h
MeasurementSets/MSWeather.h
MeasurementSets/MSWeatherColumns.h
MeasurementSets/MSWeatherEnums.h
MeasurementSets/MSWeatherIndex.h
MeasurementSets/MeasurementSet.h
MeasurementSets/NewMSSimulator.h
MeasurementSets/StokesConverter.h
DESTINATION include/casacore/ms/MeasurementSets
)

install (FILES
MeasurementSets.h
DESTINATION include/casacore/ms
)

if (BUILD_TESTING)
    add_subdirectory (MeasurementSets/test)
endif (BUILD_TESTING)
