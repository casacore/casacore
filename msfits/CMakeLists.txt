#
# CASA msfits
#

add_library (casa_msfits
MSFits/FitsIDItoMS.cc
MSFits/MSFitsIDI.cc
MSFits/MSFitsInput.cc
MSFits/MSFitsOutput.cc
MSFits/MSFitsOutputAstron.cc
MSFits/SDAntennaHandler.cc
MSFits/SDDataDescHandler.cc
MSFits/SDFeedHandler.cc
MSFits/SDFieldHandler.cc
MSFits/SDFITSHandler.cc
MSFits/SDHistoryHandler.cc
MSFits/SDMainHandler.cc
MSFits/SDObservationHandler.cc
MSFits/SDPointingHandler.cc
MSFits/SDPolarizationHandler.cc
MSFits/SDSourceHandler.cc
MSFits/SDSpWinHandler.cc
MSFits/SDSysCalHandler.cc
MSFits/SDWeatherHandler.cc
)

target_link_libraries (casa_msfits casa_ms casa_fits)

add_subdirectory (apps)

install (
TARGETS casa_msfits
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
MSFits/FitsIDItoMS.h
MSFits/MSFitsIDI.h
MSFits/MSFitsInput.h
MSFits/MSFitsOutput.h
MSFits/MSFitsOutputAstron.h
MSFits/SDAntennaHandler.h
MSFits/SDDataDescHandler.h
MSFits/SDFITSHandler.h
MSFits/SDFeedHandler.h
MSFits/SDFieldHandler.h
MSFits/SDHistoryHandler.h
MSFits/SDMainHandler.h
MSFits/SDObservationHandler.h
MSFits/SDPointingHandler.h
MSFits/SDPolarizationHandler.h
MSFits/SDSourceHandler.h
MSFits/SDSpWinHandler.h
MSFits/SDSysCalHandler.h
MSFits/SDWeatherHandler.h
DESTINATION include/casacore/msfits/MSFits
)

install (FILES
MSFits.h
DESTINATION include/casacore/msfits
)

if (BUILD_TESTING)
    add_subdirectory (MSFits/test)
endif (BUILD_TESTING)
