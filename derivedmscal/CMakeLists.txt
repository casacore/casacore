#
# CASA derivedmscal
#

add_library (casa_derivedmscal
DerivedMC/DerivedMSCal.cc
DerivedMC/DerivedColumn.cc
DerivedMC/MSCalEngine.cc
DerivedMC/Register.cc
DerivedMC/UDFMSCal.cc
)

target_link_libraries (casa_derivedmscal casa_ms)

install (TARGETS casa_derivedmscal
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
DerivedMC/DerivedMSCal.h
DerivedMC/DerivedColumn.h
DerivedMC/MSCalEngine.h
DerivedMC/Register.h
DerivedMC/UDFMSCal.h
DESTINATION include/casacore/derivedmscal/DerivedMC
)

install (FILES
DerivedMC.h
DESTINATION include/casacore/derivedmscal
)

if (BUILD_TESTING)
    add_subdirectory (DerivedMC/test)
endif (BUILD_TESTING)
