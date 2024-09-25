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

set(top_level_headers
DerivedMC.h
)

init_pch_support(casa_derivedmscal ${top_level_headers})

target_link_libraries (casa_derivedmscal casa_ms ${CASACORE_ARCH_LIBS})

install (TARGETS casa_derivedmscal
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
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

install (FILES ${top_level_headers}
DESTINATION include/casacore/derivedmscal
)

add_subdirectory (DerivedMC/test ${EXCL_ALL})
