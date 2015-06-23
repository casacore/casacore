#
# CASA build-tools
#

install (PROGRAMS
  casacore_assay
  casacore_floatcheck
  casacore_memcheck
DESTINATION bin
)

install (FILES
  casacore_memcheck.supp
DESTINATION share
)
