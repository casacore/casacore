#
# commentaar
#

project (casacore_sofa C Fortran)

file (GLOB sofa_sources *.f)

add_library (sofa SHARED ${sofa_sources} ${LAPACK_LIBRARIES})

