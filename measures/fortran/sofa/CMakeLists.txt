#
# commentaar
#

project (casacore_sofa C Fortran)

file (GLOB sofa_sources *.f)

add_library (sofa SHARED ${sofa_sources})
target_link_libraries(sofa ${LAPACK_LIBRARIES})

