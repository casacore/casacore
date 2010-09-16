#
# CASA scimath_f
#

add_library (
casa_scimath_f
abshis.f
absmax.f
ADDGRD.f
atmroutines.f
convolvegridder.f
dfftpak.f
dqags.f
fftpak.f
fgridft.f
fgridsd.f
fmosft.f
fwproj.f
getbig.f
grd2d.f
grd2dwts.f
grdde2d.f
grdsf.f
hclean.f
lawson.f
maxabs.f
parametricsolver.f
phasol.f
subcom.f
vvroutines.f
)

target_link_libraries (casa_scimath_f casa_tables ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})

install (
TARGETS casa_scimath_f
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

