#
# CASA scimath_f
#

set (buildfiles
abshis.f
absmax.f
ADDGRD.f
atmroutines.f
convolvegridder.f
dqags.f
fgridft.f
fgridsd.f
fmosft.f
fwproj.f
getbig.f
grd2d.f
grd2dwts.f
grdde2d.f
grdgauss.f
grdjinc1.f
grdsf.f
hclean.f
lawson.f
maxabs.f
parametricsolver.f
phasol.f
subcom.f
vvroutines.f
)

if (BUILD_FFTPACK_DEPRECATED)
    list (APPEND buildfiles
         dfftpak.f
         fftpak.f
    )
endif ()

add_library (casa_scimath_f ${buildfiles})
target_compile_options (casa_scimath_f PRIVATE -std=legacy)
target_link_libraries (casa_scimath_f casa_tables ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} ${CASACORE_ARCH_LIBS})

install (
TARGETS casa_scimath_f
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
