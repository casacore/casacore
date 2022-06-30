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

    # FFTPACK is getting so old that it needs deprecated behaviour of the compiler.
    # This is why BUILD_FFTPACK_DEPRECATED is also deprecated in casacore.
    include(CheckFortranCompilerFlag)
    CHECK_Fortran_COMPILER_FLAG(--allow-argument-mismatch Fortran_HAS_ALLOW_ARGUMENT_MISMATCH)
    if (Fortran_HAS_ALLOW_ARGUMENT_MISMATCH)
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --allow-argument-mismatch")
    endif()
endif ()

add_library (casa_scimath_f ${buildfiles})

target_link_libraries (casa_scimath_f casa_tables ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} ${CASACORE_ARCH_LIBS})

install (
TARGETS casa_scimath_f
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
