#
# CASA Mirlib
#

# Part of the c files is not compiling yet in C23 mode, so require
# 17.
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

add_library (
casa_mirlib
bug.c
dio.c
headio.c
hio.c
key.c
maskio.c
pack.c
scrio.c
uvio.c
xyio.c
xyzio.c
)


target_link_libraries (casa_mirlib casa_casa ${CASACORE_ARCH_LIBS})

install (
TARGETS casa_mirlib
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (
FILES
hio.h
io.h
maxdimc.h
miriad.h
sysdep.h
DESTINATION
include/casacore/mirlib
)

