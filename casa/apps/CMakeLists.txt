# Programs
foreach(prog casahdf5support)
    add_executable (${prog}  ${prog}.cc)
    target_link_libraries (${prog} casa_casa)
    install(TARGETS ${prog} DESTINATION bin)
endforeach()

# Scripts
foreach(prog countcode)
    install(PROGRAMS ${prog} DESTINATION bin)
endforeach()
