# Programs
foreach(prog casahdf5support)
    add_executable (${prog}  ${prog}.cc)
    add_pch_support(${prog})
    target_link_libraries (${prog} casa_casa)
    install(TARGETS ${prog})
endforeach()

# Scripts
##foreach(prog countcode)
##   install(PROGRAMS ${prog} DESTINATION bin)
##endforeach()
