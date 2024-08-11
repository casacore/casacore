add_executable (fits2table  fits2table/fits2table.cc)
add_pch_support(fits2table)
target_link_libraries (fits2table casa_fits ${CASACORE_ARCH_LIBS})
install(TARGETS fits2table DESTINATION ${CMAKE_INSTALL_BINDIR}) 
