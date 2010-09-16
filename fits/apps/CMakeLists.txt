add_executable (fits2table  fits2table/fits2table.cc)
target_link_libraries (fits2table casa_fits)
install(TARGETS fits2table DESTINATION bin)
