add_executable (measuresdata  measuresdata/measuresdata.cc)
target_link_libraries (measuresdata casa_measures)
#install(TARGETS measuresdata measuresdata/measuresdata.csh DESTINATION bin)
install(TARGETS measuresdata DESTINATION bin)
