set (testfiles
  tAntennaPairFile.cc
  tBitPacking.cc
  tColumnarFile.cc
  tStokesIStMan.cc
  tUvwFile.cc
)

find_package(Boost COMPONENTS filesystem system unit_test_framework)
if(Boost_FOUND)
  include_directories(${Boost_INCLUDE_DIR})

  add_executable (altmantest ${testfiles})
  add_pch_support(altmantest)
  target_link_libraries(altmantest casa_tables ${Boost_FILESYSTEM_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})
  add_test (altmantest ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./altmantest)
  add_dependencies(check altmantest)
endif(Boost_FOUND)
