set (testfiles
  tAntennaPairFile.cc
  tBitPacking.cc
  tColumnarFile.cc
  tMorphingArray.cc
  tStokesIStMan.cc
  tUvwFile.cc
)
if(BUILD_SISCO)
  set(testfiles ${testfiles}
    tBitFloat.cc
    tConditionalQueue.cc
    tDeflate.cc
    tShapesFile.cc
    tSisco.cc
    tSiscoWriter.cc
  )
  find_package(Deflate REQUIRED)
  set (EXTRA_ALTMANTEST_LIBRARIES ${DEFLATE_LIBRARY})
elseif()
  set (EXTRA_ALTMANTEST_LIBRARIES)
endif(BUILD_SISCO)

find_package(Boost 1.69 COMPONENTS filesystem unit_test_framework)
if(Boost_FOUND)
  include_directories(${Boost_INCLUDE_DIR})

  add_executable (altmantest ${testfiles})
  add_pch_support(altmantest)
  target_link_libraries(altmantest casa_tables ${Boost_LIBRARIES} ${EXTRA_ALTMANTEST_LIBRARIES})
  if(BUILD_SISCO)
    target_include_directories(altmantest PUBLIC ${DEFLATE_INCLUDE_DIR})
  endif(BUILD_SISCO)
  add_test (altmantest ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./altmantest)
  add_dependencies(check altmantest)
endif(Boost_FOUND)
