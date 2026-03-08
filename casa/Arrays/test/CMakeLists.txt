set (testfiles
  tAllocator.cc
  tArray.cc
  tArrayAccessor.cc
  tArrayBase.cc
  tArrayExceptionHandling.cc
  tArrayIter.cc
  tArrayIter1.cc
  tArrayIteratorSTL.cc
  tArrayLogical.cc
  tArrayMath.cc
  tArrayMathTransform.cc
  tArrayOperations.cc
  tArrayOpsDiffShapes.cc
  tArrayPartMath.cc
  tArrayPosIter.cc
  tArrayStr.cc
  tArrayUtil.cc
  tAxesSpecifier.cc
  tBoxedArrayMath.cc
  tConvertArray.cc
  tCpp11Features.cc
  tCube.cc
  tDiagonal.cc
  tExtendSpecifier.cc
  tIPosition.cc
  tLinAlgebra.cc
  tMaskArrExcp.cc
  tMaskArrIO.cc
  tMaskArrLogi.cc
  tMaskArrMath0.cc
  tMaskArrMath1.cc
  tMaskArrMath2.cc
  tMaskedArray.cc
  tMatrix.cc
  tMatrixMath.cc
  tMedianSmooth.cc
  tSlice.cc
  tSlicer.cc
  tSlidingArrayMath.cc
  tStringArray.cc
  tVector.cc
  tVectorSTLIterator.cc
)

find_package(Boost 1.73 COMPONENTS filesystem unit_test_framework)
if(Boost_FOUND)
	include_directories(${Boost_INCLUDE_DIR})

	add_executable (arraytest ${testfiles})
	add_pch_support(arraytest)
	target_link_libraries(arraytest casa_casa ${Boost_FILESYSTEM_LIBRARY} ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})
	add_test (arraytest ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./arraytest)
	add_dependencies(check arraytest)
endif(Boost_FOUND)
