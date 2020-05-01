set (testfiles
  tAllocator
  tArray.cc
  tArrayAccessor.cc
  tArrayBase.cc
#tArrayIO2.cc
#tArrayIO3.cc
#tArrayIO.cc
  tArrayExceptionHandling.cc
  tArrayIter.cc
  tArrayIter1.cc
  tArrayIteratorSTL.cc
  tArrayLogical.cc
  tArrayMath.cc
#tArrayMathPerf.cc
  tArrayMathTransform.cc
  tArrayOperations.cc
  tArrayOpsDiffShapes.cc
  tArrayPartMath.cc
  tArrayPosIter.cc
  tArrayStr.cc
  tArrayUtil.cc
#tArrayUtilPerf.cc
  tAxesSpecifier.cc
  tBoxedArrayMath.cc
#tCompareBoxedPartial.cc
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
#  tPerfTransform.cc
  tSlice.cc
  tSlicer.cc
  tSlidingArrayMath.cc
  tStringArray.cc
#  tSumPerformance.cc
  tVector.cc
  tVectorSTLIterator.cc
)

find_package(Boost COMPONENTS filesystem unit_test_framework system)
if(Boost_FOUND)
	include_directories(${Boost_INCLUDE_DIR})

	add_executable (arraytest ${testfiles})
	target_link_libraries(arraytest casa_casa ${Boost_FILESYSTEM_LIBRARY} ${Boost_SYSTEM_LIBRARY} ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})
	add_test (arraytest ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./arraytest)
	add_dependencies(check arraytest)
endif(Boost_FOUND)
