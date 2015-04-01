#
# CASA Lattices
#

add_library (casa_lattices
Lattices/LatticeBase.cc
Lattices/LatticeIndexer.cc
Lattices/LatticeLocker.cc
Lattices/LatticeNavigator.cc
Lattices/LatticeStepper.cc
Lattices/PixelCurve1D.cc
Lattices/TileStepper.cc
Lattices/TiledLineStepper.cc
Lattices/TiledShape.cc
LatticeMath/Fit2D.cc
LatticeMath/LatticeAddNoise.cc
LatticeMath/LatticeCleanProgress.cc
LatticeMath/LatticeFFT.cc
LatticeMath/LatticeFit.cc
LatticeMath/LatticeHistProgress.cc
LatticeMath/LatticeHistSpecialize.cc
LatticeMath/LatticeProgress.cc
LatticeMath/LatticeStatsBase.cc
LatticeMath/LattStatsProgress.cc
LatticeMath/LattStatsSpecialize.cc
LEL/LatticeExprNode.cc
LEL/LELArrayBase.cc
LEL/LELAttribute.cc
LEL/LELBinary2.cc
LEL/LELCoordinates.cc
LEL/LELFunction2.cc
LEL/LELLattCoord.cc
LEL/LELLattCoordBase.cc
LEL/LELRegion.cc
LEL/LELUnary2.cc
LRegions/FITSMask.cc
LRegions/LatticeRegion.cc
LRegions/LattRegionHolder.cc
LRegions/LCBox.cc
LRegions/LCComplement.cc
LRegions/LCConcatenation.cc
LRegions/LCDifference.cc
LRegions/LCEllipsoid.cc
LRegions/LCExtension.cc
LRegions/LCHDF5Mask.cc
LRegions/LCIntersection.cc
LRegions/LCLELMask.cc
LRegions/LCMask.cc
LRegions/LCPagedMask.cc
LRegions/LCPixelSet.cc
LRegions/LCPolygon.cc
LRegions/LCRegion.cc
LRegions/LCRegion2.cc
LRegions/LCRegionFixed.cc
LRegions/LCRegionMulti.cc
LRegions/LCRegionSingle.cc
LRegions/LCSlicer.cc
LRegions/LCStretch.cc
LRegions/LCUnion.cc
LRegions/RegionType.cc
)

target_link_libraries (
casa_lattices
casa_tables
casa_scimath
)

install (TARGETS casa_lattices
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
Lattices/ArrayLattice.h
Lattices/ArrayLattice.tcc
Lattices/CurvedLattice2D.h
Lattices/CurvedLattice2D.tcc
Lattices/ExtendLattice.h
Lattices/ExtendLattice.tcc
Lattices/HDF5LattIter.h
Lattices/HDF5LattIter.tcc
Lattices/HDF5Lattice.h
Lattices/HDF5Lattice.tcc
Lattices/Lattice.h
Lattices/Lattice.tcc
Lattices/LatticeBase.h
Lattices/LatticeCache.h
Lattices/LatticeCache.tcc
Lattices/LatticeConcat.h
Lattices/LatticeConcat.tcc
Lattices/LatticeIndexer.h
Lattices/LatticeIterInterface.h
Lattices/LatticeIterInterface.tcc
Lattices/LatticeIterator.h
Lattices/LatticeIterator.tcc
Lattices/LatticeLocker.h
Lattices/LatticeNavigator.h
Lattices/LatticeStepper.h
Lattices/LatticeUtilities.h
Lattices/LatticeUtilities.tcc
Lattices/MaskedLattice.h
Lattices/MaskedLattice.tcc
Lattices/MaskedLatticeIterator.h
Lattices/MaskedLatticeIterator.tcc
Lattices/PagedArrIter.h
Lattices/PagedArrIter.tcc
Lattices/PagedArray.h
Lattices/PagedArray.tcc
Lattices/PixelCurve1D.h
Lattices/RebinLattice.h
Lattices/RebinLattice.tcc
Lattices/SubLattice.h
Lattices/SubLattice.tcc
Lattices/TempLattice.h
Lattices/TempLattice.tcc
Lattices/TempLatticeImpl.h
Lattices/TempLatticeImpl.tcc
Lattices/TileStepper.h
Lattices/TiledLineStepper.h
Lattices/TiledShape.h
DESTINATION include/casacore/lattices/Lattices
)

install (FILES
LatticeMath/StatsTiledCollapser.h
LatticeMath/StatsTiledCollapser.tcc
LatticeMath/CLIPNearest2D.h
LatticeMath/CLIPNearest2D.tcc
LatticeMath/CLInterpolator2D.h
LatticeMath/CLInterpolator2D.tcc
LatticeMath/Fit2D.h
LatticeMath/LatticeAddNoise.h
LatticeMath/LatticeApply.h
LatticeMath/LatticeApply.tcc
LatticeMath/LatticeCleanProgress.h
LatticeMath/LatticeCleaner.h
LatticeMath/LatticeCleaner.tcc
LatticeMath/LatticeConvolver.h
LatticeMath/LatticeConvolver.tcc
LatticeMath/LatticeFFT.h
LatticeMath/LatticeFit.h
LatticeMath/LatticeFractile.h
LatticeMath/LatticeFractile.tcc
LatticeMath/LatticeHistProgress.h
LatticeMath/LatticeHistSpecialize.h
LatticeMath/LatticeHistograms.h
LatticeMath/LatticeHistograms.tcc
LatticeMath/LatticeMathUtil.h
LatticeMath/LatticeMathUtil.tcc
LatticeMath/LatticeProgress.h
LatticeMath/LatticeSlice1D.h
LatticeMath/LatticeSlice1D.tcc
LatticeMath/LatticeStatistics.h
LatticeMath/LatticeStatistics.tcc
LatticeMath/LatticeStatsBase.h
LatticeMath/LatticeStatsDataProvider.h
LatticeMath/LatticeStatsDataProvider.tcc
LatticeMath/LatticeStatsDataProviderBase.h
LatticeMath/LatticeStatsDataProviderBase.tcc
LatticeMath/LatticeTwoPtCorr.h
LatticeMath/LatticeTwoPtCorr.tcc
LatticeMath/LattStatsProgress.h
LatticeMath/LattStatsSpecialize.h
LatticeMath/LineCollapser.h
LatticeMath/LineCollapser.tcc
LatticeMath/MaskedLatticeStatsDataProvider.h
LatticeMath/MaskedLatticeStatsDataProvider.tcc
LatticeMath/MultiTermLatticeCleaner.h
LatticeMath/MultiTermLatticeCleaner.tcc
LatticeMath/TiledCollapser.h
LatticeMath/TiledCollapser.tcc
DESTINATION include/casacore/lattices/LatticeMath
)

install (FILES
LEL/LatticeExpr.h
LEL/LatticeExpr.tcc
LEL/LatticeExprNode.h
LEL/LELArray.h
LEL/LELArray.tcc
LEL/LELArrayBase.h
LEL/LELAttribute.h
LEL/LELBinary.h
LEL/LELBinary.tcc
LEL/LELBinary2.h
LEL/LELBinaryEnums.h
LEL/LELCondition.h
LEL/LELCondition.tcc
LEL/LELConvert.h
LEL/LELConvert.tcc
LEL/LELCoordinates.h
LEL/LELFunction.h
LEL/LELFunction.tcc
LEL/LELFunction2.h
LEL/LELFunctionEnums.h
LEL/LELInterface.h
LEL/LELInterface.tcc
LEL/LELLattCoord.h
LEL/LELLattCoordBase.h
LEL/LELLattice.h
LEL/LELLattice.tcc
LEL/LELRegion.h
LEL/LELScalar.h
LEL/LELScalar.tcc
LEL/LELSpectralIndex.h
LEL/LELSpectralIndex.tcc
LEL/LELUnary.h
LEL/LELUnary.tcc
LEL/LELUnary2.h
LEL/LELUnaryEnums.h
DESTINATION include/casacore/lattices/LEL
)

install (FILES
LRegions/FITSMask.h
LRegions/LatticeRegion.h
LRegions/LattRegionHolder.h
LRegions/LCBox.h
LRegions/LCComplement.h
LRegions/LCConcatenation.h
LRegions/LCDifference.h
LRegions/LCEllipsoid.h
LRegions/LCExtension.h
LRegions/LCHDF5Mask.h
LRegions/LCIntersection.h
LRegions/LCLELMask.h
LRegions/LCMask.h
LRegions/LCPagedMask.h
LRegions/LCPixelSet.h
LRegions/LCPolygon.h
LRegions/LCRegion.h
LRegions/LCRegionFixed.h
LRegions/LCRegionMulti.h
LRegions/LCRegionSingle.h
LRegions/LCSlicer.h
LRegions/LCStretch.h
LRegions/LCUnion.h
LRegions/RegionType.h
DESTINATION include/casacore/lattices/LRegions
)

install (FILES
LatticeMath.h
Lattices.h
LEL.h
LRegions.h
DESTINATION include/casacore/lattices
)

# The tests
if (BUILD_TESTING)
	add_subdirectory (Lattices/test)
	add_subdirectory (LatticeMath/test)
	add_subdirectory (LEL/test)
	add_subdirectory (LRegions/test)
endif (BUILD_TESTING)



