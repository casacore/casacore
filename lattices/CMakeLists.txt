#
# CASA Lattices
#

add_library (casa_lattices
Lattices/LELRegion.cc
Lattices/LCEllipsoid.cc
Lattices/LCComplement.cc
Lattices/LELFunction2.cc
Lattices/LatticeStepper.cc
Lattices/LCRegionFixed.cc
Lattices/LatticeLocker.cc
Lattices/LCExtension.cc
Lattices/LCRegionMulti.cc
Lattices/TiledLineStepper.cc
Lattices/LCRegion.cc
Lattices/LatticeRegion.cc
Lattices/LatticeFFT.cc
Lattices/LCRegion2.cc
Lattices/LCHDF5Mask.cc
Lattices/LCLELMask.cc
Lattices/LCRegionSingle.cc
Lattices/TiledShape.cc
Lattices/LELCoordinates.cc
Lattices/LCUnion.cc
Lattices/LCIntersection.cc
Lattices/LCDifference.cc
Lattices/LatticeStatsBase.cc
Lattices/LatticeCleanProgress.cc
Lattices/LattRegionHolder.cc
Lattices/LatticeNavigator.cc
Lattices/LatticeExprNode.cc
Lattices/FITSMask.cc
Lattices/LatticeHistProgress.cc
Lattices/LattStatsSpecialize.cc
Lattices/LatticeBase.cc
Lattices/TileStepper.cc
Lattices/LCPixelSet.cc
Lattices/LCConcatenation.cc
Lattices/LCPolygon.cc
Lattices/PixelCurve1D.cc
Lattices/LatticeHistSpecialize.cc
Lattices/LCSlicer.cc
Lattices/LattStatsProgress.cc
Lattices/LELUnary2.cc
Lattices/LatticeAddNoise.cc
Lattices/LELBinary2.cc
Lattices/LELLattCoord.cc
Lattices/LCBox.cc
Lattices/LCMask.cc
Lattices/LatticeIndexer.cc
Lattices/LELArrayBase.cc
Lattices/LELLattCoordBase.cc
Lattices/LCStretch.cc
Lattices/LELAttribute.cc
Lattices/LCPagedMask.cc
Lattices/LatticeProgress.cc
LatticeMath/Fit2D.cc
LatticeMath/LatticeFit.cc
)

target_link_libraries (
casa_lattices
casa_tables
casa_scimath
)

install (TARGETS casa_lattices
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
Lattices/ArrayLattice.h
Lattices/ArrayLattice.tcc
Lattices/CLIPNearest2D.h
Lattices/CLIPNearest2D.tcc
Lattices/CLInterpolator2D.h
Lattices/CLInterpolator2D.tcc
Lattices/CurvedLattice2D.h
Lattices/CurvedLattice2D.tcc
Lattices/ExtendLattice.h
Lattices/ExtendLattice.tcc
Lattices/FITSMask.h
Lattices/HDF5LattIter.h
Lattices/HDF5LattIter.tcc
Lattices/HDF5Lattice.h
Lattices/HDF5Lattice.tcc
Lattices/LCBox.h
Lattices/LCComplement.h
Lattices/LCConcatenation.h
Lattices/LCDifference.h
Lattices/LCEllipsoid.h
Lattices/LCExtension.h
Lattices/LCHDF5Mask.h
Lattices/LCIntersection.h
Lattices/LCLELMask.h
Lattices/LCMask.h
Lattices/LCPagedMask.h
Lattices/LCPixelSet.h
Lattices/LCPolygon.h
Lattices/LCRegion.h
Lattices/LCRegionFixed.h
Lattices/LCRegionMulti.h
Lattices/LCRegionSingle.h
Lattices/LCSlicer.h
Lattices/LCStretch.h
Lattices/LCUnion.h
Lattices/LELArray.h
Lattices/LELArray.tcc
Lattices/LELArrayBase.h
Lattices/LELAttribute.h
Lattices/LELBinary.h
Lattices/LELBinary.tcc
Lattices/LELBinary2.h
Lattices/LELBinaryEnums.h
Lattices/LELCondition.h
Lattices/LELCondition.tcc
Lattices/LELConvert.h
Lattices/LELConvert.tcc
Lattices/LELCoordinates.h
Lattices/LELFunction.h
Lattices/LELFunction.tcc
Lattices/LELFunction2.h
Lattices/LELFunctionEnums.h
Lattices/LELInterface.h
Lattices/LELInterface.tcc
Lattices/LELLattCoord.h
Lattices/LELLattCoordBase.h
Lattices/LELLattice.h
Lattices/LELLattice.tcc
Lattices/LELRegion.h
Lattices/LELScalar.h
Lattices/LELScalar.tcc
Lattices/LELSpectralIndex.h
Lattices/LELSpectralIndex.tcc
Lattices/LELUnary.h
Lattices/LELUnary.tcc
Lattices/LELUnary2.h
Lattices/LELUnaryEnums.h
Lattices/LattRegionHolder.h
Lattices/LattStatsProgress.h
Lattices/LattStatsSpecialize.h
Lattices/Lattice.h
Lattices/Lattice.tcc
Lattices/LatticeAddNoise.h
Lattices/LatticeApply.h
Lattices/LatticeApply.tcc
Lattices/LatticeBase.h
Lattices/LatticeCache.h
Lattices/LatticeCache.tcc
Lattices/LatticeCleanProgress.h
Lattices/LatticeCleaner.h
Lattices/LatticeCleaner.tcc
Lattices/LatticeConcat.h
Lattices/LatticeConcat.tcc
Lattices/LatticeConvolver.h
Lattices/LatticeConvolver.tcc
Lattices/LatticeExpr.h
Lattices/LatticeExpr.tcc
Lattices/LatticeExprNode.h
Lattices/LatticeFFT.h
Lattices/LatticeFractile.h
Lattices/LatticeFractile.tcc
Lattices/LatticeHistProgress.h
Lattices/LatticeHistSpecialize.h
Lattices/LatticeHistograms.h
Lattices/LatticeHistograms.tcc
Lattices/LatticeIndexer.h
Lattices/LatticeIterInterface.h
Lattices/LatticeIterInterface.tcc
Lattices/LatticeIterator.h
Lattices/LatticeIterator.tcc
Lattices/LatticeLocker.h
Lattices/LatticeNavigator.h
Lattices/LatticeProgress.h
Lattices/LatticeRegion.h
Lattices/LatticeSlice1D.h
Lattices/LatticeSlice1D.tcc
Lattices/LatticeStatistics.h
Lattices/LatticeStatistics.tcc
Lattices/LatticeStatsBase.h
Lattices/LatticeStepper.h
Lattices/LatticeTwoPtCorr.h
Lattices/LatticeTwoPtCorr.tcc
Lattices/LatticeUtilities.h
Lattices/LatticeUtilities.tcc
Lattices/LineCollapser.h
Lattices/LineCollapser.tcc
Lattices/MaskedLattice.h
Lattices/MaskedLattice.tcc
Lattices/MaskedLatticeIterator.h
Lattices/MaskedLatticeIterator.tcc
Lattices/MultiTermLatticeCleaner.h
Lattices/MultiTermLatticeCleaner.tcc
Lattices/PagedArrIter.h
Lattices/PagedArrIter.tcc
Lattices/PagedArray.h
Lattices/PagedArray.tcc
Lattices/PixelCurve1D.h
Lattices/RebinLattice.h
Lattices/RebinLattice.tcc
Lattices/RegionType.h
Lattices/SubLattice.h
Lattices/SubLattice.tcc
Lattices/TempLattice.h
Lattices/TempLattice.tcc
Lattices/TileStepper.h
Lattices/TiledCollapser.h
Lattices/TiledCollapser.tcc
Lattices/TiledLineStepper.h
Lattices/TiledShape.h
DESTINATION include/casacore/lattices/Lattices
)

install (FILES
LatticeMath/Fit2D.h
LatticeMath/LatticeFit.h
DESTINATION include/casacore/lattices/LatticeMath
)

install (FILES
LatticeMath.h
Lattices.h
DESTINATION include/casacore/lattices
)

# The tests
if (BUILD_TESTING)
	add_subdirectory (Lattices/test)
	add_subdirectory (LatticeMath/test)
endif (BUILD_TESTING)



