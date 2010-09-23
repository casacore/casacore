set (tests
dLattice
dPagedArray
tArrayLattice
tCurvedLattice2D
tExtendLattice
tHDF5Iterator
tHDF5Lattice
tLatticeAddNoise
tLatticeApply2
tLatticeApply
tLatticeCache
tLatticeConcat
tLatticeConvolver
tLatticeExpr2
tLatticeExpr2Node
tLatticeExpr3
tLatticeExpr3Node
tLatticeExpr
tLatticeExprNode
tLatticeFFT
tLatticeFractile
tLatticeHistograms
tLatticeIndexer
tLatticeIterator
tLatticeLocker
tLatticeRegion
tLatticeSlice1D
tLatticeStatistics
tLatticeStepper
tLatticeTwoPtCorr
tLatticeUtilities
tLCComplement
tLCConcatenation
tLCDifference
tLCExtension
tLCIntersection
tLCLELMask
tLCMask
tLCPagedMask
tLCPixelSet
tLCPolygon
tLCRegion
tLCSlicer
tLCStretch
tLCUnion
tLELAttribute
tLEL
tLELMedian
tPagedArray
tPixelCurve1D
tRebinLattice
tSubLattice
tTempLattice
tTiledLineStepper
tTiledShape
tTileStepper
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_lattices)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
