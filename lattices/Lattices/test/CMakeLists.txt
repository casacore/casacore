set (tests
dLattice
dPagedArray
tArrayLattice
tCurvedLattice2D
tExtendLattice
tHDF5Iterator
tHDF5Lattice
tLatticeCache
tLatticeConcat
tLatticeIndexer
tLatticeIterator
tLatticeLocker
tLatticePerf
tLatticeStepper
tLatticeUtilities
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
