set (tests
dRetypedArrayEngine
dVSCEngine
dVirtColEng
nISMBucket
tBitFlagsEngine
tCompressComplex
tCompressFloat
tDataManInfo
tExternalStMan
tExternalStManNew
tForwardCol
tForwardColRow
tIncrementalStMan
tMappedArrayEngine
tMemoryStMan
tScaledArrayEngine
tScaledComplexData
tSSMAddRemove
tSSMStringHandler
tStandardStMan
tStArrayFile
tStMan
tStMan1
tStManAll
tTiledBool
tTiledCellStM_1
tTiledCellStMan
tTiledColumnStMan
tTiledColumnStManPerf
tTiledDataStM_1
tTiledDataStMan
tTiledEmpty
tTiledFileAccess
tTiledShapeStM_1
tTiledShapeStM_2
tTiledShapeStMan
tTiledStMan
tTSMShape
tVirtColEng
tVirtualTaQLColumn
tVACEngine
tVSCEngine
)

if (USE_ADIOS2)
    list(APPEND tests tAdios2StMan)
endif()

# Some test sources include a test .h file.
include_directories ( . )

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    add_pch_support(${test})
    target_link_libraries (${test} casa_tables)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
