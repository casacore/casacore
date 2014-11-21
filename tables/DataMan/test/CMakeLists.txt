set (datafiles
tTable_2.data_v0/table.dat
tTable_2.data_v0/table.f0
tTable_2.data_v0/table.f0i0
tTable_2.data_v0/table.f1
tTable_2.data_v0/table.f2
tTable_2.data_v0/table.info
tTable_2.data_v1/table.dat
tTable_2.data_v1/table.f0
tTable_2.data_v1/table.f0i0
tTable_2.data_v1/table.f1
tTable_2.data_v1/table.f2
tTable_2.data_v1/table.info
)

foreach (file ${datafiles})
    configure_file (${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/${file} COPYONLY)
endforeach (file)


set (tests
dRetypedArrayEngine
dVSCEngine
dVirtColEng
nISMBucket
tBitFlagsEngine
tCompressComplex
tCompressFloat
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
tTiledBool
tTiledCellStM_1
tTiledCellStMan
tTiledColumnStMan
tTiledDataStM_1
tTiledDataStMan
tTiledEmpty
tTiledFileAccess
tTiledShapeStM_1
tTiledShapeStMan
tTiledStMan
tTSMShape
tVirtColEng
tVirtualTaQLColumn
tVSCEngine
)

# Some test sources include a test .h file.
include_directories ( . )

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_tables)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
