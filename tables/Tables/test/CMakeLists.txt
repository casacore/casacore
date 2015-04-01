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
ascii2Table
tArrayColumnSlices
tColumnsIndex
tColumnsIndexArray
tConcatRows
tConcatTable
tConcatTable2
tConcatTable3
tMemoryTable
tReadAsciiTable
tReadAsciiTable2
tRefRows
tRefTable
tRowCopier
tScalarRecordColumn
tTable
tTableAccess
tTableCopy
tTableDesc
tTableDescHyper
tTableInfo
tTableIter
tTableKeywords
tTableLock
tTableLockSync
tTableLockSync_2
tTableRecord
tTableRow
tTableVector
tTable_1
tTable_2
tTable_3
tTable_4
)

# Some test sources include a test .h file.
include_directories ( . )

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_tables)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
