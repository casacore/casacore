# Define the table files used in tests.
set (datafiles
../../Tables/test/tTable_2.data_v0/table.dat
../../Tables/test/tTable_2.data_v0/table.f0
../../Tables/test/tTable_2.data_v0/table.f0i0
../../Tables/test/tTable_2.data_v0/table.f1
../../Tables/test/tTable_2.data_v0/table.f2
../../Tables/test/tTable_2.data_v0/table.info
)

# Copying is not needed, thus outcommented.
#foreach (file ${datafiles})
#    configure_file (${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/${file} COPYONLY)
#endforeach (file)


set (tests
tExprGroup
tExprGroupArray
tExprNode
tExprNodeSet
tExprUnitNode
tExprNodeUDF
tRecordExpr
tRecordGram
tRecordGramTable
tTableExprData
tTableGram
tTaQLNode
)

# Only test scripts, no test programs.
set (testscripts
tTableGramGroupAggr
)

# Some test sources include a test .h file.
include_directories ( . )

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_tables)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)

foreach (test ${testscripts})
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
