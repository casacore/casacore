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
tExprNodeSetElem
tExprNodeSetOpt
tExprUnitNode
tExprNodeUDF
tMArray
tMArrayMath
tMArrayUtil
tRecordExpr
tRecordGram
tRecordGramTable
tTableExprData
tTableGram
tTableGramError
tTableGramFunc
tTaQLNode
)

# Only test scripts, no test programs.
set (testscripts
ttaql
tTableGramCretab
tTableGramAlttab
tTableGramUpdate
tTableGramMasked
tTableGramNull
tTableGramGroupAggr
tTableGramGroupAggrAll
tTableGramGroupAggrMaskAll
)

# Some test sources include a test .h file.
include_directories ( . )

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    add_pch_support(${test})
    target_link_libraries (${test} casa_tables)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)

foreach (test ${testscripts})
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
