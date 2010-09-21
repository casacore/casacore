#
# CASA Tables
#

set (
parser_inputs
RecordGram
TableGram
)

if (APPLE)
    foreach (src ${parser_inputs})
        BISON_TARGET (${src} Tables/${src}.yy ${CMAKE_CURRENT_BINARY_DIR}/${src} COMPILE_FLAGS "-y -p${src}")
        execute_process (COMMAND ${CMAKE_COMMAND} -E create_symlink ${src} ${src}.ycc WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
        FLEX_TARGET (${src} Tables/${src}.ll ${CMAKE_CURRENT_BINARY_DIR}/${src}.lcc COMPILE_FLAGS "-P${src}")
    endforeach (src)
else (APPLE)
    foreach (src ${parser_inputs})
        BISON_TARGET (${src} Tables/${src}.yy ${CMAKE_CURRENT_BINARY_DIR}/${src}.ycc COMPILE_FLAGS "-y -p ${src}")
        FLEX_TARGET (${src} Tables/${src}.ll ${CMAKE_CURRENT_BINARY_DIR}/${src}.lcc COMPILE_FLAGS "-P${src}")
    endforeach (src)
endif (APPLE)


include_directories (${CMAKE_CURRENT_BINARY_DIR})

add_library (casa_tables
Tables/BaseColDesc.cc
Tables/BaseColumn.cc
Tables/BaseTabIter.cc
Tables/BaseTable.cc
Tables/BitFlagsEngine.cc
Tables/ColDescSet.cc
Tables/ColumnCache.cc
Tables/ColumnDesc.cc
Tables/ColumnSet.cc
Tables/ColumnsIndex.cc
Tables/ColumnsIndexArray.cc
Tables/CompressComplex.cc
Tables/CompressFloat.cc
Tables/ConcatColumn.cc
Tables/ConcatRows.cc
Tables/ConcatTable.cc
Tables/DataManAccessor.cc
Tables/DataManError.cc
Tables/DataManInfo.cc
Tables/DataManager.cc
Tables/ExprConeNode.cc
Tables/ExprDerNode.cc
Tables/ExprDerNodeArray.cc
Tables/ExprFuncNode.cc
Tables/ExprFuncNodeArray.cc
Tables/ExprLogicNode.cc
Tables/ExprLogicNodeArray.cc
Tables/ExprMathNode.cc
Tables/ExprMathNodeArray.cc
Tables/ExprNode.cc
Tables/ExprNodeArray.cc
Tables/ExprNodeRecord.cc
Tables/ExprNodeRep.cc
Tables/ExprNodeSet.cc
Tables/ExprRange.cc
Tables/ExprUDFNode.cc
Tables/ExprUDFNodeArray.cc
Tables/ExprUnitNode.cc
Tables/ExternalLockSync.cc
Tables/ForwardCol.cc
Tables/ForwardColRow.cc
Tables/ISMBase.cc
Tables/ISMBucket.cc
Tables/ISMColumn.cc
Tables/ISMIndColumn.cc
Tables/ISMIndex.cc
Tables/IncrStManAccessor.cc
Tables/IncrementalStMan.cc
Tables/MSMBase.cc
Tables/MSMColumn.cc
Tables/MSMDirColumn.cc
Tables/MSMIndColumn.cc
Tables/MemoryStMan.cc
Tables/MemoryTable.cc
Tables/NullTable.cc
Tables/PlainColumn.cc
Tables/PlainTable.cc
Tables/ReadAsciiTable.cc
Tables/RecordExpr.cc
Tables/RecordGram.cc
Tables/RefColumn.cc
Tables/RefRows.cc
Tables/RefTable.cc
Tables/RowCopier.cc
Tables/SSMBase.cc
Tables/SSMColumn.cc
Tables/SSMDirColumn.cc
Tables/SSMIndColumn.cc
Tables/SSMIndStringColumn.cc
Tables/SSMIndex.cc
Tables/SSMStringHandler.cc
Tables/ScaRecordColData.cc
Tables/ScaRecordColDesc.cc
Tables/SetupNewTab.cc
Tables/StArrAipsIO.cc
Tables/StArrayFile.cc
Tables/StIndArrAIO.cc
Tables/StIndArray.cc
Tables/StManAipsIO.cc
Tables/StManColumn.cc
Tables/StandardStMan.cc
Tables/StandardStManAccessor.cc
Tables/SubTabDesc.cc
Tables/TSMColumn.cc
Tables/TSMCoordColumn.cc
Tables/TSMCube.cc
Tables/TSMCubeBuff.cc
Tables/TSMCubeMMap.cc
Tables/TSMDataColumn.cc
Tables/TSMFile.cc
Tables/TSMIdColumn.cc
Tables/TSMOption.cc
Tables/TSMShape.cc
Tables/TaQLNode.cc
Tables/TaQLNodeDer.cc
Tables/TaQLNodeHandler.cc
Tables/TaQLNodeRep.cc
Tables/TaQLNodeResult.cc
Tables/TaQLNodeVisitor.cc
Tables/TaQLResult.cc
Tables/TaQLStyle.cc
Tables/TabPath.cc
Tables/Table.cc
Tables/TableAttr.cc
Tables/TableCache.cc
Tables/TableColumn.cc
Tables/TableCopy.cc
Tables/TableDesc.cc
Tables/TableError.cc
Tables/TableExprData.cc
Tables/TableExprId.cc
Tables/TableGram.cc
Tables/TableIndexProxy.cc
Tables/TableInfo.cc
Tables/TableIter.cc
Tables/TableIterProxy.cc
Tables/TableKeyword.cc
Tables/TableLock.cc
Tables/TableLockData.cc
Tables/TableLocker.cc
Tables/TableParse.cc
Tables/TableProxy.cc
Tables/TableRecord.cc
Tables/TableRecordRep.cc
Tables/TableRow.cc
Tables/TableRowProxy.cc
Tables/TableSyncData.cc
Tables/TiledCellStMan.cc
Tables/TiledColumnStMan.cc
Tables/TiledDataStMan.cc
Tables/TiledDataStManAccessor.cc
Tables/TiledFileAccess.cc
Tables/TiledFileHelper.cc
Tables/TiledShapeStMan.cc
Tables/TiledStMan.cc
Tables/TiledStManAccessor.cc
Tables/UDFBase.cc
Tables/VirtColEng.cc
Tables/VirtualTaQLColumn.cc
LogTables/TableLogSink.cc
LogTables/NewFile.cc
LogTables/LoggerHolder.cc
LogTables/LogFilterTaql.cc
LogTables/LogFilterExpr.cc
${BISON_RecordGram_OUTPUTS}
${FLEX_RecordGram_OUTPUTS}
${BISON_TableGram_OUTPUTS}
${FLEX_TableGram_OUTPUTS}
)

target_link_libraries (casa_tables casa_casa)

add_subdirectory (apps)

install (TARGETS casa_tables
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
Tables/ArrColData.h
Tables/ArrColData.tcc
Tables/ArrColDesc.h
Tables/ArrColDesc.tcc
Tables/ArrayColumn.h
Tables/ArrayColumn.tcc
Tables/ArrayColumnFunc.h
Tables/BaseColDesc.h
Tables/BaseColumn.h
Tables/BaseMappedArrayEngine.h
Tables/BaseMappedArrayEngine.tcc
Tables/BaseTabIter.h
Tables/BaseTable.h
Tables/BitFlagsEngine.h
Tables/BitFlagsEngine.tcc
Tables/ColDescSet.h
Tables/ColumnCache.h
Tables/ColumnDesc.h
Tables/ColumnSet.h
Tables/ColumnsIndex.h
Tables/ColumnsIndexArray.h
Tables/CompressComplex.h
Tables/CompressFloat.h
Tables/ConcatColumn.h
Tables/ConcatRows.h
Tables/ConcatScalarColumn.h
Tables/ConcatScalarColumn.tcc
Tables/ConcatTable.h
Tables/DataManAccessor.h
Tables/DataManError.h
Tables/DataManInfo.h
Tables/DataManager.h
Tables/ExprConeNode.h
Tables/ExprDerNode.h
Tables/ExprDerNodeArray.h
Tables/ExprFuncNode.h
Tables/ExprFuncNodeArray.h
Tables/ExprLogicNode.h
Tables/ExprLogicNodeArray.h
Tables/ExprMathNode.h
Tables/ExprMathNodeArray.h
Tables/ExprNode.h
Tables/ExprNodeArray.h
Tables/ExprNodeRecord.h
Tables/ExprNodeRep.h
Tables/ExprNodeSet.h
Tables/ExprRange.h
Tables/ExprUDFNode.h
Tables/ExprUDFNodeArray.h
Tables/ExprUnitNode.h
Tables/ExternalLockSync.h
Tables/ForwardCol.h
Tables/ForwardColRow.h
Tables/ISMBase.h
Tables/ISMBucket.h
Tables/ISMColumn.h
Tables/ISMIndColumn.h
Tables/ISMIndex.h
Tables/IncrStManAccessor.h
Tables/IncrementalStMan.h
Tables/MSMBase.h
Tables/MSMColumn.h
Tables/MSMDirColumn.h
Tables/MSMIndColumn.h
Tables/MappedArrayEngine.h
Tables/MappedArrayEngine.tcc
Tables/MemoryStMan.h
Tables/MemoryTable.h
Tables/NullTable.h
Tables/PlainColumn.h
Tables/PlainTable.h
Tables/ReadAsciiTable.h
Tables/RecordExpr.h
Tables/RecordGram.h
Tables/RefColumn.h
Tables/RefRows.h
Tables/RefTable.h
Tables/RetypedArrayEngine.h
Tables/RetypedArrayEngine.tcc
Tables/RetypedArraySetGet.h
Tables/RetypedArraySetGet.tcc
Tables/RowCopier.h
Tables/SSMBase.h
Tables/SSMColumn.h
Tables/SSMDirColumn.h
Tables/SSMIndColumn.h
Tables/SSMIndStringColumn.h
Tables/SSMIndex.h
Tables/SSMStringHandler.h
Tables/ScaColData.h
Tables/ScaColData.tcc
Tables/ScaColDesc.h
Tables/ScaColDesc.tcc
Tables/ScaRecordColData.h
Tables/ScaRecordColDesc.h
Tables/ScalarColumn.h
Tables/ScalarColumn.tcc
Tables/ScaledArrayEngine.h
Tables/ScaledArrayEngine.tcc
Tables/ScaledComplexData.h
Tables/ScaledComplexData.tcc
Tables/SetupNewTab.h
Tables/StArrAipsIO.h
Tables/StArrayFile.h
Tables/StIndArrAIO.h
Tables/StIndArray.h
Tables/StManAipsIO.h
Tables/StManColumn.h
Tables/StandardStMan.h
Tables/StandardStManAccessor.h
Tables/SubTabDesc.h
Tables/TSMColumn.h
Tables/TSMCoordColumn.h
Tables/TSMCube.h
Tables/TSMCubeBuff.h
Tables/TSMCubeMMap.h
Tables/TSMDataColumn.h
Tables/TSMFile.h
Tables/TSMIdColumn.h
Tables/TSMOption.h
Tables/TSMShape.h
Tables/TVec.h
Tables/TVec.tcc
Tables/TVecLogic.h
Tables/TVecLogic.tcc
Tables/TVecMath.h
Tables/TVecMath.tcc
Tables/TVecScaCol.h
Tables/TVecScaCol.tcc
Tables/TVecTemp.h
Tables/TVecTemp.tcc
Tables/TaQLNode.h
Tables/TaQLNodeDer.h
Tables/TaQLNodeHandler.h
Tables/TaQLNodeRep.h
Tables/TaQLNodeResult.h
Tables/TaQLNodeVisitor.h
Tables/TaQLResult.h
Tables/TaQLStyle.h
Tables/TabPath.h
Tables/TabVecLogic.h
Tables/TabVecLogic.tcc
Tables/TabVecMath.h
Tables/TabVecMath.tcc
Tables/Table.h
Tables/TableAttr.h
Tables/TableCache.h
Tables/TableColumn.h
Tables/TableCopy.h
Tables/TableDesc.h
Tables/TableError.h
Tables/TableExprData.h
Tables/TableExprId.h
Tables/TableGram.h
Tables/TableIndexProxy.h
Tables/TableInfo.h
Tables/TableIter.h
Tables/TableIterProxy.h
Tables/TableKeyword.h
Tables/TableLock.h
Tables/TableLockData.h
Tables/TableLocker.h
Tables/TableParse.h
Tables/TableProxy.h
Tables/TableRecord.h
Tables/TableRecordRep.h
Tables/TableRow.h
Tables/TableRowProxy.h
Tables/TableSyncData.h
Tables/TableVector.h
Tables/TableVector.tcc
Tables/TiledCellStMan.h
Tables/TiledColumnStMan.h
Tables/TiledDataStMan.h
Tables/TiledDataStManAccessor.h
Tables/TiledFileAccess.h
Tables/TiledFileHelper.h
Tables/TiledShapeStMan.h
Tables/TiledStMan.h
Tables/TiledStManAccessor.h
Tables/UDFBase.h
Tables/VSCEngine.h
Tables/VSCEngine.tcc
Tables/VirtArrCol.h
Tables/VirtArrCol.tcc
Tables/VirtColEng.h
Tables/VirtScaCol.h
Tables/VirtScaCol.tcc
Tables/VirtualTaQLColumn.h
DESTINATION include/casacore/tables/Tables
)

install (FILES
LogTables/LogFilterTaql.h
LogTables/LoggerHolder.h
LogTables/TableLogSink.h
LogTables/LogFilterExpr.h
LogTables/NewFile.h
DESTINATION include/casacore/tables/LogTables
)

install (FILES
LogTables.h
Tables.h
DESTINATION include/casacore/tables
)

if (BUILD_TESTING)
    add_subdirectory (Tables/test)
    add_subdirectory (LogTables/test)
endif (BUILD_TESTING)
