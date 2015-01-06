#
# CASA Tables
#

set (
parser_inputs
RecordGram
TableGram
)

 
foreach (src ${parser_inputs})
    BISON_TARGET (${src} TaQL/${src}.yy ${CMAKE_CURRENT_BINARY_DIR}/${src}.ycc COMPILE_FLAGS "-y -p ${src}")
    FLEX_TARGET (${src} TaQL/${src}.ll ${CMAKE_CURRENT_BINARY_DIR}/${src}.lcc COMPILE_FLAGS "-P${src}")
endforeach (src)
 

include_directories (${CMAKE_CURRENT_BINARY_DIR})

add_library (casa_tables
Tables/BaseColDesc.cc
Tables/BaseColumn.cc
Tables/BaseTabIter.cc
Tables/BaseTable.cc
Tables/ColDescSet.cc
Tables/ColumnCache.cc
Tables/ColumnDesc.cc
Tables/ColumnSet.cc
Tables/ColumnsIndex.cc
Tables/ColumnsIndexArray.cc
Tables/ConcatColumn.cc
Tables/ConcatRows.cc
Tables/ConcatTable.cc
Tables/ExternalLockSync.cc
Tables/MemoryTable.cc
Tables/NullTable.cc
Tables/PlainColumn.cc
Tables/PlainTable.cc
Tables/ReadAsciiTable.cc
Tables/RefColumn.cc
Tables/RefRows.cc
Tables/RefTable.cc
Tables/RowCopier.cc
Tables/ScaRecordColData.cc
Tables/ScaRecordColDesc.cc
Tables/SetupNewTab.cc
Tables/StorageOption.cc
Tables/SubTabDesc.cc
Tables/TabPath.cc
Tables/Table.cc
Tables/TableAttr.cc
Tables/TableCache.cc
Tables/TableColumn.cc
Tables/TableCopy.cc
Tables/TableDesc.cc
Tables/TableError.cc
Tables/TableIndexProxy.cc
Tables/TableInfo.cc
Tables/TableIter.cc
Tables/TableIterProxy.cc
Tables/TableKeyword.cc
Tables/TableLock.cc
Tables/TableLockData.cc
Tables/TableLocker.cc
Tables/TableProxy.cc
Tables/TableRecord.cc
Tables/TableRecordRep.cc
Tables/TableRow.cc
Tables/TableRowProxy.cc
Tables/TableSyncData.cc
Tables/TableTrace.cc
DataMan/BitFlagsEngine.cc
DataMan/CompressComplex.cc
DataMan/CompressFloat.cc
DataMan/DataManAccessor.cc
DataMan/DataManError.cc
DataMan/DataManInfo.cc
DataMan/DataManager.cc
DataMan/ForwardCol.cc
DataMan/ForwardColRow.cc
DataMan/ISMBase.cc
DataMan/ISMBucket.cc
DataMan/ISMColumn.cc
DataMan/ISMIndColumn.cc
DataMan/ISMIndex.cc
DataMan/IncrStManAccessor.cc
DataMan/IncrementalStMan.cc
DataMan/MSMBase.cc
DataMan/MSMColumn.cc
DataMan/MSMDirColumn.cc
DataMan/MSMIndColumn.cc
DataMan/MemoryStMan.cc
DataMan/SSMBase.cc
DataMan/SSMColumn.cc
DataMan/SSMDirColumn.cc
DataMan/SSMIndColumn.cc
DataMan/SSMIndStringColumn.cc
DataMan/SSMIndex.cc
DataMan/SSMStringHandler.cc
DataMan/StArrAipsIO.cc
DataMan/StArrayFile.cc
DataMan/StIndArrAIO.cc
DataMan/StIndArray.cc
DataMan/StManAipsIO.cc
DataMan/StManColumn.cc
DataMan/StandardStMan.cc
DataMan/StandardStManAccessor.cc
DataMan/TSMColumn.cc
DataMan/TSMCoordColumn.cc
DataMan/TSMCube.cc
DataMan/TSMCubeBuff.cc
DataMan/TSMCubeMMap.cc
DataMan/TSMDataColumn.cc
DataMan/TSMFile.cc
DataMan/TSMIdColumn.cc
DataMan/TSMOption.cc
DataMan/TSMShape.cc
DataMan/TiledCellStMan.cc
DataMan/TiledColumnStMan.cc
DataMan/TiledDataStMan.cc
DataMan/TiledDataStManAccessor.cc
DataMan/TiledFileAccess.cc
DataMan/TiledFileHelper.cc
DataMan/TiledShapeStMan.cc
DataMan/TiledStMan.cc
DataMan/TiledStManAccessor.cc
DataMan/VirtColEng.cc
DataMan/VirtualTaQLColumn.cc
TaQL/ExprAggrNode.cc
TaQL/ExprAggrNodeArray.cc
TaQL/ExprConeNode.cc
TaQL/ExprDerNode.cc
TaQL/ExprDerNodeArray.cc
TaQL/ExprFuncNode.cc
TaQL/ExprFuncNodeArray.cc
TaQL/ExprGroup.cc
TaQL/ExprGroupAggrFunc.cc
TaQL/ExprGroupAggrFuncArray.cc
TaQL/ExprLogicNode.cc
TaQL/ExprLogicNodeArray.cc
TaQL/ExprMathNode.cc
TaQL/ExprMathNodeArray.cc
TaQL/ExprNode.cc
TaQL/ExprNodeArray.cc
TaQL/ExprNodeRecord.cc
TaQL/ExprNodeRep.cc
TaQL/ExprNodeSet.cc
TaQL/ExprRange.cc
TaQL/ExprUDFNode.cc
TaQL/ExprUDFNodeArray.cc
TaQL/ExprUnitNode.cc
TaQL/RecordExpr.cc
TaQL/RecordGram.cc
TaQL/TaQLNode.cc
TaQL/TaQLNodeDer.cc
TaQL/TaQLNodeHandler.cc
TaQL/TaQLNodeRep.cc
TaQL/TaQLNodeResult.cc
TaQL/TaQLNodeVisitor.cc
TaQL/TaQLResult.cc
TaQL/TaQLStyle.cc
TaQL/TableExprData.cc
TaQL/TableExprId.cc
TaQL/TableGram.cc
TaQL/TableParse.cc
TaQL/UDFBase.cc
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
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
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
Tables/BaseTabIter.h
Tables/BaseTable.h
Tables/ColDescSet.h
Tables/ColumnCache.h
Tables/ColumnDesc.h
Tables/ColumnSet.h
Tables/ColumnsIndex.h
Tables/ColumnsIndexArray.h
Tables/ConcatColumn.h
Tables/ConcatRows.h
Tables/ConcatScalarColumn.h
Tables/ConcatScalarColumn.tcc
Tables/ConcatTable.h
Tables/ExternalLockSync.h
Tables/MemoryTable.h
Tables/NullTable.h
Tables/PlainColumn.h
Tables/PlainTable.h
Tables/ReadAsciiTable.h
Tables/RefColumn.h
Tables/RefRows.h
Tables/RefTable.h
Tables/RowCopier.h
Tables/ScaColData.h
Tables/ScaColData.tcc
Tables/ScaColDesc.h
Tables/ScaColDesc.tcc
Tables/ScaRecordColData.h
Tables/ScaRecordColDesc.h
Tables/ScalarColumn.h
Tables/ScalarColumn.tcc
Tables/SetupNewTab.h
Tables/StorageOption.h
Tables/SubTabDesc.h
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
Tables/TableIndexProxy.h
Tables/TableInfo.h
Tables/TableIter.h
Tables/TableIterProxy.h
Tables/TableKeyword.h
Tables/TableLock.h
Tables/TableLockData.h
Tables/TableLocker.h
Tables/TableProxy.h
Tables/TableRecord.h
Tables/TableRecordRep.h
Tables/TableRow.h
Tables/TableRowProxy.h
Tables/TableSyncData.h
Tables/TableTrace.h
Tables/TableVector.h
Tables/TableVector.tcc
DESTINATION include/casacore/tables/Tables
)

install (FILES
DataMan/BaseMappedArrayEngine.h
DataMan/BaseMappedArrayEngine.tcc
DataMan/BitFlagsEngine.h
DataMan/BitFlagsEngine.tcc
DataMan/CompressComplex.h
DataMan/CompressFloat.h
DataMan/DataManAccessor.h
DataMan/DataManError.h
DataMan/DataManInfo.h
DataMan/DataManager.h
DataMan/ForwardCol.h
DataMan/ForwardColRow.h
DataMan/ISMBase.h
DataMan/ISMBucket.h
DataMan/ISMColumn.h
DataMan/ISMIndColumn.h
DataMan/ISMIndex.h
DataMan/IncrStManAccessor.h
DataMan/IncrementalStMan.h
DataMan/MSMBase.h
DataMan/MSMColumn.h
DataMan/MSMDirColumn.h
DataMan/MSMIndColumn.h
DataMan/MappedArrayEngine.h
DataMan/MappedArrayEngine.tcc
DataMan/MemoryStMan.h
DataMan/RetypedArrayEngine.h
DataMan/RetypedArrayEngine.tcc
DataMan/RetypedArraySetGet.h
DataMan/RetypedArraySetGet.tcc
DataMan/SSMBase.h
DataMan/SSMColumn.h
DataMan/SSMDirColumn.h
DataMan/SSMIndColumn.h
DataMan/SSMIndStringColumn.h
DataMan/SSMIndex.h
DataMan/SSMStringHandler.h
DataMan/ScaledArrayEngine.h
DataMan/ScaledArrayEngine.tcc
DataMan/ScaledComplexData.h
DataMan/ScaledComplexData.tcc
DataMan/StArrAipsIO.h
DataMan/StArrayFile.h
DataMan/StIndArrAIO.h
DataMan/StIndArray.h
DataMan/StManAipsIO.h
DataMan/StManColumn.h
DataMan/StandardStMan.h
DataMan/StandardStManAccessor.h
DataMan/TSMColumn.h
DataMan/TSMCoordColumn.h
DataMan/TSMCube.h
DataMan/TSMCubeBuff.h
DataMan/TSMCubeMMap.h
DataMan/TSMDataColumn.h
DataMan/TSMFile.h
DataMan/TSMIdColumn.h
DataMan/TSMOption.h
DataMan/TSMShape.h
DataMan/TiledCellStMan.h
DataMan/TiledColumnStMan.h
DataMan/TiledDataStMan.h
DataMan/TiledDataStManAccessor.h
DataMan/TiledFileAccess.h
DataMan/TiledFileHelper.h
DataMan/TiledShapeStMan.h
DataMan/TiledStMan.h
DataMan/TiledStManAccessor.h
DataMan/VSCEngine.h
DataMan/VSCEngine.tcc
DataMan/VirtArrCol.h
DataMan/VirtArrCol.tcc
DataMan/VirtColEng.h
DataMan/VirtScaCol.h
DataMan/VirtScaCol.tcc
DataMan/VirtualTaQLColumn.h
DESTINATION include/casacore/tables/DataMan
)

install (FILES
TaQL/ExprAggrNode.h
TaQL/ExprAggrNodeArray.h
TaQL/ExprConeNode.h
TaQL/ExprDerNode.h
TaQL/ExprDerNodeArray.h
TaQL/ExprFuncNode.h
TaQL/ExprFuncNodeArray.h
TaQL/ExprGroup.h
TaQL/ExprGroupAggrFunc.h
TaQL/ExprGroupAggrFuncArray.h
TaQL/ExprLogicNode.h
TaQL/ExprLogicNodeArray.h
TaQL/ExprMathNode.h
TaQL/ExprMathNodeArray.h
TaQL/ExprNode.h
TaQL/ExprNodeArray.h
TaQL/ExprNodeRecord.h
TaQL/ExprNodeRep.h
TaQL/ExprNodeSet.h
TaQL/ExprRange.h
TaQL/ExprUDFNode.h
TaQL/ExprUDFNodeArray.h
TaQL/ExprUnitNode.h
TaQL/RecordExpr.h
TaQL/RecordGram.h
TaQL/TaQLNode.h
TaQL/TaQLNodeDer.h
TaQL/TaQLNodeHandler.h
TaQL/TaQLNodeRep.h
TaQL/TaQLNodeResult.h
TaQL/TaQLNodeVisitor.h
TaQL/TaQLResult.h
TaQL/TaQLStyle.h
TaQL/TableExprData.h
TaQL/TableExprId.h
TaQL/TableExprIdAggr.h
TaQL/TableGram.h
TaQL/TableParse.h
TaQL/UDFBase.h
DESTINATION include/casacore/tables/TaQL
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
DataMan.h
LogTables.h
Tables.h
TaQL.h
DESTINATION include/casacore/tables
)

# Temporary install of stub header files.
install (FILES
Tables/BitFlagsEngine.h
Tables/CompressComplex.h
Tables/CompressFloat.h
Tables/DataManAccessor.h
Tables/DataManError.h
Tables/DataManager.h
Tables/ExprNode.h
Tables/ExprNodeSet.h
Tables/ForwardCol.h
Tables/ForwardColRow.h
Tables/IncrStManAccessor.h
Tables/IncrementalStMan.h
Tables/RecordGram.h
Tables/ScaledArrayEngine.h
Tables/ScaledComplexData.h
Tables/StManAipsIO.h
Tables/StandardStMan.h
Tables/StandardStManAccessor.h
Tables/TSMOption.h
Tables/TableExprId.h
Tables/TableParse.h
Tables/TiledCellStMan.h
Tables/TiledColumnStMan.h
Tables/TiledDataStMan.h
Tables/TiledDataStManAccessor.h
Tables/TiledShapeStMan.h
Tables/TiledStManAccessor.h
Tables/UDFBase.h
DESTINATION include/casacore/tables/Tables
)

if (BUILD_TESTING)
    add_subdirectory (Tables/test)
    add_subdirectory (DataMan/test)
    add_subdirectory (TaQL/test)
    add_subdirectory (LogTables/test)
endif (BUILD_TESTING)
