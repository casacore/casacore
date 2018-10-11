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

set (tests_mpi
tStMan
tStMan1
)

# Some test sources include a test .h file.
include_directories ( . )

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_tables)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)

foreach (test ${tests_mpi})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_tables)
    if(MPI_FOUND)
        target_compile_definitions(${test} PRIVATE HAVE_MPI)
        target_link_libraries(${test} MPI::MPI_C)
        if(ADIOS2_FOUND)
            target_compile_definitions(${test} PRIVATE HAVE_ADIOS2)
            target_link_libraries(${test} ${ADIOS2_LIBRARIES} MPI::MPI_C)
        endif(ADIOS2_FOUND)
    endif(MPI_FOUND)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)
