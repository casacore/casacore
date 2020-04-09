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
tStManAll
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
    tAdios2StMan
)

# Some test sources include a test .h file.
include_directories ( . )

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_tables)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
    add_dependencies(check ${test})
endforeach (test)

find_package(Boost COMPONENTS filesystem unit_test_framework)
include_directories(${Boost_INCLUDE_DIR})
target_link_libraries (tStManAll ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

if(MPI_FOUND)
    if(ADIOS2_FOUND)
        foreach (test ${tests_mpi})
            add_executable (${test} ${test}.cc)
            target_link_libraries (${test} casa_tables)
            target_compile_definitions(${test} PRIVATE HAVE_MPI)
            target_compile_definitions(${test} PRIVATE HAVE_ADIOS2)
            add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
            add_dependencies(check ${test})
        endforeach (test)
    endif(ADIOS2_FOUND)
endif(MPI_FOUND)
