set (tests
tAipsIOCarray
tAipsIO
tBucketBuffered
tBucketCache
tBucketFile
tBucketMapped
tByteIO
tByteSink
tByteSinkSource
tFilebufIO
tFileIO
tLargeFileIO
tLockFile
tMFFileIO
tMappedIO
tMMapIO
tMultiFile
tMultiHDF5
tTapeIO
tTypeIO
)

foreach (test ${tests})
    add_executable (${test} ${test}.cc)
    target_link_libraries (${test} casa_casa)
    add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
