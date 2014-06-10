set (tests
tBinTable
tfits1
tfits2
tfits3
tfits4
tfits5
tfits_ascTbl2
tfits_ascTbl
tfits_binTbl1
tfits_binTbl2
tFITS
tFITSDateUtil
tFITSHistoryUtil
tfits_imgExt2
tFITSKeywordUtil
tfits_priGrp
tfitsread_data
tfitsskip_all
tfitsskip
tfitsskip_hdu
tFITSSpectralUtil
t_priArr_imgExt
tfitsreader
)

foreach (test ${tests})
	add_executable (${test} ${test}.cc)
	target_link_libraries (${test} casa_fits)
	add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
