set (datafiles
qualityimage.fits          # for tFITSQualityImage
mexinputtest.fits         # for tFITSImgParser
imageStats.fits             # for tImageStatistics
#ngc5921.clean.fits       # for tImageMetaData
#ngc5921.clean.no_freq.no_stokes.fits     # for tImageMetaData
#jyperpixelimage.fits     # for tImageMetaData
imagetestimage.fits     # for tFITSImage
test_image.im/table.dat    # for dImageStatistics. image2fits
test_image.im/table.f0
test_image.im/table.f0_TSM0
test_image.im/table.info
test_image.im/table.lock
test_image.im/logtable/table.dat
test_image.im/logtable/table.f0
test_image.im/logtable/table.info
test_image.im/logtable/table.lock

decon_test.im/logtable/table.info
decon_test.im/logtable/table.lock
decon_test.im/logtable/table.dat
decon_test.im/logtable/table.f0
decon_test.im/table.info
decon_test.im/table.lock
decon_test.im/table.dat
decon_test.im/table.f0_TSM0
decon_test.im/table.f0

)

foreach (file ${datafiles})
    configure_file (${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/${file} COPYONLY)
endforeach (file)


set (tests
dImageInterface
dImageStatistics
dImageSummary
dPagedImage
tExtendImage
tFITSErrorImage
tFITSExtImage
tFITSExtImageII
tFITSImage
tFITSImgParser
tFITSQualityImage
tHDF5Image
tImageAttrHandler
tImageBeamSet
tImageConcat
tImageEmpty
tImageExpr
tImageExpr2
tImageExpr2Gram
tImageExpr3Gram
tImageExprGram
tImageExprParse
tImageExprParse_addDir
tImageInfo
tImageProxy
tImageRegrid
tImageStatistics
tImageStatistics2
tImageUtilities
tLELSpectralIndex
tMIRIADImage
tPagedImage
tPagedImage2
tRebinImage
tSubImage
tTempImage
)

foreach (test ${tests})
	add_executable (${test}  ${test}.cc)
	add_pch_support(${test})
	target_link_libraries (${test} casa_images)
	add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
	add_dependencies(check ${test})
endforeach (test)
