set (datafiles
imageStats.fits
imagetestimage.fits
imageStats.fits
jyperpixelimage.fits
mexinputtest.fits
ngc5921.clean.fits
ngc5921.clean.no_freq.no_stokes.fits
reorder_exists.im
reorder_in.fits
test_image.fits
test_image.im/table.dat
test_image.im/table.f0
test_image.im/table.f0_TSM0
test_image.im/table.info
test_image.im/table.lock
test_image.im/logtable/table.dat
test_image.im/logtable/table.f0
test_image.im/logtable/table.info
test_image.im/logtable/table.lock
)

foreach (file ${datafiles})
    configure_file (${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/${file} COPYONLY)
endforeach (file)


set (tests
dImageHistograms
dImageInterface
dImageMoments
dImageStatistics
dImageSummary
dPagedImage
tAntennaResponses
tComponentImager
tExtendImage
tFITSExtImage
tFITSExtImageII
tFITSImage
tFITSImgParser
tHDF5Image
tImageAttrHandler
tImageConcat
tImageConvolver
tImageDecomposer
tImageEmpty
tImageExpr2Gram
tImageExpr3Gram
tImageExprGram
tImageExprParse_addDir
tImageFFT
tImageInfo
tImageMetaData
tImagePolarimetry
tImageRegrid
tImageReorderer
tImageSourceFinder
tImageStatistics
tImageUtilities
tLELSpectralIndex
tMIRIADImage
tPagedImage
tPagedImage2
tRebinImage
tSepImConvolver
tSubImage
tTempImage
)

foreach (test ${tests})
	add_executable (${test}  ${test}.cc)
	target_link_libraries (${test} casa_images)
	add_test (${test} ${CMAKE_SOURCE_DIR}/cmake/cmake_assay ./${test})
endforeach (test)
