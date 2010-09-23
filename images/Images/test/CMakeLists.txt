set (datafiles
imagetestimage.fits
ngc5921.clean.fits
ngc5921.clean.no_freq.no_stokes.fits
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
dImageProfileFit
dImageStatistics
dImageSummary
dPagedImage
tComponentImager
tExtendImage
tFITSImage
tHDF5Image
tImageConcat
tImageConvolver
tImageDecomposer
tImageEmpty
tImageExpr2Gram
tImageExpr3Gram
tImageExprGram
tImageExprParse_addDir
tImageFFT
tImageFit1D
tImageInfo
tImageMetaData
tImagePolarimetry
tImageRegrid
tImageSourceFinder
tImageUtilities
tLELSpectralIndex
tMIRIADImage
tPagedImage2
tPagedImage
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
