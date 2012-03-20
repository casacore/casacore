set (datafiles
amp_0.fits
ampErr_0.fits
amp_1.fits
ampErr_1.fits
center_0.fits
centerErr_0.fits
center_1.fits
centerErr_1.fits
collapse_avg_0_1.fits
collapse_avg_0.fits
collapse_avg_2.fits
collapse_in.fits
collapse_max_0_a.fits
collapse_sum_1.fits  
estimates_2gauss.txt
estimates_convolved.txt
fwhm_0.fits
fwhmErr_0.fits
fwhm_1.fits
fwhmErr_1.fits
gaussian_convolved.fits
gaussian_model.fits
gaussian_model_with_noise.fits
gaussian_model_with_noise_model.fits
gaussian_model_with_noise_resid.fits
gauss_no_pol.fits
imagetestimage.fits
input_image_processor_dont_replace_me
image_input_processor.im/mask0/table.info
image_input_processor.im/mask0/table.lock
image_input_processor.im/mask0/table.dat
image_input_processor.im/mask0/table.f0_TSM0
image_input_processor.im/mask0/table.f0
image_input_processor.im/logtable/table.info
image_input_processor.im/logtable/table.lock
image_input_processor.im/logtable/table.dat
image_input_processor.im/logtable/table.f0
image_input_processor.im/table.info
image_input_processor.im/table.lock
image_input_processor.im/table.dat
image_input_processor.im/table.f0_TSM0
image_input_processor.im/table.f0
image_input_processor.im/mybox.rgn
imfit_stokes.fits
jyperbeamkmpersec.fits
jyperpixelimage.fits
mexinputtest.fits
ngc5921.clean.fits
ngc5921.clean.no_freq.no_stokes.fits
reorder_exists.im
reorder_in.fits
specfit_multipix_2gauss.fits
specfit_multipix_poly_2gauss.fits
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
two_gaussian_model.fits
)

foreach (file ${datafiles})
    configure_file (${CMAKE_CURRENT_SOURCE_DIR}/${file} ${CMAKE_CURRENT_BINARY_DIR}/${file} COPYONLY)
endforeach (file)


set (tests
tImageAttrHandler
dImageHistograms
dImageInterface
dImageMoments
dImageProfileFit
dImageStatistics
dImageSummary
dPagedImage
tAntennaResponses
tComponentImager
tExtendImage
tFITSImage
tFITSImgParser
tFITSExtImage
tFITSExtImageII
tHDF5Image
tImageCollapser
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
tImageFitter
tImageInfo
tImageInputProcessor
tImageMetaData
tImagePolarimetry
tImageProfileFitter
tImageRegrid
tImageReorderer
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
