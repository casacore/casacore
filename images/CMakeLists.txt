#
# CASA Images
#
if (APPLE)
    BISON_TARGET (ImageExprGram Images/ImageExprGram.yy ${CMAKE_CURRENT_BINARY_DIR}/ImageExprGram COMPILE_FLAGS "-p ImageExprGram")
    execute_process (COMMAND ${CMAKE_COMMAND} -E create_symlink ImageExprGram ImageExprGram.ycc WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    FLEX_TARGET (ImageExprGram Images/ImageExprGram.ll ${CMAKE_CURRENT_BINARY_DIR}/ImageExprGram.lcc COMPILE_FLAGS "-PImageExprGram")
else (APPLE)
    BISON_TARGET (ImageExprGram Images/ImageExprGram.yy ${CMAKE_CURRENT_BINARY_DIR}/ImageExprGram.ycc COMPILE_FLAGS "-p ImageExprGram")
    FLEX_TARGET (ImageExprGram Images/ImageExprGram.ll ${CMAKE_CURRENT_BINARY_DIR}/ImageExprGram.lcc COMPILE_FLAGS "-PImageExprGram")
endif (APPLE)

include_directories (${CMAKE_CURRENT_BINARY_DIR})

add_library (casa_images
Regions/WCPolygon.cc
Regions/RegionManager.cc
Regions/WCLELMask.cc
Regions/WCUnion.cc
Regions/WCRegion2.cc
Regions/RegionHandlerMemory.cc
Regions/WCExtension.cc
Regions/RegionHandlerTable.cc
Regions/WCCompound.cc
Regions/WCRegion.cc
Regions/WCIntersection.cc
Regions/WCConcatenation.cc
Regions/ImageRegion.cc
Regions/RFReaderWriter.cc
Regions/RegionHandler.cc
Regions/WCDifference.cc
Regions/WCBox.cc
Regions/AipsIOReaderWriter.cc
Regions/WCComplement.cc
Regions/RegionHandlerHDF5.cc
Images/ImageInfo.cc
Images/ImageMetaData.cc
Images/ImagePolarimetry.cc
Images/ImageExprGram.cc
Images/MaskSpecifier.cc
Images/ImageMomentsProgress.cc
Images/ImageAnalysis.cc
Images/FITSImage.cc
Images/PagedImage2.cc
Images/ImagePolProxy.cc
Images/ImageProfileFit.cc
Images/ImageExprParse.cc
Images/ImageOpener.cc
Images/LELImageCoord.cc
Images/ImageFFT.cc
Images/ImageUtilities.cc
Images/ImageFITS2Converter.cc
Images/MIRIADImage.cc
Images/HDF5Image2.cc
Images/ImageProxy.cc
Images/ComponentImager.cc
Wnbt/ComponentUpdate.cc
${BISON_ImageExprGram_OUTPUTS}
${FLEX_ImageExprGram_OUTPUTS}
)

target_link_libraries (
casa_images
casa_mirlib
casa_components
casa_lattices
)

add_subdirectory (apps)

install (
TARGETS casa_images
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
Images/ComponentImager.h
Images/CurvedImage2D.h
Images/CurvedImage2D.tcc
Images/ExtendImage.h
Images/ExtendImage.tcc
Images/FITS2Image.tcc
Images/FITSImage.h
Images/HDF5Image.h
Images/HDF5Image.tcc
Images/Image2DConvolver.h
Images/Image2DConvolver.tcc
Images/ImageAnalysis.h
Images/ImageConcat.h
Images/ImageConcat.tcc
Images/ImageConvolver.h
Images/ImageConvolver.tcc
Images/ImageDecomposer.h
Images/ImageDecomposer.tcc
Images/ImageExpr.h
Images/ImageExpr.tcc
Images/ImageExprGram.h
Images/ImageExprParse.h
Images/ImageFFT.h
Images/ImageFITSConverter.h
Images/ImageFITSConverter.tcc
Images/ImageFit1D.h
Images/ImageFit1D.tcc
Images/ImageHistograms.h
Images/ImageHistograms.tcc
Images/ImageInfo.h
Images/ImageInterface.h
Images/ImageInterface.tcc
Images/ImageMetaData.h
Images/ImageMoments.h
Images/ImageMoments.tcc
Images/ImageMomentsProgress.h
Images/ImageOpener.h
Images/ImagePolProxy.h
Images/ImagePolarimetry.h
Images/ImageProfileFit.h
Images/ImageProxy.h
Images/ImageRegion.h
Images/ImageRegrid.h
Images/ImageRegrid.tcc
Images/ImageSourceFinder.h
Images/ImageSourceFinder.tcc
Images/ImageStatistics.h
Images/ImageStatistics.tcc
Images/ImageSummary.h
Images/ImageSummary.tcc
Images/ImageTwoPtCorr.h
Images/ImageTwoPtCorr.tcc
Images/ImageUtilities.h
Images/ImageUtilities2.tcc
Images/LELImageCoord.h
Images/MIRIADImage.h
Images/MaskSpecifier.h
Images/MomentCalculator.h
Images/MomentCalculator.tcc
Images/PagedImage.h
Images/PagedImage.tcc
Images/RebinImage.h
Images/RebinImage.tcc
Images/SepImageConvolver.h
Images/SepImageConvolver.tcc
Images/SubImage.h
Images/SubImage.tcc
Images/TempImage.h
Images/TempImage.tcc
Images/WCBox.h
DESTINATION include/casacore/images/Images
)

install (FILES
Regions/AipsIOReaderWriter.h
Regions/ImageRegion.h
Regions/RegionHandler.h
Regions/RegionHandlerHDF5.h
Regions/RegionHandlerMemory.h
Regions/RegionHandlerTable.h
Regions/RegionManager.h
Regions/RFReaderWriter.h
Regions/WCBox.h
Regions/WCComplement.h
Regions/WCCompound.h
Regions/WCConcatenation.h
Regions/WCDifference.h
Regions/WCExtension.h
Regions/WCIntersection.h
Regions/WCLELMask.h
Regions/WCPolygon.h
Regions/WCRegion.h
Regions/WCUnion.h
DESTINATION include/casacore/images/Regions
)

install (FILES
Regions.h
Images.h
DESTINATION include/casacore/images
)

if (BUILD_TESTING)
    add_subdirectory (Images/test)
    add_subdirectory (Wnbt/test)
    add_subdirectory (Regions/test)
endif (BUILD_TESTING)
