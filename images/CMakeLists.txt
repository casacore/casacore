#
# CASA Images
#

BISON_TARGET (ImageExprGram Images/ImageExprGram.yy ${CMAKE_CURRENT_BINARY_DIR}/ImageExprGram.ycc COMPILE_FLAGS "-p ImageExprGram")
FLEX_TARGET (ImageExprGram Images/ImageExprGram.ll ${CMAKE_CURRENT_BINARY_DIR}/ImageExprGram.lcc COMPILE_FLAGS "-PImageExprGram")

include_directories (${CMAKE_CURRENT_BINARY_DIR})

add_library (casa_images
Regions/WCEllipsoid.cc
Regions/RegionManager.cc
Regions/WCLELMask.cc
Regions/WCPolygon.cc
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
Images/FITSErrorImage.cc
Images/FITSImage.cc
Images/FITSImgParser.cc
Images/FITSQualityImage.cc
Images/FITSQualityMask.cc
Images/HDF5Image2.cc
Images/ImageAttrGroup.cc
Images/ImageAttrGroupCasa.cc
Images/ImageAttrGroupHDF5.cc
Images/ImageAttrHandler.cc
Images/ImageAttrHandlerCasa.cc
Images/ImageAttrHandlerHDF5.cc
Images/ImageBeamSet.cc
Images/ImageExprGram.cc
Images/ImageExprParse.cc
Images/ImageFITS2Converter.cc
Images/ImageInfo.cc
Images/ImageOpener.cc
Images/ImageProxy.cc
Images/ImageUtilities.cc
Images/LELImageCoord.cc
Images/MIRIADImage.cc
Images/MaskSpecifier.cc
Images/PagedImage2.cc
${BISON_ImageExprGram_OUTPUTS}
${FLEX_ImageExprGram_OUTPUTS}
)

target_link_libraries (
casa_images
casa_coordinates
casa_mirlib
casa_lattices
)

add_subdirectory (apps)

install (
TARGETS casa_images
RUNTIME DESTINATION bin
LIBRARY DESTINATION lib${LIB_SUFFIX}
ARCHIVE DESTINATION lib${LIB_SUFFIX}
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
Images/CurvedImage2D.h
Images/CurvedImage2D.tcc
Images/ExtendImage.h
Images/ExtendImage.tcc
Images/FITS2Image.tcc
Images/FITSErrorImage.h
Images/FITSImage.h
Images/FITSImgParser.h
Images/FITSQualityImage.h
Images/FITSQualityMask.h
Images/HDF5Image.h
Images/HDF5Image.tcc
Images/ImageAttrGroup.h
Images/ImageAttrGroupCasa.h
Images/ImageAttrGroupHDF5.h
Images/ImageAttrHandler.h
Images/ImageAttrHandlerCasa.h
Images/ImageAttrHandlerHDF5.h
Images/ImageBeamSet.h
Images/ImageConcat.h
Images/ImageConcat.tcc
Images/ImageExpr.h
Images/ImageExpr.tcc
Images/ImageExprGram.h
Images/ImageExprParse.h
Images/ImageFITSConverter.h
Images/ImageFITSConverter.tcc
Images/ImageInfo.h
Images/ImageInterface.h
Images/ImageInterface.tcc
Images/ImageOpener.h
Images/ImageProxy.h
Images/ImageRegrid.h
Images/ImageRegrid.tcc
Images/ImageStatistics.h
Images/ImageStatistics.tcc
Images/ImageSummary.h
Images/ImageSummary.tcc
Images/ImageUtilities.h
Images/ImageUtilities2.tcc
Images/LELImageCoord.h
Images/MIRIADImage.h
Images/MaskSpecifier.h
Images/PagedImage.h
Images/PagedImage.tcc
Images/RebinImage.h
Images/RebinImage.tcc
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
Regions/WCEllipsoid.h
Regions/WCExtension.h
Regions/WCIntersection.h
Regions/WCLELMask.h
Regions/WCPolygon.h
Regions/WCRegion.h
Regions/WCUnion.h
DESTINATION include/casacore/images/Regions
)

# Temporary hack until all packages use scimath/GaussianBeam
install (FILES
Images/GaussianBeam.h
DESTINATION include/casacore/components/ComponentModels
)

install (FILES
Regions.h
Images.h
DESTINATION include/casacore/images
)

if (BUILD_TESTING)
    add_subdirectory (Images/test)
    add_subdirectory (Regions/test)
endif (BUILD_TESTING)
