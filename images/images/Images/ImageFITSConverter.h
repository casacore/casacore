//# ImageFITSConverter.h: Interconvert between AIPS++ Images and FITS files
//# Copyright (C) 1996,1999,2001,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$


#ifndef IMAGES_IMAGEFITSCONVERTER_H
#define IMAGES_IMAGEFITSCONVERTER_H

#include <casa/aips.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template<class T> class PagedImage;
template<class T> class ImageInterface;
template<class T> class Vector;
class IPosition;
class String;
class File;
class ImageInfo;
class CoordinateSystem;
class RecordInterface;
class LogIO;
class Unit;
class LoggerHolder;
class ConstFitsKeywordList;

// <summary>
// Interconvert between AIPS++ Images and FITS files.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
// <li> <linkto class="PagedImage">PagedImage</linkto>
// <li> <linkto class="PrimaryArray">PrimaryArray</linkto> (and FITS concepts in
//      general).
// </prerequisite>
//
// <synopsis>
// This class is a helper class that is used to interconvert between AIPS++
// images and FITS files. This adds no functionality over the general abilities
// available in the underlying FITS classes, however it is a useful higher-level
// packaging.
//
// There are two fundamental member functions in this class.
// <src>FITSToImage</src> which turns a FITS file into an AIPS++ image, and
// <src>ImageToFITS</src> which does the opposite.
//
// We can read images from any HDU inside the FITS file (although this isn't
// well tested). However at the moment we always write to the first HDU, i.e.
// to the primary array, not an image extension.
//
// Pixels in the FITS file which are blanked are masked out (the mask
// is set to False) in the output image.   On conversion to FITS,
// masked values are blanked.    The mask which is read is the current
// default mask.
// </synopsis>
//
// <example>
// A FITS to image conversion may be accomplished as follows:
// <srcBlock>
//    PagedImage<Float> *image = 0;
//    String fitsName = "exists.fits";
//    String imageName = "new.image";
//    String error;
//    Bool ok = ImageFITSConverter::FITSToImage(image, error, imageName, fitsName);
//    if (!image) ... error ...
// </srcBlock>
// A couple of things to note:
// <ul>
//    <li> If <src>ok</src> is False, the conversion failed and <src>error</src>
//         will be set.
//    <li> The pointer "image" is set if the conversion succeeds. If it is
//         zero the conversion failed and <src>error</src> will contain an
//         error message.
//    <li> The caller is responsible for deleting the pointer <src>image</src>
//         when the conversion is successful.
// </ul>
// Similarly, an image to FITS conversion may be accomplished as follows:
// <srcBlock>
//    String imageName = argv[1];
//    PagedImage<Float> image = ...; // An existing image from somewhere
//    String fitsName = "new.fits";
//    String error;
//    Bool ok = ImageFITSConverter::ImageToFITS(error, image, fitsName);
// </srcBlock>
// A couple of similar remarks can be made about this example:
// <ul>
//    <li> If <src>ok</src> is False, the conversion failed and <src>error</src>
//         will be set.
// </ul>
// </example>
//
// <motivation>
// FITS files are the fundamental transport format for images in Astronomy.
// </motivation>
//
// <todo asof="1999/02/15">
//   <li> It might be useful to have functions that convert between FITS
//        and general lattices.
//   <li> Add support for PagedImage<Complex>
//   <li> Convert multiple images at once?
//   <li> Allow writing FITS files to an image extension in an existing
//        FITS file.
// </todo>

class ImageFITSConverter
{
public:
    // Convert a FITS file to an AIPS++ image.
    // <ul>
    //   <li> <src>newImage</src> will be zero if the conversion fail. If the 
    //        conversion succeeds, the caller is responsible for deleting this
    //        pointer.
    //   <li> <src>error</src> will be set if the conversion fails.
    //   <li> If <src>imageName</src> is empty, a TempImage will be created,
    //        otherwise a PagedImage on disk.
    //   <li> <src>fitsName</src> must already exist (and have an image at the
    //        indicated HDU).
    //   <li> <src>whichRep</src> Zero-relative coordinate representation
    //        (Starting with wcs FITS multiple coordinate representations
    //         can be stored in a FITS file)
    //   <li> <src>whichHDU</src> Zero-relative hdu. The default is correct for
    //        a primary array, set it for an image extension. Only zero has been
    //        tested.
    //   <li> <src>memoryInMB</src>. Setting this to zero will result in
    //        row-by-row copying, otherwise it will attempt to with as large
    //        a chunk-size as possible, while fitting in the desired memory.
    //   <li> <src>allowOverwrite</src> If True, allow imageName to be 
    //        overwritten if it already exists.
    //   <li> <src>zeroBlanks</src> If True, allow any blanked pixels are set
    //         to zero rather than NaN
    // </ul>
    static Bool FITSToImage(ImageInterface<Float>*& newImage,
			    String &error,
			    const String &imageName,
			    const String &fitsName, 
			    uInt whichRep = 0,
			    uInt whichHDU = 0,
			    uInt memoryInMB = 64,
			    Bool allowOverwrite=False,
                            Bool zeroBlanks=False);

// Old version
//    static Bool FITSToImageOld(ImageInterface<Float>*& newImage,
//			    String &error,
//			    const String &imageName,
//			    const String &fitsName, 
//			    uInt whichHDU = 0,
//			    uInt memoryInMB = 64,
//			    Bool allowOverwrite=False,
//                            Bool zeroBlanks=False);

    // Convert an AIPS++ image to a FITS file.
    // <ul>
    //   <li> <src>return</src> True if the conversion succeeds, False 
    //        otherwise.
    //   <li> <src>error</src> will be set if the conversion fails.
    //   <li> <src>image</src> The image to convert.
    //   <li> <src>fitsName</src> If the name is "-" (the minus character), 
    //        then write to stdout Always writes to the primary array.
    //   <li> <src>memoryInMB</src>. Setting this to zero will result in
    //        row-by-row copying, otherwise it will attempt to with as large
    //        a chunk-size as possible, while fitting in the desired memory.
    //   <li> <src>preferVelocity</src>Write a velocity primary spectral axis
    //        if possible.
    //   <li> <src>opticalVelocity</src>If writing a velocity, use the optical
    //        definition (otherwise use radio).
    //   <li> <src>BITPIX, minPix, maxPix</src>
    //        BITPIX can presently be set to -32 or 16 only. When BITPIX is
    //        16 it will write BSCALE and BZERO into the FITS file. If minPix
    //        is greater than maxPix the minimum and maximum pixel values
    //        will be determined from the array, otherwise the supplied
    //        values will be used and pixels outside that range will be
    //        truncated to the minimum and maximum pixel values (note that
    //        this truncation does not occur for BITPIX=-32).
    //   <li> <src>allowOverwrite</src> If True, allow fitsName to be 
    //        overwritten if it already exists.
    //   <li> <src>degenerateLast</src> If True, axes of length 1 will be written
    //        last to the header.
    //   </ul>
    static Bool ImageToFITS(String &error,
			    ImageInterface<Float> &image,
			    const String &fitsName, 
			    uInt memoryInMB = 64,
			    Bool preferVelocity = True,
			    Bool opticalVelocity = True,
			    Int BITPIX=-32,
			    Float minPix = 1.0, Float maxPix = -1.0,
			    Bool allowOverwrite=False,
                            Bool degenerateLast=False,
                            Bool verbose=True);

    // Helper function - used to calculate a cursor appropriate for the desired
    // memory use. It's not intended that application programmers call this, but
    // you may if it's useful to you.
    static IPosition copyCursorShape(String &report,
				     const IPosition &shape, 
				     uInt imagePixelSize,
				     uInt fitsPixelSize,
				     uInt memoryInMB);

// Recover CoordinateSystem from header.  Used keywords are removed from header
// and the unused one returned in a Record for ease of use.  Degenerate axes 
// may be added to shape if needed
    static CoordinateSystem getCoordinateSystem (Int& imageType, RecordInterface& headerRec,
                                                 const Vector<String>& header,
                                                 LogIO& os, uInt whichRep,
                                                 IPosition& shape, Bool dropStokes);

// Old version
//    static CoordinateSystem getCoordinateSystemOld (Int& imageType, RecordInterface& header,
//                                                 LogIO& os, IPosition& shape, Bool dropStokes);

// Recover ImageInfo from header. Used keywords are removed from header
    static ImageInfo getImageInfo (RecordInterface& header);

//Old version
//    static ImageInfo getImageInfoOld (RecordInterface& header);

// Recover brightness unit from header. Used keywords are removed from header
    static Unit getBrightnessUnit (RecordInterface& header, LogIO& os);
// Old version
//    static Unit getBrightnessUnitOld (RecordInterface& header, LogIO& os);

// Recover history from FITS file keywrod list into logger
   static void restoreHistory (LoggerHolder& logger,
                               ConstFitsKeywordList& kw);
			       
// Parse header record and set MiscInfo
   static Bool extractMiscInfo (RecordInterface& miscInfo, const RecordInterface& header);

private:
   static Bool removeFile (String& error, const File& outFile,
                           const String& outName, Bool allowOverwrite);

};


// <summary>
// This class is an internal class for ImageFITSConverter.
// </summary>

// <use visibility=local>

// <synopsis>
// This class is an internal class used to implement 
// ImageFitsConverter::FITSToImage - in particular, it has the code which
// is dependent on the various types (BITPIX values).
// </synopsis>
template<class HDUType> class ImageFITSConverterImpl
{
public:
    static void FITSToImage(ImageInterface<Float> *&newImage,
			    String &error,
			    const String &imageName,
			    uInt whichRep,
			    HDUType &fitsImage,
			    uInt memoryInMB = 64,
                            Bool zeroBlanks=False);


// Old version
//    static void FITSToImageOld(ImageInterface<Float> *&newImage,
//			    String &error,
//			    const String &imageName,
//			    HDUType &fitsImage,
//			    uInt memoryInMB = 64,
//                           Bool zeroBlanks=False);

};


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/ImageFITSConverter.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
