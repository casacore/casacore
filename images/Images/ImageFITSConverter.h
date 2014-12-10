//# ImageFITSConverter.h: Interconvert between Casacore Images and FITS files
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

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>

#ifndef WCSLIB_GETWCSTAB
 #define WCSLIB_GETWCSTAB
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class PagedImage;
template<class T> class ImageInterface;
template<class T> class Vector;
class FitsOutput;
class IPosition;
class File;
class ImageInfo;
class CoordinateSystem;
class RecordInterface;
class TableRecord;
class LogIO;
class Unit;
class LoggerHolder;
class ConstFitsKeywordList;
class FitsInput;

// <summary>
// Interconvert between Casacore Images and FITS files.
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
// This class is a helper class that is used to interconvert between Casacore
// images and FITS files. This adds no functionality over the general abilities
// available in the underlying FITS classes, however it is a useful higher-level
// packaging.
//
// There are two fundamental member functions in this class.
// <src>FITSToImage</src> which turns a FITS file into a Casacore image, and
// <src>ImageToFITS</src> which does the opposite.
//
// We can read images from any HDU inside the FITS file (although this isn't
// well tested). Images with a quality axis (i.e. contain data and error values)
// are stored in the primary HDU (data) and an extension HDU (error). Other
// images are always written to the primary HDU.
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
	const static String CASAMBM;

    // Convert a FITS file to a Casacore image.
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
    //        a primary array, set it for an image extension. A value of -1
    //        makes the code look for the first readable HDU.
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
			    Int whichHDU = 0,
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

    // Convert a Casacore image to a FITS file.
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
    //   <li> <src>preferWavelength</src> If True, write a wavelength primary axis.
    //   <li> <src>airWavelength</src> If True and <src>preferWavelength</src> is True write
    //        an air wavelength primary axis.
    //   <li> <src>origin</src> gives the origin, i.e., the name of the package.
    //        If empty, it defaults to "casacore-"getVersion().
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
    		Bool verbose=True,
    		Bool stokesLast=False,
    		Bool preferWavelength=False,
    		Bool airWavelength=False,
		const String& origin = String(),
		Bool history=True);

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

   // read the BEAMS table if present and add the restoring beams to <src>info</src>
   static void readBeamsTable(
		   ImageInfo& info, const String& filename,
		   const DataType type
   );

private:

// Put a CASA image to an opened FITS image
// Parameters as in "ImageToFITS". In addition:
// <ul>
//   <li> <src>output</src> The FITS output to write to.
//   <li> <src>primHead</src> Write to a primary HDU.
//   <li> <src>allowAppend</src> Allow to append extension HDU's.
// </ul>
   static Bool ImageToFITSOut(
		   String &error, LogIO &os,
		   const ImageInterface<Float> &image,
		   FitsOutput *output, uInt memoryInMB = 64,
  		 Bool preferVelocity = True,
  		 Bool opticalVelocity = True,
  		 Int BITPIX=-32,
  		 Float minPix = 1.0, Float maxPix = -1.0,
  		 Bool degenerateLast=False,
  		 Bool verbose=True,
  		 Bool stokesLast=False,
  		 Bool preferWavelength=False,
  		 Bool airWavelength=False,
  		 Bool primHead=True,
  		 Bool allowAppend=False,
		 const String& origin = String(),
		 Bool history=True  
   );

// Put a CASA image with quality coordinate
// to an opened FITS file
// Parameters as in "ImageToFITS". In addition:
// <ul>
//   <li> <src>output</src> The FITS output to write to.
// </ul>
   static Bool QualImgToFITSOut(String &error,
      		LogIO &os,
      		ImageInterface<Float> &image,
      		FitsOutput *outfile,
      		uInt memoryInMB,
      		Bool preferVelocity,
      		Bool opticalVelocity,
      		Int BITPIX, Float minPix, Float maxPix,
      		Bool degenerateLast,
      		Bool verbose, Bool stokesLast,
      		Bool preferWavelength,
      		Bool airWavelength,
		const String& origin,
		Bool history
   );

   static Bool removeFile (String& error, const File& outFile,
                           const String& outName, Bool allowOverwrite);

// Create an open FITS file with the name given
   static Bool openFitsOutput(String &error, FitsOutput *(&openFitsOutput), const String &fitsName,
   		                            const Bool &allowOverwrite);


     static void _writeBeamsTable(FitsOutput *const &outfile, const ImageInfo& info);

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
    static void FITSToImage(
    	ImageInterface<Float> *&newImage,
    	String &error,
    	const String &newImageName,
    	const uInt whichRep,
    	HDUType &fitsImage,
    	const String& fitsFilename,
    	const DataType dataType,
    	const uInt memoryInMB = 64,
    	const Bool zeroBlanks=False
    	);

};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/ImageFITSConverter.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
