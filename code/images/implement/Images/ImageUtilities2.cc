//# ImageUtilities2.cc:  Helper class for accessing images
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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
//# $Id$
//

#include <trial/Images/ImageUtilities.h>

#include <aips/Utilities/String.h>
#include <aips/Utilities/PtrHolder.h>

#include <trial/Images/ImageInterface.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/FITSImage.h>
#include <trial/Images/MIRIADImage.h>
#include <aips/Logging/LogIO.h>
#include <aips/OS/File.h>


void ImageUtilities::openImage (ImageInterface<Float>*& pImage,
                                const String& fileName, LogIO& os)
{
   if (fileName.empty()) {
      os << "The image filename is empty" << LogIO::EXCEPTION;   
   }
   File file(fileName);
   if (!file.exists()) {
      os << "File '" << fileName << "' does not exist" << LogIO::EXCEPTION;
   }
//
   ImageUtilities::ImageTypes type = ImageUtilities::imageType(fileName);
   if (type==ImageUtilities::AIPSPP) {
      if (!Table::isReadable(fileName)) {
         os << "The aips++ image file " << fileName << " is not readable" << LogIO::EXCEPTION;
      }
      pImage = new PagedImage<Float>(fileName);
   } else if (type==ImageUtilities::FITS) { 
      pImage = new FITSImage(fileName);
   } else if (type==ImageUtilities::MIRIAD) {
      pImage = new MIRIADImage(fileName);
   } else {
      pImage = 0;
      os << "Unrecognized image type, presently aips++, FITS and Miriad images are supported" 
         << LogIO::EXCEPTION;
   }
}


void ImageUtilities::openImage (PtrHolder<ImageInterface<Float> >& image,
                                const String& fileName, LogIO& os)
{
   ImageInterface<Float>* p = 0;
   ImageUtilities::openImage(p, fileName, os);
   image.set(p);
}

