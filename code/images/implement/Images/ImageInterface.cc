//# ImageInterface.cc: defines the Image base class non pure virtual stuff
//# Copyright (C) 1996,1997
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

#include <aips/aips.h>
#include <aips/Arrays/Vector.h> // Put these early to work around g++ bug
#include <aips/Arrays/Matrix.h>

#include <trial/Images/ImageInterface.h>
#include <trial/Lattices/LatticeIterator.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Measures/Unit.h>
#include <aips/Utilities/Assert.h>

#include <aips/Logging/LogIO.h>

#include <strstream.h>

// Function which sets the units associated with the map.
template <class T> void ImageInterface<T>::setUnits(const Unit &newUnits)
{
    logSink() << LogOrigin("ImageInterface<T>",
			   "setUnits(const Unit &newUnits)", WHERE) << 
	LogIO::DEBUGGING <<"Changing units from " << units().getName() << 
	" to " << newUnits.getName() << LogIO::POST;
    units_p = newUnits;
}

// Function to return the units of the map
template <class T> const Unit &ImageInterface<T>::units() const
{
    return units_p;
}

// reset coords
template <class T> 
Bool ImageInterface<T>::setCoordinateInfo(const CoordinateSystem &coords)
{
    Bool ok = ToBool(coords.nPixelAxes() == shape().nelements());
    if (ok) {
	coords_p = coords;
	logSink() << LogIO::DEBUGGING << 
	    "Changing coordinate system:\n" <<
	    "        ndim = " << shape().nelements() << endl <<
	    "        axes = " << coords_p.worldAxisNames().ac() << endl <<
	    "     ref val = " << coords_p.referenceValue().ac() << endl <<
	    "     ref pix = " << coords_p.referencePixel().ac() << endl <<
	    "       delta = " << coords_p.increment().ac() << " units = " <<
	    coords_p.worldAxisUnits().ac() << endl <<
	    "linear xfrom = " << coords_p.linearTransform().ac() << 
	    LogIO::POST;
    } else {
	// !ok
	logSink() << LogIO::SEVERE << 
	    "Cannot set coordinate system. coords.nPixelAxes() == " <<
	    coords.nPixelAxes() << ", image.ndim == " << shape().nelements() <<
	    LogIO::POST;
    }
    return ok;
}

template <class T> 
const CoordinateSystem &ImageInterface<T>::coordinates() const 
{
    return coords_p;
}


// function to toggle the ability to write through the image mask.
// True => writes to Image will alter the map and return True, 
// False => writes to Image will fail and return False.
template <class T> void ImageInterface<T>::writeThroughMask(Bool newValue)
{
    logSink() << LogOrigin("ImageInterface<T>", 
			   "writeThroughMask(Bool newValue)", WHERE);
    logSink() << "newValue is " << newValue << " changes to the image will be ";
    if (newValue) {
        logSink() << "recorded ignoring mask values";
    } else {
        logSink() << "recorded only where the mask is False";
    }
    logSink() << LogIO::POST;
    throughmask_p = newValue;
}

// function return the value of the mask toggle
template <class T> Bool ImageInterface<T>::writeThroughMask() const
{
  return throughmask_p;
}

template <class T> 
ImageInterface<T>::ImageInterface(): throughmask_p(True)
{
  logSink() << LogIO::DEBUGGING << 
	LogOrigin("ImageInterface<T>", "ImageInterface()", WHERE) <<
	"Creating ImageInterface with null coordinates and will write"
	" through any mask";

}

template <class T> 
ImageInterface<T>::ImageInterface(const CoordinateSystem &coords, Bool masking)
: throughmask_p(masking)
{
  logSink() << LogOrigin("ImageInterface<T>",
	    "ImageInterface(const CoordinateSystem &coords, Bool masking)",
			 WHERE) << LogIO::DEBUGGING <<
    "Creating ImageInterface with supplied coordinates and masking="
	    << masking << LogIO::POST;
  AlwaysAssert(ImageInterface<T>::setCoordinateInfo(coords), 
	       AipsError);
}
