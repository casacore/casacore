//# ImageInterface.cc: defines the Image base class non pure virtual stuff
//# Copyright (C) 1996,1997,1998,1999,2000
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

#include <trial/Coordinates/StokesCoordinate.h>

#include <trial/Images/ImageInterface.h>
#include <trial/Images/LELImageCoord.h>
#include <trial/Images/ImageRegion.h>
#include <aips/Lattices/LatticeIterator.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Quanta/Unit.h>
#include <aips/Utilities/Assert.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/TableLogSink.h>
#include <strstream.h>


template <class T> 
ImageInterface<T>::ImageInterface()
{
  log_p.makePermanent();
  logSink() << LogOrigin("ImageInterface<T>",
	    "ImageInterface()",
			 WHERE) << LogIO::DEBUGGING <<
    "Creating ImageInterface" << LogIO::POST;
}

template <class T> 
ImageInterface<T>::ImageInterface (const ImageInterface& other)
: MaskedLattice<T> (other),
  coords_p (other.coords_p),
  log_p    (other.log_p),
  imageInfo_p(other.imageInfo_p)
{
  log_p.makePermanent();
  logSink() << LogOrigin("ImageInterface<T>",
	    "ImageInterface(const ImageInterface&)",
			 WHERE) << LogIO::DEBUGGING << LogIO::POST;
}

template <class T> 
ImageInterface<T>& ImageInterface<T>::operator= (const ImageInterface& other)
{
  if (this != &other) {
    MaskedLattice<T>::operator= (other);
    coords_p = other.coords_p;
    log_p    = other.log_p;
    log_p.makePermanent();
    imageInfo_p = other.imageInfo_p;
  }
  return *this;
}

template <class T> 
ImageInterface<T>::~ImageInterface()
{}

template<class T>
MaskedLattice<T>* ImageInterface<T>::cloneML() const
{
    return cloneII();
}


// reset coords
template <class T> 
Bool ImageInterface<T>::setCoordinateInfo(const CoordinateSystem &coords)
{
    ostrstream errmsg;
    errmsg << "Cannot set coordinate system: ";

    Bool ok = ToBool(coords.nPixelAxes() == shape().nelements());
    if (!ok) {
	errmsg << "coords.nPixelAxes() == " << coords.nPixelAxes() << 
	    ", image.ndim() == " << shape().nelements();
    } else {
	// Check that the shape is compatible with the stokes coordinates
	Int stkcrd = -1;
	while (ok && (stkcrd = coords.findCoordinate(Coordinate::STOKES, 
							  stkcrd)) >= 0) {
	    ok = True;
	    Int axis = coords.pixelAxes(stkcrd)(0);
	    const StokesCoordinate &stokes = coords.stokesCoordinate(stkcrd);
	    if (axis >= 0) {
		Int nstokes = stokes.stokes().nelements();
		Int axislength = shape()(axis);
		if (axislength > nstokes) {
		    ok = False;
		    errmsg << "Stokes axis is length " << axislength <<
			" but we only have " << nstokes << " stokes values"
			   << endl;
		}
	    }
	}
    }

    if (ok) {
	coords_p = coords;
	logSink() << LogIO::DEBUGGING << 
	    "Changing coordinate system:\n" <<
	    "        ndim = " << shape().nelements() << endl <<
	    "        axes = " << coords_p.worldAxisNames() << endl <<
	    "     ref val = " << coords_p.referenceValue() << endl <<
	    "     ref pix = " << coords_p.referencePixel() << endl <<
	    "       delta = " << coords_p.increment() << " units = " <<
	    coords_p.worldAxisUnits() << endl <<
	    "linear xfrom = " << coords_p.linearTransform() << 
	    LogIO::POST;
    } else {
	// !ok
	logSink() << LogIO::SEVERE << String(errmsg) << LogIO::POST;
    }
    return ok;
}

template <class T> 
const CoordinateSystem &ImageInterface<T>::coordinates() const 
{
    return coords_p;
}

template <class T> 
LELCoordinates ImageInterface<T>::lelCoordinates() const
{
    return LELCoordinates (new LELImageCoord (coords_p));
}


template <class T>
Bool ImageInterface<T>::canDefineRegion() const
{
  return False;
}
template <class T>
void ImageInterface<T>::defineRegion (const String&, const ImageRegion&,
				      RegionHandler::GroupType, Bool)
{}
template <class T>
Bool ImageInterface<T>::hasRegion (const String&,
				   RegionHandler::GroupType) const
{ return False; }
template <class T>
ImageRegion* ImageInterface<T>::getImageRegionPtr (const String&,
						   RegionHandler::GroupType,
						   Bool) const
{ return 0; }
template <class T>
void ImageInterface<T>::renameRegion (const String&,
				      const String&,
				      RegionHandler::GroupType,
				      Bool)
{}
template <class T>
void ImageInterface<T>::removeRegion (const String&,
				      RegionHandler::GroupType,
				      Bool)
{}
template<class T> 
Vector<String> ImageInterface<T>::regionNames (RegionHandler::GroupType) const
{
  return Vector<String>();
}

template <class T>
void ImageInterface<T>::setDefaultMask (const String&)
{}
template <class T>
String ImageInterface<T>::getDefaultMask() const
{ return ""; }

template <class T>
ImageRegion ImageInterface<T>::getRegion (const String& regionName,
					  RegionHandler::GroupType type) const
{
    ImageRegion* regptr = getImageRegionPtr (regionName, type, True);
    ImageRegion reg(*regptr);
    delete regptr;
    return reg;
}

template<class T>
String ImageInterface<T>::makeUniqueRegionName (const String& rootName,
						uInt startNumber) const
{
    while (True) {
        ostrstream oss;
	oss << startNumber;
	String name = rootName + String(oss);
	if (! hasRegion (name, RegionHandler::Any)) {
	    return name;
	}
	startNumber++;
   }
   return String();
}


template<class T>
ImageInfo ImageInterface<T>::imageInfo() const
{    
   return imageInfo_p;
}    
        
template<class T>
Bool ImageInterface<T>::setImageInfo(const ImageInfo& info)
//
// Derived classes like PagedImage have to put this in the
// permanent table keywordSet
// 
{ 
   imageInfo_p = info;
   return True;
}    
   
template<class T>
Bool ImageInterface<T>::restoreImageInfo(const RecordInterface& rec)
{
  Bool ok = True;
  if (rec.isDefined("imageinfo")) {
     String error;
     ok = imageInfo_p.fromRecord(error, rec.asRecord("imageinfo"));
     if (!ok) {
        logSink() << LogIO::WARN << "Failed to restore the ImageInfo because " 
          + error << LogIO::POST;
     }
  }
  return ok;
}


template<class T>
void ImageInterface<T>::mergeTableLogSink (const LogIO&)
{}
