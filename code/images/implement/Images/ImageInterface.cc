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
#include <trial/Lattices/LCRegion.h>
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
: logClosed_p  (False),
  regHandPtr_p (0)
{
  logSink() << LogOrigin("ImageInterface<T>",
	    "ImageInterface()",
			 WHERE) << LogIO::DEBUGGING <<
    "Creating ImageInterface" << LogIO::POST;
  regHandPtr_p = new RegionHandler();
}

template <class T> 
ImageInterface<T>::ImageInterface (const RegionHandler& regHand)
: logClosed_p  (False),
  regHandPtr_p (0)
{
  logSink() << LogOrigin("ImageInterface<T>",
	    "ImageInterface()",
			 WHERE) << LogIO::DEBUGGING <<
    "Creating ImageInterface" << LogIO::POST;
  regHandPtr_p = regHand.clone();
  regHandPtr_p->setObjectPtr (this);
}

template <class T> 
ImageInterface<T>::ImageInterface (const ImageInterface& other)
: MaskedLattice<T> (other),
  coords_p     (other.coords_p),
  log_p        (other.log_p),
  logClosed_p  (other.logClosed_p),
  imageInfo_p  (other.imageInfo_p),
  regHandPtr_p (0)
{
  logSink() << LogOrigin("ImageInterface<T>",
	    "ImageInterface(const ImageInterface&)",
			 WHERE) << LogIO::DEBUGGING << LogIO::POST;
  regHandPtr_p = other.regHandPtr_p->clone();
  regHandPtr_p->setObjectPtr (this);
}

template <class T> 
ImageInterface<T>& ImageInterface<T>::operator= (const ImageInterface& other)
{
  if (this != &other) {
    MaskedLattice<T>::operator= (other);
    coords_p    = other.coords_p;
    log_p       = other.log_p;
    logClosed_p = other.logClosed_p;
    imageInfo_p = other.imageInfo_p;
    delete regHandPtr_p;
    regHandPtr_p = 0;
    regHandPtr_p = other.regHandPtr_p->clone();
    regHandPtr_p->setObjectPtr (this);
  }
  return *this;
}

template <class T> 
ImageInterface<T>::~ImageInterface()
{
  delete regHandPtr_p;
}

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
ImageRegion ImageInterface<T>::makeMask (const String& name,
					 Bool defineAsRegion,
					 Bool setAsDefaultMask,
					 Bool initialize,
					 Bool value)
{
  ImageRegion region = regHandPtr_p->makeMask (*this, name);
  if (initialize) {
    region.asMask().set (value);
  }
  if (defineAsRegion) {
    defineRegion (name, region, RegionHandler::Masks);
    if (setAsDefaultMask) {
      setDefaultMask (name);
    }
  }
  return region;
}

template <class T>
void ImageInterface<T>::defineRegion (const String& name,
				      const ImageRegion& region,
				      RegionHandler::GroupType type,
				      Bool overwrite)
{
  regHandPtr_p->defineRegion (name, region, type, overwrite);
}
template <class T>
Bool ImageInterface<T>::hasRegion (const String& name,
				   RegionHandler::GroupType type) const
{
  return regHandPtr_p->hasRegion (name, type);
}
template <class T>
ImageRegion* ImageInterface<T>::getImageRegionPtr
                                     (const String& name,
				      RegionHandler::GroupType type,
				      Bool throwIfUnknown) const
{
  return regHandPtr_p->getRegion (name, type, throwIfUnknown);
}
template <class T>
void ImageInterface<T>::renameRegion (const String& newName,
				      const String& oldName,
				      RegionHandler::GroupType type,
				      Bool throwIfUnknown)
{
  regHandPtr_p->renameRegion (newName, oldName, type, throwIfUnknown);
}
template <class T>
void ImageInterface<T>::removeRegion (const String& name,
				      RegionHandler::GroupType type,
				      Bool throwIfUnknown)
{
  regHandPtr_p->removeRegion (name, type, throwIfUnknown);
}
template<class T> 
Vector<String> ImageInterface<T>::regionNames
                                     (RegionHandler::GroupType type) const
{
  return regHandPtr_p->regionNames (type);
}

template <class T>
void ImageInterface<T>::setDefaultMask (const String& name)
{
  regHandPtr_p->setDefaultMask (name);
}
template <class T>
String ImageInterface<T>::getDefaultMask() const
{
  return regHandPtr_p->getDefaultMask();
}

template <class T>
void ImageInterface<T>::useMask (MaskSpecifier)
{
  throw AipsError ("ImageInterface::useMask - not implemented");
}

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
  return regHandPtr_p->makeUniqueRegionName (rootName, startNumber);
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


template<class T>
void ImageInterface<T>::closeLogSink (Bool temporarily)
{
  // Set log object to the global logsink only.
  log_p = LogIO();
  logClosed_p = temporarily;
}

template<class T>
void ImageInterface<T>::reopenLog()
{
  if (logClosed_p) {
    doReopenLogSink();
    logClosed_p = False;
  }
}

template<class T>
void ImageInterface<T>::doReopenLogSink()
{}
