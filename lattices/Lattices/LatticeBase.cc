//# LatticeBase.cc: A non-templated, abstract base class for array-like classes
//# Copyright (C) 1999,2000,2003
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


#include <casacore/lattices/Lattices/LatticeBase.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LatticeBase::~LatticeBase()
{}

String LatticeBase::imageType() const
{
  return "Lattice";
}

Bool LatticeBase::isPersistent() const
{
  return False;
}

Bool LatticeBase::isPaged() const
{
  return False;
}

Bool LatticeBase::canReferenceArray() const
{
  return False;
}

Bool LatticeBase::isWritable() const
{
  return True;
}

void LatticeBase::save (const String&) const
{
  throw AipsError(imageType() + "::save is not implemented");
}

Bool LatticeBase::lock (FileLocker::LockType, uInt)
{
    return True;
}
void LatticeBase::unlock()
{}
Bool LatticeBase::hasLock (FileLocker::LockType) const
{
    return True;
}
void LatticeBase::resync()
{}
void LatticeBase::flush()
{}
void LatticeBase::tempClose()
{}
void LatticeBase::reopen()
{}

String LatticeBase::name (Bool) const
{
  return "";
}

uInt LatticeBase::ndim() const
{
  return shape().nelements();
}

size_t LatticeBase::nelements() const
{
  return shape().product(); 
}

LELCoordinates LatticeBase::lelCoordinates() const
{
  return LELCoordinates();
}


IPosition LatticeBase::doNiceCursorShape (uInt maxPixels) const
{
  IPosition originalShape(shape());
  uInt ndim = originalShape.nelements();
  IPosition cursorShape(ndim);
  if (ndim > 0) {
    cursorShape = 1;
    cursorShape(0) = originalShape(0);
    for (uInt i=1;
	 i < ndim  &&  cursorShape.product()*originalShape(i) <= Int(maxPixels);
	 i++) {
      cursorShape(i) = originalShape(i);
    }
  }
  return cursorShape;
}


Bool LatticeBase::ok() const
{
  return True;
}


uInt LatticeBase::maximumCacheSize() const
{
  return 0;
}

void LatticeBase::setMaximumCacheSize (uInt)
{}

void LatticeBase::setCacheSizeInTiles (uInt)
{}

void LatticeBase::setCacheSizeFromPath (const IPosition&,
					const IPosition&,
					const IPosition&,
					const IPosition&)
{}

void LatticeBase::clearCache()
{}

void LatticeBase::showCacheStatistics (ostream&) const
{}

void LatticeBase::throwBoolMath() const
{
  throw AipsError ("Operator +=, etc. cannot be used for a Boolean lattice");
}

} //# NAMESPACE CASACORE - END

