//# RegionHandler.cc: Base class for handling regions in an image
//# Copyright (C) 2000,2001,2003
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


#include <casacore/images/Regions/RegionHandler.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/Lattices/LatticeBase.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

RegionHandler::~RegionHandler()
{}

RegionHandler* RegionHandler::clone() const
{
  return new RegionHandler (*this);
}

void RegionHandler::setObjectPtr (void*)
{}

Bool RegionHandler::canDefineRegion() const
{
  return False;
}

void RegionHandler::setDefaultMask (const String&)
{
  throw AipsError ("RegionHandler::setDefaultMask"
		   " cannot be used for this image type");
}

String RegionHandler::getDefaultMask() const
{
  return "";
}

Bool RegionHandler::defineRegion (const String&,
				  const ImageRegion&,
				  RegionHandler::GroupType,
				  Bool)
{
  throw AipsError ("RegionHandler::defineRegion"
		   " cannot be used for this image type");
}

Bool RegionHandler::hasRegion (const String&,
			       RegionHandler::GroupType) const
{
  return False;
}

Bool RegionHandler::renameRegion (const String&,
				  const String&,
				  RegionHandler::GroupType,
				  Bool)
{
  throw AipsError ("RegionHandler::renameRegion"
		   " cannot be used for this image type");
  return False;
}

Bool RegionHandler::removeRegion (const String&,
				  RegionHandler::GroupType,
				  Bool throwIfUnknown)
{
  if (throwIfUnknown) {
    throw AipsError ("RegionHandler::removeRegion"
		     " cannot be used for this image type");
  }
  return False;
}

Vector<String> RegionHandler::regionNames (RegionHandler::GroupType) const
{
  return Vector<String>();
}

ImageRegion* RegionHandler::getRegion (const String&,
				       RegionHandler::GroupType,
				       Bool throwIfUnknown) const
{
  if (throwIfUnknown) {
    throw AipsError ("RegionHandler::findRegionGroup"
		     " cannot be used for this image type");
  }
  return 0;
}


ImageRegion RegionHandler::makeMask (const LatticeBase&,
				     const String&)
{
  throw (AipsError ("RegionHandler::makeMask - "
		    "cannot create mask for virtual image"));
  return ImageRegion();
}

String RegionHandler::makeUniqueRegionName (const String& rootName,
					    uInt startNumber) const
{
  while (True) {
    ostringstream oss;
    oss << startNumber;
    String name = rootName + String(oss);
    if (! hasRegion (name, RegionHandler::Any)) {
      return name;
    }
    startNumber++;
  }
  return String();
}

} //# NAMESPACE CASACORE - END

