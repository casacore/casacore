//# RegionHandlerMemory.cc: Class for keeping regions in memory
//# Copyright (C) 2000,2001
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

#include <casacore/images/Regions/RegionHandlerMemory.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCMask.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

RegionHandlerMemory::RegionHandlerMemory()
{
  itsMaps[0] = 0;
  itsMaps[1] = 0;
  itsMaps[0] = new SimpleOrderedMap<String, void*> (0);
  itsMaps[1] = new SimpleOrderedMap<String, void*> (0);
}

RegionHandlerMemory::RegionHandlerMemory (const RegionHandlerMemory& that)
 : RegionHandler(that)
{
  itsMaps[0] = 0;
  itsMaps[1] = 0;
  itsMaps[0] = new SimpleOrderedMap<String, void*> (0);
  itsMaps[1] = new SimpleOrderedMap<String, void*> (0);
  operator= (that);
}

RegionHandlerMemory::~RegionHandlerMemory()
{
  clear();
  delete itsMaps[0];
  delete itsMaps[1];
}

RegionHandlerMemory& RegionHandlerMemory::operator=
                                         (const RegionHandlerMemory& that)
{
  if (this != &that) {
    clear();
    itsDefaultName = that.itsDefaultName;
    *(itsMaps[0]) = *(that.itsMaps[0]);
    *(itsMaps[1]) = *(that.itsMaps[1]);
    for (uInt i=0; i<itsMaps[0]->ndefined(); i++) {
      void*& valref = itsMaps[0]->getVal(i);
      valref = static_cast<ImageRegion*>(valref)->clone();
    }
    for (uInt i=0; i<itsMaps[1]->ndefined(); i++) {
      void*& valref = itsMaps[1]->getVal(i);
      valref = static_cast<ImageRegion*>(valref)->clone();
    }
  }
  return *this;
}

void RegionHandlerMemory::clear()
{
  for (uInt i=0; i<itsMaps[0]->ndefined(); i++) {
    delete static_cast<ImageRegion*>(itsMaps[0]->getVal(i));
  }
  for (uInt i=0; i<itsMaps[1]->ndefined(); i++) {
    delete static_cast<ImageRegion*>(itsMaps[1]->getVal(i));
  }
}

RegionHandlerMemory* RegionHandlerMemory::clone() const
{
  return new RegionHandlerMemory (*this);
}

Bool RegionHandlerMemory::canDefineRegion() const
{
  return True;
}

void RegionHandlerMemory::setDefaultMask (const String& regionName)
{
  itsDefaultName = regionName;
}

String RegionHandlerMemory::getDefaultMask() const
{
  return itsDefaultName;
}

Bool RegionHandlerMemory::defineRegion (const String& name,
					const ImageRegion& region,
					RegionHandler::GroupType type,
					Bool overwrite)
{
  // First check if the region is already defined in "regions" or "masks".
  // If so, remove it if possible. Otherwise throw an exception.
  Int groupField = findRegionGroup (name, RegionHandler::Any, False);
  if (groupField >= 0) {
    if (!overwrite) {
      throw (AipsError ("RegionHandlerMemory::defineRegion -"
			" a region or mask with name " + name +
			" already exists"));
    }
    itsMaps[groupField]->remove (name);
  }
  // Okay, we can define the region now.
  groupField = 0;
  if (type == RegionHandler::Masks) {
    groupField = 1;
  }
  // Now define the region in the group.
  itsMaps[groupField]->define (name, region.clone());
  return True;
}

Bool RegionHandlerMemory::hasRegion (const String& name,
				     RegionHandler::GroupType type) const
{
  return  (findRegionGroup (name, type, False) >= 0);
}

Bool RegionHandlerMemory::renameRegion (const String& newName,
					const String& oldName,
					RegionHandler::GroupType type,
					Bool overwrite)
{
  // Check that the region exists.
  Int oldGroupField = findRegionGroup (oldName, type, True);
  // First check if the region is already defined.
  // Check that the region is in the same group as the original.
  // Remove it if overwrite is true. Otherwise throw an exception.
  Int groupField = findRegionGroup (newName, RegionHandler::Any, False);
  if (groupField >= 0) {
    if (groupField != oldGroupField) {
      throw (AipsError ("RegionHandlerMemory::renameRegion -"
			" a region or mask with name " + newName +
			" already exists in another group"));
    }
    if (!overwrite) {
      throw (AipsError ("RegionHandlerMemory::renameRegion -"
			" a region or mask with name " + newName +
			" already exists"));
    }
    itsMaps[groupField]->remove (newName);
  }
  // Get the old region.
  ImageRegion* regPtr = findRegion (oldName, type, True);
  // First rename a possible mask table, which could in principle fail.
  // We only need to do that when it is an LCRegion.
  // We need to clone it to make it non-const.
  if (regPtr->isLCRegion()) {
    LCRegion* lcPtr = regPtr->asLCRegion().cloneRegion();
    lcPtr->handleRename (newName, overwrite);
    delete lcPtr;
  }
  // Rename the keyword itself.
  itsMaps[oldGroupField]->rename (newName, oldName);
  // Rename the default mask name if that is the renamed region.
  if (itsDefaultName == oldName) {
    setDefaultMask (newName);
  }
  return True;
}

Bool RegionHandlerMemory::removeRegion (const String& name,
					RegionHandler::GroupType type,
					Bool throwIfUnknown)
{
  Int groupField = findRegionGroup (name, type, throwIfUnknown);
  if (groupField >= 0) {
    ImageRegion* regPtr = findRegion (name, type, True);
    // Delete a possible mask table.
    // We only need to do that when it is an LCRegion.
    // We need to clone it to make it non-const.
    if (regPtr->isLCRegion()) {
      LCRegion* lcPtr = regPtr->asLCRegion().cloneRegion();
      String msg;
      Bool error = False;
      try {
	lcPtr->handleDelete();
      } catch (AipsError x) {
	error = True;
	msg = x.getMesg();
      }
      delete lcPtr;
      if (error) {
	throw (AipsError("RegionHandlerMemory - region " + name +
			 " could not be removed\n" + msg));
      }
    }
    // Delete the region object and remove from the map.
    delete regPtr;
    itsMaps[groupField]->remove (name);
  }
  // Clear the default mask name if that is the removed region.
  if (itsDefaultName == name) {
    setDefaultMask ("");
  }
  return True;
}

Vector<String> RegionHandlerMemory::regionNames
					(RegionHandler::GroupType type) const
{
  uInt nreg = 0;
  uInt nmask = 0;
  if (type != RegionHandler::Masks) {
    nreg = itsMaps[0]->ndefined();
  }
  if (type != RegionHandler::Regions) {
    nmask = itsMaps[1]->ndefined();
  }
  Vector<String> names(nreg + nmask);
  uInt i;
  for (i=0; i<nreg; i++) {
    names(i) = itsMaps[0]->getKey(i);
  }
  for (i=0; i<nmask; i++) {
    names(i+nreg) = itsMaps[1]->getKey(i);
  }
  return names;
}

ImageRegion* RegionHandlerMemory::getRegion (const String& name,
					     RegionHandler::GroupType type,
					     Bool throwIfUnknown) const
{
  ImageRegion* regPtr = findRegion (name, type, throwIfUnknown);
  if (regPtr != 0) {
    return regPtr->clone();
  }
  return 0;
}

ImageRegion* RegionHandlerMemory::findRegion (const String& name,
					      RegionHandler::GroupType type,
					      Bool throwIfUnknown) const
{
  Int groupField = findRegionGroup (name, type, throwIfUnknown);
  if (groupField >= 0) {
    void* regPtr = (*itsMaps[groupField])(name);
    return static_cast<ImageRegion*>(regPtr);
  }
  return 0;
}

Int RegionHandlerMemory::findRegionGroup (const String& regionName,
					  RegionHandler::GroupType type,
					  Bool throwIfUnknown) const
{
  // Check if the region is defined in "regions" or "masks".
  // If so, return its number.
  if (type != RegionHandler::Masks) {
    void* field = itsMaps[0]->isDefined (regionName);
    if (field != 0) {
      return 0;
    }
  }
  if (type != RegionHandler::Regions) {
    void* field = itsMaps[1]->isDefined (regionName);
    if (field != 0) {
      return 1;
    }
  }
  if (throwIfUnknown) {
    String typeName = "region/mask ";
    if (type == RegionHandler::Regions) {
      typeName = "region ";
    } else if (type == RegionHandler::Masks) {
      typeName = "mask ";
    }
    throw (AipsError ("RegionHandlerMemory - " + typeName + regionName +
		      " does not exist"));
  }
  return -1;
}

ImageRegion RegionHandlerMemory::makeMask (const LatticeBase& lattice,
					   const String&)
{
  LCMask* mask = new LCMask (lattice.shape());
  return ImageRegion(mask);
}

} //# NAMESPACE CASACORE - END

