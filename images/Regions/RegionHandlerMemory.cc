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

#include <casacore/images/Regions/RegionHandlerMemory.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCMask.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

RegionHandlerMemory::RegionHandlerMemory()
{}

RegionHandlerMemory::RegionHandlerMemory (const RegionHandlerMemory& that)
 : RegionHandler(that)
{
  operator= (that);
}

RegionHandlerMemory::~RegionHandlerMemory()
{
  clear();
}

RegionHandlerMemory& RegionHandlerMemory::operator=
                                         (const RegionHandlerMemory& that)
{
  if (this != &that) {
    clear();
    itsDefaultName = that.itsDefaultName;
    itsMaps[0] = that.itsMaps[0];
    itsMaps[1] = that.itsMaps[1];
    for (auto& x : itsMaps[0]) {
      x.second = static_cast<ImageRegion*>(x.second)->clone();
    }
    for (auto& x : itsMaps[1]) {
      x.second = static_cast<ImageRegion*>(x.second)->clone();
    }
  }
  return *this;
}

void RegionHandlerMemory::clear()
{
  for (auto& x : itsMaps[0]) {
    delete static_cast<ImageRegion*>(x.second);
    x.second = 0;
  }
  for (auto& x : itsMaps[1]) {
    delete static_cast<ImageRegion*>(x.second);
    x.second = 0;
  }
}

RegionHandlerMemory* RegionHandlerMemory::clone() const
{
  return new RegionHandlerMemory (*this);
}

bool RegionHandlerMemory::canDefineRegion() const
{
  return true;
}

void RegionHandlerMemory::setDefaultMask (const String& regionName)
{
  itsDefaultName = regionName;
}

String RegionHandlerMemory::getDefaultMask() const
{
  return itsDefaultName;
}

bool RegionHandlerMemory::defineRegion (const String& name,
					const ImageRegion& region,
					RegionHandler::GroupType type,
					bool overwrite)
{
  // First check if the region is already defined in "regions" or "masks".
  // If so, remove it if possible. Otherwise throw an exception.
  int32_t groupField = findRegionGroup (name, RegionHandler::Any, false);
  if (groupField >= 0) {
    if (!overwrite) {
      throw (AipsError ("RegionHandlerMemory::defineRegion -"
			" a region or mask with name " + name +
			" already exists"));
    }
    itsMaps[groupField].erase (name);
  }
  // Okay, we can define the region now.
  groupField = 0;
  if (type == RegionHandler::Masks) {
    groupField = 1;
  }
  // Now define the region in the group.
  itsMaps[groupField][name] = region.clone();
  return true;
}

bool RegionHandlerMemory::hasRegion (const String& name,
				     RegionHandler::GroupType type) const
{
  return  (findRegionGroup (name, type, false) >= 0);
}

bool RegionHandlerMemory::renameRegion (const String& newName,
					const String& oldName,
					RegionHandler::GroupType type,
					bool overwrite)
{
  // Check that the region exists.
  int32_t oldGroupField = findRegionGroup (oldName, type, true);
  // First check if the region is already defined.
  // Check that the region is in the same group as the original.
  // Remove it if overwrite is true. Otherwise throw an exception.
  int32_t groupField = findRegionGroup (newName, RegionHandler::Any, false);
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
    itsMaps[groupField].erase (newName);
  }
  // Get the old region.
  ImageRegion* regPtr = findRegion (oldName, type, true);
  // First rename a possible mask table, which could in principle fail.
  // We only need to do that when it is an LCRegion.
  // We need to clone it to make it non-const.
  if (regPtr->isLCRegion()) {
    LCRegion* lcPtr = regPtr->asLCRegion().cloneRegion();
    lcPtr->handleRename (newName, overwrite);
    delete lcPtr;
  }
  // Rename the keyword itself (remove and insert).
  void* value = itsMaps[oldGroupField].at (oldName);
  itsMaps[oldGroupField].erase (oldName);
  itsMaps[oldGroupField][newName] = value;
  // Rename the default mask name if that is the renamed region.
  if (itsDefaultName == oldName) {
    setDefaultMask (newName);
  }
  return true;
}

bool RegionHandlerMemory::removeRegion (const String& name,
					RegionHandler::GroupType type,
					bool throwIfUnknown)
{
  int32_t groupField = findRegionGroup (name, type, throwIfUnknown);
  if (groupField >= 0) {
    ImageRegion* regPtr = findRegion (name, type, true);
    // Delete a possible mask table.
    // We only need to do that when it is an LCRegion.
    // We need to clone it to make it non-const.
    if (regPtr->isLCRegion()) {
      LCRegion* lcPtr = regPtr->asLCRegion().cloneRegion();
      String msg;
      bool error = false;
      try {
	lcPtr->handleDelete();
      } catch (std::exception& x) {
	error = true;
	msg = x.what();
      }
      delete lcPtr;
      if (error) {
	throw (AipsError("RegionHandlerMemory - region " + name +
			 " could not be removed\n" + msg));
      }
    }
    // Delete the region object and remove from the map.
    delete regPtr;
    itsMaps[groupField].erase (name);
  }
  // Clear the default mask name if that is the removed region.
  if (itsDefaultName == name) {
    setDefaultMask ("");
  }
  return true;
}

Vector<String> RegionHandlerMemory::regionNames
					(RegionHandler::GroupType type) const
{
  uint32_t nreg = 0;
  uint32_t nmask = 0;
  if (type != RegionHandler::Masks) {
    nreg = itsMaps[0].size();
  }
  if (type != RegionHandler::Regions) {
    nmask = itsMaps[1].size();
  }
  Vector<String> names(nreg + nmask);
  uint32_t i=0;
  if (nreg > 0) {
    for (const auto& x : itsMaps[0]) {
      names[i++] = x.first;
    }
  }
  if (nmask > 0) {
    for (const auto& x : itsMaps[1]) {
      names[i++] = x.first;
    }
  }
  return names;
}

ImageRegion* RegionHandlerMemory::getRegion (const String& name,
					     RegionHandler::GroupType type,
					     bool throwIfUnknown) const
{
  ImageRegion* regPtr = findRegion (name, type, throwIfUnknown);
  if (regPtr != 0) {
    return regPtr->clone();
  }
  return 0;
}

ImageRegion* RegionHandlerMemory::findRegion (const String& name,
					      RegionHandler::GroupType type,
					      bool throwIfUnknown) const
{
  int32_t groupField = findRegionGroup (name, type, throwIfUnknown);
  if (groupField >= 0) {
    return static_cast<ImageRegion*>(itsMaps[groupField].at(name));
  }
  return 0;
}

int32_t RegionHandlerMemory::findRegionGroup (const String& regionName,
					  RegionHandler::GroupType type,
					  bool throwIfUnknown) const
{
  // Check if the region is defined in "regions" or "masks".
  // If so, return its number.
  if (type != RegionHandler::Masks) {
    if (itsMaps[0].find (regionName) != itsMaps[0].end()) {
      return 0;
    }
  }
  if (type != RegionHandler::Regions) {
    if (itsMaps[1].find (regionName) != itsMaps[1].end()) {
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

