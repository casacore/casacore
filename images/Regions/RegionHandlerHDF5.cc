//# RegionHandlerHDF5.cc: Class for keeping regions in an HDF5 file
//# Copyright (C) 2008
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

#include <casacore/images/Regions/RegionHandlerHDF5.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCHDF5Mask.h>
#include <casacore/casa/HDF5/HDF5Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

RegionHandlerHDF5::RegionHandlerHDF5 (GetCallback* callback,
				      void* objectPtr)
  : itsChanged   (False),
    itsCallback  (callback),
    itsObjectPtr (objectPtr)
{}

RegionHandlerHDF5::RegionHandlerHDF5 (const RegionHandlerHDF5& that)
  : RegionHandler(that),
    itsChanged   (that.itsChanged),
    itsCallback  (that.itsCallback),
    itsObjectPtr (that.itsObjectPtr)
{}

RegionHandlerHDF5::~RegionHandlerHDF5()
{}

RegionHandlerHDF5& RegionHandlerHDF5::operator=
                                       (const RegionHandlerHDF5& that)
{
  if (this != &that) {
    itsChanged   = that.itsChanged;
    itsCallback  = that.itsCallback;
    itsObjectPtr = that.itsObjectPtr;
  }
  return *this;
}

RegionHandlerHDF5* RegionHandlerHDF5::clone() const
{
  return new RegionHandlerHDF5 (*this);
}

void RegionHandlerHDF5::setObjectPtr (void* objectPtr)
{
  itsObjectPtr = objectPtr;
}

Bool RegionHandlerHDF5::canDefineRegion() const
{
  return True;
}

void RegionHandlerHDF5::setDefaultMask (const String& regionName)
{
  itsRecord.define ("Image_defaultmask", regionName);
  itsChanged = True;
}

String RegionHandlerHDF5::getDefaultMask() const
{
  Int field = itsRecord.fieldNumber ("Image_defaultmask");
  if (field < 0) {
    return String();
  }
  return itsRecord.asString(field);
}

Bool RegionHandlerHDF5::defineRegion (const String& name,
				      const ImageRegion& region,
				      RegionHandler::GroupType type,
				      Bool overwrite)
{
  // First check if the region is already defined in "regions" or "masks".
  // If so, remove it if possible. Otherwise throw an exception.
  Int groupField = findRegionGroup (name, RegionHandler::Any, False);
  if (groupField >= 0) {
    if (!overwrite) {
      throw (AipsError ("RegionHandlerHDF5::defineRegion - file " +
			file()->getName() +
			" already has a region or mask with name " + name));
    }
    TableRecord& regs = itsRecord.rwSubRecord(groupField);
    if (regs.isDefined (name)) {
      regs.removeField (name);
    }
  }
  // Okay, we can define the region now.
  // Define the "regions" or "masks" group when needed.
  String groupName = "regions";
  if (type == RegionHandler::Masks) {
    groupName = "masks";
  }
  if (! itsRecord.isDefined (groupName)) {
    itsRecord.defineRecord (groupName, TableRecord());
  }
  // Now define the region in the group.
  itsRecord.rwSubRecord(groupName).defineRecord
                              (name, region.toRecord (file()->getName()));
  itsChanged = True;
  return True;
}

Bool RegionHandlerHDF5::hasRegion (const String& name,
				   RegionHandler::GroupType type) const
{
  return  (findRegionGroup (name, type, False) >= 0);
}

Bool RegionHandlerHDF5::renameRegion (const String& newName,
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
      throw (AipsError ("RegionHandlerHDF5::renameRegion - file " +
			file()->getName() +
			" already has a region or mask with name " + newName +
			" in another group"));
    }
    if (!overwrite) {
      throw (AipsError ("RegionHandlerHDF5::renameRegion - file " +
			file()->getName() +
			" already has a region or mask with name " + newName));
    }
    TableRecord& regs = itsRecord.rwSubRecord(groupField);
    regs.removeField (newName);
  }
  TableRecord& regs = itsRecord.rwSubRecord(oldGroupField);
  ImageRegion* regPtr = getRegion (oldName, type, True);
  // First rename a possible mask file, which could in principle fail.
  // We only need to do that when it is an LCRegion.
  // We need to clone it to make it non-const.
  if (regPtr->isLCRegion()) {
    LCRegion* lcPtr = regPtr->asLCRegion().cloneRegion();
    lcPtr->handleRename (newName, overwrite);
    // The region values might be changed, so redefine it.
    // Note that the ImageRegion constructor takes over the poiter,
    // so we do not need to delete lcPtr;
    TableRecord newValue = ImageRegion(lcPtr).toRecord (file()->getName());
    regs.defineRecord (oldName, newValue);
  }
  delete regPtr;
  // Rename the keyword itself.
  regs.renameField (newName, oldName);
  // Rename the default mask name if that is the renamed region.
  if (getDefaultMask() == oldName) {
    setDefaultMask (newName);
  }
  itsChanged = True;
  return True;
}

Bool RegionHandlerHDF5::removeRegion (const String& name,
				      RegionHandler::GroupType type,
				      Bool throwIfUnknown)
{
  Int groupField = findRegionGroup (name, type, throwIfUnknown);
  if (groupField >= 0) {
    ImageRegion* regPtr = getRegion (name, type, True);
    // Delete a possible mask file.
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
	delete regPtr;
	throw (AipsError("Region " + name + " could not be removed\n" + msg));
      }
    }
    delete regPtr;
    itsRecord.rwSubRecord(groupField).removeField (name);
  }
  // Clear the default mask name if that is the removed region.
  if (getDefaultMask() == name) {
    setDefaultMask (String());
  }
  itsChanged = True;
  return True;
}

Vector<String> RegionHandlerHDF5::regionNames
					(RegionHandler::GroupType type) const
{
  uInt nreg = 0;
  uInt nmask = 0;
  const RecordDesc* regs = 0;
  const RecordDesc* masks = 0;
  if (type != RegionHandler::Masks) {
    Int field = itsRecord.fieldNumber ("regions");
    if (field >= 0) {
      regs = &(itsRecord.subRecord(field).description());
      nreg = regs->nfields();
    }
  }
  if (type != RegionHandler::Regions) {
    Int field = itsRecord.fieldNumber ("masks");
    if (field >= 0) {
      masks = &(itsRecord.subRecord(field).description());
      nmask = masks->nfields();
    }
  }
  Vector<String> names(nreg + nmask);
  uInt i;
  for (i=0; i<nreg; i++) {
    names(i) = regs->name(i);
  }
  for (i=0; i<nmask; i++) {
    names(i+nreg) = masks->name(i);
  }
  return names;
}

ImageRegion* RegionHandlerHDF5::getRegion (const String& name,
					   RegionHandler::GroupType type,
					   Bool throwIfUnknown) const
{
  Int groupField = findRegionGroup (name, type, throwIfUnknown);
  if (groupField >= 0) {
    const TableRecord& regs = itsRecord.subRecord(groupField);
    Int field = regs.fieldNumber (name);
    if (field >= 0) {
      return ImageRegion::fromRecord (regs.subRecord (field),
				      file()->getName());
    }
  }
  return 0;
}

Int RegionHandlerHDF5::findRegionGroup (const String& regionName,
					RegionHandler::GroupType type,
					Bool throwIfUnknown) const
{
  // Check if the region is defined in "regions" or "masks".
  // If so, return its groupName.
  if (type != RegionHandler::Masks) {
    Int field = itsRecord.fieldNumber ("regions");
    if (field >= 0) {
      const TableRecord& regs = itsRecord.subRecord(field);
      if (regs.isDefined (regionName)) {
	return field;
      }
    }
  }
  if (type != RegionHandler::Regions) {
    Int field = itsRecord.fieldNumber ("masks");
    if (field >= 0) {
      const TableRecord& regs = itsRecord.subRecord(field);
      if (regs.isDefined (regionName)) {
	return field;
      }
    }
  }
  if (throwIfUnknown) {
    String typeName = "region/mask ";
    if (type == RegionHandler::Regions) {
      typeName = "region ";
    } else if (type == RegionHandler::Masks) {
      typeName = "mask ";
    }
    throw (AipsError ("RegionHandlerHDF5: " + typeName + regionName +
		      " does not exist in file " + file()->getName()));
  }
  return -1;
}

ImageRegion RegionHandlerHDF5::makeMask (const LatticeBase& lattice,
					 const String& name)
{
  if (! lattice.isPaged()) {
    throw (AipsError ("RegionHandlerHDF5::makeMask - "
		      "cannot create mask, because image is transient"));
  }
  LCHDF5Mask* mask = new LCHDF5Mask (TiledShape (lattice.shape(),
						 lattice.niceCursorShape()),
				     file(), name);
  return ImageRegion(mask);
}

void RegionHandlerHDF5::save (Bool always)
{
  if (itsChanged || always) {
    HDF5Record::writeRecord (*file(), "maskinfo", itsRecord);
    itsChanged = False;
  }
}

void RegionHandlerHDF5::restore()
{
  itsRecord = HDF5Record::readRecord (*file(), "maskinfo");
}

} //# NAMESPACE CASACORE - END
