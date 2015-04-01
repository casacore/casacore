//# RegionHandlerTable.cc: Class for keeping regions in a table
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

#include <casacore/images/Regions/RegionHandlerTable.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCPagedMask.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

RegionHandlerTable::RegionHandlerTable (GetCallback* callback,
					void* objectPtr)
: itsCallback  (callback),
  itsObjectPtr (objectPtr)
{}

RegionHandlerTable::RegionHandlerTable (const RegionHandlerTable& that)
: RegionHandler(that),
  itsCallback  (that.itsCallback),
  itsObjectPtr (that.itsObjectPtr)
{}

RegionHandlerTable::~RegionHandlerTable()
{}

RegionHandlerTable& RegionHandlerTable::operator=
                                       (const RegionHandlerTable& that)
{
  if (this != &that) {
    itsCallback  = that.itsCallback;
    itsObjectPtr = that.itsObjectPtr;
  }
  return *this;
}

RegionHandlerTable* RegionHandlerTable::clone() const
{
  return new RegionHandlerTable (*this);
}

void RegionHandlerTable::setObjectPtr (void* objectPtr)
{
  itsObjectPtr = objectPtr;
}

Bool RegionHandlerTable::canDefineRegion() const
{
  return True;
}

void RegionHandlerTable::setDefaultMask (const String& regionName)
{
  Table& tab = rwTable();
  // Store the new default name (when writable).
  if (tab.isWritable()) {
    TableRecord& keys = tab.rwKeywordSet();
    if (regionName.empty()) {
      // Only delete default mask if it exists.
      if (keys.isDefined ("Image_defaultmask")) {
	keys.removeField ("Image_defaultmask");
      }
    } else {
      keys.define ("Image_defaultmask", regionName);
    }
  }
}

String RegionHandlerTable::getDefaultMask() const
{
  const Table& tab = table();
  const TableRecord& keys = tab.keywordSet();
  Int field = keys.fieldNumber ("Image_defaultmask");
  if (field < 0) {
    return "";
  }
  return keys.asString(field);
}

Bool RegionHandlerTable::defineRegion (const String& name,
				       const ImageRegion& region,
				       RegionHandler::GroupType type,
				       Bool overwrite)
{
  Table& tab = rwTable();
  if (! tab.isWritable()) {
    return False;
  }
  // First check if the region is already defined in "regions" or "masks".
  // If so, remove it if possible. Otherwise throw an exception.
  TableRecord& keys = tab.rwKeywordSet();
  Int groupField = findRegionGroup (name, RegionHandler::Any, False);
  if (groupField >= 0) {
    if (!overwrite) {
      throw (AipsError ("RegionHandlerTable::defineRegion - table " +
			tab.tableName() +
			" already has a region or mask with name " + name));
    }
    TableRecord& regs = keys.rwSubRecord(groupField);
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
  if (! keys.isDefined (groupName)) {
    keys.defineRecord (groupName, TableRecord());
  }
  // Now define the region in the group.
  keys.rwSubRecord(groupName).defineRecord
                        (name, region.toRecord (tab.tableName()));
  return True;
}

Bool RegionHandlerTable::hasRegion (const String& name,
				    RegionHandler::GroupType type) const
{
  return  (findRegionGroup (name, type, False) >= 0);
}

Bool RegionHandlerTable::renameRegion (const String& newName,
				       const String& oldName,
				       RegionHandler::GroupType type,
				       Bool overwrite)
{
  Table& tab = rwTable();
  if (! tab.isWritable()) {
    return False;
  }
  // Check that the region exists.
  Int oldGroupField = findRegionGroup (oldName, type, True);
  // First check if the region is already defined.
  // Check that the region is in the same group as the original.
  // Remove it if overwrite is true. Otherwise throw an exception.
  TableRecord& keys = tab.rwKeywordSet();
  Int groupField = findRegionGroup (newName, RegionHandler::Any, False);
  if (groupField >= 0) {
    if (groupField != oldGroupField) {
      throw (AipsError ("RegionHandlerTable::renameRegion - table " +
			tab.tableName() +
			" already has a region or mask with name " + newName +
			" in another group"));
    }
    if (!overwrite) {
      throw (AipsError ("RegionHandlerTable::renameRegion - table " +
			tab.tableName() +
			" already has a region or mask with name " + newName));
    }
    TableRecord& regs = keys.rwSubRecord(groupField);
    regs.removeField (newName);
  }
  TableRecord& regs = keys.rwSubRecord(oldGroupField);
  ImageRegion* regPtr = getRegion (oldName, type, True);
  // First rename a possible mask table, which could in principle fail.
  // We only need to do that when it is an LCRegion.
  // We need to clone it to make it non-const.
  if (regPtr->isLCRegion()) {
    LCRegion* lcPtr = regPtr->asLCRegion().cloneRegion();
    lcPtr->handleRename (newName, overwrite);
    // The region values might be changed, so redefine it.
    // Note that the ImageRegion constructor takes over the poiter,
    // so we do not need to delete lcPtr;
    TableRecord newValue = ImageRegion(lcPtr).toRecord (tab.tableName());
    regs.defineRecord (oldName, newValue);
  }
  delete regPtr;
  // Rename the keyword itself.
  regs.renameField (newName, oldName);
  // Rename the default mask name if that is the renamed region.
  if (getDefaultMask() == oldName) {
      keys.define ("Image_defaultmask", newName);
  }
  return True;
}

Bool RegionHandlerTable::removeRegion (const String& name,
				       RegionHandler::GroupType type,
				       Bool throwIfUnknown)
{
  Table& tab = rwTable();
  if (! tab.isWritable()) {
    return False;
  }
  Int groupField = findRegionGroup (name, type, throwIfUnknown);
  if (groupField >= 0) {
    ImageRegion* regPtr = getRegion (name, type, True);
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
	delete regPtr;
	throw (AipsError("Region " + name + " could not be removed\n" + msg));
      }
    }
    delete regPtr;
    TableRecord& keys = tab.rwKeywordSet();
    keys.rwSubRecord(groupField).removeField (name);
  }
  // Clear the default mask name if that is the removed region.
  if (getDefaultMask() == name) {
    setDefaultMask ("");
  }
  return True;
}

Vector<String> RegionHandlerTable::regionNames
					(RegionHandler::GroupType type) const
{
  const Table& tab = table();
  uInt nreg = 0;
  uInt nmask = 0;
  const RecordDesc* regs = 0;
  const RecordDesc* masks = 0;
  const TableRecord& keys = tab.keywordSet();
  if (type != RegionHandler::Masks) {
    Int field = keys.fieldNumber ("regions");
    if (field >= 0) {
      regs = &(keys.subRecord(field).description());
      nreg = regs->nfields();
    }
  }
  if (type != RegionHandler::Regions) {
    Int field = keys.fieldNumber ("masks");
    if (field >= 0) {
      masks = &(keys.subRecord(field).description());
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

ImageRegion* RegionHandlerTable::getRegion (const String& name,
					    RegionHandler::GroupType type,
					    Bool throwIfUnknown) const
{
  const Table& tab = table();
  Int groupField = findRegionGroup (name, type, throwIfUnknown);
  if (groupField >= 0) {
    const TableRecord& regs = tab.keywordSet().subRecord(groupField);
    Int field = regs.fieldNumber (name);
    if (field >= 0) {
      return ImageRegion::fromRecord (regs.subRecord (field),
				      tab.tableName());
    }
  }
  return 0;
}

Int RegionHandlerTable::findRegionGroup (const String& regionName,
					 RegionHandler::GroupType type,
					 Bool throwIfUnknown) const
{
  const Table& tab = table();
  // Check if the region is defined in "regions" or "masks".
  // If so, return its groupName.
  const TableRecord& keys = tab.keywordSet();
  if (type != RegionHandler::Masks) {
    Int field = keys.fieldNumber ("regions");
    if (field >= 0) {
      const TableRecord& regs = keys.subRecord(field);
      if (regs.isDefined (regionName)) {
	return field;
      }
    }
  }
  if (type != RegionHandler::Regions) {
    Int field = keys.fieldNumber ("masks");
    if (field >= 0) {
      const TableRecord& regs = keys.subRecord(field);
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
    throw (AipsError ("RegionHandlerTable: " + typeName + regionName +
		      " does not exist in table " + tab.tableName()));
  }
  return -1;
}

ImageRegion RegionHandlerTable::makeMask (const LatticeBase& lattice,
					  const String& name)
{
  if (! lattice.isPaged()) {
    throw (AipsError ("RegionHandlerTable::makeMask - "
		      "cannot create mask, because image is transient"));
  }
  LCPagedMask* mask = new LCPagedMask (TiledShape (lattice.shape(),
						   lattice.niceCursorShape()),
				       lattice.name() + '/' + name);
  return ImageRegion(mask);
}

} //# NAMESPACE CASACORE - END

