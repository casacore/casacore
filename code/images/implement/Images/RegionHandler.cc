//# RegionHandler.cc: Handle regions stored as table keywords
//# Copyright (C) 1999,2000
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

#include <trial/Images/RegionHandler.h>
#include <trial/Images/ImageRegion.h>
#include <aips/Lattices/LatticeBase.h>
#include <trial/Lattices/LCPagedMask.h>
#include <aips/Lattices/TiledShape.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>


void RegionHandler::setDefaultMask (Table& table,
				    const String& regionName)
{
  // Store the new default name (when writable).
  if (table.isWritable()) {
    TableRecord& keys = table.rwKeywordSet();
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

String RegionHandler::getDefaultMask (const Table& table)
{
  const TableRecord& keys = table.keywordSet();
  Int field = keys.fieldNumber ("Image_defaultmask");
  if (field < 0) {
    return "";
  }
  return keys.asString(field);
}

Bool RegionHandler::defineRegion (Table& table,
				  const String& name,
				  const ImageRegion& region,
				  RegionHandler::GroupType type,
				  Bool overwrite)
{
  if (! table.isWritable()) {
    return False;
  }
  // First check if the region is already defined in "regions" or "masks".
  // If so, remove it if possible. Otherwise throw an exception.
  TableRecord& keys = table.rwKeywordSet();
  Int groupField = findRegionGroup (table, name, RegionHandler::Any, False);
  if (groupField >= 0) {
    if (!overwrite) {
      throw (AipsError ("RegionHandler::defineRegion - table " +
			table.tableName() +
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
                        (name, region.toRecord (table.tableName()));
  return True;
}

Bool RegionHandler::hasRegion (const Table& table, const String& name,
			       RegionHandler::GroupType type)
{
  return ToBool (findRegionGroup (table, name, type, False) >= 0);
}

Bool RegionHandler::renameRegion (Table& table, const String& oldName,
				  const String& newName,
				  RegionHandler::GroupType type,
				  Bool overwrite)
{
  if (! table.isWritable()) {
    return False;
  }
  // Check that the region exists.
  Int oldGroupField = findRegionGroup (table, oldName, type, True);
  // First check if the region is already defined.
  // Check that the region is in the same group as the original.
  // Remove it if overwrite is true. Otherwise throw an exception.
  TableRecord& keys = table.rwKeywordSet();
  Int groupField = findRegionGroup (table, newName, RegionHandler::Any, False);
  if (groupField >= 0) {
    if (groupField != oldGroupField) {
      throw (AipsError ("RegionHandler::renameRegion - table " +
			table.tableName() +
			" already has a region or mask with name " + newName +
			" in another group"));
    }
    if (!overwrite) {
      throw (AipsError ("RegionHandler::renameRegion - table " +
			table.tableName() +
			" already has a region or mask with name " + newName));
    }
    TableRecord& regs = keys.rwSubRecord(groupField);
    regs.removeField (newName);
  }
  TableRecord& regs = keys.rwSubRecord(oldGroupField);
  ImageRegion* regPtr = getRegion (table, oldName, type, True);
  // First rename a possible mask table, which could in principle fail.
  // We only need to do that when it is an LCRegion.
  // We need to clone it to make it non-const.
  if (regPtr->isLCRegion()) {
    LCRegion* lcPtr = regPtr->asLCRegion().cloneRegion();
    lcPtr->handleRename (newName, overwrite);
    // The region values might be changed, so redefine it.
    // Note that the ImageRegion constructor takes over the poiter,
    // so we do not need to delete lcPtr;
    TableRecord newValue = ImageRegion(lcPtr).toRecord (table.tableName());
    regs.defineRecord (oldName, newValue);
  }
  delete regPtr;
  // Rename the keyword itself.
  regs.renameField (newName, oldName);
  // Rename the default mask name if that is the renamed region.
  if (RegionHandler::getDefaultMask(table) == oldName) {
      keys.define ("Image_defaultmask", newName);
  }
  return True;
}

Bool RegionHandler::removeRegion (Table& table, const String& name,
				  RegionHandler::GroupType type,
				  Bool throwIfUnknown)
{
  if (! table.isWritable()) {
    return False;
  }
  Int groupField = findRegionGroup (table, name, type, throwIfUnknown);
  if (groupField >= 0) {
    ImageRegion* regPtr = getRegion (table, name, type, True);
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
    TableRecord& keys = table.rwKeywordSet();
    keys.rwSubRecord(groupField).removeField (name);
  }
  return True;
}

Vector<String> RegionHandler::regionNames (const Table& table,
					   RegionHandler::GroupType type)
{
  uInt nreg = 0;
  uInt nmask = 0;
  const RecordDesc* regs = 0;
  const RecordDesc* masks = 0;
  const TableRecord& keys = table.keywordSet();
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

ImageRegion* RegionHandler::getRegion (const Table& table, const String& name,
				       RegionHandler::GroupType type,
				       Bool throwIfUnknown)
{
  Int groupField = findRegionGroup (table, name, type, throwIfUnknown);
  if (groupField >= 0) {
    const TableRecord& regs = table.keywordSet().subRecord(groupField);
    Int field = regs.fieldNumber (name);
    if (field >= 0) {
      return ImageRegion::fromRecord (regs.subRecord (field),
				      table.tableName());
    }
  }
  return 0;
}

Int RegionHandler::findRegionGroup (const Table& table,
				    const String& regionName,
				    RegionHandler::GroupType type,
				    Bool throwIfUnknown)
{
  // Check if the region is defined in "regions" or "masks".
  // If so, return its groupName.
  const TableRecord& keys = table.keywordSet();
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
    throw (AipsError ("RegionHandler: " + typeName + regionName +
		      " does not exist in table " + table.tableName()));
  }
  return -1;
}


LCPagedMask RegionHandler::makeMask (const LatticeBase& lattice,
				     const String& name)
{
  if (! lattice.isPaged()) {
    throw (AipsError ("RegionHandler::makeMask - "
		      "cannot create mask, because lattice is transient"));
  }
  return LCPagedMask (TiledShape (lattice.shape(), lattice.niceCursorShape()),
		      lattice.name() + '/' + name);
}
