//# RegionHandler.cc: Handle regions stored as table keywords
//# Copyright (C) 1999
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
#include <trial/Lattices/LatticeBase.h>
#include <trial/Lattices/LCPagedMask.h>
#include <trial/Lattices/TiledShape.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>


void RegionHandler::setDefaultMask (Table& table,
				      const String& regionName)
{
  // Store the new default name (when writable).
  if (table.isWritable()) {
    if (regionName.empty()) {
      table.rwKeywordSet().removeField ("Image_defaultmask");
    } else {
      table.rwKeywordSet().define ("Image_defaultmask", regionName);
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

ImageRegion* RegionHandler::makeRegion (const Table& table,
					const String& regionName)
{
  // Make no region if the name is empty.
  if (regionName.empty()) {
    return 0;
  }
  // Try to find the region.
  ImageRegion* regPtr = 0;
  const TableRecord& keys = table.keywordSet();
  Int field = keys.fieldNumber ("regions");
  if (field >= 0) {
    const TableRecord& regs = keys.subRecord(field);
    field = regs.fieldNumber (regionName);
    if (field >= 0) {
      regPtr = ImageRegion::fromRecord (regs.subRecord (field),
					table.tableName());
    }
  }
  if (regPtr == 0) {
    throw (AipsError ("RegionHandler::setDefaultMask - region " + regionName +
		      " does not exist in table " + table.tableName()));
  }
  return regPtr;
}


void RegionHandler::defineRegion (Table& table,
				  const String& name,
				  const ImageRegion& region,
				  Bool overwrite)
{
  if (table.isWritable()) {
    TableRecord& keys = table.rwKeywordSet();
    Int field = keys.fieldNumber ("regions");
    if (field < 0) {
      keys.defineRecord ("regions", TableRecord());
    }
    TableRecord& regs = keys.rwSubRecord("regions");
    if (!overwrite  &&  regs.isDefined (name)) {
	throw (AipsError ("RegionHandler::defineRegion - table " +
			  table.tableName() +
			  " already has a region with name " + name));
    }
    regs.defineRecord (name, region.toRecord (table.tableName()));
  }
}

void RegionHandler::removeRegion (Table& table, const String& name)
{
  if (table.isWritable()) {
    TableRecord& keys = table.rwKeywordSet();
    Int field = keys.fieldNumber ("regions");
    if (field >= 0) {
      TableRecord& regs = keys.rwSubRecord(field);
      field = regs.fieldNumber (name);
      if (field >= 0) {
	regs.removeField (field);
      }
    }
  }
}

ImageRegion* RegionHandler::getRegion (const Table& table, const String& name)
{
  const TableRecord& keys = table.keywordSet();
  Int field = keys.fieldNumber ("regions");
  if (field >= 0) {
    const TableRecord& regs = keys.subRecord(field);
    field = regs.fieldNumber (name);
    if (field >= 0) {
      return ImageRegion::fromRecord (regs.subRecord (field),
				      table.tableName());
    }
  }
  return 0;
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
