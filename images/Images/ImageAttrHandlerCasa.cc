//# ImageAttrHandlerCasa.cc: Attributes handler for CASA images
//# Copyright (C) 2012
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

//# Includes
#include <casacore/images/Images/ImageAttrHandlerCasa.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>

using namespace std;

namespace casacore {

  ImageAttrHandlerCasa::ImageAttrHandlerCasa()
    : itsCanAdd (False)
  {}

  ImageAttrHandlerCasa::~ImageAttrHandlerCasa()
  {}

  void ImageAttrHandlerCasa::flush()
  {
    for (map<String,ImageAttrGroupCasa>::iterator it=itsGroupMap.begin();
           it!=itsGroupMap.end(); ++it) {
      it->second.flush();
    }
  }

  ImageAttrHandlerCasa& ImageAttrHandlerCasa::attachTable (const Table& image,
                                                           Bool createHandler)
  {
    itsImageTable = image;
    itsGroupMap.clear();
    // If ATTRGROUPS is defined, get all subtables (groups) in it.
    if (itsImageTable.keywordSet().isDefined("ATTRGROUPS")) {
      const TableRecord& rec = itsImageTable.keywordSet().subRecord("ATTRGROUPS");
      for (uInt i=0; i<rec.nfields(); ++i) {
        if (rec.dataType(i) == TpTable) {
          // Add group to map, but with a null object. It gets filled once
          // the group gets used.
          itsGroupMap[rec.name(i)] = ImageAttrGroupCasa();
        }
      }
      itsCanAdd = True;
    } else if (createHandler) {
      // Does not exist yet, so create and write it.
      itsImageTable.reopenRW();
      itsImageTable.rwKeywordSet().defineRecord ("ATTRGROUPS", TableRecord());
      itsCanAdd = True;
    }
    return *this;
  }

  Bool ImageAttrHandlerCasa::hasGroup (const String& groupName)
  {
    return (itsGroupMap.find(groupName) != itsGroupMap.end());
  }

  Vector<String> ImageAttrHandlerCasa::groupNames() const
  {
    Vector<String> names(itsGroupMap.size());
    uInt i=0;
    for (map<String,ImageAttrGroupCasa>::const_iterator it=itsGroupMap.begin();
           it!=itsGroupMap.end(); ++it) {
      names[i++] = it->first;
    }
    return names;
  }

  ImageAttrGroup& ImageAttrHandlerCasa::openGroup (const String& groupName)
  {
    map<String,ImageAttrGroupCasa>::iterator pos = itsGroupMap.find (groupName);
    if (pos == itsGroupMap.end()) {
      throw AipsError("ImageAttrHandlerCasa: group " + groupName +
                      " does not exist");
    }
    if (pos->second.isNull()) {
      // Open the subtable.
      pos->second = ImageAttrGroupCasa(itsImageTable, groupName);
    }
    return pos->second;
  }

  ImageAttrGroup& ImageAttrHandlerCasa::createGroup (const String& groupName)
  {
    if (hasGroup(groupName)) {
      throw AipsError("ImageAttrHandlerCasa: group " + groupName +
                      " cannot be created; it already exists");
    }
    // Assert that a group can be added.
    if (!itsCanAdd) {
      throw AipsError("ImageAttrHandlerCasa: cannot create group " + groupName +
                      " because table keyword ATTRGROUPS does not exist");
    }
    // Make write possible.
    itsImageTable.reopenRW();
    // Create an empty subtable for the new group.
    SetupNewTable newtab(itsImageTable.tableName() + '/' + groupName,
                         TableDesc(), Table::New);
    Table tab(newtab);
    tab.flush();
    // Define the keyword holding the group.
    TableRecord& keyset = itsImageTable.rwKeywordSet();
    TableRecord& rec    = keyset.rwSubRecord("ATTRGROUPS");
    rec.defineTable (groupName, tab);
    return itsGroupMap[groupName] = ImageAttrGroupCasa(itsImageTable,groupName);
  }

  void ImageAttrHandlerCasa::closeGroup (const String& groupName)
  {
    map<String,ImageAttrGroupCasa>::iterator pos = itsGroupMap.find (groupName);
    if (pos != itsGroupMap.end()) {
      pos->second.flush();
      pos->second = ImageAttrGroupCasa();
    }
  }

} //# NAMESPACE CASACORE - END
