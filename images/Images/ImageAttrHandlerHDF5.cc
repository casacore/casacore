//# ImageAttrHandlerHDF5.cc: Attributes handler for HDF5 images
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
#include <casacore/images/Images/ImageAttrHandlerHDF5.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>

using namespace std;

namespace casacore {

  ImageAttrHandlerHDF5::ImageAttrHandlerHDF5()
    : itsCanWrite (False)
  {}

  ImageAttrHandlerHDF5::~ImageAttrHandlerHDF5()
  {}

  ImageAttrHandlerHDF5& ImageAttrHandlerHDF5::attachHid (const HDF5Object& hid,
                                                         Bool createHandler,
                                                         Bool isWritable)
  {
    itsGroupMap.clear();
    // If ATTRGROUPS is defined, get all groups in it.
    if (HDF5Group::exists (hid, "ATTRGROUPS")) {
      itsGroup = CountedPtr<HDF5Group>(new HDF5Group(hid, "ATTRGROUPS", true));
      vector<String> names = HDF5Group::linkNames (*itsGroup);
      for (uInt i=0; i<names.size(); ++i) {
        // Add group to map, but with a null object. It gets filled once
        // the group gets used.
        itsGroupMap[names[i]] = ImageAttrGroupHDF5();
      }
      itsCanWrite = isWritable;
    } else if (createHandler) {
      // Does not exist yet. If possible, create and write it.
      if (!isWritable) {
        throw AipsError("ImageAttrHandlerHDF5: cannot add ATTRGROUPS because "
                        "image is not writable");
      }
      itsGroup = CountedPtr<HDF5Group>(new HDF5Group(hid, "ATTRGROUPS", false));
      itsCanWrite = True;
    }
    return *this;
  }

  void ImageAttrHandlerHDF5::flush()
  {
    for (map<String,ImageAttrGroupHDF5>::iterator it=itsGroupMap.begin();
           it!=itsGroupMap.end(); ++it) {
      it->second.flush (*itsGroup, it->first);
    }
  }

  Bool ImageAttrHandlerHDF5::hasGroup (const String& groupName)
  {
    return (itsGroupMap.find(groupName) != itsGroupMap.end());
  }

  Vector<String> ImageAttrHandlerHDF5::groupNames() const
  {
    Vector<String> names(itsGroupMap.size());
    uInt i=0;
    for (map<String,ImageAttrGroupHDF5>::const_iterator it=itsGroupMap.begin();
           it!=itsGroupMap.end(); ++it) {
      names[i++] = it->first;
    }
    return names;
  }

  ImageAttrGroup& ImageAttrHandlerHDF5::openGroup (const String& groupName)
  {
    map<String,ImageAttrGroupHDF5>::iterator pos = itsGroupMap.find (groupName);
    if (pos == itsGroupMap.end()) {
      throw AipsError("ImageAttrHandlerHDF5: group " + groupName +
                      " does not exist");
    }
    if (pos->second.isNull()) {
      // Read the group.
      pos->second = ImageAttrGroupHDF5(*itsGroup, groupName, itsCanWrite);
    }
    return pos->second;
  }

  ImageAttrGroup& ImageAttrHandlerHDF5::createGroup (const String& groupName)
  {
    if (hasGroup(groupName)) {
      throw AipsError("ImageAttrHandlerHDF5: group " + groupName +
                      " cannot be created; it already exists");
    }
    // Assert that a group can be created.
    if (!itsCanWrite) {
      throw AipsError("ImageAttrHandlerHDF5: cannot create group " + groupName +
                      " because image is not writable");
    }
    return itsGroupMap[groupName] = ImageAttrGroupHDF5 (True);
  }

  void ImageAttrHandlerHDF5::closeGroup (const String& groupName)
  {
    map<String,ImageAttrGroupHDF5>::iterator pos = itsGroupMap.find (groupName);
    if (pos != itsGroupMap.end()) {
      pos->second.flush (*itsGroup, groupName);
      pos->second = ImageAttrGroupHDF5();
    }
  }

} //# NAMESPACE CASACORE - END
