//# HDF5Group.cc: An class representing an HDF5 group
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

//# Includes
#include <casacore/casa/HDF5/HDF5Group.h>
#include <casacore/casa/HDF5/HDF5Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#ifdef HAVE_HDF5

  void HDF5Group::init (hid_t parentHid, const String& parentName,
			const String& name,
			bool mustExist, bool mustNotExist)
  {
    String type = "create";
    if (mustNotExist) {
      setHid (H5Gcreate2(parentHid, name.c_str(), H5P_DEFAULT,
			 H5P_DEFAULT, H5P_DEFAULT));
    } else {
      type = "open";
      // Note that testing if the link exists does not work for /.
      if (name == "/"  ||
          H5Lexists (parentHid, name.c_str(), H5P_LINK_ACCESS_DEFAULT) == 1) {
        setHid (H5Gopen2(parentHid, name.c_str(), H5P_DEFAULT));
      }
      if (!isValid()  &&  !mustExist) {
	type = "open or create";
	setHid (H5Gcreate2(parentHid, name.c_str(), H5P_DEFAULT,
			   H5P_DEFAULT, H5P_DEFAULT));
      }
    }
    if (! isValid()) {
      throw HDF5Error ("Could not " + type + " group " + name +
		       " in parent " + parentName);
    }
    setName (name);
  }

  HDF5Group::~HDF5Group()
  {
    close();
  }
  
  void HDF5Group::close()
  {
    if (isValid()) {
      H5Gclose(getHid());
      clearHid();
    }
  }

  // The callback function for function linkNames.
  herr_t doAddLinkName (hid_t, const char* name,
                        const H5L_info_t*, void* arg)
  {
    static_cast<std::vector<String>*>(arg)->push_back (String(name));
    return 0;
  }

  std::vector<String> HDF5Group::linkNames (const HDF5Object& parentHid)
  {
    // Now read all subrecords.
    //# Use INDEX_NAME (using INDEX_CRT_ORDER results in a return of -1).
    std::vector<String> names;
    names.reserve (16);
    hsize_t idx=0;
    H5Literate (parentHid, H5_INDEX_NAME, H5_ITER_NATIVE, &idx,
		&doAddLinkName, &names);
    return names;
  }

  bool HDF5Group::exists (const HDF5Object& parentHid, const String& name)
  {
    return H5Lexists (parentHid, name.c_str(), H5P_LINK_ACCESS_DEFAULT) == 1;
  }

  void HDF5Group::remove (const HDF5Object& parentHid, const String& name)
  {
    if (exists (parentHid, name)) {
      H5Ldelete (parentHid, name.c_str(), H5P_LINK_ACCESS_DEFAULT);
    }
  }

#else

  void HDF5Group::init (hid_t, const String&,
			const String&,
			bool, bool)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5Group::~HDF5Group()
  {}
  
  void HDF5Group::close()
  {}

  std::vector<String> HDF5Group::linkNames (const HDF5Object&)
  {
    return std::vector<String>();
  }

  bool HDF5Group::exists (const HDF5Object&, const String&)
  {
    return false;
  }

  void HDF5Group::remove (const HDF5Object&, const String&)
  {
    HDF5Object::throwNoHDF5();
  }

#endif

}
