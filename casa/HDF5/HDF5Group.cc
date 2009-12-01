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
#include <casa/HDF5/HDF5Group.h>
#include <casa/HDF5/HDF5Error.h>

namespace casa { //# NAMESPACE CASA - BEGIN

#ifdef HAVE_HDF5

  void HDF5Group::init (hid_t parentHid, const String& parentName,
			const String& name,
			bool mustExist, bool mustNotExist)
  {
    String type = "create";
    if (mustNotExist) {
      setHid (H5Gcreate(parentHid, name.c_str(), H5P_DEFAULT,
			H5P_DEFAULT, H5P_DEFAULT));
    } else {
      type = "open";
      setHid (H5Gopen(parentHid, name.c_str(), H5P_DEFAULT));
      if (!isValid()  &&  !mustExist) {
	type = "open or create";
	setHid (H5Gcreate(parentHid, name.c_str(), H5P_DEFAULT,
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

  void HDF5Group::remove (const HDF5Object& parentHid, const String& name)
  {
    // The delete fails if the group does not exist, but that is no problem.
    H5Ldelete (parentHid, name.c_str(), H5P_LINK_ACCESS_DEFAULT);
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

  void HDF5Group::remove (const HDF5Object&, const String&)
  {
    HDF5Object::throwNoHDF5();
  }

#endif

}
