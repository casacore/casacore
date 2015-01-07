//# HDF5Group.h: An class representing an HDF5 group
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

#ifndef CASA_HDF5GROUP_H
#define CASA_HDF5GROUP_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/HDF5/HDF5Object.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // A class representing an HDF5 group.
  // </summary>
  // <use visibility=export>
  // <reviewed reviewer="" date="" tests="tHDF5Dataset.cc">
  // </reviewed>
  // <synopsis>
  // This class wraps an HDF5 group hid (hdf5 id). It offers two benefits:
  // <ul>
  //  <li> The most important is resource management. In case of an exception,
  //       the hid will automatically be closed by the destructor.
  //  <li> A hid is a kind of pointer and should not be copied. These classes
  //       make it possible to use them in a shared pointer.
  // </ul>
  // </synopsis>

  class HDF5Group : public HDF5Object
  {
  public: 
    // Construct from given hid.
    HDF5Group()
    {}

    // Open or create a group at the given hid.
    // Default is that the group may exist; it is created if not existing.
    // <group>
    HDF5Group (const HDF5Object& parentHid,
	       const String& name,
	       bool mustExist=false, bool mustNotExist=false)
      { init (parentHid, parentHid.getName(), name, mustExist, mustNotExist); }
    HDF5Group (hid_t parentHid,
	       const String& name,
	       bool mustExist=false, bool mustNotExist=false)
      { init (parentHid, String(), name, mustExist, mustNotExist); }
    // </group>

    // The destructor closes the hid.
    virtual ~HDF5Group();

    // Close the hid if valid.
    virtual void close();

    // Get the names of all links at the given hid.
    static std::vector<String> linkNames (const HDF5Object& parentHid);

    // Test if the group at the given hid exists.
    static bool exists (const HDF5Object& parentHid, const String& name);

    // Delete group at the given hid if it exists.
    static void remove (const HDF5Object& parentHid, const String& name);

  private:
    // Copy constructor cannot be used.
    HDF5Group (const HDF5Group& that);
    // Assignment cannot be used.
    HDF5Group& operator= (const HDF5Group& that);

    // Initialize (execute the constructor).
    void init (hid_t parentHid, const String& parentName,
	       const String& name,
	       bool mustExist=false, bool mustNotExist=false);
  };

}

#endif
