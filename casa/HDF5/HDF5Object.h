//# HDF5Object.h: An abstract base class representing an HDF5 object
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

#ifndef CASA_HDF5OBJECT_H
#define CASA_HDF5OBJECT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>

//# Define hid_t and hsize_t if not defined (thus if HDF5 disabled).
//# They should be the same as used by HDF5.
//# This is checked by functions check_hid_t and check_hsize_t.
#ifdef HAVE_HDF5
# include <hdf5.h>
#else 
  typedef int64_t hid_t;
  typedef casacore::uInt64 hsize_t;
#endif


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // An abstract base class representing an HDF5 object
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tHDF5Dataset.cc">
  // </reviewed>

  // <synopsis>
  // This class wraps a basic HDF5 object. It offers several benefits:
  // <ul>
  //  <li> The most important is resource management. In case of an exception,
  //       the object's hid will automatically be closed by the destructor.
  //  <li> It acts as the base class for the basic objects file, group,
  //       and dataset.
  //  <li> An HDF5 hid is a kind of pointer and should not be copied.
  //       These classes forbid making a copy, but make it possible to use
  //       them in a shared pointer context.
  // </ul>
  // </synopsis>

  class HDF5Object
  {
  public: 
    // Default constructor sets to invalid hid.
    HDF5Object()
      : itsHid(-1)
    {}

    // The destructor in a derived class should close the hid appropriately.
    virtual ~HDF5Object();

    // Copy constructor cannot be used because a HID cannot be copied.
    HDF5Object (const HDF5Object&) = delete;

    // Assignment cannot be used because a HID cannot be copied .
    HDF5Object& operator= (const HDF5Object&) = delete;

    // Check if there is HDF5 support compiled in.
    static Bool hasHDF5Support();

    // Close the hid if valid.
    virtual void close() = 0;

    // Is it a valid hid?
    bool isValid() const
      { return itsHid >= 0; }

    // Get the hid.
    hid_t getHid() const
      { return itsHid; }

    // Convert automatically to hid_t.
    operator hid_t() const
      { return itsHid; }

    // Get or set the name.
    // <group>
    void setName (const String& name)
      { itsName = name; }
    const String& getName() const
      { return itsName; }
    // </group>

    // If no HDF5, throw an exception that HDF5 is not supported.
    static void throwNoHDF5();

  protected:
    // Set the hid.
    void setHid (hid_t hid)
      { itsHid = hid; }

    // Clear the hid (set to invalid).
    void clearHid()
      { itsHid = -1; }

  private:
    //# Data members
    //# Define a union to ensure that the object always uses 64 bits, even
    //# for older HDF5 versions where hid_t is a 32 bit integer.
    union {
      hid_t  itsHid;
      int64_t  itsDummyHid;
    };
    String itsName;
  };


}

#endif
