//# HDF5HidMeta.h: Classes representing an HDF5 hid of meta objects
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

#ifndef CASA_HDF5HIDMETA_H
#define CASA_HDF5HIDMETA_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/HDF5/HDF5Object.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // A class representing an HDF5 property hid.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tHDF5Dataset.cc">
  // </reviewed>
  // <synopsis>
  // This class wraps an HDF5 property hid (hdf5 id). It offers two benefits:
  // <ul>
  //  <li> The most important is resource management. In case of an exception,
  //       the hid will automatically be closed by the destructor.
  //  <li> A hid is a kind of pointer and should not be copied. These classes
  //       forbid making a copy, but make it possible to use them in a
  //       shared pointer context.
  // </ul>
  // </synopsis>
  class HDF5HidProperty
  {
  public: 
    // Default constructor sets hid to invalid.
    HDF5HidProperty()
      : itsHid(-1) {}
    // Construct from given hid.
    HDF5HidProperty (hid_t hid)
      : itsHid(hid) {}
    // The destructor closes the hid.
    ~HDF5HidProperty()
      { close(); }
    // Close the hid if valid.
    void close();
    // Put hid in it. If it already contains a hid, it will be closed.
    void operator= (hid_t hid)
      { close(); itsHid = hid; }
    // Get the hid.
    hid_t getHid() const
      { return itsHid; }
    // Convert automatically to hid_t.
    operator hid_t() const
      { return itsHid; }
  private:
    // Copy constructor cannot be used.
    HDF5HidProperty (const HDF5HidProperty& that);
    // Assignment cannot be used.
    HDF5HidProperty& operator= (const HDF5HidProperty& that);

    hid_t itsHid;
  };


  // <summary>
  // A class representing an HDF5 datatype hid.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tHDF5Dataset.cc">
  // </reviewed>
  // <synopsis>
  // This class wraps an HDF5 datatype hid (hdf5 id). It offers two benefits:
  // <ul>
  //  <li> The most important is resource management. In case of an exception,
  //       the hid will automatically be closed by the destructor.
  //  <li> A hid is a kind of pointer and should not be copied. These classes
  //       forbid making a copy, but make it possible to use them in a
  //       shared pointer context.
  // </ul>
  // </synopsis>
  class HDF5HidDataType
  {
  public: 
    // Default constructor sets hid to invalid.
    HDF5HidDataType()
      : itsHid(-1) {}
    // Construct from given hid.
    HDF5HidDataType (hid_t hid)
      : itsHid(hid) {}
    // The destructor closes the hid.
    ~HDF5HidDataType()
      { close(); }
    // Close the hid if valid.
    void close();
    // Put hid in it. If it already contains a hid, it will be closed.
    void operator= (hid_t hid)
      { close(); itsHid = hid; }
    // Get the hid.
    hid_t getHid() const
      { return itsHid; }
    // Convert automatically to hid_t.
    operator hid_t() const
      { return itsHid; }
  private:
    // Copy constructor cannot be used.
    HDF5HidDataType (const HDF5HidDataType& that);
    // Assignment cannot be used.
    HDF5HidDataType& operator= (const HDF5HidDataType& that);

    hid_t itsHid;
  };


  // <summary>
  // A class representing an HDF5 dataspace hid.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tHDF5Dataset.cc">
  // </reviewed>
  // <synopsis>
  // This class wraps an HDF5 dataspace hid (hdf5 id). It offers two benefits:
  // <ul>
  //  <li> The most important is resource management. In case of an exception,
  //       the hid will automatically be closed by the destructor.
  //  <li> A hid is a kind of pointer and should not be copied. These classes
  //       forbid making a copy, but make it possible to use them in a
  //       shared pointer context.
  // </ul>
  // </synopsis>
  class HDF5HidDataSpace
  {
  public: 
    // Default constructor sets hid to invalid.
    HDF5HidDataSpace()
      : itsHid(-1) {}
    // Construct from given hid.
    HDF5HidDataSpace (hid_t hid)
      : itsHid(hid) {}
    // The destructor closes the hid.
    ~HDF5HidDataSpace()
      { close(); }
    // Close the hid if valid.
    void close();
    // Put hid in it. If it already contains a hid, it will be closed.
    void operator= (hid_t hid)
      { close(); itsHid = hid; }
    // Get the hid.
    hid_t getHid() const
      { return itsHid; }
    // Convert automatically to hid_t.
    operator hid_t() const
      { return itsHid; }
  private:
    // Copy constructor cannot be used.
    HDF5HidDataSpace (const HDF5HidDataSpace& that);
    // Assignment cannot be used.
    HDF5HidDataSpace& operator= (const HDF5HidDataSpace& that);

    hid_t itsHid;
  };


  // <summary>
  // A class representing an HDF5 attribute hid.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tHDF5Dataset.cc">
  // </reviewed>
  // <synopsis>
  // This class wraps an HDF5 attribute hid (hdf5 id). It offers two benefits:
  // <ul>
  //  <li> The most important is resource management. In case of an exception,
  //       the hid will automatically be closed by the destructor.
  //  <li> A hid is a kind of pointer and should not be copied. These classes
  //       forbid making a copy, but make it possible to use them in a
  //       shared pointer context.
  // </ul>
  // </synopsis>
  class HDF5HidAttribute
  {
  public: 
    // Default constructor sets hid to invalid.
    HDF5HidAttribute()
      : itsHid(-1) {}
    // Construct from given hid.
    HDF5HidAttribute (hid_t hid)
      : itsHid(hid) {}
    // The destructor closes the hid.
    ~HDF5HidAttribute()
      { close(); }
    // Close the hid if valid.
    void close();
    // Put hid in it. If it already contains a hid, it will be closed.
    void operator= (hid_t hid)
      { close(); itsHid = hid; }
    // Get the hid.
    hid_t getHid() const
      { return itsHid; }
    // Convert automatically to hid_t.
    operator hid_t() const
      { return itsHid; }
  private:
    // Copy constructor cannot be used.
    HDF5HidAttribute (const HDF5HidAttribute& that);
    // Assignment cannot be used.
    HDF5HidAttribute& operator= (const HDF5HidAttribute& that);

    hid_t itsHid;
  };


}

#endif
