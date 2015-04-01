//# HDF5DataType.h: An class representing an HDF5 data type
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

#ifndef CASA_HDF5DATATYPE_H
#define CASA_HDF5DATATYPE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/HDF5/HDF5Object.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // A class representing an HDF5 data type.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="" date="" tests="tHDF5DataType.cc">
  // </reviewed>

  // <prerequisite>
  //   <li> <a href="http://hdf.ncsa.uiuc.edu">HDF5 system</a>
  // </prerequisite>

  // <synopsis>
  // This class wraps the HDF5 functions to create a data type.
  // It creates a data type for the datas in memory and for the file.
  // </synopsis> 

  // <motivation>
  // It was overkill to use the HDF5 C++ interface. Instead little wrappers
  // have been written. HDF5DataType can be embedded in a shared pointer making
  // it possible to share an HDF5 data type amongst various HDF5Array objects
  // and close (i.e. destruct) the HDF5 data type object when needed.
  // </motivation>

  class HDF5DataType
  {
  public: 
    // Create an HDF5 datatype object for the given fixed length type.
    // It uses the corresponding native HDF5 data type. Only for Bool it
    // uses a uchar, because the HDF5 bool type is a uint.
    // For the complex types it makes a compound HDF5 data type.
    // The String type is meant for an array of strings.
    // <group>
    HDF5DataType (const Bool*);
    HDF5DataType (const uChar*);
    HDF5DataType (const Short*);
    HDF5DataType (const uShort*);
    HDF5DataType (const Int*);
    HDF5DataType (const uInt*);
    HDF5DataType (const Int64*);
    HDF5DataType (const Float*);
    HDF5DataType (const Double*);
    HDF5DataType (const Complex*);
    HDF5DataType (const DComplex*);
    HDF5DataType (const String*);
    // </group>

    // Create an HDF5 datatype object for a scalar string.
    // The length of the string is part of the type.
    HDF5DataType (const String& value);

    // Create an HDF5 datatype object for an empty array.
    HDF5DataType (Int, Int);

    // The destructor closes the HDF5 data type object.
    ~HDF5DataType();

    // Get the Casacore data type for the given HDF5 data type.
    static DataType getDataType (hid_t);

    // Get the HID for the data type in memory.
    hid_t getHidMem() const
      { return itsHidMem; }

    // Get the HID for the data type in the file.
    hid_t getHidFile() const
      { return itsHidFile; }

    // Get the size in bytes of the data type.
    // Note that the size of a string is variable, thus 0.
    uInt size() const
      { return itsSize; }

  private:
    // Copy constructor cannot be used.
    HDF5DataType (const HDF5DataType& that);

    // Assignment cannot be used.
    HDF5DataType& operator= (const HDF5DataType& that);


    hid_t itsHidMem;
    hid_t itsHidFile;
    uInt  itsSize;
  };

}

#endif
