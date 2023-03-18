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

#ifndef CASA_HDF5DATATYPE_H
#define CASA_HDF5DATATYPE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/HDF5/HDF5Object.h>
#include <casacore/casa/HDF5/HDF5HidMeta.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class IPosition;

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
  // This class wraps the HDF5 functions to create a data type
  // for the data in memory and for the file.
  // The HDF5 file data type order is set to LittleEndian.
  // <br>
  // The basic constructors define a scalar of a basic data type or
  // data type Complex and DComplex. Strings are also supported.
  // However, it is also possible to define a fixed shaped array for any
  // HDF5DataType. Furthermore, it is possible to define a compound data
  // type consisting of named fields of any HDF5DataType with a fixed size.
  // Arrays and/or compounds can be nested at will.
  // <br>
  // Variable length strings are supported, but cannot be used in compounds.
  // <br>
  // Older HDF5 versions did not support empty arrays. Therefore they are
  // represented as a compound with 3 fields. Also they did not support
  // boolean values; they are represented as a signed char.
  // HDF5 does not support complex values either. They are represented
  // as a compound with fields 're' and 'im'.
  // </synopsis> 

  // <motivation>
  // The HDF5 C++ interface only supports the HDF5 C functionality and
  // resembles that to much. For instance, it does not use STL containers.
  // It does not support non-basic data types, in particular Complex.
  // </motivation>

  class HDF5DataType
  {
  public: 
    // The default constructor makes an invalid object.
    // It is needed to make a vector of objects.
    HDF5DataType()
      : itsSize(0)
    {}

    // Create an HDF5 datatype object for the given fixed length type.
    // It uses the corresponding native HDF5 data type. Only for Bool it
    // uses a uchar, because the HDF5 bool type is a uint.
    // For the complex types it makes a compound HDF5 data type.
    // The String type is meant for an array of strings.
    // <group>
    explicit HDF5DataType (const Bool*);
    explicit HDF5DataType (const uChar*);
    explicit HDF5DataType (const Short*);
    explicit HDF5DataType (const uShort*);
    explicit HDF5DataType (const Int*);
    explicit HDF5DataType (const uInt*);
    explicit HDF5DataType (const Int64*);
    explicit HDF5DataType (const Float*);
    explicit HDF5DataType (const Double*);
    explicit HDF5DataType (const Complex*);
    explicit HDF5DataType (const DComplex*);
    explicit HDF5DataType (const String*);
    // </group>

    // Create an HDF5 datatype object for a scalar string.
    // The length of the string is part of the type.
    explicit HDF5DataType (const String& value);

    // Create an HDF5 datatype object for an empty array.
    // Both arguments are dummy (needed to distinguish the constructor).
    // An empty array as represented as a compound data type with integer
    // field names emptyarray, rank and casatype.
    HDF5DataType (Int, Int);

    // Define a compound data type consisting of the given fields and types.
    // An exception is thrown if the vectors are empty or have mismatching
    // sizes.
    HDF5DataType (const std::vector<String>& names,
                  const std::vector<HDF5DataType>& types);

    // Create an array of the given data type.
    // An exception is thrown if the shape is empty.
    HDF5DataType (const HDF5DataType&, const IPosition& shape);

    // The copy constructor makes a deep copy.
    HDF5DataType (const HDF5DataType& that);

    // The destructor closes the HDF5 data type object.
    ~HDF5DataType();

    // Assignment makes a deep copy.
    HDF5DataType& operator= (const HDF5DataType& that);

    // Get the Casacore data type for the given HDF5 data type.
    // TpRecord is returned for a compound data type.
    static DataType getDataType (hid_t);

    // Get the HID for the data type in memory.
    hid_t getHidMem() const
      { return itsHidMem; }

    // Get the HID for the data type in the file.
    hid_t getHidFile() const
      { return itsHidFile; }

    // Get the size in bytes of the data type (in memory).
    // Note that the size of a string is variable, thus 0.
    uInt size() const
      { return itsSize; }

    // Test if the data type is Complex or DComplex.
    static Bool isComplex (hid_t dtid);

    // Test if the data type is an empty array.
    static Bool isEmptyArray (hid_t dtid);

    // Get the shape of an array data type.
    // It returns an empty IPosition for non-arrays.
    static IPosition getShape (hid_t dtid);

    // Helper functions to convert shapes.
    // It reverses the axes, because HDF5 uses C-order.
    // <group>
    static Block<hsize_t> fromShape (const IPosition& shape);
    static IPosition toShape (const Block<hsize_t>& b);
    // </group>

  private:
    // Add a field to a compound data type.
    // It does it for the memory and file data type.
    void addToCompound (const char* name,
                        uInt offset,
                        const HDF5DataType& dtype);

    //# Data members
    HDF5HidDataType itsHidMem;
    HDF5HidDataType itsHidFile;
    uInt            itsSize;
  };

}

#endif
