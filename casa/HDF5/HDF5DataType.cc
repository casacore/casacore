//# HDF5DataType.cc: An class representing an HDF5 data type
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

#include <casacore/casa/HDF5/HDF5DataType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#ifdef HAVE_HDF5

  HDF5DataType::HDF5DataType (const Bool*)
    : itsSize (sizeof(Bool))
  {
    itsHidFile = H5Tcopy (H5T_NATIVE_SCHAR);
    itsHidMem = H5Tcopy (itsHidFile);
    H5Tset_size (itsHidMem, sizeof(Bool));
  }

  HDF5DataType::HDF5DataType (const uChar*)
    : itsSize (sizeof(uChar))
  {
    itsHidFile = H5Tcopy (H5T_NATIVE_UCHAR);
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const Short*)
    : itsSize (sizeof(Short))
  {
    itsHidFile = H5Tcopy (H5T_NATIVE_SHORT);
    H5Tset_size (itsHidFile, sizeof(Short));
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const uShort*)
    : itsSize (sizeof(uShort))
  {
    itsHidFile = H5Tcopy (H5T_NATIVE_USHORT);
    H5Tset_size (itsHidFile, sizeof(uShort));
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const Int*)
    : itsSize (sizeof(Int))
  {
    itsHidFile = H5Tcopy (H5T_NATIVE_INT);
    H5Tset_size (itsHidFile, sizeof(Int));
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const uInt*)
    : itsSize (sizeof(uInt))
  {
    itsHidFile = H5Tcopy (H5T_NATIVE_UINT);
    H5Tset_size (itsHidFile, sizeof(uInt));
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const Int64*)
    : itsSize (sizeof(Int64))
  {
    itsHidFile = H5Tcopy (H5T_NATIVE_INT);
    H5Tset_size (itsHidFile, sizeof(Int64));
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const Float*)
    : itsSize (sizeof(Float))
  {
    itsHidFile = H5Tcopy (H5T_NATIVE_FLOAT);
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const Double*)
    : itsSize (sizeof(Double))
  {
    itsHidFile = H5Tcopy (H5T_NATIVE_DOUBLE);
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const Complex*)
    : itsSize (sizeof(Complex))
  {
    itsHidFile = H5Tcreate (H5T_COMPOUND, sizeof(Complex));
    H5Tinsert (itsHidFile, "re", 0, H5T_NATIVE_FLOAT);
    H5Tinsert (itsHidFile, "im", sizeof(Float), H5T_NATIVE_FLOAT);
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const DComplex*)
    : itsSize (sizeof(DComplex))
  {
    itsHidFile = H5Tcreate (H5T_COMPOUND, sizeof(DComplex));
    H5Tinsert (itsHidFile, "re", 0, H5T_NATIVE_DOUBLE);
    H5Tinsert (itsHidFile, "im", sizeof(Double), H5T_NATIVE_DOUBLE);
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const String& value)
    : itsSize (0)
  {
    itsHidFile = H5Tcopy (H5T_C_S1);
    H5Tset_size (itsHidFile, value.size());
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (const String*)
    : itsSize (0)
  {
    itsHidFile = H5Tcopy (H5T_C_S1);
    H5Tset_size (itsHidFile, H5T_VARIABLE);
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::HDF5DataType (Int, Int)
  {
    // An empty array is represented by its dimensionality and type.
    // Add an extra dummy field to make the compound different from (D)Complex
    // without having to test on field names.
    HDF5DataType dtInt((Int*)0);
    itsHidFile = H5Tcreate (H5T_COMPOUND, 3*sizeof(Int));
    H5Tinsert (itsHidFile, "emptyarray",           0, dtInt.getHidFile());
    H5Tinsert (itsHidFile, "rank",       sizeof(Int), dtInt.getHidFile());
    H5Tinsert (itsHidFile, "casatype", 2*sizeof(Int), dtInt.getHidFile());
    itsHidMem = H5Tcopy (itsHidFile);
  }

  HDF5DataType::~HDF5DataType()
  {
    H5Tclose (itsHidMem);
    H5Tclose(itsHidFile);
  }

  DataType HDF5DataType::getDataType (hid_t dtid)
  {
    DataType dtype = TpOther;
    int sz = H5Tget_size(dtid);
    switch (H5Tget_class(dtid)) {
    case H5T_INTEGER:
      {
	int sgn = H5Tget_sign(dtid);
	if (sgn == H5T_SGN_2) {
	  // A bool is stored as a signed char.
	  if (sz == 1) {
	    dtype = TpBool;
	  } else if (sz == sizeof(Short)) {
	    dtype = TpShort;
	  } else if (sz == sizeof(Int)) {
	    dtype = TpInt;
	  } else {
	    AlwaysAssert (sz==sizeof(Int64), AipsError);
	    dtype = TpInt64;
	  }
	} else {
	  if (sz == 1) {
	    dtype = TpUChar;
	  } else {
	    AlwaysAssert (sz==sizeof(uInt), AipsError);
	    dtype = TpUInt;
	  }
	}
      }
      break;
    case H5T_FLOAT:
      {
	if (sz == sizeof(Float)) {
	  dtype = TpFloat;
	} else {
	  AlwaysAssert (sz==sizeof(Double), AipsError);
	  dtype = TpDouble;
	}
      }
      break;
    case H5T_COMPOUND:
      {
	AlwaysAssert (H5Tget_nmembers(dtid)==2, AipsError);
	if (sz == sizeof(Complex)) {
	  dtype = TpComplex;
	} else {
	  AlwaysAssert (sz==sizeof(DComplex), AipsError);
	  dtype = TpDComplex;
	}
      }
      break;
    case H5T_STRING:
      dtype = TpString;
      break;
    default:
      break;
    }
    return dtype;
  }

#else

  HDF5DataType::HDF5DataType (const Bool*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const uChar*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const Short*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const uShort*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const Int*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const uInt*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const Float*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const Int64*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const Double*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const Complex*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const DComplex*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const String&)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const String*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (Int, Int)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::~HDF5DataType()
  {}

  DataType HDF5DataType::getDataType (hid_t)
  {
    return TpOther;
  }

#endif

}
