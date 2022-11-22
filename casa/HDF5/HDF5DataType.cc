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

#include <casacore/casa/HDF5/HDF5DataType.h>
#include <casacore/casa/HDF5/HDF5HidMeta.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <cstring>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#ifdef HAVE_HDF5

// H5free_memory was introduced in 1.8.13
// If the versions of hdf5 we depend on was formalized this could be simpler
#if H5_VERS_MAJOR == 1 && (H5_VERS_MINOR < 8 || (H5_VERS_MINOR == 8 && H5_VERS_RELEASE < 13))
auto &H5free_memory = free;
#endif

  HDF5DataType::HDF5DataType (const Bool*)
    : itsSize (sizeof(Bool))
  {
    itsHidFile = H5Tcopy (H5T_STD_I8LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_SCHAR);
    H5Tset_precision (itsHidMem, 8);
  }

  HDF5DataType::HDF5DataType (const uChar*)
    : itsSize (sizeof(uChar))
  {
    itsHidFile = H5Tcopy (H5T_STD_U8LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_UCHAR);
    H5Tset_precision (itsHidMem, 8);
  }

  HDF5DataType::HDF5DataType (const Short*)
    : itsSize (sizeof(Short))
  {
    itsHidFile = H5Tcopy (H5T_STD_I16LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_SHORT);
    H5Tset_precision (itsHidMem, 8*sizeof(Short));
  }

  HDF5DataType::HDF5DataType (const uShort*)
    : itsSize (sizeof(uShort))
  {
    itsHidFile = H5Tcopy (H5T_STD_U16LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_USHORT);
    H5Tset_precision (itsHidMem, 8*sizeof(uShort));
  }

  HDF5DataType::HDF5DataType (const Int*)
    : itsSize (sizeof(Int))
  {
    itsHidFile = H5Tcopy (H5T_STD_I32LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_INT);
    H5Tset_precision (itsHidMem, 8*sizeof(Int));
  }

  HDF5DataType::HDF5DataType (const uInt*)
    : itsSize (sizeof(uInt))
  {
    itsHidFile = H5Tcopy (H5T_STD_U32LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_UINT);
    H5Tset_precision (itsHidMem, 8*sizeof(uInt));
  }

  HDF5DataType::HDF5DataType (const Int64*)
    : itsSize (sizeof(Int64))
  {
    itsHidFile = H5Tcopy (H5T_STD_I64LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_INT);
    H5Tset_precision (itsHidMem, 8*sizeof(Int64));
  }

  HDF5DataType::HDF5DataType (const Float*)
    : itsSize (sizeof(Float))
  {
    itsHidFile = H5Tcopy (H5T_IEEE_F32LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_FLOAT);
  }

  HDF5DataType::HDF5DataType (const Double*)
    : itsSize (sizeof(Double))
  {
    itsHidFile = H5Tcopy (H5T_IEEE_F64LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_DOUBLE);
  }

  HDF5DataType::HDF5DataType (const Complex*)
    : itsSize (sizeof(Complex))
  {
    itsHidFile = H5Tcreate (H5T_COMPOUND, sizeof(Complex));
    itsHidMem  = H5Tcreate (H5T_COMPOUND, sizeof(Complex));
    HDF5DataType dtype((Float*)0);
    addToCompound ("re", 0, dtype);
    addToCompound ("im", sizeof(Float), dtype);
  }

  HDF5DataType::HDF5DataType (const DComplex*)
    : itsSize (sizeof(DComplex))
  {
    itsHidFile = H5Tcreate (H5T_COMPOUND, sizeof(DComplex));
    itsHidMem  = H5Tcreate (H5T_COMPOUND, sizeof(DComplex));
    HDF5DataType dtype((Double*)0);
    addToCompound ("re", 0, dtype);
    addToCompound ("im", sizeof(Double), dtype);
  }

  HDF5DataType::HDF5DataType (const String& value)
    : itsSize (value.size())
  {
    itsHidMem = H5Tcopy (H5T_C_S1);
    H5Tset_size (itsHidMem, value.size());
    itsHidFile = H5Tcopy (itsHidMem);
  }

  HDF5DataType::HDF5DataType (const String*)
    : itsSize (0)
  {
    itsHidMem = H5Tcopy (H5T_C_S1);
    H5Tset_size (itsHidMem, H5T_VARIABLE);
    itsHidFile = H5Tcopy (itsHidMem);
  }

  HDF5DataType::HDF5DataType (Int, Int)
  {
    // An empty array is represented by its dimensionality and type.
    // Add an extra dummy field to make the compound different from (D)Complex
    // without having to test on field names.
    itsHidFile = H5Tcreate (H5T_COMPOUND, 3*sizeof(Int));
    itsHidMem  = H5Tcreate (H5T_COMPOUND, 3*sizeof(Int));
    HDF5DataType dtype((Int*)0);
    addToCompound ("emptyarray",           0, dtype);
    addToCompound ("rank",       sizeof(Int), dtype);
    addToCompound ("casatype", 2*sizeof(Int), dtype);
  }

  HDF5DataType::HDF5DataType (const std::vector<String>& names,
                              const std::vector<HDF5DataType>& types)
    : itsSize (0)
  {
    AlwaysAssert (names.size() > 0, AipsError);
    AlwaysAssert (types.size() == names.size(), AipsError);
    for (uInt i=0; i<types.size(); ++i) {
      AlwaysAssert (types[i].size() > 0, AipsError);
      itsSize += types[i].size();
    }
    itsHidFile = H5Tcreate (H5T_COMPOUND, itsSize);
    itsHidMem  = H5Tcreate (H5T_COMPOUND, itsSize);
    uInt offset = 0;
    for (uInt i=0; i<types.size(); ++i) {
      addToCompound (names[i].c_str(), offset, types[i]);
      offset += types[i].size();
    }
  }

  HDF5DataType::HDF5DataType (const HDF5DataType& type, const IPosition& shape)
  {
    AlwaysAssert (shape.product() > 0, AipsError);
    Block<hsize_t> shp = fromShape (shape);
    itsHidFile = H5Tarray_create (type.getHidFile(),shp.size(), shp.storage());
    itsHidMem  = H5Tarray_create (type.getHidMem(), shp.size(), shp.storage());
    itsSize    = type.size() * shape.product();
  }

  HDF5DataType::HDF5DataType (const HDF5DataType& that)
    : itsHidMem  (H5Tcopy (that.itsHidMem)),
      itsHidFile (H5Tcopy (that.itsHidFile)),
      itsSize    (that.itsSize)
  {}

  HDF5DataType::~HDF5DataType()
  {}

  HDF5DataType& HDF5DataType::operator= (const HDF5DataType& that)
  {
    if (this != &that) {
      itsHidMem  = that.itsHidMem;
      itsHidFile = that.itsHidFile;
      itsSize    = that.itsSize;
    }
    return *this;
  }

  void HDF5DataType::addToCompound (const char* name,
                                    uInt offset,
                                    const HDF5DataType& dtype)
  {
    H5Tinsert (itsHidFile, name, offset, dtype.getHidFile());
    H5Tinsert (itsHidMem,  name, offset, dtype.getHidMem());
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
          } else if (sz == sizeof(uShort)) {
	    dtype = TpUShort;
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
	if (HDF5DataType::isComplex (dtid)) {
          if (sz == sizeof(Complex)) {
            dtype = TpComplex;
          } else {
            AlwaysAssert (sz==sizeof(DComplex), AipsError);
            dtype = TpDComplex;
          }
        } else {
          dtype = TpRecord;
        }
      }
      break;
    case H5T_STRING:
      dtype = TpString;
      break;
    case H5T_ARRAY:
      {
        HDF5HidDataType hid(H5Tget_super (dtid));
        switch (getDataType (hid)) {
        case TpBool:
          dtype = TpArrayBool;
          break;
        case TpUChar:
          dtype = TpArrayUChar;
          break;
        case TpShort:
          dtype = TpArrayShort;
          break;
        case TpUShort:
          dtype = TpArrayUShort;
          break;
        case TpInt:
          dtype = TpArrayInt;
          break;
        case TpUInt:
          dtype = TpArrayUInt;
          break;
        case TpInt64:
          dtype = TpArrayInt64;
          break;
        case TpFloat:
          dtype = TpArrayFloat;
          break;
        case TpDouble:
          dtype = TpArrayDouble;
          break;
        case TpComplex:
          dtype = TpArrayComplex;
          break;
        case TpDComplex:
          dtype = TpArrayDComplex;
          break;
        case TpString:
          dtype = TpArrayString;
          break;
        default:
          break;
        }
      }
      break;
    default:
      break;
    }
    return dtype;
  }

  Bool HDF5DataType::isComplex (hid_t dtid)
  {
    Bool res = False;
    if (H5Tget_class(dtid) == H5T_COMPOUND  &&  H5Tget_nmembers(dtid) == 2) {
      char* f0 = H5Tget_member_name(dtid, 0);
      char* f1 = H5Tget_member_name(dtid, 1);
      res = (strcmp(f0, "re") == 0  &&  strcmp (f1, "im") == 0);
      H5free_memory(f0);
      H5free_memory(f1);
    }
    return res;
  }

  Bool HDF5DataType::isEmptyArray (hid_t dtid)
  {
    Bool res = False;
    if (H5Tget_class(dtid) == H5T_COMPOUND  &&  H5Tget_nmembers(dtid) == 3) {
      char* f0 = H5Tget_member_name(dtid, 0);
      char* f1 = H5Tget_member_name(dtid, 1);
      char* f2 = H5Tget_member_name(dtid, 2);
      res = (strcmp(f0, "emptyarray") == 0  &&  strcmp (f1, "rank") == 0  &&
             strcmp(f2, "casatype") == 0);
      H5free_memory(f0);
      H5free_memory(f1);
      H5free_memory(f2);
    }
    return res;
  }

  IPosition HDF5DataType::getShape (hid_t dtid)
  {
    Int ndim = H5Tget_array_ndims (dtid);
    AlwaysAssert (ndim >= 0, AipsError);
    Block<hsize_t> dims(ndim);
    AlwaysAssert (H5Tget_array_dims(dtid, dims.storage()) == ndim, AipsError);
    return toShape (dims);
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

  HDF5DataType::HDF5DataType (const std::vector<String>&,
                              const std::vector<HDF5DataType>&)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const HDF5DataType&, const IPosition&)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const HDF5DataType&)
  {}

  HDF5DataType::~HDF5DataType()
  {}

  HDF5DataType& HDF5DataType::operator= (const HDF5DataType&)
  {
    return *this;
  }

  void HDF5DataType::addToCompound (const char*,
                                    uInt,
                                    const HDF5DataType&)
  {
    HDF5Object::throwNoHDF5();
  }

  DataType HDF5DataType::getDataType (hid_t)
  {
    return TpOther;
  }

  Bool HDF5DataType::isComplex (hid_t)
  {
    return False;
  }

  Bool HDF5DataType::isEmptyArray (hid_t)
  {
    return False;
  }

  IPosition HDF5DataType::getShape (hid_t)
  {
    return IPosition();
  }

#endif


  Block<hsize_t> HDF5DataType::fromShape (const IPosition& shape)
  {
    // Reverse the axes (Fortran to C).
    Block<hsize_t> b(shape.size());
    std::reverse_copy (shape.begin(), shape.end(), b.begin());
    return b;
  }
  IPosition HDF5DataType::toShape (const Block<hsize_t>& b)
  {
    // Reverse the axes (C to Fortran).
    IPosition shape(b.size());
    std::reverse_copy (b.begin(), b.end(), shape.begin());
    return shape;
  }

}
