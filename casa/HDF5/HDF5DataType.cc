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

  HDF5DataType::HDF5DataType (const bool*)
    : itsSize (sizeof(bool))
  {
    itsHidFile = H5Tcopy (H5T_STD_I8LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_SCHAR);
    H5Tset_precision (itsHidMem, 8);
  }

  HDF5DataType::HDF5DataType (const unsigned char*)
    : itsSize (sizeof(unsigned char))
  {
    itsHidFile = H5Tcopy (H5T_STD_U8LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_UCHAR);
    H5Tset_precision (itsHidMem, 8);
  }

  HDF5DataType::HDF5DataType (const int16_t*)
    : itsSize (sizeof(int16_t))
  {
    itsHidFile = H5Tcopy (H5T_STD_I16LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_SHORT);
    H5Tset_precision (itsHidMem, 8*sizeof(int16_t));
  }

  HDF5DataType::HDF5DataType (const uint16_t*)
    : itsSize (sizeof(uint16_t))
  {
    itsHidFile = H5Tcopy (H5T_STD_U16LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_USHORT);
    H5Tset_precision (itsHidMem, 8*sizeof(uint16_t));
  }

  HDF5DataType::HDF5DataType (const int32_t*)
    : itsSize (sizeof(int32_t))
  {
    itsHidFile = H5Tcopy (H5T_STD_I32LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_INT);
    H5Tset_precision (itsHidMem, 8*sizeof(int32_t));
  }

  HDF5DataType::HDF5DataType (const uint32_t*)
    : itsSize (sizeof(uint32_t))
  {
    itsHidFile = H5Tcopy (H5T_STD_U32LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_UINT);
    H5Tset_precision (itsHidMem, 8*sizeof(uint32_t));
  }

  HDF5DataType::HDF5DataType (const int64_t*)
    : itsSize (sizeof(int64_t))
  {
    itsHidFile = H5Tcopy (H5T_STD_I64LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_INT);
    H5Tset_precision (itsHidMem, 8*sizeof(int64_t));
  }

  HDF5DataType::HDF5DataType (const float*)
    : itsSize (sizeof(float))
  {
    itsHidFile = H5Tcopy (H5T_IEEE_F32LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_FLOAT);
  }

  HDF5DataType::HDF5DataType (const double*)
    : itsSize (sizeof(double))
  {
    itsHidFile = H5Tcopy (H5T_IEEE_F64LE);
    itsHidMem  = H5Tcopy (H5T_NATIVE_DOUBLE);
  }

  HDF5DataType::HDF5DataType (const Complex*)
    : itsSize (sizeof(Complex))
  {
    itsHidFile = H5Tcreate (H5T_COMPOUND, sizeof(Complex));
    itsHidMem  = H5Tcreate (H5T_COMPOUND, sizeof(Complex));
    HDF5DataType dtype((float*)0);
    addToCompound ("re", 0, dtype);
    addToCompound ("im", sizeof(float), dtype);
  }

  HDF5DataType::HDF5DataType (const DComplex*)
    : itsSize (sizeof(DComplex))
  {
    itsHidFile = H5Tcreate (H5T_COMPOUND, sizeof(DComplex));
    itsHidMem  = H5Tcreate (H5T_COMPOUND, sizeof(DComplex));
    HDF5DataType dtype((double*)0);
    addToCompound ("re", 0, dtype);
    addToCompound ("im", sizeof(double), dtype);
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

  HDF5DataType::HDF5DataType (int32_t, int32_t)
  {
    // An empty array is represented by its dimensionality and type.
    // Add an extra dummy field to make the compound different from (D)Complex
    // without having to test on field names.
    itsHidFile = H5Tcreate (H5T_COMPOUND, 3*sizeof(int32_t));
    itsHidMem  = H5Tcreate (H5T_COMPOUND, 3*sizeof(int32_t));
    HDF5DataType dtype((int32_t*)0);
    addToCompound ("emptyarray",           0, dtype);
    addToCompound ("rank",       sizeof(int32_t), dtype);
    addToCompound ("casatype", 2*sizeof(int32_t), dtype);
  }

  HDF5DataType::HDF5DataType (const std::vector<String>& names,
                              const std::vector<HDF5DataType>& types)
    : itsSize (0)
  {
    AlwaysAssert (names.size() > 0, AipsError);
    AlwaysAssert (types.size() == names.size(), AipsError);
    for (uint32_t i=0; i<types.size(); ++i) {
      AlwaysAssert (types[i].size() > 0, AipsError);
      itsSize += types[i].size();
    }
    itsHidFile = H5Tcreate (H5T_COMPOUND, itsSize);
    itsHidMem  = H5Tcreate (H5T_COMPOUND, itsSize);
    uint32_t offset = 0;
    for (uint32_t i=0; i<types.size(); ++i) {
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
                                    uint32_t offset,
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
	  } else if (sz == sizeof(int16_t)) {
	    dtype = TpShort;
	  } else if (sz == sizeof(int32_t)) {
	    dtype = TpInt;
	  } else {
	    AlwaysAssert (sz==sizeof(int64_t), AipsError);
	    dtype = TpInt64;
	  }
	} else {
	  if (sz == 1) {
	    dtype = TpUChar;
          } else if (sz == sizeof(uint16_t)) {
	    dtype = TpUShort;
	  } else {
	    AlwaysAssert (sz==sizeof(uint32_t), AipsError);
	    dtype = TpUInt;
	  }
	}
      }
      break;
    case H5T_FLOAT:
      {
	if (sz == sizeof(float)) {
	  dtype = TpFloat;
	} else {
	  AlwaysAssert (sz==sizeof(double), AipsError);
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

  bool HDF5DataType::isComplex (hid_t dtid)
  {
    bool res = false;
    if (H5Tget_class(dtid) == H5T_COMPOUND  &&  H5Tget_nmembers(dtid) == 2) {
      char* f0 = H5Tget_member_name(dtid, 0);
      char* f1 = H5Tget_member_name(dtid, 1);
      res = (strcmp(f0, "re") == 0  &&  strcmp (f1, "im") == 0);
      H5free_memory(f0);
      H5free_memory(f1);
    }
    return res;
  }

  bool HDF5DataType::isEmptyArray (hid_t dtid)
  {
    bool res = false;
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
    int32_t ndim = H5Tget_array_ndims (dtid);
    AlwaysAssert (ndim >= 0, AipsError);
    Block<hsize_t> dims(ndim);
    AlwaysAssert (H5Tget_array_dims(dtid, dims.storage()) == ndim, AipsError);
    return toShape (dims);
  }

#else

  HDF5DataType::HDF5DataType (const bool*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const unsigned char*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const int16_t*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const uint16_t*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const int32_t*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const uint32_t*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const float*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const int64_t*)
  {
    HDF5Object::throwNoHDF5();
  }

  HDF5DataType::HDF5DataType (const double*)
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

  HDF5DataType::HDF5DataType (int32_t, int32_t)
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
                                    uint32_t,
                                    const HDF5DataType&)
  {
    HDF5Object::throwNoHDF5();
  }

  DataType HDF5DataType::getDataType (hid_t)
  {
    return TpOther;
  }

  bool HDF5DataType::isComplex (hid_t)
  {
    return false;
  }

  bool HDF5DataType::isEmptyArray (hid_t)
  {
    return false;
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
