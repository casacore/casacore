//# PycArrayNP.cc: Convert a Casacore Array to a Python numpy array
//# Copyright (C) 2006
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
//# $Id: PycArrayNP.cc,v 1.2 2006/11/07 00:17:23 gvandiep Exp $

#include <casacore/python/Converters/PycArrayNP.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <numpy/arrayobject.h>
#include <boost/python/dict.hpp>


#define PYC_USE_PYARRAY "numpy"
namespace casacore { namespace python { namespace numpy {

  Bool importArray()
  {
    // numpy has diferent versions of import_array (from version 1.0.1 on).
    // Therefore import_array1 is used.
    import_array1(True);
    return True;
  }

  Array<String> ArrayCopyStr_toArray (const IPosition& shape,
				      void* data, uInt slen)
  {
    // This code converts from a numpy String array.
    // The longest string determines the length of each value.
    // They are padded with zeroes if shorter.
    Array<String> arr(shape);
    String* to = arr.data();
    const char* src = static_cast<const char*>(data);
    uInt nr = arr.size();
    for (uInt i=0; i<nr; ++i) {
      if (src[slen-1] == 0) {
	to[i] = String(src);
      } else {
	to[i] = String(src, slen);
      }
      src += slen;
    }
    return arr;
  }

  //# Code to deal with numpy array scalars (i.e. a value returned
  //# by taking an element from a numpy array).

  // Check if the object is an array scalar and return its type.
  Bool PycArrayScalarCheck (PyObject* obj_ptr, int& type)
  {
    if (!PyArray_API) {
      if (!isImported()) return False;
      loadAPI();
    }
    // No scalar if array scalar nor 0-dim array.
    if (! PyArray_CheckScalar(obj_ptr)) {
      return False;
    }
    // See if the object is a 0-dim array.
    Bool is0dim = PyArray_Check(obj_ptr);
    const int ntypes = 13;
    // Define them in order of expected usage.
     int types[ntypes] = {
			 NPY_INT32,
			 NPY_INT64,
			 NPY_FLOAT32,
			 NPY_FLOAT64,
			 NPY_COMPLEX64,
			 NPY_COMPLEX128,
			 NPY_UINT32,
			 NPY_UINT64,
			 NPY_BOOL,
			 NPY_INT16,
			 NPY_UINT16,
			 NPY_INT8,
			 NPY_UINT8};
    for (int i=0; i<ntypes; ++i) {
      if (is0dim) {
        if (types[i] == PyArray_TYPE(obj_ptr)) {
          type = types[i];
          return True;
        }
      } else {
        if (obj_ptr->ob_type == (PyTypeObject*)PyArray_TypeObjectFromType(types[i])) {
          type = types[i];
          return True;
        }
      }
    }
    return False;
  }

  DataType PycArrayScalarType (PyObject* obj_ptr)
  {
    int type;
    if (! PycArrayScalarCheck(obj_ptr, type)) {
      return TpOther;
    }
    switch (type) {
    case NPY_BOOL:
      return TpBool;
    case NPY_INT8:
    case NPY_UINT8:
    case NPY_INT16:
    case NPY_UINT16:
    case NPY_INT32:
    case NPY_UINT32:
      return TpInt;
    case NPY_INT64:
    case NPY_UINT64:
      return TpInt64;
    case NPY_FLOAT32:
    case NPY_FLOAT64:
      return TpDouble;
    case NPY_COMPLEX64:
    case NPY_COMPLEX128:
      return TpDComplex;
    default:
      return TpOther;
    }
  }

  ValueHolder makeScalar (PyObject* obj_ptr, int type)
  {
    if (PyArray_Check(obj_ptr)) {
      PyArrayObject* obj = (PyArrayObject*)obj_ptr;
      switch (type) {
      case NPY_BOOL:
        return ValueHolder(*(::npy_bool*)(obj->data) != 0);
      case NPY_INT8:
        return ValueHolder(int(*(::npy_int8*)(obj->data)));
      case NPY_UINT8:
        return ValueHolder(uint(*(::npy_uint8*)(obj->data)));
      case NPY_INT16:
        return ValueHolder(int(*(::npy_int16*)(obj->data)));
      case NPY_UINT16:
        return ValueHolder(uint(*(::npy_uint16*)(obj->data)));
      case NPY_INT32:
        return ValueHolder(int(*(::npy_int32*)(obj->data)));
      case NPY_UINT32:
        return ValueHolder(uint(*(::npy_uint32*)(obj->data)));
      case NPY_INT64:
        return ValueHolder(Int64(*(::npy_int64*)(obj->data)));
      case NPY_UINT64:
        return ValueHolder(Int64(*(::npy_uint64*)(obj->data)));
      case NPY_FLOAT32:
        return ValueHolder(float(*(::npy_float32*)(obj->data)));
      case NPY_FLOAT64:
        return ValueHolder(double(*(::npy_float64*)(obj->data)));
      case NPY_COMPLEX64:
        return ValueHolder(*(Complex*)(obj->data));
      case NPY_COMPLEX128:
        return ValueHolder(*(DComplex*)(obj->data));
      default:
        break;
      }
    } else {
      char buffer[32];
      PyArray_ScalarAsCtype(obj_ptr, buffer);  
      switch (type) {
      case NPY_BOOL:
        {
          ::npy_bool* ptr = (::npy_bool* )buffer;
          return ValueHolder(*ptr != 0);
        }
      case NPY_INT8:
        {
          ::npy_int8* ptr = (::npy_int8*)buffer;
          return ValueHolder(Short(*ptr));
        }
      case NPY_UINT8:
        {
          ::npy_uint8* ptr = (::npy_uint8*)buffer;
          return ValueHolder(uShort(*ptr));
        }
      case NPY_INT16:
        {
          ::npy_int16* ptr = (::npy_int16*)buffer;
          return ValueHolder(Short(*ptr));
        }
      case NPY_UINT16:
        {
          ::npy_uint16* ptr = (::npy_uint16*)buffer;
          return ValueHolder(uShort(*ptr));
        }
      case NPY_INT32:
        {
          ::npy_int32* ptr = (::npy_int32*)buffer;
          return ValueHolder(Int(*ptr));
        }
      case NPY_UINT32:
        {
          ::npy_uint32* ptr = (::npy_uint32*)buffer;
          return ValueHolder(uInt(*ptr));
        }
      case NPY_INT64:
        {
          ::npy_int64* ptr = (::npy_int64*)buffer;
          return ValueHolder(Int64(*ptr));
        }
      case NPY_UINT64:
        {
          ::npy_uint64* ptr = (::npy_uint64*)buffer;
          return ValueHolder(Int64(*ptr));
        }
      case NPY_FLOAT32:
        {
          ::npy_float32* ptr = (::npy_float32*)buffer;
          return ValueHolder(float(*ptr));
        }
      case NPY_FLOAT64:
        {
          ::npy_float64* ptr = (::npy_float64*)buffer;
          return ValueHolder(double(*ptr));
        }
      case NPY_COMPLEX64:
        {
          Complex* ptr = (Complex*)buffer;
          return ValueHolder(*ptr);
        }
      case NPY_COMPLEX128:
        {
          DComplex* ptr = (DComplex*)buffer;
          return ValueHolder(*ptr);
        }
      default:
        break;
      }
    }
    throw AipsError("invalid data type");
  }


  void register_convert_arrayscalars()
  {
    // Register as casa types.
    // A type like ssize_t maps to Int or Long (depending on machine).
    array_scalar_from_python<Bool>();
    array_scalar_from_python<Char>();
    array_scalar_from_python<uChar>();
    array_scalar_from_python<Short>();
    array_scalar_from_python<uShort>();
    array_scalar_from_python<Int>();
    array_scalar_from_python<uInt>();
    array_scalar_from_python<Long>();
    array_scalar_from_python<uLong>();
    array_scalar_from_python<Int64>();
    array_scalar_from_python<uInt64>();
    array_scalar_from_python<Float>();
    array_scalar_from_python<Double>();
    array_scalar_from_python<Complex>();
    array_scalar_from_python<DComplex>();
  }


#include <casacore/python/Converters/PycArrayComCC.h>

  template <typename T>
  boost::python::object makePyArrayObject (casacore::Array<T> const& arr)
  {
    // Load the API if needed.
    if (!PyArray_API) loadAPI();
    // Swap axes, because Casacore has row minor and Python row major order.
    // A Python array needs at least 1 dimension, otherwise it's a scalar.
    int nd = arr.ndim();
    vector<npy_intp> newshp(1, 0);
    if (nd == 0) {
      nd = 1;
    } else {
      newshp.resize (nd);
      const IPosition& shp = arr.shape();
      for (int i=0; i<nd; i++) {
	newshp[i] = shp[nd-i-1];
      }
    }
    // Create the array from the shape.
    // This gives a warning because a function pointer is converted
    // to a data pointer.
    // A union could be used as in e.g. DynLib.cc, but numpy
    // declarations are too complicated for it.
    PyArrayObject* po = (PyArrayObject*)
      (PyArray_SimpleNew(nd, &(newshp[0]), TypeConvTraits<T>::pyType()));
    // Copy the data to numarray.
    if (arr.size() > 0) {
      casacore::Bool deleteIt;
      const T* src = arr.getStorage(deleteIt);
      ArrayCopy<T>::toPy (po->data, src, arr.size());
      arr.freeStorage(src, deleteIt);
    }
    // Return the python array.
    return boost::python::object(boost::python::handle<>((PyObject*)po));
  }


}}}

