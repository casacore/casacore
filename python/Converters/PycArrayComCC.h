//# PycArrayCom.h: Common code to convert an Array to/from a Python array
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


#include <boost/python.hpp>
#include <numpy/arrayobject.h>

#if PY_MAJOR_VERSION >= 3
#define IS_PY3K
#endif


  bool PycArrayCheck (PyObject* obj_ptr)
  {
    if (!PyArray_API) {
      if (!isImported()) return false;
      loadAPI();
    }
    return PyArray_Check (obj_ptr);
  }

  bool isImported()
  {
    using namespace boost::python;
    // PySys_GetObject uses char* instead of const char*, so use a cast.
    const char* modStr = "modules";
    PyObject* mods = PySys_GetObject(const_cast<char*>(modStr));
    dict d =  extract<dict>(mods)();
    return d.has_key(PYC_USE_PYARRAY);
  }

  void loadAPI()
  {
    if (!PyArray_API) {
      if (!importArray()  ||  !PyArray_API) {
	throw AipsError ("PycArray: failed to load the " PYC_USE_PYARRAY
			 " API");
      }
    }
  }


  template <typename T> struct TypeConvTraits {
    typedef T     casa_type;
    typedef void* python_type;
    static NPY_TYPES pyType()
      { throw AipsError ("PycArray: unknown casa type"); }
  };
  template <> struct TypeConvTraits<bool> {
    typedef bool   casa_type;
    typedef npy_bool     python_type;
    static NPY_TYPES pyType() { return NPY_BOOL; }
  };
  template <> struct TypeConvTraits<unsigned char> {
    typedef unsigned char  casa_type;
    typedef npy_uint16   python_type;    // Note: numarray uInt8 is bool
    static NPY_TYPES pyType() { return NPY_UINT16; }
  };
  template <> struct TypeConvTraits<int16_t> {
    typedef int16_t  casa_type;
    typedef npy_int16    python_type;
    static NPY_TYPES pyType() { return NPY_INT16; }
  };
  template <> struct TypeConvTraits<uint16_t> {
    typedef uint16_t casa_type;
    typedef npy_uint16   python_type;
    static NPY_TYPES pyType() { return NPY_UINT16; }
  };
  template <> struct TypeConvTraits<int32_t> {
    typedef int32_t    casa_type;
    typedef npy_int32    python_type;
    static NPY_TYPES pyType() { return NPY_INT32; }
  };
  template <> struct TypeConvTraits<uint32_t> {
    typedef uint32_t   casa_type;
    typedef npy_uint32   python_type;
    static NPY_TYPES pyType() { return NPY_UINT32; }
  };
  template <> struct TypeConvTraits<int32_t64> {
    typedef int32_t64  casa_type;
    typedef npy_int64    python_type;
    static NPY_TYPES pyType() { return NPY_INT64; }
  };
  template <> struct TypeConvTraits<uint32_t64> {
    typedef uint32_t64 casa_type;
    typedef npy_uint64   python_type;
    static NPY_TYPES pyType() { return NPY_UINT64; }
  };
  template <> struct TypeConvTraits<float> {
    typedef float  casa_type;
    typedef npy_float32  python_type;
    static NPY_TYPES pyType() { return NPY_FLOAT32; }
  };
  template <> struct TypeConvTraits<double> {
    typedef double casa_type;
    typedef npy_float64  python_type;
    static NPY_TYPES pyType() { return NPY_FLOAT64; }
  };
  template <> struct TypeConvTraits<casacore::Complex> {
    typedef casacore::Complex casa_type;
    typedef npy_complex64 python_type;
    static NPY_TYPES pyType() { return NPY_COMPLEX64; }
  };
  template <> struct TypeConvTraits<casacore::DComplex> {
    typedef casacore::DComplex casa_type;
    typedef npy_complex128 python_type;
    static NPY_TYPES pyType() { return NPY_COMPLEX128; }
  };
  template <> struct TypeConvTraits<casacore::String> {
    typedef casacore::String casa_type;
    typedef ::PyObject*  python_type;
    static NPY_TYPES pyType() { return NPY_OBJECT; }
  };
  // This one is only used to convert numpy BYTE and SBYTE to casa short.
  // There is no back conversion, so an exception is thrown.
  template <> struct TypeConvTraits<signed char> {
    typedef signed char casa_type;
    typedef npy_int8 python_type;
    static NPY_TYPES pyType()
      { throw AipsError ("PycArray: unknown casa type"); }
  };


  template <typename T>
  void ArrayCopy<T>::toPy (void* to, const T* from, size_t nr)
  {
    if (sizeof(T) == sizeof(typename TypeConvTraits<T>::python_type)) {
      ::memcpy (to, from, nr*sizeof(T));
    } else {
      typename TypeConvTraits<T>::python_type* dst =
	static_cast<typename TypeConvTraits<T>::python_type*>(to);
      for (size_t i=0; i<nr; i++) {
	dst[i] = from[i];
      }
    }
  }
  template <typename T>
  void  ArrayCopy<T>::fromPy (T* to, const void* from, size_t nr)
  {
    if (sizeof(T) == sizeof(typename TypeConvTraits<T>::python_type)) {
      ::memcpy (to, from, nr*sizeof(T));
    } else {
      const typename TypeConvTraits<T>::python_type* src =
	static_cast<const typename TypeConvTraits<T>::python_type*>(from);
      for (size_t i=0; i<nr; i++) {
	to[i] = src[i];
      }
    }
  }
  template <typename T>
  Array<T> ArrayCopy<T>::toArray (const IPosition& shape,
				  void* data, bool copy)
  {
    // If the python array was contiguous, etc., we can directly use
    // its data because the Array used is only temporary.
    // However, if a copy of the Python array was made in PycArray.cc,
    // we cannot do that because the Python copy is out of scope when
    // the Array object gets used.
    if (!copy) {
      if (sizeof(T) == sizeof(typename TypeConvTraits<T>::python_type)) {
	return Array<T> (shape, static_cast<T*>(data), SHARE);
      }
    }
    Array<T> arr(shape);
    fromPy (arr.data(), data, arr.size());
    return arr;
  }


  void ArrayCopy<Complex>::toPy (void* to, const Complex* from, size_t nr)
  {
    if (sizeof(Complex) != sizeof(TypeConvTraits<Complex>::python_type)) {
      throw AipsError("PycArray: size of Complex data type mismatches");
    }
    ::memcpy (to, from, nr*sizeof(Complex));
  }
  void ArrayCopy<Complex>::fromPy (Complex* to, const void* from, size_t nr)
  {
    if (sizeof(Complex) != sizeof(TypeConvTraits<Complex>::python_type)) {
      throw AipsError("PycArray: size of Complex data type mismatches");
    }
    ::memcpy (to, from, nr*sizeof(Complex));
  }
  Array<Complex> ArrayCopy<Complex>::toArray (const IPosition& shape,
					      void* data, bool copy)
  {
    if (!copy) {
      if (sizeof(Complex) == sizeof(TypeConvTraits<Complex>::python_type)) {
	return Array<Complex> (shape, static_cast<Complex*>(data), SHARE);
      }
    }
    Array<Complex> arr(shape);
    fromPy (arr.data(), data, arr.size());
    return arr;
  }


  void ArrayCopy<DComplex>::toPy (void* to, const DComplex* from, size_t nr)
  {
    if (sizeof(DComplex) != sizeof(TypeConvTraits<DComplex>::python_type)) {
      throw AipsError("PycArray: size of DComplex data type mismatches");
    }
    ::memcpy (to, from, nr*sizeof(DComplex));
  }
  void ArrayCopy<DComplex>::fromPy (DComplex* to, const void* from, size_t nr)
  {
    if (sizeof(DComplex) != sizeof(TypeConvTraits<DComplex>::python_type)) {
      throw AipsError("PycArray: size of DComplex data type mismatches");
    }
    ::memcpy (to, from, nr*sizeof(DComplex));
  }
  Array<DComplex> ArrayCopy<DComplex>::toArray (const IPosition& shape,
						void* data, bool copy)
  {
    if (!copy) {
      if (sizeof(DComplex) == sizeof(TypeConvTraits<DComplex>::python_type)) {
	return Array<DComplex> (shape, static_cast<DComplex*>(data), SHARE);
      }
    }
    Array<DComplex> arr(shape);
    fromPy (arr.data(), data, arr.size());
    return arr;
  }


  void ArrayCopy<String>::toPy (void* to, const String* from, size_t nr)
  {
    PyObject** dst = static_cast<PyObject**>(to);
    for (size_t i=0; i<nr; i++) {
#ifdef IS_PY3K
      dst[i] = PyUnicode_FromString(from[i].chars());
#else
      dst[i] = PyString_FromString(from[i].chars());
#endif
    }
  }
  void ArrayCopy<String>::fromPy (String* to, const void* from, size_t nr)
  {
    using namespace boost::python;
    PyObject** src = (PyObject**)from;
    for (size_t i=0; i<nr; i++) {
      handle<> py_elem_hdl(src[i]);
      object py_elem_obj(py_elem_hdl);
      extract<String> elem_proxy(py_elem_obj);
      to[i] = elem_proxy();
    }
  }
  Array<String> ArrayCopy<String>::toArray (const IPosition& shape,
					    void* data, bool)
  {
    Array<String> arr(shape);
    fromPy (arr.data(), data, arr.size());
    return arr;
  }

  ValueHolder makeArray (PyObject* obj_ptr, bool copyData)
  {
    if (! PycArrayCheck(obj_ptr)) {
      throw AipsError ("PycArray: python object is not an array");
    }
    PyArrayObject* po = (PyArrayObject*)obj_ptr;
    boost::python::object obj;
    bool docopy = copyData;               // copy data if wanted or needed
    if (! PyArray_ISCONTIGUOUS(po)
	||  ! PyArray_ISALIGNED(po)
	||  PyArray_ISBYTESWAPPED(po)) {
      boost::python::handle<> py_hdl(obj_ptr);
      boost::python::object py_obj(py_hdl);
      // incr refcount, because ~object decrements it
      boost::python::incref(obj_ptr);
      obj = py_obj.attr("copy")();
      po = (PyArrayObject*)(obj.ptr());
      docopy = true;
    }
    // Swap axes, because Casacore has row minor and Python row major order.
    // A scalar is treated as a vector with length 1.
    int nd = PyArray_NDIM(po);
    IPosition shp(1, 1);
    if (nd > 0) {
      shp.resize (nd);
      for (int i=0; i<nd; i++) {
	shp[i] = PyArray_DIMS(po)[nd-i-1];
      }
    }
    // Assert array is contiguous now.
    // If the array is empty, numarray still sees it as non-contiguous.
    if (shp.product() > 0) {
      AlwaysAssert (PyArray_ISCONTIGUOUS(po)
		    ///&&  PyArray_ISALIGNED(po)   fails on MIPS (see issue 531)
		    &&  !PyArray_ISBYTESWAPPED(po), AipsError);
    }
    // Create the correct array.
    switch (PyArray_TYPE(po)) {
    case NPY_BOOL:
      return ValueHolder (ArrayCopy<bool>::toArray(shp, PyArray_DATA(po), docopy));
    case NPY_INT16:
      return ValueHolder (ArrayCopy<int16_t>::toArray(shp, PyArray_DATA(po), docopy));
    case NPY_UINT16:
      return ValueHolder (ArrayCopy<uint16_t>::toArray(shp, PyArray_DATA(po), docopy));
    case NPY_INT32:
      return ValueHolder (ArrayCopy<int32_t>::toArray(shp, PyArray_DATA(po), docopy));
    case NPY_UINT32:
      return ValueHolder (ArrayCopy<uint32_t>::toArray(shp, PyArray_DATA(po), docopy));
    case NPY_INT64:
      return ValueHolder (ArrayCopy<int64_t>::toArray(shp, PyArray_DATA(po), docopy));
    case NPY_FLOAT32:
      return ValueHolder (ArrayCopy<float>::toArray(shp, PyArray_DATA(po), docopy));
    case NPY_FLOAT64:
      return ValueHolder (ArrayCopy<double>::toArray(shp, PyArray_DATA(po), docopy));
    case NPY_COMPLEX64:
      return ValueHolder (ArrayCopy<Complex>::toArray(shp, PyArray_DATA(po), docopy));
    case NPY_COMPLEX128:
      return ValueHolder (ArrayCopy<DComplex>::toArray(shp, PyArray_DATA(po), docopy));
    case NPY_OBJECT:
      return ValueHolder (ArrayCopy<String>::toArray(shp, PyArray_DATA(po), docopy));
    default:
      // Some types can be the same as other types, so they cannot
      // be used in the switch (compiler complains).
      // This is true for BYTE and SBYTE which can equal to BOOL in numarray.
      // Similarly for STRING which exists for numpy and is set to
      // INT for numarray.
      if (PyArray_TYPE(po) == NPY_UINT64) {
	Array<uint64_t> arr = ArrayCopy<uint64_t>::toArray(shp, PyArray_DATA(po), false);
	Array<int64_t> res(arr.shape());
	convertArray (res, arr);
	return ValueHolder(res);
      } else if (PyArray_TYPE(po) == NPY_INT8) {
	Array<signed char> arr = ArrayCopy<signed char>::toArray(shp, PyArray_DATA(po), false);
	Array<int16_t> res(arr.shape());
	convertArray (res, arr);
	return ValueHolder(res);
      } else if (PyArray_TYPE(po) == NPY_UINT8) {
	// Copy using signed char, because unsigned char is mapped to int16_t in the Traits.
	Array<signed char> arr = ArrayCopy<signed char>::toArray(shp, PyArray_DATA(po), false);
	Array<int16_t> res(arr.shape());
        void* varr = &arr;
        Array<unsigned char>* uarr = static_cast<Array<unsigned char>*>(varr);
	convertArray (res, *uarr);
	return ValueHolder(res);
      } else if (PyArray_TYPE(po) == NPY_STRING) {
	size_t slen = 0;
	if (nd > 0) {
	  slen = PyArray_STRIDES(po)[nd-1];
	}
	return ValueHolder (ArrayCopyStr_toArray(shp, PyArray_DATA(po), slen));
      } else if (PyArray_TYPE(po) == NPY_UNICODE) {
	size_t slen = 0;
	if (nd > 0) {
	  slen = PyArray_STRIDES(po)[nd-1];
	}
	return ValueHolder (ArrayCopyUnicode_toArray(shp, PyArray_DATA(po), slen));
      }
      break;
    }
    throw AipsError ("PycArray: unknown python array data type");
  }


  // Instantiate the various templates.
  template struct ArrayCopy<bool>;
  template struct ArrayCopy<signed char>;
  template struct ArrayCopy<unsigned char>;
  template struct ArrayCopy<int16_t>;
  template struct ArrayCopy<uint16_t>;
  template struct ArrayCopy<int32_t>;
  template struct ArrayCopy<uint32_t>;
  template struct ArrayCopy<int64_t>;
  template struct ArrayCopy<uint64_t>;
  template struct ArrayCopy<float>;
  template struct ArrayCopy<double>;

  template boost::python::object makePyArrayObject
    (casacore::Array<bool> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<unsigned char> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<int16_t> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<uint16_t> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<int32_t> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<uint32_t> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<int64_t> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<float> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<double> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<Complex> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<DComplex> const& arr);
