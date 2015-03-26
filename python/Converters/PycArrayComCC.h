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
//#
//# $Id: PycArrayComCC.h,v 1.3 2006/11/20 23:58:17 gvandiep Exp $


  Bool PycArrayCheck (PyObject* obj_ptr)
  {
    if (!PyArray_API) {
      if (!isImported()) return False;
      loadAPI();
    }
    return PyArray_Check (obj_ptr);
  }

  Bool isImported()
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
    static PyArray_TYPES pyType()
      { throw AipsError ("PycArray: unknown casa type"); }
  };
  template <> struct TypeConvTraits<casacore::Bool> {
    typedef casacore::Bool   casa_type;
    typedef npy_bool     python_type;
    static PyArray_TYPES pyType() { return NPY_BOOL; }
  };
  template <> struct TypeConvTraits<casacore::uChar> {
    typedef casacore::uChar  casa_type;
    typedef npy_uint16   python_type;    // Note: numarray uInt8 is Bool
    static PyArray_TYPES pyType() { return NPY_UINT16; }
  };
  template <> struct TypeConvTraits<casacore::Short> {
    typedef casacore::Short  casa_type;
    typedef npy_int16    python_type;
    static PyArray_TYPES pyType() { return NPY_INT16; }
  };
  template <> struct TypeConvTraits<casacore::uShort> {
    typedef casacore::uShort casa_type;
    typedef npy_uint16   python_type;
    static PyArray_TYPES pyType() { return NPY_UINT16; }
  };
  template <> struct TypeConvTraits<casacore::Int> {
    typedef casacore::Int    casa_type;
    typedef npy_int32    python_type;
    static PyArray_TYPES pyType() { return NPY_INT32; }
  };
  template <> struct TypeConvTraits<casacore::uInt> {
    typedef casacore::uInt   casa_type;
    typedef npy_uint32   python_type;
    static PyArray_TYPES pyType() { return NPY_UINT32; }
  };
  template <> struct TypeConvTraits<casacore::Int64> {
    typedef casacore::Int64  casa_type;
    typedef npy_int64    python_type;
    static PyArray_TYPES pyType() { return NPY_INT64; }
  };
  template <> struct TypeConvTraits<casacore::uInt64> {
    typedef casacore::uInt64 casa_type;
    typedef npy_uint64   python_type;
    static PyArray_TYPES pyType() { return NPY_UINT64; }
  };
  template <> struct TypeConvTraits<casacore::Float> {
    typedef casacore::Float  casa_type;
    typedef npy_float32  python_type;
    static PyArray_TYPES pyType() { return NPY_FLOAT32; }
  };
  template <> struct TypeConvTraits<casacore::Double> {
    typedef casacore::Double casa_type;
    typedef npy_float64  python_type;
    static PyArray_TYPES pyType() { return NPY_FLOAT64; }
  };
  template <> struct TypeConvTraits<casacore::Complex> {
    typedef casacore::Complex casa_type;
    typedef npy_complex64 python_type;
    static PyArray_TYPES pyType() { return NPY_COMPLEX64; }
  };
  template <> struct TypeConvTraits<casacore::DComplex> {
    typedef casacore::DComplex casa_type;
    typedef npy_complex128 python_type;
    static PyArray_TYPES pyType() { return NPY_COMPLEX128; }
  };
  template <> struct TypeConvTraits<casacore::String> {
    typedef casacore::String casa_type;
    typedef ::PyObject*  python_type;
    static PyArray_TYPES pyType() { return NPY_OBJECT; }
  };
  // This one is only used to convert numpy BYTE and SBYTE to casa short.
  // There is no back conversion, so an exception is thrown.
  template <> struct TypeConvTraits<casacore::Char> {
    typedef casacore::Char   casa_type;
    typedef npy_int8     python_type;
    static PyArray_TYPES pyType()
      { throw AipsError ("PycArray: unknown casa type"); }
  };


  template <typename T>
  void ArrayCopy<T>::toPy (void* to, const T* from, uInt nr)
  {
    if (sizeof(T) == sizeof(typename TypeConvTraits<T>::python_type)) {
      ::memcpy (to, from, nr*sizeof(T));
    } else {
      typename TypeConvTraits<T>::python_type* dst =
	static_cast<typename TypeConvTraits<T>::python_type*>(to);
      for (uInt i=0; i<nr; i++) {
	dst[i] = from[i];
      }
    }
  }
  template <typename T>
  void  ArrayCopy<T>::fromPy (T* to, const void* from, uInt nr)
  {
    if (sizeof(T) == sizeof(typename TypeConvTraits<T>::python_type)) {
      ::memcpy (to, from, nr*sizeof(T));
    } else {
      const typename TypeConvTraits<T>::python_type* src =
	static_cast<const typename TypeConvTraits<T>::python_type*>(from);
      for (uInt i=0; i<nr; i++) {
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


  void ArrayCopy<Complex>::toPy (void* to, const Complex* from, uInt nr)
  {
    if (sizeof(Complex) != sizeof(TypeConvTraits<Complex>::python_type)) {
      throw AipsError("PycArray: size of Complex data type mismatches");
    }
    ::memcpy (to, from, nr*sizeof(Complex));
  }
  void ArrayCopy<Complex>::fromPy (Complex* to, const void* from, uInt nr)
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


  void ArrayCopy<DComplex>::toPy (void* to, const DComplex* from, uInt nr)
  {
    if (sizeof(DComplex) != sizeof(TypeConvTraits<DComplex>::python_type)) {
      throw AipsError("PycArray: size of DComplex data type mismatches");
    }
    ::memcpy (to, from, nr*sizeof(DComplex));
  }
  void ArrayCopy<DComplex>::fromPy (DComplex* to, const void* from, uInt nr)
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


  void ArrayCopy<String>::toPy (void* to, const String* from, uInt nr)
  {
    PyObject** dst = static_cast<PyObject**>(to);
    for (uInt i=0; i<nr; i++) {
      dst[i] = PyString_FromString(from[i].chars());
    }
  }
  void ArrayCopy<String>::fromPy (String* to, const void* from, uInt nr)
  {
    using namespace boost::python;
    PyObject** src = (PyObject**)from;
    for (uInt i=0; i<nr; i++) {
      handle<> py_elem_hdl(src[i]);
      object py_elem_obj(py_elem_hdl);
      extract<std::string> elem_proxy(py_elem_obj);
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

  ValueHolder makeArray (PyObject* obj_ptr, Bool copyData)
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
    int nd = po->nd;
    IPosition shp(1, 1);
    if (nd > 0) {
      shp.resize (nd);
      for (int i=0; i<nd; i++) {
	shp[i] = po->dimensions[nd-i-1];
      }
    }
    // Assert array is contiguous now.
    // If the array is empty, numarray still sees it as non-contiguous.
    if (shp.product() > 0) {
      AlwaysAssert (PyArray_ISCONTIGUOUS(po)
		    &&  PyArray_ISALIGNED(po)
		    &&  !PyArray_ISBYTESWAPPED(po), AipsError);
    }
    // Create the correct array.
    switch (po->descr->type_num) {
    case NPY_BOOL:
      return ValueHolder (ArrayCopy<Bool>::toArray(shp, po->data, docopy));
    case NPY_INT16:
      return ValueHolder (ArrayCopy<Short>::toArray(shp, po->data, docopy));
    case NPY_UINT16:
      return ValueHolder (ArrayCopy<uShort>::toArray(shp, po->data, docopy));
    case NPY_INT32:
      return ValueHolder (ArrayCopy<Int>::toArray(shp, po->data, docopy));
    case NPY_UINT32:
      return ValueHolder (ArrayCopy<uInt>::toArray(shp, po->data, docopy));
    case NPY_INT64:
      return ValueHolder (ArrayCopy<Int64>::toArray(shp, po->data, docopy));
    case NPY_FLOAT32:
      return ValueHolder (ArrayCopy<Float>::toArray(shp, po->data, docopy));
    case NPY_FLOAT64:
      return ValueHolder (ArrayCopy<Double>::toArray(shp, po->data, docopy));
    case NPY_COMPLEX64:
      return ValueHolder (ArrayCopy<Complex>::toArray(shp, po->data, docopy));
    case NPY_COMPLEX128:
      return ValueHolder (ArrayCopy<DComplex>::toArray(shp, po->data, docopy));
    case NPY_OBJECT:
      return ValueHolder (ArrayCopy<String>::toArray(shp, po->data, docopy));
    default:
      // Some types can be the same as other types, so they cannot
      // be used in the switch (compiler complains).
      // This is true for BYTE and SBYTE which can equal to BOOL in numarray.
      // Similarly for STRING which exists for numpy and is set to
      // INT for numarray.
      if (po->descr->type_num == NPY_UINT64) {
	Array<uInt64> arr = ArrayCopy<uInt64>::toArray(shp, po->data, False);
	Array<Int64> res(arr.shape());
	convertArray (res, arr);
	return ValueHolder(res);
      } else if (po->descr->type_num == NPY_INT8) {
	Array<Char> arr = ArrayCopy<Char>::toArray(shp, po->data, False);
	Array<Short> res(arr.shape());
	convertArray (res, arr);
	return ValueHolder(res);
      } else if (po->descr->type_num == NPY_UINT8) {
	// Copy using Char, because uChar is mapped to Short in the Traits.
	Array<Char> arr = ArrayCopy<Char>::toArray(shp, po->data, False);
	Array<Short> res(arr.shape());
        void* varr = &arr;
        Array<uChar>* uarr = static_cast<Array<uChar>*>(varr);
	convertArray (res, *uarr);
	return ValueHolder(res);
      } else if (po->descr->type_num == NPY_STRING) {
	int slen = 0;
	if (nd > 0) {
	  slen = po->strides[nd-1];
	}
	return ValueHolder (ArrayCopyStr_toArray(shp, po->data, slen));
      }
      break;
    }
    throw AipsError ("PycArray: unknown python array data type");
  } 


  // Instantiate the various templates.
  template struct ArrayCopy<Bool>;
  template struct ArrayCopy<Char>;
  template struct ArrayCopy<uChar>;
  template struct ArrayCopy<Short>;
  template struct ArrayCopy<uShort>;
  template struct ArrayCopy<Int>;
  template struct ArrayCopy<uInt>;
  template struct ArrayCopy<Int64>;
  template struct ArrayCopy<uInt64>;
  template struct ArrayCopy<Float>;
  template struct ArrayCopy<Double>;

  template boost::python::object makePyArrayObject
    (casacore::Array<Bool> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<uChar> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<Short> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<uShort> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<Int> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<uInt> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<Int64> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<Float> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<Double> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<Complex> const& arr);
  template boost::python::object makePyArrayObject
    (casacore::Array<DComplex> const& arr);
