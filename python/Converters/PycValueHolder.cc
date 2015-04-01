//# PycValueHolder.cc: Class to convert a ValueHolder to/from Python
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
//# $Id: PycValueHolder.cc,v 1.4 2006/11/20 23:56:04 gvandiep Exp $

#include <casacore/python/Converters/PycValueHolder.h>
#include <casacore/python/Converters/PycBasicData.h>
#include <casacore/python/Converters/PycRecord.h>
#include <casacore/python/Converters/PycArray.h>
#include <casacore/casa/Exceptions/Error.h>
#include <boost/python/object.hpp>

namespace casacore { namespace python {

  boost::python::object casa_value_to_python::makeobject
  (ValueHolder const& vh)
  {
    if (vh.isNull()) {
      return boost::python::object(boost::python::handle<>(Py_None));
    }
    switch (vh.dataType()) {
    case TpBool:
      return boost::python::object(vh.asBool());
    case TpShort:
    case TpInt:
      return boost::python::object(vh.asInt());
    case TpUChar:
    case TpUShort:
    case TpUInt:
      return boost::python::object(vh.asuInt());
    case TpInt64:
      return boost::python::object(vh.asInt64());
    case TpFloat:
    case TpDouble:
      return boost::python::object(vh.asDouble());
    case TpComplex:
    case TpDComplex:
      return boost::python::object(vh.asDComplex());
    case TpString:
      return boost::python::object((std::string const&)(vh.asString()));
    case TpArrayBool:
      return casa_array_to_python<Bool>::makeobject (vh.asArrayBool());
    case TpArrayUChar:
      return casa_array_to_python<uChar>::makeobject (vh.asArrayuChar());
    case TpArrayShort:
      return casa_array_to_python<Short>::makeobject (vh.asArrayShort());
    case TpArrayInt:
      return casa_array_to_python<Int>::makeobject (vh.asArrayInt());
    case TpArrayUInt:
      return casa_array_to_python<uInt>::makeobject (vh.asArrayuInt());
    case TpArrayInt64:
      return casa_array_to_python<Int64>::makeobject (vh.asArrayInt64());
    case TpArrayFloat:
      return casa_array_to_python<Float>::makeobject (vh.asArrayFloat());
    case TpArrayDouble:
      return casa_array_to_python<Double>::makeobject (vh.asArrayDouble());
    case TpArrayComplex:
      return casa_array_to_python<Complex>::makeobject (vh.asArrayComplex());
    case TpArrayDComplex:
      return casa_array_to_python<DComplex>::makeobject (vh.asArrayDComplex());
    case TpArrayString:
      return casa_array_to_python<String>::makeobject (vh.asArrayString());
    case TpRecord:
      return casa_record_to_python::makeobject (vh.asRecord());
    default:
      throw AipsError ("PycValueHolder: unknown casa data type " +
                       String::toString(vh.dataType()));
    }
  }


  void* casa_value_from_python::convertible(PyObject* obj_ptr)
  {
    if (! (PyBool_Check(obj_ptr)
	   || PyInt_Check(obj_ptr)
	   || PyLong_Check(obj_ptr)
	   || PyFloat_Check(obj_ptr)
	   || PyComplex_Check(obj_ptr)
	   || PyString_Check(obj_ptr)
	   || PyDict_Check(obj_ptr)
	   || PyList_Check(obj_ptr)
	   || PyTuple_Check(obj_ptr)
	   || PyIter_Check(obj_ptr)
	   || PyRange_Check(obj_ptr)
	   || PySequence_Check(obj_ptr)
	   || PycArrayCheck(obj_ptr)
	   || PycArrayScalarCheck(obj_ptr)  )) {
      // An empty numarray is Py_None, so accept that.
      if (obj_ptr != Py_None) {
	return 0;
      }
    }
    return obj_ptr;
  }

  void casa_value_from_python::construct
  (PyObject* obj_ptr,
   boost::python::converter::rvalue_from_python_stage1_data* data)
  {
    using namespace boost::python;
    using boost::python::converter::rvalue_from_python_storage; // dito
    using boost::python::throw_error_already_set; // dito
    void* storage = ((rvalue_from_python_storage<ValueHolder>*)
		     data)->storage.bytes;
    new (storage) ValueHolder();
    data->convertible = storage;
    ValueHolder& result = *((ValueHolder*)storage);
    result = makeValueHolder (obj_ptr);
  }

  ValueHolder casa_value_from_python::makeValueHolder (PyObject* obj_ptr)
  {
    using namespace boost::python;
    // An empty numarray is Py_None, so return an empty 0-dim Array.
    if (obj_ptr == Py_None) {
      return ValueHolder(0, True);
    }
    // First do array scalar check, otherwise PyInt_Check or so might
    // match depending on the machine type (32 or 64 bit).
    // In such a case an np.int64 is treated as Int instead of Int64.
    if (PycArrayScalarCheck(obj_ptr)) {
      return casa_array_from_python::makeScalar (obj_ptr);
    } else if (PyBool_Check(obj_ptr)) {
      return ValueHolder(extract<bool>(obj_ptr)());
    } else if (PyInt_Check(obj_ptr)) {
      return ValueHolder(extract<int>(obj_ptr)());
    } else if (PyLong_Check(obj_ptr)) {
      return ValueHolder(extract<Int64>(obj_ptr)());
    } else if (PyFloat_Check(obj_ptr)) {
      return ValueHolder(extract<double>(obj_ptr)());
    } else if (PyComplex_Check(obj_ptr)) {
      return ValueHolder(extract<std::complex<double> >(obj_ptr)());
    } else if (PyString_Check(obj_ptr)) {
      return ValueHolder(String(extract<std::string>(obj_ptr)()));
    } else if (PyDict_Check(obj_ptr)) {
      dict d = extract<dict>(obj_ptr)();
      if (d.has_key("shape") && d.has_key("array")) {
	return casa_array_from_python::makeArrayFromDict(obj_ptr);
      }
      return ValueHolder(casa_record_from_python::makeRecord (obj_ptr));
    } else if (PycArrayCheck(obj_ptr)) {
      return casa_array_from_python::makeArray (obj_ptr);
    } else {
      return toVector (obj_ptr);
    }
    throw AipsError ("PycValueHolder: unknown python data type");
  }

  // A bit similar to PycBasicData.h
  ValueHolder casa_value_from_python::toVector (PyObject* obj_ptr)
  {
    DataType dt = checkDataType (obj_ptr);
    switch (dt) {
    case TpBool:
      return ValueHolder(from_python_sequence< Vector<Bool>, casa_variable_capacity_policy >::make_container (obj_ptr)); 
    case TpInt:
      return ValueHolder(from_python_sequence< Vector<Int>, casa_variable_capacity_policy >::make_container (obj_ptr)); 
    case TpUInt:
      return ValueHolder(from_python_sequence< Vector<uInt>, casa_variable_capacity_policy >::make_container (obj_ptr)); 
    case TpInt64:
      return ValueHolder(from_python_sequence< Vector<Int64>, casa_variable_capacity_policy >::make_container (obj_ptr)); 
    case TpDouble:
      return ValueHolder(from_python_sequence< Vector<Double>, casa_variable_capacity_policy >::make_container (obj_ptr)); 
    case TpDComplex:
      return ValueHolder(from_python_sequence< Vector<DComplex>, casa_variable_capacity_policy >::make_container (obj_ptr)); 
    case TpString:
      return ValueHolder(from_python_sequence< Vector<String>, casa_variable_capacity_policy >::make_container (obj_ptr)); 
    case TpOther:
      // empty sequence is set as empty 1-dim array
      return ValueHolder(1, True);
    default:
      break;
    }
    throw AipsError ("PycValueHolder: python data type could not be handled");
  }

  DataType casa_value_from_python::checkDataType (PyObject* obj_ptr)
  {
    using namespace boost::python;
    // Restriction to list, tuple, iter, xrange until
    // Boost.Python overload resolution is enhanced.
    if (!(PyList_Check(obj_ptr)
	  || PyTuple_Check(obj_ptr)
	  || PyIter_Check(obj_ptr)
	  || PyRange_Check(obj_ptr)
	  || PySequence_Check(obj_ptr) )) {
      return TpOther;
    }
    handle<> obj_iter(allow_null(PyObject_GetIter(obj_ptr)));
    if (!obj_iter.get()) {       // must be convertible to an iterator
      PyErr_Clear();
      return TpOther;
    }
    DataType result = TpOther;
    int i = 0;
    for (;;i++) {
      handle<> py_elem_hdl(allow_null(PyIter_Next(obj_iter.get())));
      if (PyErr_Occurred()) {
	PyErr_Clear();
	return TpOther;
      }
      if (!py_elem_hdl.get()) break;         // end of iteration
      object py_elem_obj(py_elem_hdl);
      DataType dt;
      if (PycArrayScalarCheck (py_elem_obj.ptr())) {
        dt = PycArrayScalarType (py_elem_obj.ptr());
      } else if (PyBool_Check (py_elem_obj.ptr())) {
	dt = TpBool;
      } else if (PyInt_Check (py_elem_obj.ptr())) {
	dt = TpInt;
      } else if (PyLong_Check (py_elem_obj.ptr())) {
	dt = TpInt64;
      } else if (PyFloat_Check (py_elem_obj.ptr())) {
	dt = TpDouble;
      } else if (PyComplex_Check (py_elem_obj.ptr())) {
	dt = TpDComplex;
      } else if (PyString_Check (py_elem_obj.ptr())) {
	dt = TpString;
      } else {
        throw AipsError ("PycValueHolder: unknown python data type");
      }
      if (result == TpOther) {
	result = dt;         // first time
      } else if (dt != result) {
        // bool, string, and numeric cannot be mixed.
	if (result == TpBool  ||  result == TpString
	    || dt == TpBool  ||  dt == TpString) {
	  throw AipsError ("PycValueHolder: incompatible types in sequence");
	}
        // Use the 'highest' type.
	if (result != TpDComplex) {
	  if (dt == TpDComplex) {
	    result = dt;
	  } else if (result != TpDouble) {
            if (dt == TpDouble) {
              result = dt;
            } else if (result != TpInt64) {
              result = dt;
            }
	  }
	}
      }
    }
    return result;
  }


  bool convert_casa_valueholder::_done = false;
  void convert_casa_valueholder::reg()
  {
    if (! _done) {
      _done = true;
      boost::python::to_python_converter<ValueHolder, casa_value_to_python>();
      casa_value_from_python();
    }
  }

}}
