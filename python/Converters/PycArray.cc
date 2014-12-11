//# PycArray.cc: Class to convert an Array to/from Python
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
//# $Id: PycArray.cc,v 1.4 2006/11/06 00:14:44 gvandiep Exp $

#include <casacore/python/Converters/PycArray.tcc>
#include <casacore/python/Converters/PycArrayNP.h>
#include <casacore/python/Converters/PycBasicData.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <boost/python/dict.hpp>
#include <sysmodule.h>

using namespace boost::python;

namespace casacore { namespace python {

  Bool PycArrayCheck (PyObject* obj_ptr)
  {
    return numpy::PycArrayCheck(obj_ptr);
  }

  Bool PycArrayScalarCheck (PyObject* obj_ptr)
  {
    int type;
    return numpy::PycArrayScalarCheck(obj_ptr, type);
  }

  DataType PycArrayScalarType (PyObject* obj_ptr)
  {
    return numpy::PycArrayScalarType(obj_ptr);
  }

  ValueHolder casa_array_from_python::makeArray (PyObject* obj_ptr,
						 Bool copyData)
  {
    if (! numpy::PycArrayCheck(obj_ptr)) {
      throw AipsError ("PycArray: python object is not a numpy array");
    }
    return numpy::makeArray (obj_ptr, copyData);
  }

  ValueHolder casa_array_from_python::makeScalar (PyObject* obj_ptr)
  {
    int type;
    if (!numpy::PycArrayScalarCheck(obj_ptr, type)) {
      throw AipsError ("PycArray: python object is not a numpy array scalar");
    }
    return numpy::makeScalar (obj_ptr, type);
  }

  ValueHolder casa_array_from_python::makeArrayFromDict (PyObject* obj_ptr)
  {
    if (! PyDict_Check(obj_ptr)) {
      throw AipsError ("PycArray: python object is not a dict");
    }
    dict d = extract<dict>(obj_ptr)();
    IPosition shp = extract<IPosition>(d.get("shape").ptr())();
    Array<String> arr = extract<Vector<String> >(d.get("array").ptr())();
    if (Int(arr.size()) != shp.product()) {
      throw AipsError("PycArray: array size mismatches the shape");
    }
    return ValueHolder(arr.reform (shp));
  }

  template <>
  object makePyArrayObject (casacore::Array<String> const& arr)
  {
    object a = to_list< Array<String> >::makeobject (arr);
    if (arr.ndim() == 1) {
      return a;
    }
    dict d;
    d.setdefault (std::string("shape"),
		  to_list<IPosition>::makeobject (arr.shape()));
    d.setdefault (std::string("array"), a);
    return d;
  }


  // Instantiate the templates.
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

}}
