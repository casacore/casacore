//# PycArray.h: Class to convert an Array to/from Python
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
//# $Id: PycArray.h,v 1.4 2006/11/06 00:14:44 gvandiep Exp $


#ifndef PYRAP_PYCARRAY_H
#define PYRAP_PYCARRAY_H

//# Includes
// include first to avoid _POSIX_C_SOURCE redefined warnings
#include <boost/python.hpp>
#include <boost/python/object.hpp>
#include <casa/Arrays/Array.h>
#include <casa/Containers/ValueHolder.h>
#include <casa/Utilities/DataType.h>
#include <casa/Exceptions/Error.h>
#include <iostream>

namespace casa { namespace pyrap {


  // <summary>
  // A class to convert an Array to/from Python objects.
  // </summary>

  // <use visibility=export>
  // <reviewed reviewer="" date="" tests="">
  // </reviewed>

  // <synopsis>
  // </synopsis>

  // Check if numpy or numarray can be used.
  // <group>
  Bool PycCanUseNumpy();
  Bool PycCanUseNumarray();
  // </group>

  // Check if the PyObject is an array object.
  Bool PycArrayCheck (PyObject* obj_ptr);

  // Check if the PyObject is an array scalar object.
  Bool PycArrayScalarCheck (PyObject* obj_ptr);

  // Get the data type of the array scalar object.
  // It returns TpBool, TpInt, TpFloat, or TpComplex.
  // TpOther is returned if unrecognized.
  DataType PycArrayScalarType (PyObject* obj_ptr);

  struct casa_array_from_python
  {
    // Constructs an Array from a Python object.
    // If copyData=False, the array data is only copied if needed meaning
    // that the Array object in the ValueHolder can reference the data in
    // Python array.
    // That should only be used if the ValueHolder and its Array will be
    // destructed before the Python array.
    static ValueHolder makeArray(PyObject* obj_ptr, Bool copyData=False);

    // Construct an Array<String> from a special Python dict object.
    static ValueHolder makeArrayFromDict (PyObject* obj_ptr);

    // Construct a scalar from an array scalar (i.e. element in array).
    static ValueHolder makeScalar (PyObject* obj_ptr);
  };

  // Do the actual making of the PyArrayObject.
  // Specialize for strings.
  // <group>
  template <typename T>
  boost::python::object makePyArrayObject (casa::Array<T> const& arr);
  template <>
  boost::python::object makePyArrayObject (casa::Array<String> const& arr);
  // </group>

  // Convert Array to Python.
  template <typename T>
  struct casa_array_to_python
  {
    static boost::python::object makeobject (Array<T> const& arr)
      { return makePyArrayObject (arr); }
    static PyObject* convert (Array<T> const& c)
      { return boost::python::incref(makeobject(c).ptr()); }
  };

}}

#endif
