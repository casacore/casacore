//# PycValueHolder.h: Class to convert a ValueHolder to/from Python
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
//# $Id: PycValueHolder.h,v 1.2 2006/10/17 03:33:50 gvandiep Exp $


#ifndef PYRAP_PYCVALUEHOLDER_H
#define PYRAP_PYCVALUEHOLDER_H

//# Includes

// include first to avoid _POSIX_C_SOURCE redefined warnings
#include <boost/python.hpp>
#include <casa/Containers/ValueHolder.h>
#include <casa/Utilities/DataType.h>

namespace casa { namespace pyrap {


  // <summary>
  // A class to convert a ValueHolder to/from Python objects.
  // </summary>

  // <use visibility=export>
  // <reviewed reviewer="" date="" tests="">
  // </reviewed>

  // <synopsis>
  // </synopsis>

  struct casa_value_to_python
  {
    static boost::python::object makeobject (ValueHolder const&);
    static PyObject* convert (ValueHolder const& c)
    {
      return boost::python::incref(makeobject(c).ptr());
    }
  };

  struct casa_value_from_python
  {
    casa_value_from_python()
    {
      boost::python::converter::registry::push_back(
        &convertible,
        &construct,
        boost::python::type_id<ValueHolder>());
    }

    // Check if it is a type we can convert.
    static void* convertible(PyObject* obj_ptr);

    // Constructs a ValueHolder from a Python object.
    static void construct(
      PyObject* obj_ptr,
      boost::python::converter::rvalue_from_python_stage1_data* data);

    // Make a ValueHolder from all possible python data types.
    static ValueHolder makeValueHolder (PyObject* obj_ptr);

    // Make a vector from a python sequence.
    static ValueHolder toVector (PyObject* obj_ptr);
    // Get (and check) the data type in a python sequence.
    static DataType checkDataType (PyObject* obj_ptr);
  };


  // Register the ValueHolder conversion.
  struct convert_casa_valueholder
  {
    static void reg();
    static bool _done;
  };
  inline void register_convert_casa_valueholder()
    { convert_casa_valueholder::reg(); }

}}

#endif
