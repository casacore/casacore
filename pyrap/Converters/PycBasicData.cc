//# PycBasicData.cc: Convert casa data types to/from python
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
//# $Id: PycBasicData.cc,v 1.4 2007/01/29 04:23:01 mmarquar Exp $

#include <pyrap/Converters/PycBasicData.h>
#include <pyrap/Converters/PycArrayNP.h>

using namespace boost::python;


namespace casa { namespace pyrap {

  std::map<std::string,bool> pyregistry::_registry;
  bool pyregistry::get (const std::string& name)
  {
    return _registry[name];
  }
  void pyregistry::set (const std::string& name)
  {
    _registry[name] = true;
  }


  void convert_casa_string::reg()
  {
    std::string tname(typeid(casa::String).name());
    if (! pyregistry::get (tname)) {
      pyregistry::set (tname);
      boost::python::to_python_converter<String, casa_string_to_python_str>();
      casa_string_from_python_str();
    }
  }

  void convert_casa_iposition::reg()
  {
    std::string tname(typeid(casa::IPosition).name());
    if (! pyregistry::get (tname)) {
      pyregistry::set (tname);
      casa_iposition_to_list();
      from_python_sequence < casa::IPosition,
                             casa_reversed_variable_capacity_policy > ();
    }
  }


  void register_convert_basicdata()
  {
    casa::pyrap::numpy::register_convert_arrayscalars();
    casa::pyrap::register_convert_casa_string();
    casa::pyrap::register_convert_casa_iposition();
    casa::pyrap::register_convert_casa_vector<casa::Bool>();
    casa::pyrap::register_convert_casa_vector<casa::Int>();
    casa::pyrap::register_convert_casa_vector<casa::Double>();
    casa::pyrap::register_convert_casa_vector<casa::Float>();
    casa::pyrap::register_convert_casa_vector<casa::DComplex>();
    casa::pyrap::register_convert_casa_vector<casa::String>();
  }


  bool getSeqObject (object& py_obj)
  {
    // Restriction to list, tuple, iter, xrange until
    // Boost.Python overload resolution is enhanced.
    //  PySequence_Check() is used for numarray.
    PyObject* obj_ptr = py_obj.ptr();
    if (!(PyList_Check(obj_ptr)
	  || PyTuple_Check(obj_ptr)
	  || PyIter_Check(obj_ptr)
	  || PyRange_Check(obj_ptr)
	  || PySequence_Check(obj_ptr) )) return false;
    // Must be a measurable sequence.
    int obj_size = -1;
    bool done = false;
    // Try to get attribute size, because length fails for a numpy scalar.
    try {
      // A numpy scalar size should be 1.
      object py_tmp = py_obj.attr("size");
      if (extract<int>(py_tmp) == 1) {
	done = true;
      }
    } catch (...) {
      PyErr_Clear();
    }
    // If it failed, try to get the length.
    if (!done) {
      obj_size = PyObject_Length(obj_ptr);
      if (obj_size < 0) {
	done = true;
	PyErr_Clear();
      }
    }
    // If we seem to have a numpy/numarray scalar, try to flatten it.
    // Return the flattened object.
    if (done) {
      done = false;
      object py_flat;
      // Try if the object is a scalar numarray/numpy object which
      // can be flattened to a vector num object.
      try {
	py_flat = py_obj.attr("flatten")();    // numpy attr name
	done = true;
      } catch (...) {
	PyErr_Clear();
      }
      if (!done) {
	try {
	  py_flat = py_obj.attr("flat");       // numarray attr name
	  done = true;
	} catch (...) {
	  PyErr_Clear();
	}
      }
      if (done) py_obj = py_flat;
    }
    // If it failed, try to get the length.
    if (!done) {
      obj_size = PyObject_Length(obj_ptr);
      if (obj_size >= 0) {
	done = true;
      } else {
	PyErr_Clear();
      }
    }
    return done;
  }


}}
