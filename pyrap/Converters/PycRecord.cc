//# PycRecord.cc: Class to convert a Record to/from Python
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
//# $Id: PycRecord.cc,v 1.2 2006/10/17 03:33:50 gvandiep Exp $

#include <pyrap/Converters/PycRecord.h>
#include <pyrap/Converters/PycValueHolder.h>
#include <casa/Utilities/Assert.h>
#include <boost/python/object.hpp>

namespace casa { namespace pyrap {

  boost::python::dict casa_record_to_python::makeobject
  (Record const& rec)
  {
    boost::python::dict d;
    // Copy over the record field by field
    uInt nf = rec.nfields();
    for (uInt i=0; i<nf; i++) {
      d.setdefault ((std::string const&)(rec.name(i)),
		    casa_value_to_python::makeobject(rec.asValueHolder(i)));
    }
    return d;
  }


  void* casa_record_from_python::convertible(PyObject* obj_ptr)
  {
    if (!(PyDict_Check(obj_ptr))) return 0;
    return obj_ptr;
  }

  void casa_record_from_python::construct
  (PyObject* obj_ptr,
   boost::python::converter::rvalue_from_python_stage1_data* data)
  {
    using namespace boost::python;
    void* storage = ((converter::rvalue_from_python_storage<Record>*)
		     data)->storage.bytes;
    new (storage) Record();
    data->convertible = storage;
    Record& result = *((Record*)storage);
    result = makeRecord (obj_ptr);
  }

  Record casa_record_from_python::makeRecord (PyObject* obj_ptr)
  {
    using namespace boost::python;
    AlwaysAssert (PyDict_Check(obj_ptr), AipsError);
    dict d = extract<dict>(obj_ptr)();
    list keys = d.keys();
    Record result;
    handle<> obj_iter(PyObject_GetIter(keys.ptr()));
    std::size_t i=0;
    for(;;i++) {
      handle<> py_elem_hdl(allow_null(PyIter_Next(obj_iter.get())));
      if (PyErr_Occurred()) throw_error_already_set();
      if (!py_elem_hdl.get()) break;             // end of iteration
      object py_elem_key(py_elem_hdl);
      result.defineFromValueHolder (extract<string>(py_elem_key)(),
           casa_value_from_python::makeValueHolder(d.get(py_elem_key).ptr()));
    }
    return result;
  }


  bool convert_casa_record::_done = false;
  void convert_casa_record::reg()
  {
    if (! _done) {
      _done = true;
      boost::python::to_python_converter<Record, casa_record_to_python>();
      casa_record_from_python();
    }
  }

}}
