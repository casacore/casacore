//# PycRecord.h: Class to convert a Record to/from Python
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
//# $Id: PycRecord.h,v 1.2 2006/10/17 03:33:50 gvandiep Exp $


#ifndef PYRAP_PYCRECORD_H
#define PYRAP_PYCRECORD_H

//# Includes
// include first to avoid _POSIX_C_SOURCE redefined warnings
#include <boost/python.hpp>
#include <boost/python/dict.hpp>
#include <casa/Containers/Record.h>

namespace casa { namespace pyrap {

  // <summary>
  // A class to convert a (Table)Record to/from Python objects.
  // </summary>

  // <use visibility=export>
  // <reviewed reviewer="" date="" tests="">
  // </reviewed>

  // <synopsis>
  // </synopsis>

  // convert casa::Record to PyDict
  struct casa_record_to_python
  {
    static boost::python::dict makeobject (Record const& rec);
    static PyObject* convert (Record const& rec)
    {
      return boost::python::incref(makeobject(rec).ptr());
    }
  };


  struct casa_record_from_python
  {
    casa_record_from_python()
    {
      boost::python::converter::registry::push_back(
        &convertible,
        &construct,
        boost::python::type_id<Record>());
    }

    // Check if it is a type we can convert.
    static void* convertible(PyObject* obj_ptr);

    // Constructs a Record from a Python object.
    static void construct(
      PyObject* obj_ptr,
      boost::python::converter::rvalue_from_python_stage1_data* data);

    static Record makeRecord (PyObject* obj_ptr);
  };


  // Register the Record conversion.
  struct convert_casa_record
  {
    static void reg();
    static bool _done;
  };
  inline void register_convert_casa_record()
    { convert_casa_record::reg(); }

}}

#endif
