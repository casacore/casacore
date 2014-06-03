//# PycExcp.cc: Functions to convert a C++ exception to Python
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
//# $Id: PycExcp.cc,v 1.1 2006/10/17 03:33:50 gvandiep Exp $

#include <pyrap/Converters/PycExcp.h>
#include <casa/Containers/IterError.h>
//# The following include is necessary to work around a Boost-Python problem.
#ifndef PYRAP_NO_BOOSTPYTHON_FIX
# include <boost/type_traits/add_reference.hpp>
#endif
#include <boost/python/exception_translator.hpp>

namespace casa { namespace pyrap {

  void translate_iterexcp (const casa::IterError& e)
  {
    // Use the Python 'C' API to set up an exception object
    PyErr_SetString(PyExc_StopIteration, e.what());
  }

  void translate_stdexcp (const std::exception& e)
  {
    // Use the Python 'C' API to set up an exception object
    PyErr_SetString(PyExc_RuntimeError, e.what());
  }


  //# Note that the most general exception must be registered first.
  void register_convert_excp()
  {
    boost::python::register_exception_translator<std::exception>
      (&translate_stdexcp);
    boost::python::register_exception_translator<casa::IterError>
      (&translate_iterexcp);
  }

}}
