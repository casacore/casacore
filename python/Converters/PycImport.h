//# PycImport.h: Function to import a module and class in Python
//# Copyright (C) 2015
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

#ifndef PYTHON_PYCIMPORT
#define PYTHON_PYCIMPORT

#include <casacore/python/Converters/PycImport.h>
#include <casacore/casa/OS/Path.h>

#include <boost/python.hpp>

namespace casacore { namespace python {

    // This function can be used to create a Python class from C++.
    // <br>It returns the object to be used to create a class instance.
    // It initializes Python, imports the main module and the given
    // module, and finally creates the class object.
    // Before the import it adds the working directory to the Python
    // module search path, because Boost-Python does not do that.
    // <br>For example:
    // <srcblock>
    //   // Create the class object.
    //   boost::python::object pyClass =
    //               casacore::python::PycImport("modulenm", "classnm");
    //   // Register the converters needed.
    //   casacore::python::register_convert_excp();
    //   casacore::python::register_convert_basicdata();
    //   casacore::python::register_convert_casa_valueholder();
    //   casacore::python::register_convert_casa_record();
    //   casacore::python::register_convert_std_vector<Bool>();
    //   casacore::python::register_convert_std_vector<Int>();
    //   casacore::python::register_convert_std_vector<String>();
    //   // Instantiate the Python class object with possible arguments
    //   // for the __init__ function.
    //   boost::python::object classObject = pyClass(arg1, arg2);
    //   // Invoke the 'setup' function in the Python object
    //   boost::python::object result =
    //      classObject.attr("setup")(arg1, arg2, arg3);
    //   // Extract the result (a dict in this case) as a Record.
    //   Record rec = boost::python::extract<Record>(result);
    // </srcblock>

    boost::python::object PycImport (const String& moduleName,
                                     const String& className);

}}

#endif
