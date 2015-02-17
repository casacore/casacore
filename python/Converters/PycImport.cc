//# PycImport.cc: Function to import a module and class in Python
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

#include <casacore/python/Converters/PycImport.h>
#include <casacore/casa/OS/Path.h>

namespace casacore { namespace python {

    boost::python::object PycImport (const String& moduleName,
                                     const String& className)
    {
      try {
        // Initialize the Python interpreter.
        // Note: a second call is a no-op.
        Py_Initialize();
        // Insert the current working directory into the python path,
        // because Boost-Python does not do that.
        // (from http://stackoverflow.com/questions/9285384/
        //  how-does-import-work-with-boost-python-from-inside-python-files)
        string workingDir = Path(".").absoluteName();
        char path[] = "path";      // warning if "path" is used below
        PyObject* sysPath = PySys_GetObject(path);
        PyList_Insert (sysPath, 0, PyString_FromString(workingDir.c_str()));
        // First import main.
        boost::python::object mainModule = boost::python::import
          ("__main__");
        // Import the given module.
        boost::python::object pyModule = boost::python::import
          (moduleName.c_str());
        // Get the python class object from the imported module.
        return pyModule.attr (className.c_str());
      } catch (boost::python::error_already_set const &) {
        // handle the exception in some way
        PyErr_Print();
        throw;
      }
    }

}}
