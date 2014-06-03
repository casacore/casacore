//# PycArrayNP.h: Class to convert an Array to/from a Python numpy array
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
//# $Id: PycArrayNP.h,v 1.1 2006/11/06 00:14:44 gvandiep Exp $


#ifndef PYRAP_PYCARRAYNP_H
#define PYRAP_PYCARRAYNP_H

// include first to avoid _POSIX_C_SOURCE redefined warnings
#include <boost/python.hpp>
#include <boost/python/object.hpp>
#include <casa/Containers/ValueHolder.h>
#include <casa/Arrays/Array.h>

#if defined(AIPS_USENUMPY)
#include <numpy/arrayobject.h>
#endif

namespace casa { namespace pyrap { namespace numpy {

//# Define the common functions if numpy is used.
#if defined(AIPS_USENUMPY)
#define PYC_USE_PYARRAY "numpy"
#endif
#include <pyrap/Converters/PycArrayComH.h>
#undef PYC_USE_PYARRAY


      //# Define functions to deal with numpy array scalars.
#if defined(AIPS_USENUMPY)

      // Check if it is an array scalar object.
      bool PycArrayScalarCheck (PyObject* obj, int& type);

      // Get the data type of the array scalar object.
      // It returns TpBool, TpInt, TpFloat, or TpComplex.
      // TpOther is returned if unrecognized.
      DataType PycArrayScalarType (PyObject* obj_ptr);

      // Make a scalar object.
      ValueHolder makeScalar (PyObject* obj, int type);

      // Register all array scalar converters.
      void register_convert_arrayscalars();

      // Templated helper function to get a value from a ValueHolder.
      // Specialize for each type supported.
      // <group>
      template<typename T> T getScalar (const ValueHolder&);
      template<> inline Bool     getScalar (const ValueHolder& vh)
        { return vh.asBool(); }
      template<> inline Char     getScalar (const ValueHolder& vh)
        { return vh.asShort(); }
      template<> inline uChar    getScalar (const ValueHolder& vh)
        { return vh.asuChar(); }
      template<> inline Short    getScalar (const ValueHolder& vh)
        { return vh.asShort(); }
      template<> inline uShort   getScalar (const ValueHolder& vh)
        { return vh.asuShort(); }
      template<> inline Int      getScalar (const ValueHolder& vh)
        { return vh.asInt(); }
      template<> inline uInt     getScalar (const ValueHolder& vh)
        { return vh.asuInt(); }
      template<> inline Long     getScalar (const ValueHolder& vh)
        { return vh.asInt(); }
      template<> inline uLong    getScalar (const ValueHolder& vh)
        { return vh.asuInt(); }
      template<> inline Int64    getScalar (const ValueHolder& vh)
        { return vh.asInt(); }
      template<> inline uInt64   getScalar (const ValueHolder& vh)
        { return vh.asuInt(); }
      template<> inline Float    getScalar (const ValueHolder& vh)
        { return vh.asFloat(); }
      template<> inline Double   getScalar (const ValueHolder& vh)
        { return vh.asDouble(); }
      template<> inline Complex  getScalar (const ValueHolder& vh)
        { return vh.asComplex(); }
      template<> inline DComplex getScalar (const ValueHolder& vh)
        { return vh.asDComplex(); }
      // </group>

      // Struct with static functions to convert a numpy array scalar to
      // the templated type (e.g. Int).
      template <typename T>
      struct array_scalar_from_python
      {
	array_scalar_from_python()
	{
	  boost::python::converter::registry::push_back(
            &convertible,
            &construct,
            boost::python::type_id<T>());
	}

	// Check if it is a type we can convert.
	static void* convertible(PyObject* obj_ptr)
	{
	  int type;
	  if (PycArrayScalarCheck(obj_ptr, type)) {
	    return obj_ptr;
	  }
	  return 0;
	}

	// Constructs a T from a Python array scalar object.
	static void construct(
          PyObject* obj_ptr,
          boost::python::converter::rvalue_from_python_stage1_data* data)
	{
	  using namespace boost::python;
	  void* storage = ((converter::rvalue_from_python_storage<T>*)
		     data)->storage.bytes;
	  new (storage) T();
	  data->convertible = storage;
	  int type;
	  PycArrayScalarCheck (obj_ptr, type);
	  *static_cast<T*>(storage) = getScalar<T> (makeScalar(obj_ptr, type));
	}
      };

#else

      bool PycArrayScalarCheck (PyObject*, int&)
        { return False; }

      DataType PycArrayScalarType (PyObject*)
        { return TpOther; }

      ValueHolder makeScalar (PyObject*, int)
        { return ValueHolder(); }

      void register_convert_arrayscalars()
      {}

#endif


}}}

#endif
