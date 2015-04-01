//# PycBasicData.h: Convert casa data types to/from python
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
//# $Id: PycBasicData.h,v 1.5 2006/10/25 01:42:13 gvandiep Exp $

#ifndef PYRAP_PYCBASICDATA_H
#define PYRAP_PYCBASICDATA_H

// include python first to avoid _POSIX_C_SOURCE redefined warnings
#include <boost/python.hpp>
#include <boost/python/object.hpp>
#include <casacore/python/Converters/PycArray.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <vector>
#include <map>

// Define classes and functions to convert the basic data types and
// containers to and from Python.

namespace casacore { namespace python {

  // Prevent a converter from being registered multiple times.
  class pyregistry
  {
  public:
    static bool get (const std::string& name);
    static void set (const std::string& name);
  private:
    static std::map<std::string,bool> _registry;
  };

  // Check if the given object is a sequence object.
  // If so, return true.
  // py_obj gets changed if the given object was a scalar numpy/numarray.
  // In that case it is flattened.
  bool getSeqObject (boost::python::object& py_obj);

  // Convert a String object to python.
  struct casa_string_to_python_str
  {
    static boost::python::object makeobject(String const& s)
      { return boost::python::object((const std::string&)s); }
    static PyObject* convert(String const& s)
      { return boost::python::incref(makeobject(s).ptr()); }
  };

  // Convert a String object from python.
  struct casa_string_from_python_str
  {
    casa_string_from_python_str()
    {
      boost::python::converter::registry::push_back(
        &convertible,
        &construct,
        boost::python::type_id<String>());
    }

    static void* convertible(PyObject* obj_ptr)
    {
      if (!PyString_Check(obj_ptr)) return 0;
      return obj_ptr;
    }

    static void construct(
      PyObject* obj_ptr,
      boost::python::converter::rvalue_from_python_stage1_data* data)
    {
      const char* value = PyString_AsString(obj_ptr);
      if (value == 0) boost::python::throw_error_already_set();
      void* storage = (
        (boost::python::converter::rvalue_from_python_storage<String>*)
          data)->storage.bytes;
      new (storage) String(value);
      data->convertible = storage;
    }
  };


  // Default operations on all containers for conversion from Python
  // container to C++ one.

  // Copied from
  // scitbx/include/scitbx/boost_python/container_conversions.h that is
  // described in the <a
  // href="http://www.boost.org/libs/python/doc/v2/faq.html">
  // Boost.Python FAQ. </a>

  // @author Ralf W. Grosse-Kunstleve <rwgk@yahoo.com> of
  // <a href="http://www.lbl.gov/">Lawrence Berkeley National Laboratory</a>
  struct default_policy
  {
    static bool check_convertibility_per_element() { return true; }

    template <typename ContainerType>
    static bool check_size(boost::type<ContainerType>, std::size_t)
    {
      return true;
    }

    template <typename ContainerType>
    static void assert_size(boost::type<ContainerType>, std::size_t)
    {}

    template <typename ContainerType>
    static void reserve(ContainerType&, std::size_t)
    {}
  };

  // Operations on containers that have variable capacity for
  // conversion from Python container to C++ one.

  // Copied from
  // scitbx/include/scitbx/boost_python/container_conversions.h that is
  // described in the <a
  // href="http://www.boost.org/libs/python/doc/v2/faq.html">
  // Boost.Python FAQ. </a>

  // @author Ralf W. Grosse-Kunstleve <rwgk@yahoo.com> of
  // <a href="http://www.lbl.gov/">Lawrence Berkeley National Laboratory</a>
  struct stl_variable_capacity_policy : default_policy
  {
    template <typename ContainerType>
    static void reserve(ContainerType& a, std::size_t sz)
    {
      a.reserve(sz);
    }

    template <typename ContainerType, typename ValueType>
    static void set_value(ContainerType& a, std::size_t i, ValueType const& v)
    {
      AlwaysAssert(a.size() == i, AipsError);
      a.push_back(v);
    }
  };

  struct casa_variable_capacity_policy : default_policy
  {
    template <typename ContainerType>
    static void reserve(ContainerType& a, std::size_t sz)
    {
      a.resize(sz);
    }

    template <typename ContainerType, typename ValueType>
    static void set_value(ContainerType& a, std::size_t i, ValueType const& v)
    {
      assert(a.size() > i);
      a[i] = v;
    }
  };

  struct casa_reversed_variable_capacity_policy : default_policy
  {
    template <typename ContainerType>
    static void reserve(ContainerType& a, std::size_t sz)
    {
      a.resize(sz);
    }

    template <typename ContainerType, typename ValueType>
    static void set_value(ContainerType& a, std::size_t i, ValueType const& v)
    {
      assert(a.size() > i);
      a[a.size() - i - 1] = v;
    }
  };


  // A wrapper of a conversion function to convert a STL vector to a
  // Python list.  This class satisfies the requirements of the
  // boost::python::to_python_converter conversion template argument.

  // Copied from
  // scitbx/include/scitbx/boost_python/container_conversions.h that is
  // described in the <a
  // href="http://www.boost.org/libs/python/doc/v2/faq.html">
  // Boost.Python FAQ. </a>

  // @author Ralf W. Grosse-Kunstleve <rwgk@yahoo.com> of 
  // <a href="http://www.lbl.gov/">Lawrence Berkeley National Laboratory</a>
  template < typename ContainerType >
  struct to_list
  {
    // Creates and returns a Python list from the elements copied
    // from a STL container. The ContainerType must be a container
    // with STL iterators defined on it.
    // It may contain any type of object supported by the
    // boost::python::object constructor.
    static boost::python::object makeobject (ContainerType const& c)
    {
      boost::python::list result;
      typename ContainerType::const_iterator i = c.begin();
      typename ContainerType::const_iterator iEnd = c.end();
      for( ; i != iEnd; ++i) {
	result.append(*i);
      }
      return result;
    }
    static PyObject* convert (ContainerType const& c)
    {
      return boost::python::incref(makeobject(c).ptr());
    }
  };
  //# Make specialisations for various types.
  template <>
  struct to_list <casacore::IPosition >
  {
    typedef IPosition ContainerType;
    static boost::python::list makeobject (ContainerType const& c)
    {
      // Reverse IPosition values.
      boost::python::list result;
      for (int i=c.size()-1; i>=0; --i) {
	result.append(c[i]);
      }
      return result;
    }
    static PyObject* convert (ContainerType const& c)
    {
      return boost::python::incref(makeobject(c).ptr());
    }
  };
  //# This specialisation is needed because on OS-X 10.9 clang-3.5 with
  //# Boost-Python 1.57 gives a compile error
  //# /opt/casa/01/include/boost/python/converter/arg_to_python.hpp:209:9:
  //#   error: no matching constructor for initialization of
  //#     'boost::python::converter::detail::arg_to_python_base'
  //#       : arg_to_python_base(&x, registered<T>::converters)
  template <>
  struct to_list <std::vector <bool> >
  {
    typedef std::vector <bool> ContainerType;
    static boost::python::list makeobject (ContainerType const& c)
    {
      boost::python::list result;
      ContainerType::const_iterator i = c.begin();
      ContainerType::const_iterator iEnd = c.end();
      for( ; i != iEnd; ++i) {
        bool b = *i;
	result.append(b);
      }
      return result;
    }
    static PyObject* convert (ContainerType const& c)
    {
      return boost::python::incref(makeobject(c).ptr());
    }
  };
  template <>
  struct to_list <std::vector <casacore::String> >
  {
    typedef std::vector <casacore::String> ContainerType;
    static boost::python::list makeobject (ContainerType const& c)
    {
      boost::python::list result;
      ContainerType::const_iterator i = c.begin();
      ContainerType::const_iterator iEnd = c.end();
      for( ; i != iEnd; ++i) {
	result.append((std::string const&)(*i));
      }
      return result;
    }
    static PyObject* convert (ContainerType const& c)
    {
      return boost::python::incref(makeobject(c).ptr());
    }
  };
  template <>
  struct to_list <casacore::Array <casacore::String> >
  {
    typedef casacore::Array <casacore::String> ContainerType;
    static boost::python::object makeobject (ContainerType const& c)
    {
      boost::python::list result;
      ContainerType::const_iterator i = c.begin();
      ContainerType::const_iterator iEnd = c.end();
      for( ; i != iEnd; ++i) {
	result.append((std::string const&)(*i));
      }
      return result;
    }
    static PyObject* convert (ContainerType const& c)
    {
      return boost::python::incref(makeobject(c).ptr());
    }
  };
  template <>
  struct to_list <casacore::Vector <casacore::String> >
  {
    typedef casacore::Vector <casacore::String> ContainerType;
    static boost::python::object makeobject (ContainerType const& c)
    {
      boost::python::list result;
      ContainerType::const_iterator i = c.begin();
      ContainerType::const_iterator iEnd = c.end();
      for( ; i != iEnd; ++i) {
	result.append((std::string const&)(*i));
      }
      return result;
    }
    static PyObject* convert (ContainerType const& c)
    {
      return boost::python::incref(makeobject(c).ptr());
    }
  };

  // Converts an STL vector or casa Array of T objects to Python list. 
  // Copied from
  // scitbx/include/scitbx/boost_python/container_conversions.h that is
  // described in the <a
  // href="http://www.boost.org/libs/python/doc/v2/faq.html">
  // Boost.Python FAQ. </a>
  // @author Ralf W. Grosse-Kunstleve <rwgk@yahoo.com> of
  // <a href="http://www.lbl.gov/">Lawrence Berkeley National Laboratory</a>
  template < typename T >
  struct std_vector_to_list 
  {
    std_vector_to_list ()
    {
      boost::python::to_python_converter < std::vector < T >, 
	                         to_list < std::vector < T > >  > ();
    }
  };
  template < typename T >
  struct casa_array_to_list 
  {
    casa_array_to_list ()
    {
      boost::python::to_python_converter < casacore::Array < T >, 
	                         to_list < casacore::Array < T > >  > ();
    }
  };
  template < typename T >
  struct casa_vector_to_list 
  {
    casa_vector_to_list ()
    {
      boost::python::to_python_converter < casacore::Vector < T >, 
	                         to_list < casacore::Vector < T > >  > ();
    }
  };
  struct casa_iposition_to_list 
  {
    casa_iposition_to_list ()
    {
      boost::python::to_python_converter < casacore::IPosition, 
	                         to_list < casacore::IPosition >  > ();
    }
  };


  // Conversion of Python sequence to C++ container.

  // Copied from
  // scitbx/include/scitbx/boost_python/container_conversions.h that is
  // described in the <a
  // href="http://www.boost.org/libs/python/doc/v2/faq.html">
  // Boost.Python FAQ. </a>
  // @author Ralf W. Grosse-Kunstleve <rwgk@yahoo.com> of
  // <a href="http://www.lbl.gov/">Lawrence Berkeley National Laboratory</a>
  template <typename ContainerType, typename ConversionPolicy>
  struct from_python_sequence
  {
    typedef typename ContainerType::value_type container_element_type;

    from_python_sequence()
    {
      boost::python::converter::registry::push_back(
        &convertible,
        &construct,
        boost::python::type_id<ContainerType>());
    }

    // Appears to return @a obj_ptr if it is type of Python sequence
    // that can be convertible to C++ container.
    static void* convertible(PyObject* obj_ptr)
    {
      using namespace boost::python;
      handle<> py_hdl(obj_ptr);
      if (PyErr_Occurred()) {
	PyErr_Clear();
	return 0;
      }
      object py_obj(py_hdl);
      incref(obj_ptr);        // incr refcount, because ~object decrements it
      // Accept single values.
      if (PyBool_Check(obj_ptr)
	  || PyInt_Check(obj_ptr)
	  || PyLong_Check(obj_ptr)
	  || PyFloat_Check(obj_ptr)
	  || PyComplex_Check(obj_ptr)
	  || PyString_Check(obj_ptr)) {
	extract<container_element_type> elem_proxy(py_obj);
	if (!elem_proxy.check()) return 0;
	return obj_ptr;
      }
      // An array scalar is accepted.
      if (PycArrayScalarCheck(obj_ptr)) {
	return obj_ptr;
      }
      // Get the sequence object.
      // It can be a numarray/numpy scalar in which case
      // it fills py_obj with a flattened array.
      if (! getSeqObject (py_obj)) {
	return 0;
      }
      // Check the sequence.
      // It must be convertible to an iterator.
      handle<> obj_iter(allow_null(PyObject_GetIter(py_obj.ptr())));
      if (!obj_iter.get()) {
	PyErr_Clear();
	return 0;
      }
      if (!check_convertibility (py_obj.ptr())) {
	return 0;
      }
      return obj_ptr;
    }

    // Constructs a C++ container from a Python sequence.
    static void construct(
      PyObject* obj_ptr,
      boost::python::converter::rvalue_from_python_stage1_data* data)
    {
      using namespace boost::python;
      using boost::python::converter::rvalue_from_python_storage;
      void* storage = (
        (rvalue_from_python_storage<ContainerType>*)
          data)->storage.bytes;
      new (storage) ContainerType();
      data->convertible = storage;
      ContainerType& result = *((ContainerType*)storage);
      if (PyBool_Check(obj_ptr)
	  || PyInt_Check(obj_ptr)
	  || PyLong_Check(obj_ptr)
	  || PyFloat_Check(obj_ptr)
	  || PyComplex_Check(obj_ptr)
	  || PyString_Check(obj_ptr)
	  || PycArrayScalarCheck(obj_ptr)) {
	extract<container_element_type> elem_proxy(obj_ptr);
	ConversionPolicy::reserve(result, 1);
        ConversionPolicy::set_value(result, 0, elem_proxy());
	return;
      }
      handle<> py_hdl(obj_ptr);
      object py_obj = object(py_hdl);
      incref(obj_ptr);      // incr refcount, because ~object decrements it
      assert (getSeqObject (py_obj));
      fill_container (result, py_obj.ptr());
      //	ConversionPolicy::reserve(result, 1);
      //	ConversionPolicy::set_value(result, 0, 
      //	    extract<container_element_type>(py_flat.attr("__getitem__")(0)));
    }

    // Constructs a C++ container from a Python sequence.
    static ContainerType make_container(PyObject* obj_ptr)
    {
      ContainerType result;
      fill_container (result, obj_ptr);
      return result;
    }

  private:
    static void fill_container (ContainerType& result, PyObject* obj_ptr)
    {
      using namespace boost::python;
      int obj_size = PyObject_Length(obj_ptr);
      handle<> obj_iter(PyObject_GetIter(obj_ptr));
      ConversionPolicy::reserve(result, obj_size);
      std::size_t i=0;
      for(;;i++) {
        handle<> py_elem_hdl(allow_null(PyIter_Next(obj_iter.get())));
        if (PyErr_Occurred()) throw_error_already_set();
        if (!py_elem_hdl.get()) break; // end of iteration
        object py_elem_obj(py_elem_hdl);
        extract<container_element_type> elem_proxy(py_elem_obj);
        ConversionPolicy::set_value(result, i, elem_proxy());
      }
      ConversionPolicy::assert_size(boost::type<ContainerType>(), i);
    }

    static bool check_convertibility(PyObject* obj_ptr)
    {
      using namespace boost::python;
      handle<> obj_iter(allow_null(PyObject_GetIter(obj_ptr)));
      if (!obj_iter.get()) {       // must be convertible to an iterator
        PyErr_Clear();
        return false;
      }
      int obj_size = PyObject_Length(obj_ptr);
      if (obj_size < 0) {        // must be a measurable sequence
	PyErr_Clear();
	return false;
      }
      if (ConversionPolicy::check_convertibility_per_element()) {
        if (!ConversionPolicy::check_size(
          boost::type<ContainerType>(), obj_size)) return false;
	// All elements in a range and array have the same type, so
	// need to check the first element only.
        bool is_same = PyRange_Check(obj_ptr) ||
	              (PySequence_Check(obj_ptr)
		       && !PyTuple_Check(obj_ptr) && !PyList_Check(obj_ptr));
	int i = 0;
        for (;;i++) {
          handle<> py_elem_hdl(allow_null(PyIter_Next(obj_iter.get())));
          if (PyErr_Occurred()) {
            PyErr_Clear();
            return false;
          }
          if (!py_elem_hdl.get()) break;         // end of iteration
          object py_elem_obj(py_elem_hdl);
          extract<container_element_type> elem_proxy(py_elem_obj);
          if (!elem_proxy.check()) return false;
          if (is_same) break; // all elements are of the same type
        }
        if (!is_same) assert(i == obj_size );
      }
      return true;
    }
  };

  // Register the String conversion.
  struct convert_casa_string
  {
    static void reg();
  };
  inline void register_convert_casa_string()
    { convert_casa_string::reg(); }

  // Register the IPosition conversion.
  struct convert_casa_iposition
  {
    static void reg();
  };
  inline void register_convert_casa_iposition()
    { convert_casa_iposition::reg(); }

  // Register the std::vector conversions.
  template < typename T >
  struct convert_std_vector
  {
    static void reg()
    {
      std::string tname(typeid(std::vector<T>).name());
      if (! pyregistry::get (tname)) {
	pyregistry::set (tname);
	std_vector_to_list < T > ();
	from_python_sequence < std::vector < T >,
	                       stl_variable_capacity_policy > ();
      }
    }
  };
  template < typename T >
  inline void register_convert_std_vector()
    { convert_std_vector<T>::reg(); }

  // Register the casacore::Vector conversions.
  template < typename T >
  struct convert_casa_vector
  {
    static void reg()
    {
      std::string tname(typeid(casacore::Vector<T>).name());
      if (! pyregistry::get (tname)) {
	pyregistry::set (tname);
	casa_array_to_list < T > ();
	casa_vector_to_list < T > ();
	from_python_sequence < casacore::Vector < T >,
	                       casa_variable_capacity_policy > ();
      }
    }
  };
  template < typename T >
  inline void register_convert_casa_vector()
    { convert_casa_vector<T>::reg(); }


  // Register all standard basic conversions.
  // These are String, IPosition, Vector<String>, Vector<Int>,
  // Vector<Double>, Vector<DComplex>.
  void register_convert_basicdata();
}}

#endif
