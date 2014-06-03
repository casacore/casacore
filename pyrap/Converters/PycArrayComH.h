//# PycArrayComH.h: Common code to convert an Array to/from a Python array
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
//# $Id: PycArrayComH.h,v 1.2 2006/11/07 00:17:23 gvandiep Exp $

#ifndef PYC_USE_PYARRAY

  inline Bool PycArrayCheck (PyObject*)
    { return False; }
  inline Bool isImported()
    { return False; }
  inline Bool canImport()
    { return False; }
  inline void loadAPI()
    {}
  inline ValueHolder makeArray (PyObject*, Bool)
    { return ValueHolder(); }
  template <typename T>
  inline boost::python::object makePyArrayObject (casa::Array<T> const&)
    { return boost::python::object(); }

#else

  // Check if the PyObject is an array object.
  Bool PycArrayCheck (PyObject* obj_ptr);

  // Check if the API is or can be imported.
  // <group>
  Bool isImported();
  inline Bool canImport()
    { return True; }
  Bool importArray();
  void loadAPI();
  // </group>

  // Convert the python array to an AIPS++ array in the ValueHolder.
  // If copyData is True, the array data is always copied.
  // Otherwise only if needed.
  ValueHolder makeArray (PyObject* obj_ptr, Bool copyData);

  // Copy/convert the array data as needed.
  // Specializations are defined for complex and string.
  // <group>
  template <typename T> struct ArrayCopy
  {
    static void toPy (void* to, const T* from, uInt nr);
    static void fromPy (T* to, const void* from, uInt nr);
    static Array<T> toArray (const IPosition& shape,
			     void* data, bool copy);
  };

  template <> struct ArrayCopy<Complex>
  {
    static void toPy (void* to, const Complex* from, uInt nr);
    static void fromPy (Complex* to, const void* from, uInt nr);
    static Array<Complex> toArray (const IPosition& shape,
				   void* data, bool copy);
  };

  template <> struct ArrayCopy<DComplex>
  {
    static void toPy (void* to, const DComplex* from, uInt nr);
    static void fromPy (DComplex* to, const void* from, uInt nr);
    static Array<DComplex> toArray (const IPosition& shape,
				    void* data, bool copy);
  };

  template <> struct ArrayCopy<String>
  {
    static void toPy (void* to, const String* from, uInt nr);
    static void fromPy (String* to, const void* from, uInt nr);
    static Array<String> toArray (const IPosition& shape,
				  void* data, bool copy);
  };
  // </group>

  Array<String> ArrayCopyStr_toArray (const IPosition& shape,
				      void* data, uInt slen);

  // Convert an AIPS++ array to a Python array object.
  template <typename T>
  boost::python::object makePyArrayObject (casa::Array<T> const& arr);

#endif
