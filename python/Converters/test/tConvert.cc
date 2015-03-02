//# tConvert.cc: Test program for libpython's C++/Python converters
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
//# $Id: tConvert.cc,v 1.4 2006/11/06 00:14:44 gvandiep Exp $

#include <casacore/python/Converters/PycExcp.h>
#include <casacore/python/Converters/PycBasicData.h>
#include <casacore/python/Converters/PycValueHolder.h>
#include <casacore/python/Converters/PycRecord.h>
#include <casacore/python/Converters/PycArray.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/BasicSL/STLIO.h>

#include <boost/python.hpp>

using namespace boost::python;

namespace casacore { namespace python {

  struct TConvert
  {
    TConvert() {}
    Bool  testbool (Bool in)
      {cout << "bool " << in << endl; return in;}
    Int testint (Int in)
      {cout << "Int " << in << endl; return in;}
    Int64 testint64 (Int64 in)
      {cout << "Int64 " << in << endl; return in;}
    Int testssize (::ssize_t in)
      {cout << "ssize " << in << endl; return in;}
    Float testfloat (Float in)
      {cout << "Float " << in << endl; return in;}
    Double testdouble (Double in)
      {cout << "Double " << in << endl; return in;}
    Complex testcomplex (const Complex& in)
      {cout << "Complex " << in << endl; return in;}
    DComplex testdcomplex (const DComplex& in)
      {cout << "DComplex " << in << endl; return in;}
    String teststring (const String& in)
      {cout << "String " << in << endl; String out=in; return out;}
    Record testrecord (const Record& in)
      {cout << "Record "; in.print(cout); cout << endl; return in;}
    ValueHolder testvh (const ValueHolder& in)
      {cout << "VH " << in.dataType() << endl; return in;}
    Vector<Bool> testvecbool (const Vector<Bool>& in)
      {cout << "VecBool " << in << endl; return in;}
    Vector<Int> testvecint (const Vector<int>& in)
      {cout << "VecInt " << in << endl; return in;}
    Vector<DComplex> testveccomplex (const Vector<DComplex>& in)
      {cout << "VecComplex " << in << endl; return in;}
    Vector<String> testvecstr (const Vector<String>& in)
      {cout << "VecStr " << in << endl; return in;}
    std::vector<bool> teststdvecbool (const std::vector<bool>& in)
      {cout << "vecbool " << in << endl; return in;}
    std::vector<uInt> teststdvecuint (const std::vector<uInt>& in)
      {cout << "vecuInt " << in << endl; return in;}
    std::vector<std::vector<uInt> > teststdvecvecuint
    (const std::vector<std::vector<uInt> >& in)
      {cout << "vecvecuInt " << in << endl; return in;}
    std::vector<ValueHolder> teststdvecvh (const std::vector<ValueHolder>& in)
      {cout << "vecvh " << in.size() << endl; return in;}
    IPosition testipos (const IPosition& in)
      {cout << "IPos " << in << endl; return in;}
  };


  void testConvert()
  {
    class_<TConvert> ("tConvert", init<>())
      .def ("testbool",       &TConvert::testbool)
      .def ("testint",        &TConvert::testint)
      .def ("testint64",      &TConvert::testint64)
      .def ("testssize",      &TConvert::testssize)
      .def ("testfloat",      &TConvert::testfloat)
      .def ("testdouble",     &TConvert::testdouble)
      .def ("testcomplex",    &TConvert::testcomplex)
      .def ("testdcomplex",   &TConvert::testdcomplex)
      .def ("teststring",     &TConvert::teststring)
      .def ("testrecord",     &TConvert::testrecord)
      .def ("testvh",         &TConvert::testvh)
      .def ("testvecbool",    &TConvert::testvecbool)
      .def ("testvecint",     &TConvert::testvecint)
      .def ("testveccomplex", &TConvert::testveccomplex)
      .def ("testvecstr",     &TConvert::testvecstr)
      .def ("teststdvecbool", &TConvert::teststdvecbool)
      .def ("teststdvecuint", &TConvert::teststdvecuint)
      .def ("teststdvecvecuint", &TConvert::teststdvecvecuint)
      .def ("teststdvecvh"  , &TConvert::teststdvecvh)
      .def ("testipos",       &TConvert::testipos)
      ;
  }

}}


BOOST_PYTHON_MODULE(_tConvert)
{
  // Register the required converters.
  casacore::python::register_convert_excp();
  casacore::python::register_convert_basicdata();
  casacore::python::register_convert_casa_valueholder();
  casacore::python::register_convert_casa_record();
  casacore::python::register_convert_std_vector<bool>();
  casacore::python::register_convert_std_vector<casacore::uInt>();
  casacore::python::register_convert_std_vector<std::vector<casacore::uInt> >();
  casacore::python::register_convert_std_vector<casacore::ValueHolder>();

  // Execute the test.
  casacore::python::testConvert();
}
