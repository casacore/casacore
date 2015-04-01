//# FunctionOrder.h: Container of function description details
//# Copyright (C) 2002,2003
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
//# $Id$

#ifndef SCIMATH_FUNCTIONORDER_H
#define SCIMATH_FUNCTIONORDER_H

//# Include files
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/RecordTransformable.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class RecordInterface;

// <summary> Container of function description details
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25"tFunctionOrder"
//	 demos="">
// </reviewed>
//
// <synopsis>
// <src>FunctionOrder</src> is used to provide an interface to an entity which
// has special fixed parameters (like dimension of Gaussian; oder of
// Polynomial). 
// This is useful, for example, in implementinggeneric function factories.
// 
// </synopsis>
//
// <example>
// See the <linkto class=FunctionHolder>FunctionHolder</linkto>
// class for a usage interface.
// </example>
//
// <motivation>
// Generically manipulatable parameters are important for Glish interface
// </motivation>
//
// <templating arg=T>
//  <li> <src>T</src> must have a default constructor, assignment operator,
//	 and copy constructor (for the Vector interface). 
//  <li> Complex/DComplex or Float/Double supported
// </templating>
//
// <todo asof="2002/05/28">
//   <li> Nothing I know of
// </todo>

template<class T> class FunctionOrder : public RecordTransformable {
 public:
  //# Constructors
  // Construct a default FunctionOrder with 0 parameters
  FunctionOrder();
  // Copy constructor (deep copy)
  FunctionOrder(const FunctionOrder<T> &other);
  // Destructor
  virtual ~FunctionOrder();

  //# Operators
  // Copy assignment (deep copy)
  FunctionOrder &operator=(const FunctionOrder<T> &other);

  //# Member functions
  // Get and set the various parameters (no check for index range).
  // Automatic extension for write.
  // <group>
  Int &getInt(const uInt n);
  const Int &getInt(const uInt n) const;
  T &getPar(const uInt n);
  const T &getPar(const uInt n) const;
  String &getString();
  const String &getString() const;
  T &getScale(const uInt n);
  const T &getScale(const uInt n) const;
  T &getCenter(const uInt n);
  const T &getCenter(const uInt n) const;
  T &getWidth(const uInt n);
  const T &getWidth(const uInt n) const;
  const Function<T> &getFunction(const uInt n) const;
  void setFunction(const uInt n, Function<T> &other);
  // </group>

  // Create a FunctionOrder from a record
  // Error messages are postfixed to error.
  // <group>
  virtual Bool fromRecord(String &error, const RecordInterface &in);
  virtual Bool fromString(String &error, const String &in);
  // </group>
  // Create a record from a FunctionOrder.
  // Error messages are postfixed to error.
  virtual Bool toRecord(String &error, RecordInterface &out) const;
  // Get identification of record
  virtual const String &ident() const;

  // Output the parameters
  ostream &print(ostream &os) const;

 private:
  //# Data
  // All data vectors can be empty
  // <group>
  // Integer details (order etc)
  Vector<Int> int_p;
  // Double parameters
  Vector<T> double_p;
  // String parameters
  String string_p;
  // List of functions (say for Combi and Compound)
  PtrBlock<Function<T> *> function_p;
  // Scale of y (length 1)
  Vector<T> scale_p;
  // Centers of x (length ndim)
  Vector<T> center_p;
  // Width of x (ndim)
  Vector<T> width_p;
  // </group>

};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
template<class T>
ostream &operator<<(ostream &os, const FunctionOrder<T> &par);
// </group>

//# Inlines
template<class T>
inline ostream &operator<<(ostream &os, const FunctionOrder<T> &par) {
  return par.print(os); }


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/FunctionOrder.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif

