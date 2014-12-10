//# FunctionHolder.h: A holder for Functions to enable record conversions
//# Copyright (C) 2002,2003,2004
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

#ifndef SCIMATH_FUNCTIONHOLDER_H
#define SCIMATH_FUNCTIONHOLDER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/casa/Utilities/RecordTransformable.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary> A holder for Functions to enable record conversions </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tFunctionHolder" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=RecordInterface>RecordInterface</linkto> class
//   <li> <linkto class=Function>Function</linkto> class
// </prerequisite>
//
// <etymology>
// A Holder of general Measures
// </etymology>
//
// <synopsis>
// This class can be used to handle heterogeneous collections of Functions,
// e.g. as a <src>Vector<FunctionHolder></src>. With the aid of the
// toRecord() and fromRecord() functions it can be used
// to convert a Function object into or from a record.
// A FunctionHolder is created from a Function, or can be empty.
//
// </synopsis>
//
// <example>
// <srcblock>
//	TableRecord rec;
//	MDirection dir(MVDirection(Quantity(12.5, 'deg'), Quantity(-2, 'deg')),
//		       MDirection::J2000);
//	String error;		// error message
//	if (!FunctionHolder(dir).toRecord(error, rec)) {
//		cout << error << endl;
//	}
//	Record grec;		// a Record
//	if (!FunctionHolder(dir).toRecord(error, grec)) {  // make record
//		cout << error << endl;
//	}
// // Note that for GlishRecords use can be made of the
// // GlishRecord::to/fromrecord() methods.
// </srcblock>
// </example>
//
// <motivation>
// To make general conversions between Functions and records, without knowing
// the actual Function being converted.
// </motivation>

template <class T> class FunctionHolder : public RecordTransformable {
 public:
  //# Enumerations
  // Types of functions
  enum Types {
    GAUSSIAN1D,
    GAUSSIAN2D,
    GAUSSIAN3D,
    GAUSSIANND,
    HYPERPLANE,
    POLYNOMIAL,
    EVENPOLYNOMIAL,
    ODDPOLYNOMIAL,
    SINUSOID1D,
    CHEBYSHEV,
    BUTTERWORTH,
    COMBINE,
    COMPOUND,
    COMPILED,
    N_Types
  };
  //# Structures
  // Structure to hold functional status
  struct FuncStat {
    // Name
    String nam;
    // type
    Types tp;
    // Order (True if needed)
    Bool order;
  };

  //# Constructors
  // Creates an empty holder
  FunctionHolder();
  // Create from a Function (copy made)
  FunctionHolder(const Function<T> &in);
  // Copy a holder (copy semantics)
  FunctionHolder(const FunctionHolder<T> &other);
  //# Destructor
  ~FunctionHolder();

  //# Operators
  // Assignment (copy semantics)
  FunctionHolder &operator=(const FunctionHolder<T> &other);

  //# Member Functions
  // Check the the FunctionHolder holds the specified type. Return
  // True if if does and False otherwise.
  // <group>
  Bool isEmpty() const;
  // </group>
  // Get the known names
  const Vector<String> &names() const;
  // Get a specific Function from the holder (with lifetime as long 
  // as holder exists).
  // <thrown>
  // <li> AipsError if holder empty
  // <li> AipsError if holder contains wrong Function
  // </thrown>
  // <group>
  const Function<T> &asFunction() const;
  // </group>
  // Add a function
  Bool addFunction(const Function<T> &fnc); 
  // Get the type of currently filled holder
  Types type() const;
  // Create a Function from a record. An error message is generated, and False
  // returned if an invalid record is given. A valid record will return True.
  // A valid record contains at least the following fields (any additional fields are
  // ignored):
  // <ul>
  // <li> tp = TpString: type of Function (gaussian1d, etc; case
  //	 insensitive) -- OR an enumeration code
  // <li> order = TpInt: the order needed to create a Function (-1 if not
  // 	necessary or default)
  // <li> ndim, npar, params are optional
  // <li> nfunc, funcs are required for COMBI or COMPOUND
  // </ul>
  // A Function can be created from a string. In that case the string
  // will only indicate the type of function (like polynomial), and will
  // create a default polynomial of that given type. 
  // Error messages are postfixed to error.
  // <group>
  virtual Bool fromRecord(String &error, const RecordInterface &in);
  virtual Bool fromString(String &error, const String &in);
  template <class U>
    Bool getRecord(String &error, Function<U> *&fn,
		   const RecordInterface &in);
  // </group>
  // Create a record from a Function. The return will be False and an error
  // message generated only if the FunctionHolder does not contain a Function.
  // Error messages are postfixed to error.
  virtual Bool toRecord(String &error, RecordInterface &out) const;
  // Get identification of record
  virtual const String &ident() const;

private:
  //# Data Members
  // Pointer to a Function
  PtrHolder<Function<T> > hold_p;
  // Aids (only filled after a succesful to/fromRecord
  // <group>
  mutable Types nf_p;
  mutable Int order_p;
  mutable String text_p;
  mutable PtrHolder<RecordInterface> mode_p;
  // </group>
  // List of known names
  mutable Vector<String> nam_p;
  // Filled list?
  mutable Bool isFilled;

  //# Member functions
  // Initialise and check the name list
  void init() const;
  // Aid for to/from Record, String
  // <group>
  Bool putType(String &error, RecordInterface &out) const;
  template <class U>
    Bool getType(String &error, Function<U> *&fn, const RecordInterface &in);  
  template <class U>
    Bool getType(String &error, Function<U> *&fn);
  void setParameters(Function<T> *&fn, const Vector<T> &params);
  void setParameters(Function<AutoDiff<T> > *&fn, const Vector<T> &params);
  // </group>
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/FunctionHolder.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
