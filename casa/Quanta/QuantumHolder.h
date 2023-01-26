//# QuantumHolder.h: A holder for Quantities to enable record conversions
//# Copyright (C) 1998,1999,2000,2003
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

#ifndef CASA_QUANTUMHOLDER_H
#define CASA_QUANTUMHOLDER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/casa/Utilities/RecordTransformable.h>
#include <casacore/casa/BasicSL/Complexfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class QBase;
class String;
class RecordInterface;
class Record;
template <class Qtype> class Quantum;

// <summary> A holder for Quantums to enable record conversions </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tQuantumHolder" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=RecordInterface>RecordInterface</linkto> class
//   <li> <linkto class=Quantum>Quantity</linkto> class
// </prerequisite>
//
// <etymology>
// A Holder of general Quantums
// </etymology>
//
// <synopsis>
// This class can be used to handle a heterogeneous list of Quantums, and
// can handle toRecord() and fromRecord() conversions.
// A QuantumHolder
// is created empty, from a Quantum (e.g. a <src>Quantum<double></src>) or a
// <src>Quantum<Vector<float> ></src>).
//
// The accepted range of Quantums is:
// <ul>
//  <li> <src>Quantum<int32_t>, Quantum<float>, Quantum<double> == Quantity</src>
//  <li> <src>Quantum<Complex>, Quantum<DComplex></src>
//  <li> <src>Quantum<Vector<int32_t> >, Quantum<Vector<float> ></src>, 
//	 <src>Quantum<Vector<double> ></src>
//  <li> <src>Quantum<Vector<Complex> >, Quantum<Vector<DComplex> ></src>
//  <li> <src>Quantum<Array<int32_t> >, Quantum<Array<float> ></src>, 
//	 <src>Quantum<Array<double> ></src>
//  <li> <src>Quantum<Array<Complex> >, Quantum<Array<DComplex> ></src>
// </ul>
// Scalars in the same group can be converted to any in the same group (e.g.
// int32_t to double); Vectors of length 1 can be converted to scalars in the 
// corresponding group; Scalars can always be converted to Vectors in the 
// corresponding group. Real scalar values can be converted to Complex values.
// Vectors cannot be converted to other type vectors.
//
// Checks on the contents can be made with functions like
// <src>isQuantity</src> and the contents can be obtained with
// functions like <src>asQuantity</src>. It is an error to try and
// retrieve a Quantum of the wrong type and doing so will generate an
// exception (AipsError).
// </synopsis>
//
// <example>
// <srcblock>
//	TableRecord rec;		// an empty record
//	Quantity x(12.5, "km/s");	// a Quantity
//	String error;			// an error message
//	if (!QuantumHolder(x).toRecord(error, rec)) {  // make record
//		cout << error << endl;
//	};
//	Record grec;		        // a Record
//	if (!QuantumHolder(x).toRecord(error, grec)) {  // make record
//		cout << error << endl;
//	};
// // Note that for GlishRecords use can be made of the
// // GlishRecord::to/fromrecord() methods.
// </srcblock>
// </example>
//
// <motivation>
// To make general conversions between Quantums and records, without knowing
// the actual Quantum being converted.
// </motivation>

class QuantumHolder : public RecordTransformable {

public:

//# Friends

//# Enumerations

//# Constructors
  // Creates an empty holder
  QuantumHolder();
  // Create from a Quantum (copy semantics)
  QuantumHolder(const QBase &in);
  // Copy a holder (copy semantics)
  QuantumHolder(const QuantumHolder &other);
//# Destructor
  ~QuantumHolder();

//# Operators
  // Assignment (copy semantics)
  QuantumHolder &operator=(const QuantumHolder &other);

//# Member Functions
  // Check if it holds a Quantity. Note that a Vector of length 1 will give
  // true to scalar questions.
  // <group>
  bool isEmpty() const;
  bool isQuantum() const;
  bool isScalar() const;
  bool isVector() const;
  bool isArray() const;
  bool isReal() const;
  bool isComplex() const;
  bool isQuantity() const;
  bool isQuantumDouble() const;
  bool isQuantumFloat() const;
  bool isQuantumInt() const;
  bool isQuantumComplex() const;
  bool isQuantumDComplex() const;
  bool isQuantumVectorDouble() const;
  bool isQuantumVectorFloat() const;
  bool isQuantumVectorInt() const;
  bool isQuantumVectorComplex() const;
  bool isQuantumVectorDComplex() const;
  bool isQuantumArrayDouble() const;
  bool isQuantumArrayFloat() const;
  bool isQuantumArrayInt() const;
  bool isQuantumArrayComplex() const;
  bool isQuantumArrayDComplex() const;
  // </group>
  // Get number of numeric elements (1 if scalar, else
  // vector length) or dimensions (0 if scalar)
  // <thrown>
  //  <li> AipsError if holder empty
  // </thrown>
  // <group>
  int32_t nelements() const;
  int32_t ndim() const;
  // </group>

  // Get a Quantum from the holder (with lifetime as long 
  // as holder exists). Conversions done if necessary and as described in
  // introduction.
  // <thrown>
  // <li> AipsError if holder empty or no conversion possible
  // </thrown>
  // <group>
  const QBase &asQuantum() const;
  const Quantum<double> &asQuantity() ;
  const Quantum<double> &asQuantumDouble() ;
  const Quantum<float> &asQuantumFloat() ;
  const Quantum<int32_t> &asQuantumInt() ;
  const Quantum<Complex> &asQuantumComplex() ;
  const Quantum<DComplex> &asQuantumDComplex() ;
  const Quantum<Vector<double> > &asQuantumVectorDouble() ;
  const Quantum<Vector<float> > &asQuantumVectorFloat() ;
  const Quantum<Vector<int32_t> > &asQuantumVectorInt() ;
  const Quantum<Vector<Complex> > &asQuantumVectorComplex() ;
  const Quantum<Vector<DComplex> > &asQuantumVectorDComplex() ;
  const Quantum<Array<double> > &asQuantumArrayDouble() ;
  const Quantum<Array<float> > &asQuantumArrayFloat() ;
  const Quantum<Array<int32_t> > &asQuantumArrayInt() ;
  const Quantum<Array<Complex> > &asQuantumArrayComplex() ;
  const Quantum<Array<DComplex> > &asQuantumArrayDComplex() ;
  // </group>

  // Create a Quantum from a record or a string.
  // A valid record will contain the following fields:
  // <ul>
  //  <li> value: contains a numeric value of int32_t, float, double, Complex,
  //		DComplex or a vector thereof
  //  <li> unit: a string with a valid unit string.
  // </ul>
  // A valid string will be one of the special time/angle formats or a
  // value with a valid unit string.
  // Illegal values or units will return false and write an error message.
  // <group>
  virtual bool fromRecord(String &error, const RecordInterface &in);
  virtual bool fromString(String &error, const String &in);
  // </group>
  // Create a record from a Quantum. A false return and an error message is
  // only generated if there is no valid Quantum in the holder.
  virtual bool toRecord(String &error, RecordInterface &out) const;
  // this version throws an exception rather than returning false
  virtual void toRecord(RecordInterface &out) const;
  // this version throws an exception or returns the result Record.
  virtual Record toRecord() const;


  // Return identification
  virtual const String &ident() const;

private:

//# Data Members
  // Pointer to a Quantity
  PtrHolder<QBase> hold_p;

//# General member functions
  // Convert to a different real scalar quantum
  void toReal(const uint32_t &tp);
  // Convert to a different complex scalar quantum
  void toComplex(const uint32_t &tp);
  // Convert scalar to Vector
  void toVector();
  // Convert scalar to Array
  void toArray();
};


} //# NAMESPACE CASACORE - END

#endif
