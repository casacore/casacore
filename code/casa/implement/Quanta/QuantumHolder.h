//# QuantumHolder.h: A holder for Quantities to enable record conversions
//# Copyright (C) 1998
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

#if !defined(AIPS_QUANTUMHOLDER_H)
#define AIPS_QUANTUMHOLDER_H

//# Includes
#include <aips/aips.h>
#include <aips/Utilities/PtrHolder.h>
#include <trial/Utilities/RecordTransformable.h>

//# Forward Declarations
template <class Qtype> class Quantum;
class GlishRecord;

// <summary> A holder for Quantities to enable record conversions </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tQuantumHolder" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=RecordInterface>RecordInterface</linkto> class
//   <li> <linkto class=GlishRecord>GlishRecord</linkto> class
//   <li> <linkto class=Quantum>Quantity</linkto> class
// </prerequisite>
//
// <etymology>
// A Holder of general Quantities
// </etymology>
//
// <synopsis>
// This class can be used to handle a heterogeneous list of Quantities, and
// can handle torecord() and fromrecord() conversions.
// A QuantumHolder
// is created empty, from a Quantity (<src>Quantum<Double></src>), a
// <src>Quantum<Float></src> or a <src>Quantum<Int></src>.
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
//	if (!QuantumHolder(x).torecord(error, rec)) {  // make record
//		cout << error << endl;
//	};
// </srcblock>
// </example>
//
// <motivation>
// To make general conversions between Quantities and records, without knowing
// the actual Quantity being converted.
// </motivation>
//
// <todo asof="1998/04/14">
//   <li> possible change if GlishRecord derived from RecordInterface
// </todo>

class QuantumHolder : public RecordTransformable {

public:

//# Friends

//# Enumerations

//# Constructors
  // Creates an empty holder
  QuantumHolder();
  // Create from a Quantity (copy semantics)
  // <group>
  QuantumHolder(const Quantum<Double> &in);
  QuantumHolder(const Quantum<Float> &in);
  QuantumHolder(const Quantum<Int> &in);
  // </group>
  // Copy a holder (copy semantics)
  QuantumHolder(const QuantumHolder &other);
//# Destructor
  ~QuantumHolder();

//# Operators
  // Assignment (copy semantics)
  QuantumHolder &operator=(const QuantumHolder &other);
  // Get value (will have lifetime only as long as MeasHolder exists)
  const Quantum<Double> &operator()() const;

//# Member Functions
  // Check if it holds a Quantity
  // <group>
  Bool isQuantity() const;
  Bool isEmpty() const;
  // </group>

  // Get a Quantity from the holder (with lifetime as long 
  // as holder exists).
  // <thrown>
  // <li> AipsError if holder empty
  // </thrown>
  // <group>
  const Quantum<Double> &asQuantity() const;
  // </group>

  // Create a Quantity from a record
  // <group>
  virtual Bool fromRecord(String &error,
			  const RecordInterface &in);
  Bool fromRecord(String &error,
		  const GlishRecord &in);
  // </group>
  // Create a record from a Quantity
  // <group>
  virtual Bool toRecord(String &error, RecordInterface &out) const;
  Bool toRecord(String &error, GlishRecord &out) const;
  // </group>

private:

//# Data Members
  // Pointer to a Quantity
  PtrHolder<Quantum<Double> > hold;

};

#endif
