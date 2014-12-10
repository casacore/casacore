//# MeasureHolder.h: A holder for Measures to enable record conversions
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
//#
//# $Id$

#ifndef MEASURES_MEASUREHOLDER_H
#define MEASURES_MEASUREHOLDER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/casa/Utilities/RecordTransformable.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Measure;
class MDirection;
class MDoppler;
class MEpoch;
class MFrequency;
class MPosition;
class MRadialVelocity;
class Muvw;
class MBaseline;
class MEarthMagnetic;
class MeasValue;

// <summary> A holder for Measures to enable record conversions </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasureHolder" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=RecordInterface>RecordInterface</linkto> class
//   <li> <linkto class=Measure>Measure</linkto> class
// </prerequisite>
//
// <etymology>
// A Holder of general Measures
// </etymology>
//
// <synopsis>
// This class can be used to handle heterogeneous collections of Measures, e.g.
// as a <src>Vector<MeasureHolder></src>. With the aid of the
// toRecord() and fromRecord() functions it can be used
// to convert a Measure object into or from a record.
// A MeasureHolder is created from a Measure, or can be empty.
//
// Checks on the contents can be made with functions like
// <src>isMDirection</src> and the contents can be obtained with
// functions like <src>asMDirection</src>. It is an error to try and
// retrieve a measure of the wrong type and doing so will generate an
// exception (AipsError).
//
// The MeasureHolder can, in addition to the Measure it is holding, also hold
// a block of MeasValues. This is especially useful for intertask
// communication (e.g. with Glish), for reasons of speed. In general the
// additional values will be created when the record used to create
// a Holder contains a <src>Quantity<Vector></src> rather than a quantity in
// the m0, m1 and/or m2 fields. The <src>getMV()</src> method can be used to
// access the <src>nelements()</src> additional information. They can be
// (re-)set with the <src>setMV()</src> method (after a possible creation
// of the extra block if not already there, or of the wrong length, 
// with <src>makeMV()</src>. If any value is set they will be used in
// creating records, with the first value always overwriting the actual
// Measure value.
//
// </synopsis>
//
// <example>
// <srcblock>
//	TableRecord rec;
//	MDirection dir(MVDirection(Quantity(12.5, 'deg'), Quantity(-2, 'deg')),
//		       MDirection::J2000);
//	String error;		// error message
//	if (!MeasureHolder(dir).toRecord(error, rec)) {
//		cout << error << endl;
//	}
//	Record grec;		// a Record
//	if (!MeasureHolder(dir).toRecord(error, grec)) {  // make record
//		cout << error << endl;
//	}
// // Note that for GlishRecords use can be made of the
// // GlishRecord::to/fromrecord() methods.
// </srcblock>
// </example>
//
// <motivation>
// To make general conversions between Measures and records, without knowing
// the actual Measure being converted.
// </motivation>

class MeasureHolder : public RecordTransformable {

public:

//# Friends

//# Enumerations

//# Constructors
  // Creates an empty holder
  MeasureHolder();
  // Create from a Measure (copy made)
  MeasureHolder(const Measure &in);
  // Copy a holder (copy semantics)
  MeasureHolder(const MeasureHolder &other);
//# Destructor
  ~MeasureHolder();

//# Operators
  // Assignment (copy semantics)
  MeasureHolder &operator=(const MeasureHolder &other);

//# Member Functions
  // Check the the MeasureHolder holds the specified Measure type. Return
  // True if if does and False otherwise.
  // <group>
  Bool isEmpty() const;
  Bool isMeasure() const;
  Bool isMDirection() const;
  Bool isMDoppler() const;
  Bool isMEpoch() const;
  Bool isMFrequency() const;
  Bool isMPosition() const;
  Bool isMRadialVelocity() const;
  Bool isMBaseline() const;
  Bool isMuvw() const;
  Bool isMEarthMagnetic() const;
  // </group>

  // Get a specific Measure from the holder (with lifetime as long 
  // as holder exists).
  // <thrown>
  // <li> AipsError if holder empty
  // <li> AipsError if holder contains wrong Measure
  // </thrown>
  // <group>
  const Measure &asMeasure() const;
  const MDirection &asMDirection() const;
  const MDoppler &asMDoppler() const;
  const MEpoch &asMEpoch() const;
  const MFrequency &asMFrequency() const;
  const MPosition &asMPosition() const;
  const MRadialVelocity &asMRadialVelocity() const;
  const MBaseline &asMBaseline() const;
  const Muvw &asMuvw() const;
  const MEarthMagnetic &asMEarthMagnetic() const;
  // </group>
  // Create a Measure from a record. An error message is generated, and False
  // returned if an invalid record is given. A valid record will return True.
  // A valid record contains the following fields (any additional fields are
  // ignored):
  // <ul>
  // <li> type = TpString: type of Measure (direction, epoch, etc; case
  //	 insensitive)
  // <li> refer = TpString: reference type of Measure (case insensitive; 
  //	  enough characters to be unique (e.g. J20, j200, utc, b1950, J2000);
  //	  unknown reference type will log an error message and translate into
  //	  the default type for the Measure.
  // <li> m0, m1, ... = TpRecord(Quantity): one or more Quantities giving
  //	  the value(s) for this Measure (e.g. longitude and latitude for a
  //	  direction). Each quantity can either be a scalar quantity or a
  //	  Quantum<Vector<Double> >.
  // <li> offset = TpRecord(Measure)--optional: an optional offset as a
  //	  Measure of the same type as the main Measure (e.g. an MEpoch for an
  //	   MEpoch)
  // </ul>
  // A Measure can be created from a string. In that case the string
  // will only indicate the type of measure (like direction), and will
  // create a default measure of that given type. In essence identical
  // to the fromType() method. 
  // Error messages are postfixed to error.
  // <group>
  virtual Bool fromRecord(String &error, const RecordInterface &in);
  virtual Bool fromString(String &error, const String &in);
  // </group>
  // Create a record from a Measure. The return will be False and an error
  // message generated only if the MeasureHolder does not contain a Measure.
  // Error messages are postfixed to error.
  virtual Bool toRecord(String &error, RecordInterface &out) const;

  // This version  throws an exception if the conversion cannot
  // occur. It is meant for more allow more compact calling code for callers
  // that are content with just letting the exception proceed up the call stack
  // so they do not have to check a return status. This is, among other things, what
  // exceptions are for after all.
  virtual void toRecord(RecordInterface& outRecord) const;


  // Create a default Measure or a record with only a type from a Measure
  // <group>
  Bool toType(String &error, RecordInterface &out) const;
  Bool fromType(String &error, const RecordInterface &in);
  // </group>
  // Get identification of record
  virtual const String &ident() const;
  // Do we write MeasValues to record?
  Bool writeMV() const { return convertmv_p; }
  // Make a block of n MeasValues
  void makeMV(uInt n) { createMV(n); }
  // Get number of MeasValue pointers in block
  uInt nelements() const { return mvhold_p.nelements(); }
  // Set a measvalue at position pos (False if illegal pos)
  Bool setMV(uInt pos, const MeasValue &in);
  // Get a pointer to a MeasValue (or 0)
  MeasValue *getMV(uInt pos) const;

private:
  
  //# Data Members
  // Pointer to a Measure
  PtrHolder<Measure> hold_p;
  // Block of pointers to measure values to make a faster interface
  Block<MeasValue *> mvhold_p;
  // Should the mvhold_p be converted into record?
  Bool convertmv_p;
  //# Member functions
  // Aid for to/from Record, String and Type
  // <group>
  Bool putType(String &error, RecordInterface &out) const;
  Bool getType(String &error, const RecordInterface &in);  
  Bool getType(String &error, const String &in);  
  // </group>
  // Make a MeasValue block of pointers of length n
  void createMV(uInt n);
};


} //# NAMESPACE CASACORE - END

#endif
