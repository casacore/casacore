//# MeasureHolder.h: A holder for Measures to enable record conversions
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

#if !defined(AIPS_MEASUREHOLDER_H)
#define AIPS_MEASUREHOLDER_H

//# Includes
#include <aips/aips.h>
#include <aips/Utilities/PtrHolder.h>
#include <aips/Utilities/RecordTransformable.h>

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
class GlishRecord;

// <summary> A holder for Measures to enable record conversions </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasureHolder" demos="">
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
//	};
//	GlishRecord grec;		// a GlishRecord
//	if (!MeasureHolder(dir).toRecord(error, grec)) {  // make record
//		cout << error << endl;
//	};
// // Note that for GlishRecords use can be made of the to/fromGlishrecord()
// // methods	
// </srcblock>
// </example>
//
// <motivation>
// To make general conversions between Measures and records, without knowing
// the actual Measure being converted.
// </motivation>
//
// <todo asof="1998/04/14">
//   <li> possible change if GlishRecord derived from RecordInterface
// </todo>

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
  //	  direction.
  // <li> offset = TpRecord(Measure)--optional: an optional offset as a
  //	  Measure of the same type as the main Measure (e.g. an MEpoch for an
  //	   MEpoch)
  // </ul>
  // Error messages are postfixed to error.
  // <group>
  virtual Bool fromRecord(String &error, const RecordInterface &in);
  Bool fromRecord(String &error, const GlishRecord &in);
  // </group>
  // Create a record from a Measure. The return will be False and an error
  // message generated only if the MeasureHolder does not contain a Measure.
  // Error messages are postfixed to error.
  // <group>
  virtual Bool toRecord(String &error, RecordInterface &out) const;
  Bool toRecord(String &error, GlishRecord &out) const;
  // </group>
private:

//# Data Members
  // Pointer to a Measure
  PtrHolder<Measure> hold_p;

};

#endif
