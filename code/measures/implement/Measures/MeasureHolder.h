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
#include <trial/Utilities/RecordTransformable.h>

//# Forward Declarations
class Measure;
class MDirection;
class MDoppler;
class MEpoch;
class MFrequency;
class MPosition;
class MRadialVelocity;
class GlishRecord;

// <summary>
// A holder for Measures to enable record conversions
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=RecordInterface>RecordInterface</linkto> class
//   <li> <linkto class=GlishRecord>GlishRecord</linkto> class
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
// torecord() and fromrecord() functions it can be used
// to convert a Measure object into or from a record.
// A MeasureHolder is created from a Measure, or can be empty.
//
// Checks on the contents can be made with with isMDirection() and other
// functions. The contents can be obtained with operator() (no test on
// existence), and with specific asMDirection() etc methods.
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
// </srcblock>
// </example>
//
// <motivation>
// To make general conversions between Measures and records, without knowing
// the actual Measure being converted.
// </motivation>
//
// <todo asof="1998/04/14">
//   <li> use dynamic_cast once turned on to improve speed
//   <li> possible change if GlishRecord derived from RecordInterface
// </todo>

class MeasureHolder : public RecordTransformable {

public:

//# Friends

//# Enumerations

//# Constructors
  // Creates an empty holder
  MeasureHolder();
  // Create from a Measure
  MeasureHolder(const Measure &in);
  // Copy a holder
  MeasureHolder(const MeasureHolder &other);
//# Destructor
  ~MeasureHolder();

//# Operators
  // Assignment (copy semantics)
  MeasureHolder &operator=(const MeasureHolder &other);
  // Get value (will have lifetime only as long as MeasHolder exists)
  const Measure &operator()() const;

//# Member Functions
  // Check if it holds a specific Measure
  // <group>
  Bool isEmpty() const;
  Bool isMeasure() const;
  Bool isMDirection() const;
  Bool isMDoppler() const;
  Bool isMEpoch() const;
  Bool isMFrequency() const;
  Bool isMPosition() const;
  Bool isMRadialVelocity() const;
  // </group>

  // Get a specific Measure from the holder (with lifetime as long 
  // as holder exists).
  // <thrown>
  // <li> AipsError if holder empty
  // <li> AipsError if holder contains wrong Measure
  // </thrown>
  const Measure &asMeasure() const;
  const MDirection &asMDirection();
  const MDoppler &asMDoppler();
  const MEpoch &asMEpoch();
  const MFrequency &asMFrequency();
  const MPosition &asMPosition();
  const MRadialVelocity &asMRadialVelocity();
  // </group>
  // Create a Measure from a record
  // <group>
  virtual Bool fromRecord(String &error,
			  const RecordInterface &in);
  Bool fromRecord(String &error,
		  const GlishRecord &in);
  // </group>
  // Create a record from a Measure
  // <group>
  virtual Bool toRecord(String &error, RecordInterface &out) const;
  Bool toRecord(String &error, GlishRecord &out) const;
  // </group>
private:

//# Data Members
  // Pointer to a Measure
  PtrHolder<Measure> hold;
  // The following list can disappear once dynamic_cast available
  PtrHolder<MDirection> hdir;
  PtrHolder<MDoppler> hdop;
  PtrHolder<MEpoch> hepo;
  PtrHolder<MFrequency> hfrq;
  PtrHolder<MPosition> hpos;
  PtrHolder<MRadialVelocity> hrad;

};

#endif
