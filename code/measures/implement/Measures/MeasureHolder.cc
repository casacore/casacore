//# MeasureHolder.cc: A holder for Measures to enable record conversions
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

//# Includes
#include <trial/Measures/MeasureHolder.h>
#include <trial/Measures/QuantityHolder.h>
#include <aips/Exceptions.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/Record.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/String.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>

//# Constructors
MeasureHolder::MeasureHolder() : hold(),
  hdir(), hdop(), hepo(), hfrq(), hpos(), hrad() {};

MeasureHolder::MeasureHolder(const Measure &in) : hold(in.clone()),
    hdir(), hdop(), hepo(), hfrq(), hpos(), hrad() {}

MeasureHolder::MeasureHolder(const MeasureHolder &other) : hold(),
      hdir(), hdop(), hepo(), hfrq(), hpos(), hrad() {
	if (other.hold.ptr()) hold.set(other.hold.ptr()->clone());
      }

//# Destructor
MeasureHolder::~MeasureHolder() {}

//# Operators
MeasureHolder &MeasureHolder::operator=(const MeasureHolder &other) {
  if (this != &other) hold.set(other.hold.ptr()->clone());
  return *this;
}

const Measure &MeasureHolder::operator()() const {
  return *hold.ptr();
}

//# Member Functions
Bool MeasureHolder::isEmpty() const {
  return ToBool(!hold.ptr());
}

Bool MeasureHolder::isMeasure() const {
  return ToBool(hold.ptr());
}

Bool MeasureHolder::isMDirection() const {
  return ToBool(hold.ptr() && hold.ptr()->type() == MDirection::myType());
}

Bool MeasureHolder::isMDoppler() const {
  return ToBool(hold.ptr() && hold.ptr()->type() == MDoppler::myType());
}

Bool MeasureHolder::isMEpoch() const {
  return ToBool(hold.ptr() && hold.ptr()->type() == MEpoch::myType());
}

Bool MeasureHolder::isMFrequency() const {
  return ToBool(hold.ptr() && hold.ptr()->type() == MFrequency::myType());
}

Bool MeasureHolder::isMPosition() const {
  return ToBool(hold.ptr() && hold.ptr()->type() == MPosition::myType());
}

Bool MeasureHolder::isMRadialVelocity() const {
  return ToBool(hold.ptr() && hold.ptr()->type() == MRadialVelocity::myType());
}

const Measure &MeasureHolder::asMeasure() const {
  if (!hold.ptr()) {
    throw(AipsError("Empty MeasureHolder argument for asMeasure"));
  };
  return *hold.ptr();
}

const MDirection &MeasureHolder::asMDirection() {
  if (!hold.ptr() || !isMDirection()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMDirection"));
  };
  /// return dynamic_cast<MDirection &>(*hold.ptr());
  hdir.set(new MDirection(hold.ptr()));
  return *hdir.ptr();
}

const MDoppler &MeasureHolder::asMDoppler() {
  if (!hold.ptr() || !isMDoppler()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMDoppler"));
  };
  /// return dynamic_cast<MDoppler &>(*hold.ptr());
  hdop.set(new MDoppler(hold.ptr()));
  return *hdop.ptr();
}

const MEpoch &MeasureHolder::asMEpoch() {
  if (!hold.ptr() || !isMEpoch()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMEpoch"));
  };
  /// return dynamic_cast<MEpoch &>(*hold.ptr());
  hepo.set(new MEpoch(hold.ptr()));
  return *hepo.ptr();
}

const MFrequency &MeasureHolder::asMFrequency() {
  if (!hold.ptr() || !isMFrequency()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMFrequency"));
  };
  /// return dynamic_cast<MFrequency &>(*hold.ptr());
  hfrq.set(new MFrequency(hold.ptr()));
  return *hfrq.ptr();
}

const MPosition &MeasureHolder::asMPosition() {
  if (!hold.ptr() || !isMPosition()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMPosition"));
  };
  /// return dynamic_cast<MPosition &>(*hold.ptr());
  hpos.set(new MPosition(hold.ptr()));
  return *hpos.ptr();
}

const MRadialVelocity &MeasureHolder::asMRadialVelocity() {
  if (!hold.ptr() || !isMRadialVelocity()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMRadialVelocity"));
  };
  /// return dynamic_cast<MRadialVelocity &>(*hold.ptr());
  hrad.set(new MRadialVelocity(hold.ptr()));
  return *hrad.ptr();
}

Bool MeasureHolder::fromRecord(String &error,
			       const RecordInterface &in) {
  if (in.isDefined(String("type")) &&
      in.isDefined(String("refer")) &&
      in.type(in.idToNumber(RecordFieldId("type"))) == TpString &&
      in.type(in.idToNumber(RecordFieldId("refer"))) == TpString) {
    String tp;
    in.get(RecordFieldId("type"), tp);
    tp.downcase();
    hold.clear();
    if (tp == downcase(MDirection::showMe())) {
      hold.set(new MDirection());
    } else if (tp == downcase(MDoppler::showMe())) {
      hold.set(new MDoppler());
    } else if (tp == downcase(MEpoch::showMe())) {
      hold.set(new MEpoch());
    } else if (tp == downcase(MFrequency::showMe())) {
      hold.set(new MFrequency());
    } else if (tp == downcase(MPosition::showMe())) {
      hold.set(new MPosition());
    } else if (tp == downcase(MRadialVelocity::showMe())) {
      hold.set(new MRadialVelocity());
    } else {
      error = String("Unknown Measure record in MeasureHolder::fromRecord\n") +
	error;
      return False;
    };
    String rf;
    in.get(RecordFieldId("refer"), rf);
    if (!hold.ptr()->setRefString(rf)) {
      LogIO os(LogOrigin("MeasureHolder", 
			 String("fromRecord(String, const RecordInterface"),
			 WHERE));
      os << LogIO::WARN <<
	String("Illegal or unknown reference type '") +
	rf + "' for " + tp + " definition. DEFAULT (" + 
	hold.ptr()->getDefaultType() + ") assumed." <<
	LogIO::POST;
    };
    if (in.isDefined(String("offset")) &&
	in.type(in.idToNumber(RecordFieldId("offset"))) == TpRecord) {
      MeasureHolder x;
      if (!x.fromRecord(error, in.asRecord(RecordFieldId("offset")))) {
	return False;
      };
      if (!hold.ptr()->setOffset(x.asMeasure())) {
	error = String("Unmatched offset type in MeasureHolder::fromRecord\n") +
	  error;
	return False;
      };
    };
    QuantityHolder q0, q1, q2;
    Int n;
    if (in.isDefined(String("m0")) &&
	in.type(in.idToNumber(RecordFieldId("m0"))) == TpRecord) {
      if (!q0.fromRecord(error, in.asRecord(RecordFieldId("m0")))) {
	return False;
      };
      n = 1;
      if (in.isDefined(String("m1")) &&
	  in.type(in.idToNumber(RecordFieldId("m1"))) == TpRecord) {
	if (!q1.fromRecord(error, in.asRecord(RecordFieldId("m1")))) {
	  return False;
	};
	n = 2;
	if (in.isDefined(String("m2")) &&
	    in.type(in.idToNumber(RecordFieldId("m2"))) == TpRecord) {
	  if (!q2.fromRecord(error, in.asRecord(RecordFieldId("m2")))) {
	    return False;
	  };
	  n = 3;
	};
      };
    };
    Vector<Quantity> vq(n);
    if (n > 0) vq(0) = q0.asQuantity();
    if (n > 1) vq(1) = q1.asQuantity();
    if (n > 2) vq(2) = q2.asQuantity();
    if (!hold.ptr()->putValue(vq)) {
      error = String("Illegal quantity in MeasureHolder::fromRecord\n") +
	error;
      return False;
    };
    return True;
  };
  error = String("Illegal Measure record in MeasureHolder::fromRecord\n") +
    error;
  return False;
}

Bool MeasureHolder::toRecord(String &error, RecordInterface &out) const {
  if (hold.ptr()) {
    out.define(RecordFieldId("type"),
	       downcase(String(hold.ptr()->tellMe())));
    out.define(RecordFieldId("refer"), hold.ptr()->getRefString());
    const Measure *off = hold.ptr()->getRefPtr()->offset();
    if (off) {
      Record offs;
      if (!MeasureHolder(*off).toRecord(error, offs)) return False;
      out.defineRecord(RecordFieldId("offset"), offs);
    };
    Vector<Quantum<Double> > res = hold.ptr()->getData()->getRecordValue();
    Int n = res.nelements();
    Record val;
    Bool ok = True;
    if (n > 2) {
      if (!QuantityHolder(res(2)).toRecord(error, val)) return False;
      out.defineRecord(RecordFieldId("m2"), val);
    };
    if (n > 1) {
      if (!QuantityHolder(res(1)).toRecord(error, val)) return False;
      out.defineRecord(RecordFieldId("m1"), val);
    };
    if (n > 0) {
      if (!QuantityHolder(res(0)).toRecord(error, val)) return False;
      out.defineRecord(RecordFieldId("m0"), val);
    };
    res.resize(0);
    res = hold.ptr()->getData()->getXRecordValue();
    val = Record();
    n = res.nelements();
    if (n > 2) {
      if (!QuantityHolder(res(2)).toRecord(error, val)) return False;
      out.defineRecord(RecordFieldId("ev2"), val);
    };
    if (n > 1) {
      if (!QuantityHolder(res(1)).toRecord(error, val)) return False;
      out.defineRecord(RecordFieldId("ev1"), val);
    };
    if (n > 0) {
      if (!QuantityHolder(res(0)).toRecord(error, val)) return False;
      out.defineRecord(RecordFieldId("ev0"), val);
    };
    return True;
  };
  error = String("No Measure specified in MeasureHolder::toRecord\n") +
    error;
  return False;
}
