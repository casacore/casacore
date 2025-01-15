//# MeasureHolder.cc: A holder for Measures to enable record conversions
//# Copyright (C) 1998,1999,2000,2001
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casacore/measures/Measures/MeasureHolder.h>
#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MeasValue.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/measures/Measures/MBaseline.h>
#include <casacore/measures/Measures/Muvw.h>
#include <casacore/measures/Measures/MEarthMagnetic.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Containers/RecordFieldId.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
MeasureHolder::MeasureHolder() 
  : mvhold_p(0), convertmv_p(False) {
  createMV(0);
}

MeasureHolder::MeasureHolder(const Measure &in) 
  : hold_p(in.clone()), mvhold_p(0), convertmv_p(False) {}

MeasureHolder::MeasureHolder(const MeasureHolder &other) 
  : RecordTransformable(),
    mvhold_p(0), convertmv_p(False)
{
  if (other.hold_p) hold_p.reset(other.hold_p->clone());
  createMV(other.mvhold_p.nelements());
  for (uInt i=0; i<mvhold_p.nelements(); i++) {
    mvhold_p[i] = other.mvhold_p[i]->clone();
  }
}

//# Destructor
MeasureHolder::~MeasureHolder() {
  createMV(0);
}

//# Operators
MeasureHolder &MeasureHolder::operator=(const MeasureHolder &other) {
  if (this != &other) {
    if (other.hold_p) {
      hold_p.reset(other.hold_p->clone());
    } else {
      hold_p.reset();
    }
    createMV(other.mvhold_p.nelements());
    for (uInt i=0; i<mvhold_p.nelements(); i++) {
      mvhold_p[i] = other.mvhold_p[i]->clone();
    }
  }
  return *this;
}

//# Member Functions
Bool MeasureHolder::isEmpty() const {
  return (!hold_p);
}

Bool MeasureHolder::isMeasure() const {
  return static_cast<Bool>(hold_p);
}

Bool MeasureHolder::isMDirection() const {
  return (hold_p && dynamic_cast<const MDirection*>(hold_p.get()));
}

Bool MeasureHolder::isMDoppler() const {
  return (hold_p && dynamic_cast<const MDoppler*>(hold_p.get()));
}

Bool MeasureHolder::isMEpoch() const {
  return (hold_p && dynamic_cast<const MEpoch*>(hold_p.get()));
}

Bool MeasureHolder::isMFrequency() const {
  return (hold_p && dynamic_cast<const MFrequency*>(hold_p.get()));
}

Bool MeasureHolder::isMPosition() const {
  return (hold_p && dynamic_cast<const MPosition*>(hold_p.get()));
}

Bool MeasureHolder::isMRadialVelocity() const {
  return (hold_p && dynamic_cast<const MRadialVelocity*>(hold_p.get()));
}

Bool MeasureHolder::isMBaseline() const {
  return (hold_p && dynamic_cast<const MBaseline*>(hold_p.get()));
}

Bool MeasureHolder::isMuvw() const {
  return (hold_p && dynamic_cast<const Muvw*>(hold_p.get()));
}

Bool MeasureHolder::isMEarthMagnetic() const {
  return (hold_p && dynamic_cast<const MEarthMagnetic*>(hold_p.get()));
}

const Measure &MeasureHolder::asMeasure() const {
  if (!hold_p) {
    throw(AipsError("Empty MeasureHolder argument for asMeasure"));
  }
  return *hold_p;
}

const MDirection &MeasureHolder::asMDirection() const {
  if (!hold_p || !isMDirection()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMDirection"));
  }
  return dynamic_cast<const MDirection &>(*hold_p.get());
}

const MDoppler &MeasureHolder::asMDoppler() const {
  if (!hold_p || !isMDoppler()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMDoppler"));
  }
  return dynamic_cast<const MDoppler &>(*hold_p.get());
}

const MEpoch &MeasureHolder::asMEpoch() const {
  if (!hold_p || !isMEpoch()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMEpoch"));
  }
  return dynamic_cast<const MEpoch &>(*hold_p.get());
}

const MFrequency &MeasureHolder::asMFrequency() const {
  if (!hold_p || !isMFrequency()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMFrequency"));
  }
  return dynamic_cast<const MFrequency &>(*hold_p.get());
}

const MPosition &MeasureHolder::asMPosition() const {
  if (!hold_p || !isMPosition()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMPosition"));
  }
  return dynamic_cast<const MPosition &>(*hold_p.get());
}

const MRadialVelocity &MeasureHolder::asMRadialVelocity() const {
  if (!hold_p || !isMRadialVelocity()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMRadialVelocity"));
  }
  return dynamic_cast<const MRadialVelocity &>(*hold_p.get());
}

const MEarthMagnetic &MeasureHolder::asMEarthMagnetic() const {
  if (!hold_p || !isMEarthMagnetic()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMEarthMagnetic"));
  }
  return dynamic_cast<const MEarthMagnetic &>(*hold_p.get());
}

const MBaseline &MeasureHolder::asMBaseline() const {
  if (!hold_p || !isMBaseline()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMBaseline"));
  }
  return dynamic_cast<const MBaseline &>(*hold_p.get());
}

const Muvw &MeasureHolder::asMuvw() const {
  if (!hold_p || !isMuvw()) {
    throw(AipsError("Empty or wrong MeasureHolder for asMuvw"));
  }
  return dynamic_cast<const Muvw &>(*hold_p.get());
}

Bool MeasureHolder::fromRecord(String &error,
			       const RecordInterface &in) {
  if (in.isDefined(String("type")) &&
      in.isDefined(String("refer")) &&
      in.type(in.idToNumber(RecordFieldId("type"))) == TpString &&
      in.type(in.idToNumber(RecordFieldId("refer"))) == TpString) {
    if (!getType(error, in)) {
      error += String("Unknown Measure record in MeasureHolder::fromRecord\n");
      return False;
    }
    String rf;
    in.get(RecordFieldId("refer"), rf);
    if (!hold_p->setRefString(rf)) {
      if (!rf.empty()) {
	LogIO os(LogOrigin("MeasureHolder", 
			   String("fromRecord(String, const RecordInterface"),
			   WHERE));
	os << LogIO::WARN <<
	  String("Illegal or unknown reference type '") +
	  rf + "' for " + downcase(String(hold_p->tellMe())) +
	  " definition. DEFAULT (" + 
	  hold_p->getDefaultType() + ") assumed." <<
	  LogIO::POST;
      }
    }
    if (in.isDefined(String("offset")) &&
	in.type(in.idToNumber(RecordFieldId("offset"))) == TpRecord) {
      MeasureHolder x;
      if (!x.fromRecord(error, in.asRecord(RecordFieldId("offset")))) {
	return False;
      }
      if (!hold_p->setOffset(x.asMeasure())) {
	error += String("Unmatched offset type in MeasureHolder::fromRecord\n");
	return False;
      }
    }
    QuantumHolder q0, q1, q2;
    uInt n(0);
    if (in.isDefined(String("m0")) &&
	in.type(in.idToNumber(RecordFieldId("m0"))) == TpRecord) {
      if (!q0.fromRecord(error, in.asRecord(RecordFieldId("m0")))) {
	return False;
      }
      n = 1;
      if (in.isDefined(String("m1")) &&
	  in.type(in.idToNumber(RecordFieldId("m1"))) == TpRecord) {
	if (!q1.fromRecord(error, in.asRecord(RecordFieldId("m1")))) {
	  return False;
	}
	n = 2;
	if (in.isDefined(String("m2")) &&
	    in.type(in.idToNumber(RecordFieldId("m2"))) == TpRecord) {
	  if (!q2.fromRecord(error, in.asRecord(RecordFieldId("m2")))) {
	    return False;
	  }
	  n = 3;
	}
      }
    }
    Vector<Quantity> vq(n);
    if (n > 0) vq(0) = Quantity(q0.asQuantumVectorDouble().getValue()(0),
				q0.asQuantumVectorDouble().getFullUnit());
    if (n > 1) vq(1) = Quantity(q1.asQuantumVectorDouble().getValue()(0),
				q1.asQuantumVectorDouble().getFullUnit());
    if (n > 2) vq(2) = Quantity(q2.asQuantumVectorDouble().getValue()(0),
				q2.asQuantumVectorDouble().getFullUnit());
    if (!hold_p->putValue(vq)) {
      error += String("Illegal quantity in MeasureHolder::fromRecord\n");
      return False;
    }
    uInt nel(0);
    if (n>0) nel = q0.asQuantumVectorDouble().getValue().nelements();
    if (n>1 && nel != q1.asQuantumVectorDouble().getValue().nelements()) {
      error += String("Illegal number of values in MeasureHolder m1\n");
      return False;
    }
    if (n>2 && nel != q2.asQuantumVectorDouble().getValue().nelements()) {
      error += String("Illegal number of values in MeasureHolder m2\n");
      return False;
    }
    if (nel>1) {
      makeMV(nel);
      for (uInt i=nel-1; i<nel; i--) {
	if (n > 0) vq(0) = Quantity(q0.asQuantumVectorDouble().getValue()(i),
				    q0.asQuantumVectorDouble().getFullUnit());
	if (n > 1) vq(1) = Quantity(q1.asQuantumVectorDouble().getValue()(i),
				    q1.asQuantumVectorDouble().getFullUnit());
	if (n > 2) vq(2) = Quantity(q2.asQuantumVectorDouble().getValue()(i),
				    q2.asQuantumVectorDouble().getFullUnit());
	if (!hold_p->putValue(vq)) {
	  error += String("Illegal quantity in MeasureHolder value\n");
	  return False;
	}
	if (!setMV(i, *hold_p->getData())) {
	  error += String("Illegal MeasValue in MeasureHolder value\n");
	  return False;
	}
      }
    }
    convertmv_p = False;
    return True;
  }
  error += String("Illegal Measure record in MeasureHolder::fromRecord\n");
  return False;
}

Bool MeasureHolder::fromString(String &error,
			       const String &in) {
  if (!getType(error, in)) {
    error += String("Unknown Measure type in MeasureHolder::fromString\n");
    return False;
  }
  return True;
}

Bool MeasureHolder::toRecord(String &error, RecordInterface &out) const {
  if (hold_p && putType(error, out)) {
    out.define(RecordFieldId("refer"), hold_p->getRefString());
    const Measure *off = hold_p->getRefPtr()->offset();
    if (off) {
      Record offs;
      if (!MeasureHolder(*off).toRecord(error, offs)) return False;
      out.defineRecord(RecordFieldId("offset"), offs);
    }
    // Make sure units available
    Vector<Quantum<Double> > res = hold_p->getData()->getRecordValue();
    uInt n(res.nelements());
    uInt nel(nelements());
    Record val;
    // Single value only
    if (!convertmv_p || nel==0) {
      if (n > 2) {
	if (!QuantumHolder(res(2)).toRecord(error, val)) return False;
	out.defineRecord(RecordFieldId("m2"), val);
      }
      if (n > 1) {
	if (!QuantumHolder(res(1)).toRecord(error, val)) return False;
	out.defineRecord(RecordFieldId("m1"), val);
      }
      if (n > 0) {
	if (!QuantumHolder(res(0)).toRecord(error, val)) return False;
	out.defineRecord(RecordFieldId("m0"), val);
      }
    } else {			// multiple values
      Vector<Double> m2(nel);
      Vector<Double> m1(nel);
      Vector<Double> m0(nel);
      for (uInt i=0; i<nelements(); i++) {
	if (!mvhold_p[i]) {
	  error += String("No value specified in MeasureHolder::toRecord\n");
	  return False;
	}
	res = mvhold_p[i]->getRecordValue();
	if (n>2) m2(i) = res(2).getValue();
	if (n>1) m1(i) = res(1).getValue();
	if (n>0) m0(i) = res(0).getValue();
      }
      if (n > 2) {
	if (!QuantumHolder(Quantum<Vector<Double> >(m2,
						    res(2).getFullUnit())).
	    toRecord(error, val)) return False;
	out.defineRecord(RecordFieldId("m2"), val);
      }
      if (n > 1) {
	if (!QuantumHolder(Quantum<Vector<Double> >(m1,
						    res(1).getFullUnit())).
	    toRecord(error, val)) return False;
	out.defineRecord(RecordFieldId("m1"), val);
      }
      if (n > 0) {
	if (!QuantumHolder(Quantum<Vector<Double> >(m0,
						    res(0).getFullUnit())).
	    toRecord(error, val)) return False;
	out.defineRecord(RecordFieldId("m0"), val);
      }
    }
    return True;
  }
  error += String("No Measure specified in MeasureHolder::toRecord\n");
  return False;
}

void MeasureHolder::toRecord(RecordInterface& out) const {
	String error;
	if (! toRecord(error, out)) {
		throw AipsError(error);
	}
}



Bool MeasureHolder::toType(String &error, RecordInterface &out) const {
  if (hold_p && putType(error, out)) return True;
  error += String("No Measure specified in MeasureHolder::toType\n");
  return False;    
}

Bool MeasureHolder::fromType(String &error, const RecordInterface &in) {
  if (in.isDefined(String("type")) &&
      in.type(in.idToNumber(RecordFieldId("type"))) == TpString) {
    if (!getType(error, in)) {
      error += String("Unknown Measure record in MeasureHolder::fromType\n");
      return False;
    }
    return True;
  }
  error += String("Illegal Measure record in MeasureHolder::fromType\n");
  return False;
}

const String &MeasureHolder::ident() const {
  static String myid = "meas";
  return myid;
}

Bool MeasureHolder::setMV(uInt pos, const MeasValue &in) {
  if (mvhold_p.nelements() > pos) mvhold_p[pos] = in.clone();
  else return False;
  convertmv_p = True;
  return True;
}

MeasValue *MeasureHolder::getMV(uInt pos) const {
  if (mvhold_p.nelements() > pos)  return mvhold_p[pos];
  else return static_cast<MeasValue *>(0);
}

Bool MeasureHolder::putType(String &, RecordInterface &out) const {
  out.define(RecordFieldId("type"),
	     downcase(String(hold_p->tellMe())));
  return True;
}

Bool MeasureHolder::getType(String &error, const RecordInterface &in) {
  String tp;
  in.get(RecordFieldId("type"), tp);
  return getType(error, tp);
}

Bool MeasureHolder::getType(String &error, const String &in) {
  String tp(in);
  tp.downcase();
  hold_p.reset();
  if (tp == downcase(MDirection::showMe())) {
    hold_p.reset(new MDirection());
  } else if (tp == downcase(MDoppler::showMe())) {
    hold_p.reset(new MDoppler());
  } else if (tp == downcase(MEpoch::showMe())) {
    hold_p.reset(new MEpoch());
  } else if (tp == downcase(MFrequency::showMe())) {
    hold_p.reset(new MFrequency());
  } else if (tp == downcase(MPosition::showMe())) {
    hold_p.reset(new MPosition());
  } else if (tp == downcase(MRadialVelocity::showMe())) {
    hold_p.reset(new MRadialVelocity());
  } else if (tp == downcase(MBaseline::showMe())) {
    hold_p.reset(new MBaseline());
  } else if (tp == downcase(Muvw::showMe())) {
    hold_p.reset(new Muvw());
  } else if (tp == downcase(MEarthMagnetic::showMe())) {
    hold_p.reset(new MEarthMagnetic());
  } else {
    error = in + " is an unknown measure type";
    return False;
  }
  return True;
}

void MeasureHolder::createMV(uInt n) {
  for (uInt i=0; i<mvhold_p.nelements(); i++) {
    delete mvhold_p[i]; mvhold_p[i] = 0;
  }
  mvhold_p.resize(n);
  for (uInt i=0; i<mvhold_p.nelements(); i++) mvhold_p[i] = 0;
}



} //# NAMESPACE CASACORE - END

