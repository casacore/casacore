//# MPosition.cc:  A Measure: position on Earth
//# Copyright (C) 1995,1996,1997,1998
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
#ifdef __GNUG__
#include <aips/Quanta/Quantum.h>
typedef Quantum<Double> gpp_mposition_bug1;
#endif
#include <aips/Exceptions.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Math.h>
#include <aips/RTTI/Register.h>
#include <aips/Measures/MPosition.h>
#include <aips/Utilities/Assert.h>

//# Constructors
MPosition::MPosition() :
  MeasBase<MVPosition,MPosition::Ref>() {}

MPosition::MPosition(const MVPosition &dt) : 
  MeasBase<MVPosition,MPosition::Ref>(dt,MPosition::DEFAULT) {}

MPosition::MPosition(const MVPosition &dt, const MPosition::Ref &rf) : 
  MeasBase<MVPosition,MPosition::Ref>(dt,rf) {}

MPosition::MPosition(const MVPosition &dt, uInt rf) : 
  MeasBase<MVPosition,MPosition::Ref>(dt,rf) {}

MPosition::MPosition(const Quantity &dt, const Quantity &dt1,
		     const Quantity &dt2) : 
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt,dt1,dt2),
				      MPosition::DEFAULT) {}

MPosition::MPosition(const Quantity &dt, const Quantity &dt1,
		     const Quantity &dt2, const MPosition::Ref &rf) : 
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt,dt1,dt2),rf) {}

MPosition::MPosition(const Quantity &dt, const Quantity &dt1,
		     const Quantity &dt2, uInt rf) : 
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt,dt1,dt2),rf) {}

MPosition::MPosition(const Quantity &dt0, const Quantum<Vector<Double> > &dt) :
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt0,dt),
				      MPosition::DEFAULT) {}

MPosition::MPosition(const Quantity &dt0, const Quantum<Vector<Double> > &dt,
		     const MPosition::Ref &rf) : 
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt0,dt),rf) {}

MPosition::MPosition(const Quantity &dt0, const Quantum<Vector<Double> > &dt,
		     uInt rf) : 
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt0,dt),rf) {}

MPosition::MPosition(const Measure *dt) :
  MeasBase<MVPosition,MPosition::Ref>(dt) {}

MPosition::MPosition(const MeasValue *dt) :
  MeasBase<MVPosition,MPosition::Ref>(*(MVPosition*)dt,
				MPosition::DEFAULT) {}

MPosition::MPosition(const MPosition &other)
: MeasBase<MVPosition, MeasRef<MPosition> > (other)
{
  // Nothing
}

MPosition &MPosition::operator=(const MPosition &other)
{
  if (this != &other) {
    MeasBase<MVPosition, MeasRef<MPosition> > &This = *this;
    const MeasBase<MVPosition, MeasRef<MPosition> > &Other = other;
    This = Other;
  }
  return *this;
}

//# Destructor
MPosition::~MPosition() {}

//# Operators

//# Member functions

const String &MPosition::tellMe() const {
    return MPosition::showMe();
}

const String &MPosition::showMe() {
    static const String name("Position");
    return name;
}

uInt MPosition::type() const {
  return Register((MPosition *)0);
}

void MPosition::assert(const Measure &in) {
  if (in.type() != Register((MPosition *)0)) {
    throw(AipsError("Illegal Measure type argument: " +
		    MPosition::showMe()));
  };
}

const String &MPosition::showType(uInt tp) {
    static const String tname[MPosition::N_Types] = {
	"ITRF",
	"WGS84"};
    DebugAssert(tp < MPosition::N_Types, AipsError);
    return tname[tp];
}

Bool MPosition::getType(MPosition::Types &tp, const String &in) {
  static const Int N_name = 2;
  static const String tname[N_name] = {
    "ITRF",
    "WGS84"};
  
  static const MPosition::Types oname[N_name] = {
    MPosition::ITRF,
    MPosition::WGS84};
  
  uInt i = Measure::giveMe(in, N_name, tname);
  
  if (i>=N_name) {
    return False;
  } else {
    tp = oname[i];
  };
  return True;
}

Bool MPosition::giveMe(MPosition::Ref &mr, const String &in) {
  MPosition::Types tp;
  if (MPosition::getType(tp, in)) {
    mr = MPosition::Ref(tp);
  } else {
    mr = MPosition::Ref();
    return False;
  };
  return True;
};

Bool MPosition::giveMe(const String &in, MPosition::Ref &mr) {
  return MPosition::giveMe(mr, in);
}

Bool MPosition::setOffset(const Measure &in) {
  if (in.type() != Register((MPosition *)0)) return False;
  ref.set(in);
  return True;
}

Bool MPosition::setRefString(const String &in) {
  MPosition::Types tp;
  if (MPosition::getType(tp, in)) {
    ref.setType(tp);
    return True;
  };
  ref.setType(MPosition::DEFAULT);
  return False;
}

const String &MPosition::getDefaultType() const {
  return MPosition::showType(MPosition::DEFAULT);
}

String MPosition::getRefString() const {
  return MPosition::showType(ref.getType());
}

uInt MPosition::myType() {
  return Register((MPosition *)0);
}

Quantum<Vector<Double> > MPosition::get(const Unit &inunit) const {
    return Quantum<Vector<Double> >(data.getValue(),"m").get(inunit);
}

Quantum<Vector<Double> > MPosition::getAngle() const {
    return (data.getAngle());
}

Quantum<Vector<Double> > MPosition::getAngle(const Unit &inunit) const {
    return (data.getAngle(inunit));
}

Measure *MPosition::clone() const {
    return (new MPosition(*this));
}
