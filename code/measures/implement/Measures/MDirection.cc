//# MDirection.cc:  A Measure: astronomical direction
//# Copyright (C) 1995,1996,1997
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
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_direction_bug1;
#endif
#include <aips/Exceptions.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/RTTI/Register.h>
#include <aips/Measures/MDirection.h>

//# Constructors
MDirection::MDirection() :
  MeasBase<MVDirection,MDirection::Ref>() {}

MDirection::MDirection(const MVDirection &dt) : 
  MeasBase<MVDirection,MDirection::Ref>(dt,MDirection::DEFAULT) {}

MDirection::MDirection(const MVDirection &dt, const MDirection::Ref &rf) : 
  MeasBase<MVDirection,MDirection::Ref>(dt,rf) {}

MDirection::MDirection(const MVDirection &dt, uInt rf) : 
  MeasBase<MVDirection,MDirection::Ref>(dt,rf) {}

MDirection::MDirection(const Quantity &dt, const Quantity &dt1) : 
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt,dt1),
					MDirection::DEFAULT) {}

MDirection::MDirection(const Quantity &dt, const Quantity &dt1,
		       const MDirection::Ref &rf) : 
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt,dt1),rf) {}

MDirection::MDirection(const Quantity &dt, const Quantity &dt1,
		       uInt rf) : 
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt,dt1),rf) {}

MDirection::MDirection(const Quantum<Vector<Double> > &dt) :
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt),
					MDirection::DEFAULT) {}

MDirection::MDirection(const Quantum<Vector<Double> > &dt,
		       const MDirection::Ref &rf) : 
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt),rf) {}

MDirection::MDirection(const Quantum<Vector<Double> > &dt,
		       uInt rf) : 
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt),rf) {}

MDirection::MDirection(const Measure *dt) :
  MeasBase<MVDirection,MDirection::Ref>(dt) {}

MDirection::MDirection(const MeasValue *dt) :
  MeasBase<MVDirection,MDirection::Ref>(*(MVDirection*)dt,
					MDirection::DEFAULT) {}

//# Destructor
MDirection::~MDirection() {}

//# Operators

//# Member functions

const String &MDirection::tellMe() const {
    return MDirection::showMe();
}

const String &MDirection::showMe() {
    static const String name("Direction");
    return name;
}

uInt MDirection::type() const {
  return Register((MDirection *)0);
}

void MDirection::assert(const Measure &in) {
  if (in.type() != Register((MDirection *)0)) {
    throw(AipsError("Illegal Measure type argument: " +
		    MDirection::showMe()));
  };
}

const String &MDirection::showType(uInt tp) {
    static const String tname[MDirection::N_Types] = {
	"J2000",
	"JMEAN",
	"JTRUE",
	"APP",
	"B1950",
	"BMEAN",
	"BTRUE",
	"GALACTIC",
	"HADEC",
	"AZEL",
        "AZELSW"};
    DebugAssert(tp < MDirection::N_Types, AipsError);
    return tname[tp];
}

Bool MDirection::giveMe(const String &in, MDirection::Ref &mr) {
    static const Int N_name = 10;
    static const String tname[N_name] = {
	"J2000",
	"JMEAN",
	"JTRUE",
	"APP",
	"B1950",
	"BMEAN",
	"BTRUE",
	"GALACTIC",
	"HADEC",
	"AZEL",
        "AZELSW"};

    static const uInt oname[N_name] = {
	MDirection::J2000,
	MDirection::JMEAN,
	MDirection::JTRUE,
	MDirection::APP,
	MDirection::B1950,
	MDirection::BMEAN,
	MDirection::BTRUE,
	MDirection::GALACTIC,
	MDirection::HADEC,
	MDirection::AZEL,
        MDirection::AZELSW};

    uInt i = Measure::giveMe(in, N_name, tname);

    if (i>=N_name) {
	mr = MDirection::Ref();
	return False;
    } else {
	mr = MDirection::Ref(oname[i]);
    };
    return True;
}

Quantum<Vector<Double> > MDirection::getAngle() const {
    return (data.getAngle());
}

Quantum<Vector<Double> > MDirection::getAngle(const Unit &inunit) const {
    return (data.getAngle(inunit));
}

Measure *MDirection::clone() const {
    return (new MDirection(*this));
}
