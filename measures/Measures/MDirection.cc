//# MDirection.cc:  A Measure: astronomical direction
//# Copyright (C) 1995-2002,2004,2007
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
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/QMath.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
MDirection::MDirection() :
  MeasBase<MVDirection, MDirection::Ref>() {}

MDirection::MDirection(const MVDirection &dt) : 
  MeasBase<MVDirection, MDirection::Ref>(dt,MDirection::DEFAULT) {}

MDirection::MDirection(const MVDirection &dt, const MDirection::Ref &rf) : 
  MeasBase<MVDirection, MDirection::Ref>(dt,rf) {}

MDirection::MDirection(const MVDirection &dt, MDirection::Types rf) : 
  MeasBase<MVDirection, MDirection::Ref>(dt,rf) {}

MDirection::MDirection(const Quantity &dt, const Quantity &dt1) : 
  MeasBase<MVDirection, MDirection::Ref>(MVDirection(dt,dt1),
					 MDirection::DEFAULT) {}

MDirection::MDirection(const Quantity &dt, const Quantity &dt1,
		       const MDirection::Ref &rf) : 
  MeasBase<MVDirection, MDirection::Ref>(MVDirection(dt,dt1),rf) {}

MDirection::MDirection(const Quantity &dt, const Quantity &dt1,
		       MDirection::Types rf) : 
  MeasBase<MVDirection, MDirection::Ref>(MVDirection(dt,dt1),rf) {}

MDirection::MDirection(const Quantum<Vector<Double> > &dt) :
  MeasBase<MVDirection, MDirection::Ref>(MVDirection(dt),
					 MDirection::DEFAULT) {}

MDirection::MDirection(const Quantum<Vector<Double> > &dt,
		       const MDirection::Ref &rf) : 
  MeasBase<MVDirection, MDirection::Ref>(MVDirection(dt),rf) {}

MDirection::MDirection(const Quantum<Vector<Double> > &dt,
		       MDirection::Types rf) : 
  MeasBase<MVDirection, MDirection::Ref>(MVDirection(dt),rf) {}

MDirection::MDirection(const Measure *dt) :
  MeasBase<MVDirection, MDirection::Ref>(dt) {}

MDirection::MDirection(const MeasValue *dt) :
  MeasBase<MVDirection, MDirection::Ref>(*(MVDirection*)dt,
					 MDirection::DEFAULT) {}

MDirection::MDirection(const MDirection::Ref &rf) : 
  MeasBase<MVDirection, MDirection::Ref>(rf) {}

MDirection::MDirection(MDirection::Types rf) :
  MeasBase<MVDirection, MDirection::Ref>(rf) {}

//# Destructor
MDirection::~MDirection() {}

MDirection MDirection::makeMDirection (const String& sourceName)
{
    // Look if it is a known moving source.
    MDirection::Types refType;
    if (MDirection::getType (refType, sourceName)) {
      // Only planetary objects are valid.
      if (refType > MDirection::N_Types  &&  refType < MDirection::COMET) {
        return MDirection(refType);
      }
    }
    // Now see if it is a known standard source.
    MVDirection mvdir;
    // Make it case-insensitive.
    String name(sourceName);
    name.upcase();
    if (name == "CASA") {
      mvdir = MVDirection (6.123487680622104,  1.0265153995604648);
    } else if (name == "CYGA") {
      mvdir = MVDirection (5.233686575770755,  0.7109409582180791);
    } else if (name == "TAUA") {
      mvdir = MVDirection (1.4596748493730913, 0.38422502335921294);
    } else if (name == "VIRA") {
      mvdir = MVDirection (3.276086511413598,  0.21626589533567378);
    } else if (name == "HERA") {
      mvdir = MVDirection (4.4119087330382163, 0.087135562905816893);
    } else if (name == "HYDA") {
      mvdir = MVDirection (2.4351466,         -0.21110706);
    } else if (name == "PERA") {
      mvdir = MVDirection (0.87180363,         0.72451580);
    } else {
      throw AipsError ("MDirection: " + sourceName +
                       " is an unknown source name");
    }
    return MDirection (mvdir, MDirection::J2000);
}


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
  return Register(static_cast<MDirection *>(0));
}

void MDirection::assure(const Measure &in) {
  if (in.type() != Register(static_cast<MDirection *>(0))) {
    throw(AipsError("Illegal Measure type argument: " +
		    MDirection::showMe()));
  }
}

MDirection::Types MDirection::castType(uInt tp) {
  MDirection::checkMyTypes();
  if ((tp & MDirection::EXTRA) == 0) {
    AlwaysAssert(tp < MDirection::N_Types, AipsError);
  } else {
    AlwaysAssert((tp & ~MDirection::EXTRA) < 
		 (MDirection::N_Planets - MDirection::MERCURY), AipsError);
  }
  return static_cast<MDirection::Types>(tp);
}

const String &MDirection::showType(MDirection::Types tp) {
  static const String tname[MDirection::N_Types] = {
    "J2000",
    "JMEAN",
    "JTRUE",
    "APP",
    "B1950",
    "B1950_VLA",
    "BMEAN",
    "BTRUE",
    "GALACTIC",
    "HADEC",
    "AZEL",
    "AZELSW",
    "AZELGEO",
    "AZELSWGEO",
    "JNAT",
    "ECLIPTIC",
    "MECLIPTIC",
    "TECLIPTIC",
    "SUPERGAL",
    "ITRF",
    "TOPO",
    "ICRS" };
  static const String pname[MDirection::N_Planets - MDirection::MERCURY] = {
    "MERCURY",
    "VENUS",
    "MARS",
    "JUPITER",
    "SATURN",
    "URANUS",
    "NEPTUNE",
    "PLUTO",
    "SUN",
    "MOON",
    "COMET" };
  
  MDirection::checkMyTypes();
  if ((tp & MDirection::EXTRA) == 0) return tname[tp];
  return pname[tp & ~MDirection::EXTRA];
}

const String &MDirection::showType(uInt tp) {
  return MDirection::showType(MDirection::castType(tp));
}

const String* MDirection::allMyTypes(Int &nall, Int &nextra,
                                     const uInt *&typ) {
  static const Int N_name  = 35;
  static const Int N_extra = 11;
  static const String tname[N_name] = {
    "J2000",
    "JMEAN",
    "JTRUE",
    "APP",
    "B1950",
    "B1950_VLA",
    "BMEAN",
    "BTRUE",
    "GALACTIC",
    "HADEC",
    "AZEL",
    "AZELSW",
    "AZELNE",
    "AZELGEO",
    "AZELSWGEO",
    "AZELNEGEO",
    "JNAT",
    "ECLIPTIC",
    "MECLIPTIC",
    "TECLIPTIC",
    "SUPERGAL",
    "ITRF",
    "TOPO",
    "ICRS",
    "MERCURY",
    "VENUS",
    "MARS",
    "JUPITER",
    "SATURN",
    "URANUS",
    "NEPTUNE",
    "PLUTO",
    "SUN",
    "MOON",
    "COMET" };
  
  static const uInt oname[N_name] = {
    MDirection::J2000,
    MDirection::JMEAN,
    MDirection::JTRUE,
    MDirection::APP,
    MDirection::B1950,
    MDirection::B1950_VLA,
    MDirection::BMEAN,
    MDirection::BTRUE,
    MDirection::GALACTIC,
    MDirection::HADEC,
    MDirection::AZEL,
    MDirection::AZELSW,
    MDirection::AZEL,
    MDirection::AZELGEO,
    MDirection::AZELSWGEO,
    MDirection::AZELGEO,
    MDirection::JNAT,
    MDirection::ECLIPTIC,
    MDirection::MECLIPTIC,
    MDirection::TECLIPTIC,
    MDirection::SUPERGAL,
    MDirection::ITRF,
    MDirection::TOPO,
    MDirection::ICRS,
    MDirection::MERCURY,
    MDirection::VENUS,
    MDirection::MARS,
    MDirection::JUPITER,
    MDirection::SATURN,
    MDirection::URANUS,
    MDirection::NEPTUNE,
    MDirection::PLUTO,
    MDirection::SUN,
    MDirection::MOON,
    MDirection::COMET };

  MDirection::checkMyTypes();
  nall   = N_name;
  nextra = N_extra;
  typ    = oname;
  return tname;
}

const String* MDirection::allTypes(Int &nall, Int &nextra,
                                   const uInt *&typ) const {
  return MDirection::allMyTypes(nall, nextra, typ);
}

void MDirection::checkTypes() const {
  MDirection::checkMyTypes();
}

void MDirection::checkMyTypes() {
  // Multiple threads could execute this, but that is harmless.
  static Bool first(True);
  if (first) {
    first = False;
    Int nall, nex;
    const uInt *typ;
    const String *const tps = MDirection::allMyTypes(nall,nex, typ);
    MDirection::Types tp;
    for (Int i=0; i<nall; i++) {
      AlwaysAssert(MDirection::getType(tp, MDirection::showType(typ[i])) &&
		   tp == Int(typ[i]) &&
		   MDirection::getType(tp, tps[i]) &&
		   tp == Int(typ[i]), AipsError);
    }
    for (Int i=0; i<N_Types; i++) {
      AlwaysAssert(MDirection::getType(tp, MDirection::showType(i)) &&
		   tp == i, AipsError);
    }
    for (Int i=MERCURY; i<N_Planets; i++) {
      AlwaysAssert(MDirection::getType(tp, MDirection::showType(i)) &&
		   tp == i, AipsError);
    }
  }
}

Bool MDirection::getType(MDirection::Types &tp, const String &in) {
  const uInt *oname;
  Int nall, nex;
  const String *tname = MDirection::allMyTypes(nall, nex, oname);
  
  Int i = Measure::giveMe(in, nall, tname);
  
  if (i>=nall) return False;
  else tp = static_cast<MDirection::Types>(oname[i]);
  return True;
}

Bool MDirection::giveMe(MDirection::Ref &mr, const String &in) {
  MDirection::Types tp;
  if (MDirection::getType(tp, in)) mr = MDirection::Ref(tp);
  else {
    mr = MDirection::Ref();
    return False;
  }
  return True;
}

MDirection::GlobalTypes MDirection::globalType(uInt tp) {

    static const MDirection::GlobalTypes oname[MDirection::N_Types] = {
	MDirection::GRADEC,
	MDirection::GRADEC,
	MDirection::GRADEC,
	MDirection::GRADEC,
	MDirection::GRADEC,
	MDirection::GRADEC,
	MDirection::GRADEC,
	MDirection::GRADEC,
	MDirection::GLONGLAT,
	MDirection::GHADEC,
	MDirection::GAZEL,
        MDirection::GAZEL,
        MDirection::GAZEL,
        MDirection::GAZEL,
	MDirection::GRADEC,
	MDirection::GLONGLAT,
	MDirection::GLONGLAT,
	MDirection::GLONGLAT,
	MDirection::GLONGLAT,
	MDirection::GRADEC,
	MDirection::GRADEC };
    if ((tp & MDirection::EXTRA) != 0) tp = 0;
    AlwaysAssert(tp < MDirection::N_Types, AipsError);

    return oname[tp];
}

Bool MDirection::setOffset(const Measure &in) {
  if (in.type() != Register(static_cast<MDirection *>(0))) return False;
  ref.set(in);
  return True;
}

Bool MDirection::setRefString(const String &in) {
  MDirection::Types tp;
  if (MDirection::getType(tp, in)) {
    ref.setType(tp);
    return True;
  }
  ref.setType(MDirection::DEFAULT);
  return False;
}

const String &MDirection::getDefaultType() const {
  return MDirection::showType(MDirection::DEFAULT);
}

String MDirection::getRefString() const {
  return MDirection::showType(ref.getType());
}

uInt MDirection::myType() {
  return Register(static_cast<MDirection *>(0));
}

Bool MDirection::isModel() const {
  return ((ref.getType() & MDirection::EXTRA) != 0);
}

Quantum<Vector<Double> > MDirection::getAngle() const {
    return (data.getAngle());
}

Quantum<Vector<Double> > MDirection::getAngle(const Unit &inunit) const {
    return (data.getAngle(inunit));
}

void MDirection::shift(const Quantum<Double> &lng,
			const Quantum<Double> &lat, Bool trueAngle) {
  data.shift(lng, lat, trueAngle);
}

void MDirection::shift(Double lng, Double lat, Bool trueAngle) {
  data.shift(lng, lat, trueAngle);
}

void MDirection::shiftLongitude(const Quantum<Double> &lng, Bool trueAngle) {
  data.shiftLongitude(lng, trueAngle);
}

void MDirection::shiftLongitude(Double lng, Bool trueAngle) {
  data.shiftLongitude(lng, trueAngle);
}

void MDirection::shiftLatitude(const Quantum<Double> &lat, Bool trueAngle) {
  data.shiftLatitude(lat, trueAngle);
}

void MDirection::shiftLatitude(Double lat, Bool trueAngle) {
  data.shiftLatitude(lat, trueAngle);
}

void MDirection::shift(const MVDirection &shft, Bool trueAngle) {
  data.shift(shft, trueAngle);
}

void MDirection::shiftAngle(const Quantum<Double> &off,
			    const Quantum<Double> &pa) {
  data.shiftAngle(off, pa);
}

void MDirection::shiftAngle(Double off, Double pa) {
  data.shiftAngle(off, pa);
}

Measure *MDirection::clone() const {
    return (new MDirection(*this));
}

String MDirection::toString() const {
	Quantity longitude = getValue().getLong("deg");
	Quantity lat = getValue().getLat("deg");
	Types type = castType(ref.getType());
	String output;
	if (
		type == J2000  || type == JMEAN
		|| type == JTRUE || type == APP
		|| type == B1950 || type == B1950_VLA
		|| type == BMEAN || type == BTRUE
	) {
		String ra = MVTime(longitude).string(MVTime::TIME, 12);
		String dec = MVAngle(abs(lat)).string(MVAngle::ANGLE_CLEAN, 11);
		dec.trim();
		if (lat.getValue() < 0) {
			dec = "-" + dec;
		}
		output = ra + " " + dec;
	}
	else {
		String longStr = MVAngle(longitude).string(MVAngle::ANGLE, 11);
		String latStr = MVAngle(lat).string(MVAngle::ANGLE, 11);
		output = longStr + " " + latStr;
	}
	output += " " + showType(type);
	return output;
}

} //# NAMESPACE CASACORE - END

