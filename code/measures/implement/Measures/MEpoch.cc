//# MEpoch.cc: A Measure: instant in time
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
typedef Quantum<Double> gpp_mepoch_bug1;
#endif
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>
#include <aips/RTTI/Register.h>
#include <aips/Measures/MEpoch.h>

//# Constructors
MEpoch::MEpoch() : 
  MeasBase<MVEpoch,MEpoch::Ref>() {}

MEpoch::MEpoch(const MVEpoch &dt) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,MEpoch::DEFAULT) {}

MEpoch::MEpoch(const MVEpoch &dt, const MEpoch::Ref &rf) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,rf) {}

MEpoch::MEpoch(const MVEpoch &dt, uInt rf) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,rf) {}

MEpoch::MEpoch(const Quantity &dt) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,MEpoch::DEFAULT) {}

MEpoch::MEpoch(const Quantity &dt, const MEpoch::Ref &rf) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,rf) {}

MEpoch::MEpoch(const Quantity &dt, uInt rf) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,rf) {}

MEpoch::MEpoch(const Measure *dt) :
  MeasBase<MVEpoch,MEpoch::Ref>(dt) {}

MEpoch::MEpoch(const MeasValue *dt) :
  MeasBase<MVEpoch,MEpoch::Ref>(*(MVEpoch*)dt,
				MEpoch::DEFAULT) {}

//# Destructor
MEpoch::~MEpoch() {}

//# Operators

//# Member functions

const String &MEpoch::tellMe() const {
  return MEpoch::showMe();
}

const String &MEpoch::showMe() {
  static const String name("Epoch");
  return name;
}

uInt MEpoch::type() const {
  return Register((MEpoch *)0);
}

void MEpoch::assert(const Measure &in) {
  if (in.type() != Register((MEpoch *)0)) {
    throw(AipsError("Illegal Measure type argument: " +
		    MEpoch::showMe()));
  };
}

const String &MEpoch::showType(uInt tp) {
  static const String tname[MEpoch::N_Types] = {
    "LAST",
    "LMST",
    "GMST1",
    "GAST",
    "UT1",
    "UT2",
    "UTC",
    "TAI",
    "TDT",
    "TCG",
    "TDB",
    "TCB"};
  DebugAssert((tp & ~MEpoch::EXTRA) < MEpoch::N_Types, AipsError);
  return tname[tp & ~MEpoch::EXTRA];
}

Bool MEpoch::getType(MEpoch::Types &tp, const String &in) {
  static const Int N_name = 17;
  static const String tname[N_name] = {
    "LAST",
    "LMST",
    "GMST1",
    "GAST",
    "UT1",
    "UT2",
    "UTC",
    "TAI",
    "TDT",
    "TCG",
    "TDB",
    "TCB",
    "IAT",
    "GMST",
    "TT",
    "ET",
    "UT"};
  
  static const MEpoch::Types oname[N_name] = {
    MEpoch::LAST,
    MEpoch::LMST,
    MEpoch::GMST1,
    MEpoch::GAST,
    MEpoch::UT1,
    MEpoch::UT2,
    MEpoch::UTC,
    MEpoch::TAI,
    MEpoch::TDT,
    MEpoch::TCG,
    MEpoch::TDB,
    MEpoch::TCB,
    MEpoch::TAI,
    MEpoch::GMST1,
    MEpoch::TDT,
    MEpoch::TDT,
    MEpoch::UT1};
  
  uInt i = Measure::giveMe(in, N_name, tname);

  if (i>=N_name) {
    return False;
  } else {
    tp = oname[i];
  };
  return True;
}

Bool MEpoch::giveMe(MEpoch::Ref &mr, const String &in) {
  MEpoch::Types tp;
  if (MEpoch::getType(tp, in)) {
    mr = MEpoch::Ref(tp);
  } else {
    mr = MEpoch::Ref();
    return False;
  };
  return True;
};

Bool MEpoch::giveMe(const String &in, MEpoch::Ref &mr) {
  return MEpoch::giveMe(mr, in);
}

Bool MEpoch::setOffset(const Measure &in) {
  if (in.type() != Register((MEpoch *)0)) return False;
  ref.set(in);
  return True;
}

Bool MEpoch::setRefString(const String &in) {
  MEpoch::Types tp;
  String x = in;
  Bool raze = False;
  if (x.before(2) == "r_" || x.before(2) == "R_") {
    raze = True;
    x = x.from(2);
  };
  if (MEpoch::getType(tp, x)) {
    if (raze) {
      ref.setType(tp | MEpoch::RAZE);
    } else {
      ref.setType(tp);
    };
    return True;
  };
  ref.setType(MEpoch::DEFAULT);
  return False;
}

const String &MEpoch::getDefaultType() const {
  return MEpoch::showType(MEpoch::DEFAULT);
}

String MEpoch::getRefString() const {
  String x;
  if ((ref.getType() & MEpoch::RAZE) != 0) x = String("R_");
  x += MEpoch::showType(ref.getType());
  return x;
}

uInt MEpoch::myType() {
  return Register((MEpoch *)0);
}

Quantity MEpoch::get(const Unit &inunit) const {
  return (data.getTime().get(inunit));
}

Measure *MEpoch::clone() const {
  return (new MEpoch(*this));
}
