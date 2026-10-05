// # MEpoch.cc: A Measure: instant in time
// # Copyright (C) 1995,1996,1997,1998,1999,2000,2001
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

// # Includes
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/measures/Measures/MEpoch.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// # Constructors
MEpoch::MEpoch() : MeasBase<MVEpoch, MEpoch::Ref>() {}

MEpoch::MEpoch(const MVEpoch &dt) : MeasBase<MVEpoch, MEpoch::Ref>(dt, MEpoch::DEFAULT) {}

MEpoch::MEpoch(const MVEpoch &dt, const MEpoch::Ref &rf) : MeasBase<MVEpoch, MEpoch::Ref>(dt, rf) {}

MEpoch::MEpoch(const MVEpoch &dt, MEpoch::Types rf) : MeasBase<MVEpoch, MEpoch::Ref>(dt, rf) {}

MEpoch::MEpoch(const Quantity &dt) : MeasBase<MVEpoch, MEpoch::Ref>(dt, MEpoch::DEFAULT) {}

MEpoch::MEpoch(const Quantity &dt, const MEpoch::Ref &rf)
    : MeasBase<MVEpoch, MEpoch::Ref>(dt, rf) {}

MEpoch::MEpoch(const Quantity &dt, MEpoch::Types rf) : MeasBase<MVEpoch, MEpoch::Ref>(dt, rf) {}

MEpoch::MEpoch(const Measure *dt) : MeasBase<MVEpoch, MEpoch::Ref>(dt) {}

MEpoch::MEpoch(const MeasValue *dt)
    : MeasBase<MVEpoch, MEpoch::Ref>(*(MVEpoch *)dt, MEpoch::DEFAULT) {}

// # Destructor
MEpoch::~MEpoch() {}

// # Operators

// # Member functions

const String &MEpoch::tellMe() const { return MEpoch::showMe(); }

const String &MEpoch::showMe() {
  static const String name("Epoch");
  return name;
}

void MEpoch::assure(const Measure &in) {
  if (!dynamic_cast<const MEpoch *>(&in)) {
    throw(AipsError("Illegal Measure type argument: " + MEpoch::showMe()));
  }
}

MEpoch::Types MEpoch::castType(unsigned int tp) {
  MEpoch::checkMyTypes();
  AlwaysAssert((tp & ~MEpoch::EXTRA) < MEpoch::N_Types, AipsError);
  return static_cast<MEpoch::Types>(tp);
}

const String &MEpoch::showType(MEpoch::Types tp) {
  static const String tname[MEpoch::N_Types] = {"LAST", "LMST", "GMST1", "GAST", "UT1", "UT2",
                                                "UTC",  "TAI",  "TDT",   "TCG",  "TDB", "TCB"};

  MEpoch::checkMyTypes();
  return tname[tp & ~MEpoch::EXTRA];
}

const String &MEpoch::showType(unsigned int tp) { return MEpoch::showType(MEpoch::castType(tp)); }

const String *MEpoch::allMyTypes(int &nall, int &nextra, const unsigned int *&typ) {
  static const int N_name = 17;
  static const int N_extra = 0;
  static const String tname[N_name] = {"LAST", "LMST", "GMST1", "GAST", "UT1", "UT2",
                                       "UTC",  "TAI",  "TDT",   "TCG",  "TDB", "TCB",
                                       "IAT",  "GMST", "TT",    "ET",   "UT"};

  static const unsigned int oname[N_name] = {
      MEpoch::LAST, MEpoch::LMST,  MEpoch::GMST1, MEpoch::GAST, MEpoch::UT1, MEpoch::UT2,
      MEpoch::UTC,  MEpoch::TAI,   MEpoch::TDT,   MEpoch::TCG,  MEpoch::TDB, MEpoch::TCB,
      MEpoch::TAI,  MEpoch::GMST1, MEpoch::TDT,   MEpoch::TDT,  MEpoch::UT1};

  MEpoch::checkMyTypes();
  nall = N_name;
  nextra = N_extra;
  typ = oname;
  return tname;
}

const String *MEpoch::allTypes(int &nall, int &nextra, const unsigned int *&typ) const {
  return MEpoch::allMyTypes(nall, nextra, typ);
}

void MEpoch::checkTypes() const { MEpoch::checkMyTypes(); }

void MEpoch::checkMyTypes() {
  // Multiple threads could execute this, but that is harmless.
  static bool first(true);
  if (first) {
    first = false;
    int nall, nex;
    const unsigned int *typ;
    const String *const tps = MEpoch::allMyTypes(nall, nex, typ);
    MEpoch::Types tp;
    for (int i = 0; i < nall; i++) {
      AlwaysAssert(MEpoch::getType(tp, MEpoch::showType(typ[i])) && tp == int(typ[i]) &&
                       MEpoch::getType(tp, tps[i]) && tp == int(typ[i]),
                   AipsError);
    }
    for (int i = 0; i < N_Types; i++) {
      AlwaysAssert(MEpoch::getType(tp, MEpoch::showType(i)) && tp == i, AipsError);
    }
  }
}

bool MEpoch::getType(MEpoch::Types &tp, const String &in) {
  const unsigned int *oname;
  int nall, nex;
  const String *tname = MEpoch::allMyTypes(nall, nex, oname);

  int i = Measure::giveMe(in, nall, tname);

  if (i >= nall)
    return false;
  else
    tp = static_cast<MEpoch::Types>(oname[i]);
  return true;
}

bool MEpoch::giveMe(MEpoch::Ref &mr, const String &in) {
  MEpoch::Types tp;
  if (MEpoch::getType(tp, in))
    mr = MEpoch::Ref(tp);
  else {
    mr = MEpoch::Ref();
    return false;
  }
  return true;
}

bool MEpoch::setOffset(const Measure &in) {
  if (!dynamic_cast<const MEpoch *>(&in)) return false;
  ref.set(in);
  return true;
}

bool MEpoch::setRefString(const String &in) {
  MEpoch::Types tp;
  String x = in;
  bool raze = false;
  if (x.starts_with("r_") || x.starts_with("R_")) {
    raze = true;
    x = x.substr(2);
  }
  if (MEpoch::getType(tp, x)) {
    if (raze) {
      ref.setType(tp | MEpoch::RAZE);
    } else {
      ref.setType(tp);
    }
    return true;
  }
  ref.setType(MEpoch::DEFAULT);
  return false;
}

const String &MEpoch::getDefaultType() const { return MEpoch::showType(MEpoch::DEFAULT); }

String MEpoch::getRefString() const {
  String x;
  if ((ref.getType() & MEpoch::RAZE) != 0) x = String("R_");
  x += MEpoch::showType(ref.getType());
  return x;
}

Quantity MEpoch::get(const Unit &inunit) const { return (data.getTime().get(inunit)); }

Measure *MEpoch::clone() const { return (new MEpoch(*this)); }

}  // namespace casacore
