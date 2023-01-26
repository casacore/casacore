//# MCFrame.cc: Measure frame calculations proxy
//# Copyright (C) 1996-2000,2002,2003,2007
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

//# Includes
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/measures/Measures/MCRadialVelocity.h>
#include <casacore/measures/Measures/MCFrame.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MeasComet.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MCFrame class
 
//# Constructors
MCFrame::MCFrame(MeasFrame &inf) :
  myf(inf),
  epConvTDB(0), epTDBp(0), 
  epConvUT1(0), epUT1p(0), 
  epConvTT(0), epTTp(0), 
  epConvLAST(0), epLASTp(0), 
  posConvLong(0), posLongp(0), posITRFp(0),
  posConvLongGeo(0), posLongGeop(0), posGeop(0),
  dirConvJ2000(0), j2000Longp(0), dirJ2000p(0),
  dirConvB1950(0), b1950Longp(0), dirB1950p(0),
  dirConvApp(0), appLongp(0), dirAppp(0),
  radConvLSR(0), radLSRp(0) {;}

// Destructor
MCFrame::~MCFrame() {
  delete static_cast<MEpoch::Convert *>(epConvTDB);
  delete epTDBp;
  delete static_cast<MEpoch::Convert *>(epConvUT1);
  delete epUT1p;
  delete static_cast<MEpoch::Convert *>(epConvTT);
  delete epTTp;
  delete static_cast<MEpoch::Convert *>(epConvLAST);
  delete epLASTp;
  delete static_cast<MPosition::Convert *>(posConvLong);
  delete posLongp;
  delete posITRFp;
  delete static_cast<MPosition::Convert *>(posConvLongGeo);
  delete posLongGeop;
  delete posGeop;
  delete static_cast<MDirection::Convert *>(dirConvJ2000);
  delete j2000Longp;
  delete dirJ2000p;
  delete static_cast<MDirection::Convert *>(dirConvB1950);
  delete b1950Longp;
  delete dirB1950p;
  delete static_cast<MDirection::Convert *>(dirConvApp);
  delete appLongp;
  delete dirAppp;
  delete static_cast<MRadialVelocity::Convert *>(radConvLSR);
  delete radLSRp;
}

// Operators

// General member functions

void MCFrame::resetEpoch() {
    delete epTDBp; epTDBp = 0;
    delete epUT1p; epUT1p = 0;
    delete epTTp; epTTp = 0;
    delete epLASTp; epLASTp = 0;
    delete appLongp; appLongp = 0;
    delete dirAppp; dirAppp = 0;
    delete radLSRp; radLSRp = 0;
}

void MCFrame::resetPosition() {
  if (posLongp) {
    delete posLongp; posLongp = 0;
    delete posITRFp; posITRFp = 0;
    delete posLongGeop; posLongGeop = 0;
    delete posGeop; posGeop = 0;
  }
  if (epLASTp) {
    delete epLASTp; epLASTp = 0;
  }
}

void MCFrame::resetDirection() {
  if (j2000Longp) {
    delete j2000Longp; j2000Longp = 0;
    delete dirJ2000p; dirJ2000p = 0;
  }
  if (b1950Longp) {
    delete b1950Longp; b1950Longp = 0;
    delete dirB1950p; dirB1950p = 0;
  }
  if (appLongp) {
    delete appLongp; appLongp = 0;
    delete dirAppp; dirAppp = 0;
  }
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  }
}

void MCFrame::resetRadialVelocity() {
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  }
}

void MCFrame::resetComet() {
}

bool MCFrame::getTDB(double &tdb) {
  if (myf.epoch()) {
    if (!epTDBp) {
      epTDBp = new double;
      *epTDBp = static_cast<MEpoch::Convert *>(epConvTDB)->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    }
    tdb = *epTDBp;
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getUT1(double &tdb) {
  if (myf.epoch()) {
    if (!epUT1p) {
      epUT1p = new double;
      *epUT1p = static_cast<MEpoch::Convert *>(epConvUT1)->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    }
    tdb = *epUT1p;
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getTT(double &tdb) {
  if (myf.epoch()) {
    if (!epTTp) {
      epTTp = new double;
      *epTTp = static_cast<MEpoch::Convert *>(epConvTT)->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    }
    tdb = *epTTp;
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getLong(double &tdb) {
  if (myf.position()) {
    if (!posLongp) {
      posLongp = new Vector<double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      *posLongp = posITRFp->get();
    }
    tdb = MVAngle(posLongp->operator()(1))(-0.5);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getLat(double &tdb) {
  if (myf.position()) {
    if (!posLongp) {
      posLongp = new Vector<double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      *posLongp = posITRFp->get();
    }
    tdb = posLongp->operator()(2);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getLatGeo(double &tdb) {
  if (myf.position()) {
    if (!posLongGeop) {
      posLongGeop = new Vector<double>(3);
      posGeop = new MVPosition;
      *posGeop = static_cast<MPosition::Convert *>(posConvLongGeo)->operator()
        (*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
        getValue();
      *posLongGeop = posGeop->get();
    }
    tdb = posLongGeop->operator()(2);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getITRF(MVPosition &tdb) {
  if (myf.position()) {
    if (!posLongp) {
      posLongp = new Vector<double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      *posLongp = posITRFp->get();
    }
    tdb = *posITRFp;
    return true;
  }
  tdb = MVPosition(0.0);
  return false;
}

bool MCFrame::getRadius(double &tdb) {
  if (myf.position()) {
    if (!posLongp) {
      posLongp = new Vector<double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      *posLongp = posITRFp->get();
    }
    tdb = posLongp->operator()(0);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getLAST(double &tdb) {
  if (myf.epoch()) {
    if (!epLASTp) {
      epLASTp = new double;
      *epLASTp = static_cast<MEpoch::Convert *>(epConvLAST)->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    }
    tdb = fmod(*epLASTp, 1.0);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getLASTr(double &tdb) {
  bool tmp = MCFrame::getLAST(tdb);
  tdb *= C::circle;
  return tmp;
}

bool MCFrame::getJ2000Long(double &tdb) {
  if (myf.direction()) {
    if (!j2000Longp) {
      j2000Longp = new Vector<double>(2);
      dirJ2000p = new MVDirection;
      *dirJ2000p = static_cast<MDirection::Convert *>(dirConvJ2000)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *j2000Longp = dirJ2000p->get();
    }
    tdb = j2000Longp->operator()(0);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getJ2000Lat(double &tdb) {
  if (myf.direction()) {
    if (!j2000Longp) {
      j2000Longp = new Vector<double>(2);
      dirJ2000p = new MVDirection;
      *dirJ2000p = static_cast<MDirection::Convert *>(dirConvJ2000)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *j2000Longp = dirJ2000p->get();
    }
    tdb = j2000Longp->operator()(1);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getJ2000(MVDirection &tdb) {
  if (myf.direction()) {
    if (!j2000Longp) {
      j2000Longp = new Vector<double>(2);
      dirJ2000p = new MVDirection;
      *dirJ2000p = static_cast<MDirection::Convert *>(dirConvJ2000)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *j2000Longp = dirJ2000p->get();
    }
    tdb = *dirJ2000p;
    return true;
  }
  tdb = MVDirection(0.0);
  return false;
}

bool MCFrame::getB1950Long(double &tdb) {
  if (myf.direction()) {
    if (!b1950Longp) {
      b1950Longp = new Vector<double>(2);
      dirB1950p = new MVDirection;
      *dirB1950p = static_cast<MDirection::Convert *>(dirConvB1950)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *b1950Longp = dirB1950p->get();
    }
    tdb = b1950Longp->operator()(0);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getB1950Lat(double &tdb) {
  if (myf.direction()) {
    if (!b1950Longp) {
      b1950Longp = new Vector<double>(2);
      dirB1950p = new MVDirection;
      *dirB1950p = static_cast<MDirection::Convert *>(dirConvB1950)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *b1950Longp = dirB1950p->get();
    }
    tdb = b1950Longp->operator()(1);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getB1950(MVDirection &tdb) {
  if (myf.direction()) {
    if (!b1950Longp) {
      b1950Longp = new Vector<double>(2);
      dirB1950p = new MVDirection;
      *dirB1950p = static_cast<MDirection::Convert *>(dirConvB1950)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *b1950Longp = dirB1950p->get();
    }
    tdb = *dirB1950p;
    return true;
  }
  tdb = MVDirection(0.0);
  return false;
}

bool MCFrame::getAppLong(double &tdb) {
  if (myf.direction()) {
    if (!appLongp) {
      appLongp = new Vector<double>(2);
      dirAppp = new MVDirection;
      *dirAppp = static_cast<MDirection::Convert *>(dirConvApp)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *appLongp = dirAppp->get();
    }
    tdb = appLongp->operator()(0);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getAppLat(double &tdb) {
  if (myf.direction()) {
    if (!appLongp) {
      appLongp = new Vector<double>(2);
      dirAppp = new MVDirection;
      *dirAppp = static_cast<MDirection::Convert *>(dirConvApp)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *appLongp = dirAppp->get();
    }
    tdb = appLongp->operator()(1);
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getApp(MVDirection &tdb) {
  if (myf.direction()) {
    if (!appLongp) {
      appLongp = new Vector<double>(2);
      dirAppp = new MVDirection;
      *dirAppp = static_cast<MDirection::Convert *>(dirConvApp)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *appLongp = dirAppp->get();
    }
    tdb = *dirAppp;
    return true;
  }
  tdb = MVDirection(0.0);
  return false;
}

bool MCFrame::getLSR(double &tdb) {
  if (myf.radialVelocity()) {
    if (!radLSRp) {
      radLSRp = new double;
      *radLSRp = static_cast<MRadialVelocity::Convert *>(radConvLSR)->operator()
	(*dynamic_cast<const MVRadialVelocity *const>(myf.radialVelocity()->
						      getData())).
	getValue();
    }
    tdb = *radLSRp;
    return true;
  }
  tdb = 0.0;
  return false;
}

bool MCFrame::getCometType(uint32_t &tdb) {
  if (myf.comet()) {
    tdb = static_cast<uint32_t>(myf.comet()->getType());
    return true;
  }
  tdb = 0;
  return false;
}

bool MCFrame::getComet(MVPosition &tdb) {
  if (myf.comet()) {
    double x(0);
    if (getTDB(x) && myf.comet()->get(tdb, x)) return true;
  }
  tdb = MVPosition(0.0);
  return false;
}

void MCFrame::makeEpoch() {
  static const MEpoch::Ref REFTDB = MEpoch::Ref(MEpoch::TDB);
  static const MEpoch::Ref REFUT1 = MEpoch::Ref(MEpoch::UT1);
  static const MEpoch::Ref REFTT  = MEpoch::Ref(MEpoch::TT);
  delete static_cast<MEpoch::Convert *>(epConvTDB);
  delete static_cast<MEpoch::Convert *>(epConvUT1);
  delete static_cast<MEpoch::Convert *>(epConvTT);
  epConvTDB = new MEpoch::Convert(*(myf.epoch()), REFTDB);
  epConvUT1 = new MEpoch::Convert(*(myf.epoch()), REFUT1);
  epConvTT  = new MEpoch::Convert(*(myf.epoch()), REFTT);
  uint32_t locker = 0;			// locking assurance
  if (epTDBp) {
    delete epTDBp; epTDBp = 0;
  }
  if (epUT1p) {
    delete epUT1p; epUT1p = 0;
  }
  if (epTTp) {
    delete epTTp; epTTp = 0;
  }
  myf.lock(locker);
  if (epConvLAST) {
    delete static_cast<MEpoch::Convert *>(epConvLAST);
    epConvLAST = 0;
  }
  epConvLAST = new MEpoch::Convert(*(myf.epoch()),
				   MEpoch::Ref(MEpoch::LAST, this->myf));
  myf.unlock(locker);
  if (epLASTp) {
    delete epLASTp; epLASTp = 0;
  }
  if (appLongp) {
    delete appLongp; appLongp = 0;
    delete dirAppp; dirAppp = 0;
  }
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  }
}

void MCFrame::makePosition() {
  static const MPosition::Ref REFLONG 
    = MPosition::Ref(MPosition::ITRF);
  delete static_cast<MPosition::Convert *>(posConvLong);
  posConvLong = new MPosition::Convert(*(myf.position()),
				       REFLONG);
  if (posLongp) {
    delete posLongp; posLongp = 0;
    delete posITRFp; posITRFp = 0;
  }
  if (epLASTp) {
    delete epLASTp; epLASTp = 0;
  }
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  }
  static const MPosition::Ref REFGEO
    = MPosition::Ref(MPosition::WGS84);
  delete static_cast<MPosition::Convert *>(posConvLongGeo);
  posConvLongGeo = new MPosition::Convert(*(myf.position()),
					  REFGEO);
  if (posLongGeop) {
    delete posLongGeop; posLongGeop = 0;
    delete posGeop; posGeop = 0;
  }
}

void MCFrame::makeDirection() {
  static const MDirection::Ref REFJ2000 = MDirection::Ref(MDirection::J2000);
  uint32_t locker =0;
  myf.lock(locker);
  if (dirConvJ2000) {
    delete static_cast<MDirection::Convert *>(dirConvJ2000);
    dirConvJ2000 = 0;
  }
  dirConvJ2000 = new MDirection::Convert(*(myf.direction()),
					 MDirection::Ref(MDirection::J2000,
							 this->myf));
  myf.unlock(locker);

  static const MDirection::Ref REFB1950 = MDirection::Ref(MDirection::B1950);
  myf.lock(locker);
  if (dirConvB1950) {
    delete static_cast<MDirection::Convert *>(dirConvB1950);
    dirConvB1950 = 0;
  }
  dirConvB1950 = new MDirection::Convert(*(myf.direction()),
					 MDirection::Ref(MDirection::B1950,
							 this->myf));
  myf.unlock(locker);
  myf.lock(locker);
  if (dirConvApp) {
    delete static_cast<MDirection::Convert *>(dirConvApp);
    dirConvApp = 0;
  }
  dirConvApp = new MDirection::Convert(*(myf.direction()),
				       MDirection::Ref(MDirection::APP,
						       this->myf));
  myf.unlock(locker);
  if (j2000Longp) {
    delete j2000Longp; j2000Longp = 0;
    delete dirJ2000p; dirJ2000p = 0;
  }
  if (b1950Longp) {
    delete b1950Longp; b1950Longp = 0;
    delete dirB1950p; dirB1950p = 0;
  }
  if (appLongp) {
    delete appLongp; appLongp = 0;
    delete dirAppp; dirAppp = 0;
  }
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  }
}

void MCFrame::makeRadialVelocity() {
  static const MRadialVelocity::Ref REFLSR 
    = MRadialVelocity::Ref(MRadialVelocity::LSRK);
  delete static_cast<MRadialVelocity::Convert *>(radConvLSR);
  radConvLSR = new MRadialVelocity::Convert(*(myf.radialVelocity()),
					    REFLSR);
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  }
}

void MCFrame::makeComet() {;}

} //# NAMESPACE CASACORE - END

