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
//#
//# $Id$

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
MCFrame::MCFrame(std::weak_ptr<MeasFrame::FrameRep> inf) :
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
    delete epTDBp; epTDBp = nullptr;
    delete epUT1p; epUT1p = nullptr;
    delete epTTp; epTTp = nullptr;
    delete epLASTp; epLASTp = nullptr;
    delete appLongp; appLongp = nullptr;
    delete dirAppp; dirAppp = nullptr;
    delete radLSRp; radLSRp = nullptr;
}

void MCFrame::resetPosition() {
  if (posLongp) {
    delete posLongp; posLongp = nullptr;
    delete posITRFp; posITRFp = nullptr;
    delete posLongGeop; posLongGeop = nullptr;
    delete posGeop; posGeop = nullptr;
  }
  if (epLASTp) {
    delete epLASTp; epLASTp = nullptr;
  }
}

void MCFrame::resetDirection() {
  if (j2000Longp) {
    delete j2000Longp; j2000Longp = nullptr;
    delete dirJ2000p; dirJ2000p = nullptr;
  }
  if (b1950Longp) {
    delete b1950Longp; b1950Longp = nullptr;
    delete dirB1950p; dirB1950p = nullptr;
  }
  if (appLongp) {
    delete appLongp; appLongp = nullptr;
    delete dirAppp; dirAppp = nullptr;
  }
  if (radLSRp) {
    delete radLSRp; radLSRp = nullptr;
  }
}

void MCFrame::resetRadialVelocity() {
  if (radLSRp) {
    delete radLSRp; radLSRp = nullptr;
  }
}

void MCFrame::resetComet() {
}

Bool MCFrame::getTDB(Double &tdb) {
  std::unique_ptr<casacore::Measure>& epoch = myf.lock()->epval;
  if (epoch) {
    if (!epTDBp) {
      epTDBp = new Double;
      *epTDBp = static_cast<MEpoch::Convert *>(epConvTDB)->operator()
	(*dynamic_cast<const MVEpoch *const>(epoch->getData())).
	getValue().get();
    }
    tdb = *epTDBp;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getUT1(Double &tdb) {
  std::unique_ptr<casacore::Measure>& epoch = myf.lock()->epval;
  if (epoch) {
    if (!epUT1p) {
      epUT1p = new Double;
      *epUT1p = static_cast<MEpoch::Convert *>(epConvUT1)->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.lock()->epval->getData())).
	getValue().get();
    }
    tdb = *epUT1p;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getTT(Double &tdb) {
  std::unique_ptr<casacore::Measure>& epoch = myf.lock()->epval;
  if (epoch) {
    if (!epTTp) {
      epTTp = new Double;
      *epTTp = static_cast<MEpoch::Convert *>(epConvTT)->operator()
	(*dynamic_cast<const MVEpoch *const>(epoch->getData())).
	getValue().get();
    }
    tdb = *epTTp;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLong(Double &tdb) {
  std::unique_ptr<casacore::Measure>& position = myf.lock()->posval;
  if (position) {
    if (!posLongp) {
      posLongp = new Vector<Double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(position->getData())).
	getValue();
      *posLongp = posITRFp->get();
    }
    tdb = MVAngle(posLongp->operator()(1))(-0.5);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLat(Double &tdb) {
  std::unique_ptr<casacore::Measure>& position = myf.lock()->posval;
  if (position) {
    if (!posLongp) {
      posLongp = new Vector<Double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(position->getData())).
	getValue();
      *posLongp = posITRFp->get();
    }
    tdb = posLongp->operator()(2);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLatGeo(Double &tdb) {
  std::unique_ptr<casacore::Measure>& position = myf.lock()->posval;
  if (position) {
    if (!posLongGeop) {
      posLongGeop = new Vector<Double>(3);
      posGeop = new MVPosition;
      *posGeop = static_cast<MPosition::Convert *>(posConvLongGeo)->operator()
        (*dynamic_cast<const MVPosition *const>(position->getData())).
        getValue();
      *posLongGeop = posGeop->get();
    }
    tdb = posLongGeop->operator()(2);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getITRF(MVPosition &tdb) {
  std::unique_ptr<casacore::Measure>& position = myf.lock()->posval;
  if (position) {
    if (!posLongp) {
      posLongp = new Vector<Double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(position->getData())).
	getValue();
      *posLongp = posITRFp->get();
    }
    tdb = *posITRFp;
    return True;
  }
  tdb = MVPosition(0.0);
  return False;
}

Bool MCFrame::getRadius(Double &tdb) {
  std::unique_ptr<casacore::Measure>& position = myf.lock()->posval;
  if (position) {
    if (!posLongp) {
      posLongp = new Vector<Double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(position->getData())).
	getValue();
      *posLongp = posITRFp->get();
    }
    tdb = posLongp->operator()(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLAST(Double &tdb) {
  std::unique_ptr<casacore::Measure>& epoch = myf.lock()->epval;
  if (epoch) {
    if (!epLASTp) {
      epLASTp = new Double;
      *epLASTp = static_cast<MEpoch::Convert *>(epConvLAST)->operator()
	(*dynamic_cast<const MVEpoch *const>(epoch->getData())).
	getValue().get();
    }
    tdb = fmod(*epLASTp, 1.0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLASTr(Double &tdb) {
  Bool tmp = MCFrame::getLAST(tdb);
  tdb *= C::circle;
  return tmp;
}

Bool MCFrame::getJ2000Long(Double &tdb) {
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  if (direction) {
    if (!j2000Longp) {
      j2000Longp = new Vector<Double>(2);
      dirJ2000p = new MVDirection;
      *dirJ2000p = static_cast<MDirection::Convert *>(dirConvJ2000)->operator()
	(*dynamic_cast<const MVDirection *const>(direction->getData())).
	getValue();
      *j2000Longp = dirJ2000p->get();
    }
    tdb = j2000Longp->operator()(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getJ2000Lat(Double &tdb) {
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  if (direction) {
    if (!j2000Longp) {
      j2000Longp = new Vector<Double>(2);
      dirJ2000p = new MVDirection;
      *dirJ2000p = static_cast<MDirection::Convert *>(dirConvJ2000)->operator()
	(*dynamic_cast<const MVDirection *const>(direction->getData())).
	getValue();
      *j2000Longp = dirJ2000p->get();
    }
    tdb = j2000Longp->operator()(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getJ2000(MVDirection &tdb) {
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  if (direction) {
    if (!j2000Longp) {
      j2000Longp = new Vector<Double>(2);
      dirJ2000p = new MVDirection;
      *dirJ2000p = static_cast<MDirection::Convert *>(dirConvJ2000)->operator()
	(*dynamic_cast<const MVDirection *const>(direction->getData())).
	getValue();
      *j2000Longp = dirJ2000p->get();
    }
    tdb = *dirJ2000p;
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getB1950Long(Double &tdb) {
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  if (direction) {
    if (!b1950Longp) {
      b1950Longp = new Vector<Double>(2);
      dirB1950p = new MVDirection;
      *dirB1950p = static_cast<MDirection::Convert *>(dirConvB1950)->operator()
	(*dynamic_cast<const MVDirection *const>(direction->getData())).
	getValue();
      *b1950Longp = dirB1950p->get();
    }
    tdb = b1950Longp->operator()(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getB1950Lat(Double &tdb) {
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  if (direction) {
    if (!b1950Longp) {
      b1950Longp = new Vector<Double>(2);
      dirB1950p = new MVDirection;
      *dirB1950p = static_cast<MDirection::Convert *>(dirConvB1950)->operator()
	(*dynamic_cast<const MVDirection *const>(direction->getData())).
	getValue();
      *b1950Longp = dirB1950p->get();
    }
    tdb = b1950Longp->operator()(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getB1950(MVDirection &tdb) {
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  if (direction) {
    if (!b1950Longp) {
      b1950Longp = new Vector<Double>(2);
      dirB1950p = new MVDirection;
      *dirB1950p = static_cast<MDirection::Convert *>(dirConvB1950)->operator()
	(*dynamic_cast<const MVDirection *const>(direction->getData())).
	getValue();
      *b1950Longp = dirB1950p->get();
    }
    tdb = *dirB1950p;
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getAppLong(Double &tdb) {
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  if (direction) {
    if (!appLongp) {
      appLongp = new Vector<Double>(2);
      dirAppp = new MVDirection;
      *dirAppp = static_cast<MDirection::Convert *>(dirConvApp)->operator()
	(*dynamic_cast<const MVDirection *const>(direction->getData())).
	getValue();
      *appLongp = dirAppp->get();
    }
    tdb = appLongp->operator()(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getAppLat(Double &tdb) {
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  if (direction) {
    if (!appLongp) {
      appLongp = new Vector<Double>(2);
      dirAppp = new MVDirection;
      *dirAppp = static_cast<MDirection::Convert *>(dirConvApp)->operator()
	(*dynamic_cast<const MVDirection *const>(direction->getData())).
	getValue();
      *appLongp = dirAppp->get();
    }
    tdb = appLongp->operator()(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getApp(MVDirection &tdb) {
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  if (direction) {
    if (!appLongp) {
      appLongp = new Vector<Double>(2);
      dirAppp = new MVDirection;
      *dirAppp = static_cast<MDirection::Convert *>(dirConvApp)->operator()
	(*dynamic_cast<const MVDirection *const>(direction->getData())).
	getValue();
      *appLongp = dirAppp->get();
    }
    tdb = *dirAppp;
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getLSR(Double &tdb) {
  std::unique_ptr<casacore::Measure>& radialVelocity = myf.lock()->radval;
  if (radialVelocity) {
    if (!radLSRp) {
      radLSRp = new Double;
      *radLSRp = static_cast<MRadialVelocity::Convert *>(radConvLSR)->operator()
	(*dynamic_cast<const MVRadialVelocity *const>(radialVelocity->
						      getData())).
	getValue();
    }
    tdb = *radLSRp;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getCometType(uInt &tdb) {
  std::unique_ptr<casacore::MeasComet>& comet = myf.lock()->comval;
  if (comet) {
    tdb = static_cast<uInt>(comet->getType());
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getComet(MVPosition &tdb) {
  std::unique_ptr<casacore::MeasComet>& comet = myf.lock()->comval;
  if (comet) {
    Double x(0);
    if (getTDB(x) && comet->get(tdb, x)) return True;
  }
  tdb = MVPosition(0.0);
  return False;
}

void MCFrame::makeEpoch() {
  static const MEpoch::Ref REFTDB = MEpoch::Ref(MEpoch::TDB);
  static const MEpoch::Ref REFUT1 = MEpoch::Ref(MEpoch::UT1);
  static const MEpoch::Ref REFTT  = MEpoch::Ref(MEpoch::TT);
  delete static_cast<MEpoch::Convert *>(epConvTDB);
  delete static_cast<MEpoch::Convert *>(epConvUT1);
  delete static_cast<MEpoch::Convert *>(epConvTT);
  std::unique_ptr<casacore::Measure>& epoch = myf.lock()->epval;
  epConvTDB = new MEpoch::Convert(*epoch, REFTDB);
  epConvUT1 = new MEpoch::Convert(*epoch, REFUT1);
  epConvTT  = new MEpoch::Convert(*epoch, REFTT);
  if (epTDBp) {
    delete epTDBp; epTDBp = nullptr;
  }
  if (epUT1p) {
    delete epUT1p; epUT1p = nullptr;
  }
  if (epTTp) {
    delete epTTp; epTTp = nullptr;
  }
  if (epConvLAST) {
    delete static_cast<MEpoch::Convert *>(epConvLAST);
    epConvLAST = nullptr;
  }
  epConvLAST = new MEpoch::Convert(*(epoch),
				   MEpoch::Ref(MEpoch::LAST, MeasFrame(this->myf.lock())));
  if (epLASTp) {
    delete epLASTp; epLASTp = nullptr;
  }
  if (appLongp) {
    delete appLongp; appLongp = nullptr;
    delete dirAppp; dirAppp = nullptr;
  }
  if (radLSRp) {
    delete radLSRp; radLSRp = nullptr;
  }
}

void MCFrame::makePosition() {
  static const MPosition::Ref REFLONG 
    = MPosition::Ref(MPosition::ITRF);
  delete static_cast<MPosition::Convert *>(posConvLong);
  std::unique_ptr<casacore::Measure>& position = myf.lock()->posval;
  posConvLong = new MPosition::Convert(*position,
				       REFLONG);
  if (posLongp) {
    delete posLongp; posLongp = nullptr;
    delete posITRFp; posITRFp = nullptr;
  }
  if (epLASTp) {
    delete epLASTp; epLASTp = nullptr;
  }
  if (radLSRp) {
    delete radLSRp; radLSRp = nullptr;
  }
  static const MPosition::Ref REFGEO
    = MPosition::Ref(MPosition::WGS84);
  delete static_cast<MPosition::Convert *>(posConvLongGeo);
  posConvLongGeo = new MPosition::Convert(*position,
					  REFGEO);
  if (posLongGeop) {
    delete posLongGeop; posLongGeop = nullptr;
    delete posGeop; posGeop = nullptr;
  }
}

void MCFrame::makeDirection() {
  static const MDirection::Ref REFJ2000 = MDirection::Ref(MDirection::J2000);
  if (dirConvJ2000) {
    delete static_cast<MDirection::Convert *>(dirConvJ2000);
    dirConvJ2000 = nullptr;
  }
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  dirConvJ2000 = new MDirection::Convert(*direction,
					 MDirection::Ref(MDirection::J2000,
							 MeasFrame(this->myf.lock())));

  static const MDirection::Ref REFB1950 = MDirection::Ref(MDirection::B1950);
  if (dirConvB1950) {
    delete static_cast<MDirection::Convert *>(dirConvB1950);
    dirConvB1950 = nullptr;
  }
  dirConvB1950 = new MDirection::Convert(*direction,
					 MDirection::Ref(MDirection::B1950,
							 MeasFrame(this->myf.lock())));
  if (dirConvApp) {
    delete static_cast<MDirection::Convert *>(dirConvApp);
    dirConvApp = nullptr;
  }
  dirConvApp = new MDirection::Convert(*direction,
				       MDirection::Ref(MDirection::APP,
						       MeasFrame(this->myf.lock())));
  if (j2000Longp) {
    delete j2000Longp; j2000Longp = nullptr;
    delete dirJ2000p; dirJ2000p = nullptr;
  }
  if (b1950Longp) {
    delete b1950Longp; b1950Longp = nullptr;
    delete dirB1950p; dirB1950p = nullptr;
  }
  if (appLongp) {
    delete appLongp; appLongp = nullptr;
    delete dirAppp; dirAppp = nullptr;
  }
  if (radLSRp) {
    delete radLSRp; radLSRp = nullptr;
  }
}

void MCFrame::makeRadialVelocity() {
  static const MRadialVelocity::Ref REFLSR 
    = MRadialVelocity::Ref(MRadialVelocity::LSRK);
  delete static_cast<MRadialVelocity::Convert *>(radConvLSR);
  std::unique_ptr<casacore::Measure>& radialVelocity = myf.lock()->radval;
  radConvLSR = new MRadialVelocity::Convert(*radialVelocity,
					    REFLSR);
  if (radLSRp) {
    delete radLSRp; radLSRp = nullptr;
  }
}

void MCFrame::makeComet() {;}

} //# NAMESPACE CASACORE - END

