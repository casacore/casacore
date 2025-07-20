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
//#        Internet email: casa-feedback@nrao.edu.
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

MCFrame::MCFrame(const MCFrame &other) = default;
MCFrame::MCFrame(MCFrame &&other) = default;
MCFrame::~MCFrame() = default;

void MCFrame::resetEpoch() {
    epTDBp.reset();
    epUT1p.reset();
    epTTp.reset();
    epLASTp.reset();
    appLongp = casacore::Vector<Double>();
    dirAppp.reset();
    radLSRp.reset();
}

void MCFrame::resetPosition() {
  if (posLongp.empty()) {
    posLongp = casacore::Vector<Double>();
    posITRFp.reset();
    posLongGeop = casacore::Vector<Double>();
    posGeop.reset();
  }
  if (epLASTp) {
    epLASTp.reset();
  }
}

void MCFrame::resetDirection() {
  if (j2000Longp.empty()) {
    j2000Longp = casacore::Vector<Double>();
    dirJ2000p.reset();
  }
  if (b1950Longp.empty()) {
    b1950Longp = casacore::Vector<Double>();
    dirB1950p.reset();
  }
  if (appLongp.empty()) {
    appLongp = casacore::Vector<Double>();
    dirAppp.reset();
  }
  radLSRp.reset();
}

void MCFrame::resetRadialVelocity() {
  radLSRp.reset();
}

void MCFrame::resetComet() {
}

Bool MCFrame::getTDB(Double &tdb, const MeasFrame& myf) {
  if (myf.epoch()) {
    if (!epTDBp) {
      epTDBp = epConvTDB->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    }
    tdb = *epTDBp;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getUT1(Double &tdb, const MeasFrame& myf) {
  if (myf.epoch()) {
    if (!epUT1p) {
      epUT1p = epConvUT1->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    }
    tdb = *epUT1p;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getTT(Double &tdb, const MeasFrame& myf) {
  if (myf.epoch()) {
    if (!epTTp) {
      epTTp = epConvTT->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    }
    tdb = *epTTp;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLong(Double &tdb, const MeasFrame& myf) {
  if (myf.position()) {
    if (posLongp.empty()) {
      posITRFp.reset(new MVPosition);
      *posITRFp = posConvLong->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      posLongp = posITRFp->get();
    }
    tdb = MVAngle(posLongp(1))(-0.5);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLat(Double &tdb, const MeasFrame& myf) {
  if (myf.position()) {
    if (posLongp.empty()) {
      posITRFp.reset(new MVPosition);
      *posITRFp = posConvLong->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      posLongp = posITRFp->get();
    }
    tdb = posLongp(2);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLatGeo(Double &tdb, const MeasFrame& myf) {
  if (myf.position()) {
    if (posLongGeop.empty()) {
      posGeop.reset(new MVPosition);
      *posGeop = posConvLongGeo->operator()
        (*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
        getValue();
      posLongGeop = posGeop->get();
    }
    tdb = posLongGeop(2);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getITRF(MVPosition &tdb, const MeasFrame& myf) {
  if (myf.position()) {
    if (posLongp.empty()) {
      posITRFp.reset(new MVPosition);
      *posITRFp = posConvLong->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      posLongp = posITRFp->get();
    }
    tdb = *posITRFp;
    return True;
  }
  tdb = MVPosition(0.0);
  return False;
}

Bool MCFrame::getRadius(Double &tdb, const MeasFrame& myf) {
  if (myf.position()) {
    if (posLongp.empty()) {
      posITRFp.reset(new MVPosition);
      *posITRFp = posConvLong->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      posLongp = posITRFp->get();
    }
    tdb = posLongp(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLAST(Double &tdb, const MeasFrame& myf) {
  if (myf.epoch()) {
    if (!epLASTp) {
      epLASTp = epConvLAST->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    }
    tdb = fmod(*epLASTp, 1.0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLASTr(Double &tdb, const MeasFrame& myf) {
  Bool tmp = MCFrame::getLAST(tdb, myf);
  tdb *= C::circle;
  return tmp;
}

Bool MCFrame::getJ2000Long(Double &tdb, const MeasFrame& myf) {
  if (myf.direction()) {
    if (j2000Longp.empty()) {
      dirJ2000p.reset(new MVDirection);
      *dirJ2000p = dirConvJ2000->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      j2000Longp = dirJ2000p->get();
    }
    tdb = j2000Longp(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getJ2000Lat(Double &tdb, const MeasFrame& myf) {
  if (myf.direction()) {
    if (j2000Longp.empty()) {
      dirJ2000p.reset(new MVDirection);
      *dirJ2000p = dirConvJ2000->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      j2000Longp = dirJ2000p->get();
    }
    tdb = j2000Longp(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getJ2000(MVDirection &tdb, const MeasFrame& myf) {
  if (myf.direction()) {
    if (j2000Longp.empty()) {
      dirJ2000p.reset(new MVDirection);
      *dirJ2000p = dirConvJ2000->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      j2000Longp = dirJ2000p->get();
    }
    tdb = *dirJ2000p;
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getB1950Long(Double &tdb, const MeasFrame& myf) {
  if (myf.direction()) {
    if (b1950Longp.empty()) {
      dirB1950p.reset(new MVDirection);
      *dirB1950p = dirConvB1950->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      b1950Longp = dirB1950p->get();
    }
    tdb = b1950Longp(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getB1950Lat(Double &tdb, const MeasFrame& myf) {
  if (myf.direction()) {
    if (b1950Longp.empty()) {
      dirB1950p.reset(new MVDirection);
      *dirB1950p = dirConvB1950->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      b1950Longp = dirB1950p->get();
    }
    tdb = b1950Longp(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getB1950(MVDirection &tdb, const MeasFrame& myf) {
  if (myf.direction()) {
    if (b1950Longp.empty()) {
      dirB1950p.reset(new MVDirection);
      *dirB1950p = dirConvB1950->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      b1950Longp = dirB1950p->get();
    }
    tdb = *dirB1950p;
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getAppLong(Double &tdb, const MeasFrame& myf) {
  if (myf.direction()) {
    if (appLongp.empty()) {
      dirAppp.reset(new MVDirection);
      *dirAppp = dirConvApp->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      appLongp = dirAppp->get();
    }
    tdb = appLongp(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getAppLat(Double &tdb, const MeasFrame& myf) {
  if (myf.direction()) {
    if (appLongp.empty()) {
      dirAppp.reset(new MVDirection);
      *dirAppp = dirConvApp->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      appLongp = dirAppp->get();
    }
    tdb = appLongp(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getApp(MVDirection &tdb, const MeasFrame& myf) {
  if (myf.direction()) {
    if (appLongp.empty()) {
      dirAppp.reset(new MVDirection);
      *dirAppp = dirConvApp->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      appLongp = dirAppp->get();
    }
    tdb = *dirAppp;
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getLSR(Double &tdb, const MeasFrame& myf) {
  if (myf.radialVelocity()) {
    if (!radLSRp) {
      radLSRp = radConvLSR->operator()
	(*dynamic_cast<const MVRadialVelocity *const>(myf.radialVelocity()->
						      getData())).
	getValue();
    }
    tdb = *radLSRp;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getCometType(uInt &tdb, const MeasFrame& myf) {
  if (myf.comet()) {
    tdb = static_cast<uInt>(myf.comet()->getType());
    return True;
  }
  tdb = 0;
  return False;
}

Bool MCFrame::getComet(MVPosition &tdb, const MeasFrame& myf) {
  if (myf.comet()) {
    Double x(0);
    if (getTDB(x, myf) && myf.comet()->get(tdb, x)) return True;
  }
  tdb = MVPosition(0.0);
  return False;
}

void MCFrame::makeEpoch(const MeasFrame& myf) {
  const MEpoch::Ref REFTDB = MEpoch::Ref(MEpoch::TDB);
  const MEpoch::Ref REFUT1 = MEpoch::Ref(MEpoch::UT1);
  const MEpoch::Ref REFTT  = MEpoch::Ref(MEpoch::TT);
  epConvTDB = new MEpoch::Convert(*(myf.epoch()), REFTDB);
  epConvUT1 = new MEpoch::Convert(*(myf.epoch()), REFUT1);
  epConvTT  = new MEpoch::Convert(*(myf.epoch()), REFTT);
  epTDBp.reset();
  epUT1p.reset();
  epTTp.reset();
  epConvLAST.reset();
  epConvLAST = new MEpoch::Convert(*(myf.epoch()),
				   MEpoch::Ref(MEpoch::LAST, myf));
  epLASTp.reset();
  if (!appLongp.empty()) {
    appLongp = casacore::Vector<Double>();
    dirAppp.reset();
  }
  radLSRp.reset();
}

void MCFrame::makePosition(const MeasFrame& myf) {
  static const MPosition::Ref REFLONG 
    = MPosition::Ref(MPosition::ITRF);
  posConvLong = new MPosition::Convert(*(myf.position()),
				       REFLONG);
  if (!posLongp.empty()) {
    posLongp = casacore::Vector<Double>();
    posITRFp.reset();
  }
  epLASTp.reset();
  radLSRp.reset();
  static const MPosition::Ref REFGEO
    = MPosition::Ref(MPosition::WGS84);
  posConvLongGeo = new MPosition::Convert(*(myf.position()),
					  REFGEO);
  if (!posLongGeop.empty()) {
    posLongGeop = casacore::Vector<Double>();
    posGeop.reset();
  }
}

void MCFrame::makeDirection(const MeasFrame& myf) {
  static const MDirection::Ref REFJ2000 = MDirection::Ref(MDirection::J2000);
  dirConvJ2000 = new MDirection::Convert(*(myf.direction()),
					 MDirection::Ref(MDirection::J2000,
							 myf));

  static const MDirection::Ref REFB1950 = MDirection::Ref(MDirection::B1950);
  dirConvB1950 = new MDirection::Convert(*(myf.direction()),
					 MDirection::Ref(MDirection::B1950,
							 myf));
  dirConvApp = new MDirection::Convert(*(myf.direction()),
				       MDirection::Ref(MDirection::APP,
						       myf));
  if (!j2000Longp.empty()) {
    j2000Longp = casacore::Vector<Double>();
    dirJ2000p.reset();
  }
  if (!b1950Longp.empty()) {
    b1950Longp = casacore::Vector<Double>();
    dirB1950p.reset();
  }
  if (!appLongp.empty()) {
    appLongp = casacore::Vector<Double>();
    dirAppp.reset();
  }
  if (radLSRp) {
    radLSRp.reset();
  }
}

void MCFrame::makeRadialVelocity(const MeasFrame& myf) {
  static const MRadialVelocity::Ref REFLSR 
    = MRadialVelocity::Ref(MRadialVelocity::LSRK);
  radConvLSR = new MRadialVelocity::Convert(*(myf.radialVelocity()),
					    REFLSR);
  radLSRp.reset();
}

void MCFrame::makeComet() {;}

} //# NAMESPACE CASACORE - END

