//# MCFrame.cc: Measure frame calculations proxy
//# Copyright (C) 1996,1997,1998,1999,2000,2002,2003
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
#include <casa/Exceptions/Error.h>
#include <casa/BasicMath/Math.h>
#include <casa/BasicSL/Constants.h>
#include <casa/Quanta/MVAngle.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MCEpoch.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MCPosition.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MCDirection.h>
#include <measures/Measures/MRadialVelocity.h>
#include <measures/Measures/MCRadialVelocity.h>
#include <measures/Measures/MCFrame.h>
#include <measures/Measures/MeasConvert.h>
#include <measures/Measures/MeasComet.h>

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
  radConvLSR(0), radLSRp(0) {
    myf.setMCFramePoint(static_cast<void *>(this));
    myf.setMCFrameDelete(MCFrameDelete);
    myf.setMCFrameGetdbl(MCFrameGetdbl);
    myf.setMCFrameGetmvdir(MCFrameGetmvdir);
    myf.setMCFrameGetmvpos(MCFrameGetmvpos);
    myf.setMCFrameGetuint(MCFrameGetuint);
    create();
    myf.unlock();
}

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

void MCFrame::make(MeasFrame &in) {
  if (!in.empty() && !in.getMCFramePoint()) {
    MCFrame *tmp = new MCFrame(in);
    if (!tmp) {};		// to stop compiler warnings
  };
  if (!in.empty()) {
    (static_cast<MCFrame *>(in.getMCFramePoint()))->create();
  };
}

void MCFrame::resetEpoch() {
    delete epTDBp; epTDBp = 0;
    delete epUT1p; epUT1p = 0;
    delete epTTp; epTTp = 0;
    delete epLASTp; epLASTp = 0;
    delete dirAppp; dirAppp = 0;
    delete radLSRp; radLSRp = 0;
}

void MCFrame::resetPosition() {
  if (posLongp) {
    delete posLongp; posLongp = 0;
    delete posITRFp; posITRFp = 0;
    delete posLongGeop; posLongGeop = 0;
    delete posGeop; posGeop = 0;
  };
  if (epLASTp) {
    delete epLASTp; epLASTp = 0;
  };
}

void MCFrame::resetDirection() {
  if (j2000Longp) {
    delete j2000Longp; j2000Longp = 0;
    delete dirJ2000p; dirJ2000p = 0;
  };
  if (b1950Longp) {
    delete b1950Longp; b1950Longp = 0;
    delete dirB1950p; dirB1950p = 0;
  };
  if (appLongp) {
    delete appLongp; appLongp = 0;
    delete dirAppp; dirAppp = 0;
  };
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  };
}

void MCFrame::resetRadialVelocity() {
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  };
}

void MCFrame::resetComet() {
}

Bool MCFrame::getTDB(Double &tdb) {
  if (myf.epoch()) {
    if (!epTDBp) {
      epTDBp = new Double;
      *epTDBp = static_cast<MEpoch::Convert *>(epConvTDB)->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    };
    tdb = *epTDBp;
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getUT1(Double &tdb) {
  if (myf.epoch()) {
    if (!epUT1p) {
      epUT1p = new Double;
      *epUT1p = static_cast<MEpoch::Convert *>(epConvUT1)->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    };
    tdb = *epUT1p;
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getTT(Double &tdb) {
  if (myf.epoch()) {
    if (!epTTp) {
      epTTp = new Double;
      *epTTp = static_cast<MEpoch::Convert *>(epConvTT)->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    };
    tdb = *epTTp;
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLong(Double &tdb) {
  if (myf.position()) {
    if (!posLongp) {
      posLongp = new Vector<Double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      *posLongp = posITRFp->get();
    };
    tdb = MVAngle(posLongp->operator()(1))(-0.5);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLat(Double &tdb) {
  if (myf.position()) {
    if (!posLongp) {
      posLongp = new Vector<Double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      *posLongp = posITRFp->get();
    };
    tdb = posLongp->operator()(2);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLatGeo(Double &tdb) {
  if (myf.position()) {
    if (!posLongGeop) {
      posLongGeop = new Vector<Double>(3);
      posGeop = new MVPosition;
      *posGeop = static_cast<MPosition::Convert *>(posConvLongGeo)->operator()
        (*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
        getValue();
      *posLongGeop = posGeop->get();
    };
    tdb = posLongGeop->operator()(2);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getITRF(MVPosition &tdb) {
  if (myf.position()) {
    if (!posLongp) {
      posLongp = new Vector<Double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      *posLongp = posITRFp->get();
    };
    tdb = *posITRFp;
    return True;
  };
  tdb = MVPosition(0.0);
  return False;
}

Bool MCFrame::getRadius(Double &tdb) {
  if (myf.position()) {
    if (!posLongp) {
      posLongp = new Vector<Double>(3);
      posITRFp = new MVPosition;
      *posITRFp = static_cast<MPosition::Convert *>(posConvLong)->operator()
	(*dynamic_cast<const MVPosition *const>(myf.position()->getData())).
	getValue();
      *posLongp = posITRFp->get();
    };
    tdb = posLongp->operator()(0);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLAST(Double &tdb) {
  if (myf.epoch()) {
    if (!epLASTp) {
      epLASTp = new Double;
      *epLASTp = static_cast<MEpoch::Convert *>(epConvLAST)->operator()
	(*dynamic_cast<const MVEpoch *const>(myf.epoch()->getData())).
	getValue().get();
    };
    tdb = fmod(*epLASTp, 1.0);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLASTr(Double &tdb) {
  Bool tmp = MCFrame::getLAST(tdb);
  tdb *= C::circle;
  return tmp;
}

Bool MCFrame::getJ2000Long(Double &tdb) {
  if (myf.direction()) {
    if (!j2000Longp) {
      j2000Longp = new Vector<Double>(2);
      dirJ2000p = new MVDirection;
      *dirJ2000p = static_cast<MDirection::Convert *>(dirConvJ2000)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *j2000Longp = dirJ2000p->get();
    };
    tdb = j2000Longp->operator()(0);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getJ2000Lat(Double &tdb) {
  if (myf.direction()) {
    if (!j2000Longp) {
      j2000Longp = new Vector<Double>(2);
      dirJ2000p = new MVDirection;
      *dirJ2000p = static_cast<MDirection::Convert *>(dirConvJ2000)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *j2000Longp = dirJ2000p->get();
    };
    tdb = j2000Longp->operator()(1);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getJ2000(MVDirection &tdb) {
  if (myf.direction()) {
    if (!j2000Longp) {
      j2000Longp = new Vector<Double>(2);
      dirJ2000p = new MVDirection;
      *dirJ2000p = static_cast<MDirection::Convert *>(dirConvJ2000)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *j2000Longp = dirJ2000p->get();
    };
    tdb = *dirJ2000p;
    return True;
  };
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getB1950Long(Double &tdb) {
  if (myf.direction()) {
    if (!b1950Longp) {
      b1950Longp = new Vector<Double>(2);
      dirB1950p = new MVDirection;
      *dirB1950p = static_cast<MDirection::Convert *>(dirConvB1950)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *b1950Longp = dirB1950p->get();
    };
    tdb = b1950Longp->operator()(0);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getB1950Lat(Double &tdb) {
  if (myf.direction()) {
    if (!b1950Longp) {
      b1950Longp = new Vector<Double>(2);
      dirB1950p = new MVDirection;
      *dirB1950p = static_cast<MDirection::Convert *>(dirConvB1950)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *b1950Longp = dirB1950p->get();
    };
    tdb = b1950Longp->operator()(1);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getB1950(MVDirection &tdb) {
  if (myf.direction()) {
    if (!dirB1950p) {
      dirB1950p = new MVDirection;
      *dirB1950p = static_cast<MDirection::Convert *>(dirConvB1950)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
    };
    tdb = *dirB1950p;
    return True;
  };
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getAppLong(Double &tdb) {
  if (myf.direction()) {
    if (!appLongp) {
      appLongp = new Vector<Double>(2);
      dirAppp = new MVDirection;
      *dirAppp = static_cast<MDirection::Convert *>(dirConvApp)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *appLongp = dirAppp->get();
    };
    tdb = appLongp->operator()(0);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getAppLat(Double &tdb) {
  if (myf.direction()) {
    if (!appLongp) {
      appLongp = new Vector<Double>(2);
      dirAppp = new MVDirection;
      *dirAppp = static_cast<MDirection::Convert *>(dirConvApp)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
      *appLongp = dirAppp->get();
    };
    tdb = appLongp->operator()(1);
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getApp(MVDirection &tdb) {
  if (myf.direction()) {
    if (!dirAppp) {
      dirAppp = new MVDirection;
      *dirAppp = static_cast<MDirection::Convert *>(dirConvApp)->operator()
	(*dynamic_cast<const MVDirection *const>(myf.direction()->getData())).
	getValue();
    };
    tdb = *dirAppp;
    return True;
  };
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getLSR(Double &tdb) {
  if (myf.radialVelocity()) {
    if (!radLSRp) {
      radLSRp = new Double;
      *radLSRp = static_cast<MRadialVelocity::Convert *>(radConvLSR)->operator()
	(*dynamic_cast<const MVRadialVelocity *const>(myf.radialVelocity()->
						      getData())).
	getValue();
    };
    tdb = *radLSRp;
    return True;
  };
  tdb = 0.0;
  return False;
}

Bool MCFrame::getCometType(uInt &tdb) {
  if (myf.comet()) {
    tdb = static_cast<uInt>(myf.comet()->getType());
    return True;
  };
  tdb = 0;
  return False;
}

Bool MCFrame::getComet(MVPosition &tdb) {
  if (myf.comet()) {
    Double x(0);
    if (getTDB(x) && myf.comet()->get(tdb, x)) return True;
  };
  tdb = MVPosition(0.0);
  return False;
}

void MCFrame::create() {
  if (myf.getEpset()) {
    makeEpoch();
    myf.setEpset(False);
    myf.setEpreset(False);
  };
  if (myf.getPosset()) {
    makePosition();
    myf.setPosset(False);
    myf.setPosreset(False);
  };
  if (myf.getDirset()) {
    makeDirection();
    myf.setDirset(False);
    myf.setDirreset(False);
  };
  if (myf.getRadset()) {
    makeRadialVelocity();
    myf.setRadset(False);
    myf.setRadreset(False);
  };
  if (myf.getComset()) {
    makeComet();
    myf.setComset(False);
    myf.setComreset(False);
  };
  if (myf.getEpreset()) {
    resetEpoch();
    myf.setEpreset(False);
  };
  if (myf.getPosreset()) {
    resetPosition();
    myf.setPosreset(False);
  };
  if (myf.getDirreset()) {
    resetDirection();
    myf.setDirreset(False);
  };
  if (myf.getRadreset()) {
    resetRadialVelocity();
    myf.setRadreset(False);
  };
  if (myf.getComreset()) {
    resetComet();
    myf.setComreset(False);
  };
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
  if (epTDBp) {
    delete epTDBp; epTDBp = 0;
  };
  if (epUT1p) {
    delete epUT1p; epUT1p = 0;
  };
  if (epTTp) {
    delete epTTp; epTTp = 0;
  };
 if (epConvLAST) {
    myf.lock();
    delete static_cast<MEpoch::Convert *>(epConvLAST);
    epConvLAST = 0;
  };
  epConvLAST = new MEpoch::Convert(*(myf.epoch()),
				   MEpoch::Ref(MEpoch::LAST, this->myf));
  if (epConvLAST) {
    myf.unlock();
  };
  if (epLASTp) {
    delete epLASTp; epLASTp = 0;
  };
  if (dirAppp) {
    delete dirAppp; dirAppp = 0;
  };
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  };
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
  };
  if (epLASTp) {
    delete epLASTp; epLASTp = 0;
  };
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  };
  static const MPosition::Ref REFGEO
    = MPosition::Ref(MPosition::WGS84);
  delete static_cast<MPosition::Convert *>(posConvLongGeo);
  posConvLongGeo = new MPosition::Convert(*(myf.position()),
					  REFGEO);
  if (posLongGeop) {
    delete posLongGeop; posLongGeop = 0;
    delete posGeop; posGeop = 0;
  };
}

void MCFrame::makeDirection() {
  static const MDirection::Ref REFJ2000
    = MDirection::Ref(MDirection::J2000);
  if (dirConvJ2000) {
    myf.lock();
    delete static_cast<MDirection::Convert *>(dirConvJ2000);
    dirConvJ2000 = 0;
  };
  dirConvJ2000 = new MDirection::Convert(*(myf.direction()),
					 MDirection::Ref(MDirection::J2000,
							 this->myf));
  if (dirConvJ2000) myf.unlock();

  static const MDirection::Ref REFB1950
    = MDirection::Ref(MDirection::B1950);
  if (dirConvB1950) {
    myf.lock();
    delete static_cast<MDirection::Convert *>(dirConvB1950);
    dirConvB1950 = 0;
  };
  dirConvB1950 = new MDirection::Convert(*(myf.direction()),
					 MDirection::Ref(MDirection::B1950,
							 this->myf));
  if (dirConvB1950) myf.unlock();

  if (dirConvApp) {
    myf.lock();
    delete static_cast<MDirection::Convert *>(dirConvApp);
    dirConvApp = 0;
  };
  dirConvApp = new MDirection::Convert(*(myf.direction()),
				       MDirection::Ref(MDirection::APP,
						       this->myf));
  if (dirConvApp) {
    myf.unlock();
  };
  if (j2000Longp) {
    delete j2000Longp; j2000Longp = 0;
    delete dirJ2000p; dirJ2000p = 0;
  };
  if (b1950Longp) {
    delete b1950Longp; b1950Longp = 0;
    delete dirB1950p; dirB1950p = 0;
  };
  if (appLongp) {
    delete appLongp; appLongp = 0;
  };
  if (dirAppp) {
    delete dirAppp; dirAppp = 0;
  };
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  };
}

void MCFrame::makeRadialVelocity() {
  static const MRadialVelocity::Ref REFLSR 
    = MRadialVelocity::Ref(MRadialVelocity::LSRK);
  delete static_cast<MRadialVelocity::Convert *>(radConvLSR);
  radConvLSR = new MRadialVelocity::Convert(*(myf.radialVelocity()),
					    REFLSR);
  if (radLSRp) {
    delete radLSRp; radLSRp = 0;
  };
}

void MCFrame::makeComet() {
}

void MCFrameDelete(void *dmf) {
  delete static_cast<MCFrame *>(dmf);
}

Bool MCFrameGetdbl(void *dmf, uInt tp, Double &result) {
  try {
    switch (tp) {
      
    case MeasFrame::GetTDB:
      return static_cast<MCFrame *>(dmf)->getTDB(result);
      break;
      
    case MeasFrame::GetUT1:
      return static_cast<MCFrame *>(dmf)->getUT1(result);
      break;
       
    case MeasFrame::GetTT:
      return static_cast<MCFrame *>(dmf)->getTT(result);
      break;
       
    case MeasFrame::GetLong:
      return static_cast<MCFrame *>(dmf)->getLong(result);
      break;
      
    case MeasFrame::GetLat:
      return static_cast<MCFrame *>(dmf)->getLat(result);
      break;
      
    case MeasFrame::GetRadius:
      return static_cast<MCFrame *>(dmf)->getRadius(result);
      break;

    case MeasFrame::GetLatGeo:
      return static_cast<MCFrame *>(dmf)->getLatGeo(result);
      break;
      
    case MeasFrame::GetJ2000Long:
      return static_cast<MCFrame *>(dmf)->getJ2000Long(result);
      break;
      
    case MeasFrame::GetJ2000Lat:
      return static_cast<MCFrame *>(dmf)->getJ2000Lat(result);
      break;
      
    case MeasFrame::GetB1950Long:
      return static_cast<MCFrame *>(dmf)->getB1950Long(result);
      break;
      
    case MeasFrame::GetB1950Lat:
      return static_cast<MCFrame *>(dmf)->getB1950Lat(result);
      break;
      
    case MeasFrame::GetAppLong:
      return static_cast<MCFrame *>(dmf)->getAppLong(result);
      break;
      
    case MeasFrame::GetAppLat:
      return static_cast<MCFrame *>(dmf)->getAppLat(result);
      break;
      
    case MeasFrame::GetLAST:
      return static_cast<MCFrame *>(dmf)->getLAST(result);
      break;
      
    case MeasFrame::GetLASTr:
      return static_cast<MCFrame *>(dmf)->getLASTr(result);
      break;
      
    case MeasFrame::GetLSR:
      return static_cast<MCFrame *>(dmf)->getLSR(result);
      break;
      
    default:
      break;
    };
  } catch (AipsError x) {
  } 
  
  result = 0;
  return False;
}

Bool MCFrameGetmvdir(void *dmf, uInt tp, MVDirection &result) {
  try {
    switch (tp) {
      
    case MeasFrame::GetJ2000:
      return static_cast<MCFrame *>(dmf)->getJ2000(result);
      break;
      
    case MeasFrame::GetB1950:
      return static_cast<MCFrame *>(dmf)->getB1950(result);
      break;
      
    case MeasFrame::GetApp:
      return static_cast<MCFrame *>(dmf)->getApp(result);
      break;
      
    default:
      break;
    };
  } catch (AipsError x) {
  } 
  MVDirection tmp;
  result = tmp;
  return False;
}

Bool MCFrameGetmvpos(void *dmf, uInt tp, MVPosition &result) {
  try {
    switch (tp) {
       
    case MeasFrame::GetITRF:
      return static_cast<MCFrame *>(dmf)->getITRF(result);
      break;
       
    case MeasFrame::GetComet:
      return static_cast<MCFrame *>(dmf)->getComet(result);
      break;
      
    default:
      break;
    };
  } catch (AipsError x) {
  } 
  MVPosition tmp;
  result = tmp;
  return False;
}

Bool MCFrameGetuint(void *dmf, uInt tp, uInt &result) {
  try {
    switch (tp) {
      
    case MeasFrame::GetCometType:
      return static_cast<MCFrame *>(dmf)->getCometType(result);
      break;
      
    default:
      break;
    };
  } catch (AipsError x) {
  } 
  result = 0;
  return False;
}
