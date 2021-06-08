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
  epTDBp(), 
  epUT1p(), 
  epTTp(), 
  epLASTp(), 
  posLongp(), posITRFp(),
  posLongGeop(), 
  posGeop(),j2000Longp(), dirJ2000p(),
  b1950Longp(), dirB1950p(),
  appLongp(), dirAppp(),
  radLSRp() {;}

// Destructor
MCFrame::~MCFrame() { }

// Operators

// General member functions

void MCFrame::resetEpoch() {
  epTDBp.reset();
  epUT1p.reset();;
  epTTp.reset();
  epLASTp.reset();
  appLongp.reset();
  dirAppp.reset();
  radLSRp.reset();
}

void MCFrame::resetPosition() {
  posLongp.reset();
  posITRFp.reset();
  posLongGeop.reset();
  posGeop.reset();
  epLASTp.reset();
}

void MCFrame::resetDirection() {
  j2000Longp.reset();
  dirJ2000p.reset();
  b1950Longp.reset();
  dirB1950p.reset();
  appLongp.reset();
  dirAppp.reset();
  radLSRp.reset();
}

void MCFrame::resetRadialVelocity() {
  radLSRp.reset();
}

void MCFrame::resetComet() {
}

Bool MCFrame::getTDB(Double &tdb) {
  std::unique_ptr<casacore::Measure>& epoch = myf.lock()->epval;
  if (epoch) {
    if (!epTDBp) {
      epTDBp.reset(new Double);
      *epTDBp = getEpConvTDB()
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
      epUT1p.reset(new Double);
      *epUT1p = getEpConvUT1()
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
      epTTp.reset(new Double);
      *epTTp = getEpConvTT()
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
      posLongp.reset(new Vector<Double>(3));
      posITRFp.reset(new MVPosition());
      *posITRFp = getPosConvLong()
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
      posLongp.reset(new Vector<Double>(3));
      posITRFp.reset(new MVPosition);
      *posITRFp = getPosConvLong()
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
      posLongGeop.reset(new Vector<Double>(3));
      posGeop.reset(new MVPosition);
      *posGeop = getPosConvLongGeo()
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
      posLongp.reset(new Vector<Double>(3));
      posITRFp.reset(new MVPosition);
      *posITRFp = getPosConvLong()
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
      posLongp.reset(new Vector<Double>(3));
      posITRFp.reset(new MVPosition);
      *posITRFp = getPosConvLong()
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
      epLASTp.reset(new Double);
      *epLASTp = getEpConvLAST()
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
      j2000Longp.reset(new Vector<Double>(2));
      dirJ2000p.reset(new MVDirection);
      *dirJ2000p = getDirConvJ2000()
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
      j2000Longp.reset(new Vector<Double>(2));
      dirJ2000p.reset(new MVDirection);
      *dirJ2000p = getDirConvJ2000()
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
      j2000Longp.reset(new Vector<Double>(2));
      dirJ2000p.reset(new MVDirection);
      *dirJ2000p = getDirConvJ2000()
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
      b1950Longp.reset(new Vector<Double>(2));
      dirB1950p.reset(new MVDirection);
      *dirB1950p = getDirConvB1950()
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
      b1950Longp.reset(new Vector<Double>(2));
      dirB1950p.reset(new MVDirection);
      *dirB1950p = getDirConvB1950()
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
      b1950Longp.reset(new Vector<Double>(2));
      dirB1950p.reset(new MVDirection);
      *dirB1950p = getDirConvB1950()
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
      appLongp.reset(new Vector<Double>(2));
      dirAppp.reset(new MVDirection);
      *dirAppp = getDirConvApp()
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
      appLongp.reset(new Vector<Double>(2));
      dirAppp.reset(new MVDirection);
      *dirAppp = getDirConvApp()
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
      appLongp.reset(new Vector<Double>(2));
      dirAppp.reset(new MVDirection);
      *dirAppp = getDirConvApp()
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
      radLSRp.reset(new Double);
      *radLSRp = getRadConvLSR()
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

MEpoch::Convert MCFrame::getEpConvTDB() const {
  const MEpoch::Ref REFTDB = MEpoch::Ref(MEpoch::TDB);
  std::unique_ptr<casacore::Measure>& epoch = myf.lock()->epval;
  return MEpoch::Convert(*epoch, REFTDB);
}

MEpoch::Convert MCFrame::getEpConvUT1() const {
  static const MEpoch::Ref REFUT1 = MEpoch::Ref(MEpoch::UT1);
  std::unique_ptr<casacore::Measure>& epoch = myf.lock()->epval;
  return MEpoch::Convert(*epoch, REFUT1);
}

MEpoch::Convert MCFrame::getEpConvTT() const {
  static const MEpoch::Ref REFTT  = MEpoch::Ref(MEpoch::TT);
  std::unique_ptr<casacore::Measure>& epoch = myf.lock()->epval;
  return MEpoch::Convert(*epoch, REFTT);
}

MEpoch::Convert MCFrame::getEpConvLAST() const {
  std::unique_ptr<casacore::Measure>& epoch = myf.lock()->epval;
  return MEpoch::Convert(*(epoch), MEpoch::Ref(MEpoch::LAST, MeasFrame(this->myf.lock())));
}

void MCFrame::makeEpoch() {
  epTDBp.reset();
  epUT1p.reset();
  epTTp.reset();
  epLASTp.reset();
  appLongp.reset();
  dirAppp.reset();
  radLSRp.reset();
}

MPosition::Convert MCFrame::getPosConvLong() const
{
  const MPosition::Ref REFLONG = MPosition::Ref(MPosition::ITRF);
  std::unique_ptr<casacore::Measure>& position = myf.lock()->posval;
  return MPosition::Convert(*position, REFLONG);
}

MPosition::Convert MCFrame::getPosConvLongGeo() const
{
  const MPosition::Ref REFGEO = MPosition::Ref(MPosition::WGS84);
  std::unique_ptr<casacore::Measure>& position = myf.lock()->posval;
  return MPosition::Convert(*position, REFGEO);
}

void MCFrame::makePosition() {
  posLongp.reset();
  posITRFp.reset();
  epLASTp.reset();
  radLSRp.reset();
  posLongGeop.reset();
  posGeop.reset();
}

MDirection::Convert MCFrame::getDirConvJ2000() const
{
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  return MDirection::Convert(*direction,
					 MDirection::Ref(MDirection::J2000,
							 MeasFrame(this->myf.lock())));
}

MDirection::Convert MCFrame::getDirConvB1950() const
{
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  return MDirection::Convert(*direction,
					 MDirection::Ref(MDirection::B1950,
							 MeasFrame(this->myf.lock())));
}

MDirection::Convert MCFrame::getDirConvApp() const
{
  std::unique_ptr<casacore::Measure>& direction = myf.lock()->dirval;
  return MDirection::Convert(*direction,
				       MDirection::Ref(MDirection::APP,
						       MeasFrame(this->myf.lock())));
}

void MCFrame::makeDirection() {
  j2000Longp.reset();
  dirJ2000p.reset();
  b1950Longp.reset();
  dirB1950p.reset();
  appLongp.reset();
  dirAppp.reset();
  radLSRp.reset();
}

MRadialVelocity::Convert MCFrame::getRadConvLSR() const
{
  const MRadialVelocity::Ref REFLSR 
    = MRadialVelocity::Ref(MRadialVelocity::LSRK);
  std::unique_ptr<casacore::Measure>& radialVelocity = myf.lock()->radval;
  return MRadialVelocity::Convert(*radialVelocity, REFLSR);
}

void MCFrame::makeRadialVelocity() {
  radLSRp.reset();
}

void MCFrame::makeComet() { }

} //# NAMESPACE CASACORE - END

