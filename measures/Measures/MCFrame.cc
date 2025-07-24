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
#include <optional>

#include <casacore/casa/Arrays/Vector.h>
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

struct MCFrameImplementation {
  // Conversion to TDB time
  MeasConvert<MEpoch> epConvTDB;
  // TDB time
  std::optional<Double> epTDBp;
  // Conversion to UT1 time
  MeasConvert<MEpoch> epConvUT1;
  // UT1 time
  std::optional<Double> epUT1p;
  // Conversion to TT time
  MeasConvert<MEpoch> epConvTT;
  // TT time
  std::optional<Double> epTTp;
  // Conversion to LAST time
  MeasConvert<MEpoch> epConvLAST;
  // LAST time
  std::optional<Double> epLASTp;
  // Conversion to ITRF longitude/latitude
  MeasConvert<MPosition> posConvLong;
  // Longitude
  Vector<Double> posLongp;
  // Position
  MVPosition posITRFp;
  // Conversion to geodetic longitude/latitude
  MeasConvert<MPosition> posConvLongGeo;
  // Latitude
  Vector<Double> posLongGeop;
  // Position
  MVPosition posGeop;
  // Conversion to J2000
  MeasConvert<MDirection> dirConvJ2000;
  // Longitude
  Vector<Double> j2000Longp;
  // J2000 coordinates
  MVDirection dirJ2000p;
  // Conversion to B1950
  MeasConvert<MDirection> dirConvB1950;
  // Longitude
  Vector<Double> b1950Longp;
  // B1950 coordinates
  MVDirection dirB1950p;
  // Conversion to apparent coordinates
  MeasConvert<MDirection> dirConvApp;
  // Longitude
  Vector<Double> appLongp;
  // Apparent coordinates
  MVDirection dirAppp;
  // Conversion to LSR radial velocity
  MeasConvert<MRadialVelocity> radConvLSR;
  // Radial velocity
  std::optional<Double> radLSRp;
};
  
MCFrame::MCFrame() :
  impl_(std::make_unique<MCFrameImplementation>()) {
}

MCFrame::MCFrame(const MCFrame &other) :
  impl_(std::make_unique<MCFrameImplementation>(*other.impl_))
{
}

MCFrame::MCFrame(MCFrame &&other) = default;
MCFrame::~MCFrame() = default;

void MCFrame::resetEpoch() {
  impl_->epTDBp.reset();
  impl_->epUT1p.reset();
  impl_->epTTp.reset();
  impl_->epLASTp.reset();
  impl_->appLongp = casacore::Vector<Double>();
  impl_->dirAppp = MVDirection();
  impl_->radLSRp.reset();
}

void MCFrame::resetPosition() {
  impl_->posLongp = casacore::Vector<Double>();
  impl_->posITRFp = MVPosition();
  impl_->posLongGeop = casacore::Vector<Double>();
  impl_->posGeop = MVPosition();
  impl_->epLASTp.reset();
}

void MCFrame::resetDirection() {
  impl_->j2000Longp = casacore::Vector<Double>();
  impl_->dirJ2000p = MVDirection();
  impl_->b1950Longp = casacore::Vector<Double>();
  impl_->dirB1950p = MVDirection();
  impl_->appLongp = casacore::Vector<Double>();
  impl_->dirAppp = MVDirection();
  impl_->radLSRp.reset();
}

void MCFrame::resetRadialVelocity() {
  impl_->radLSRp.reset();
}

void MCFrame::resetComet() {
}

Bool MCFrame::getTDB(Double &tdb, const MeasFrame& frame) {
  if (frame.epoch()) {
    if (!impl_->epTDBp) {
      impl_->epTDBp = impl_->epConvTDB(*dynamic_cast<const MVEpoch *>(frame.epoch()->getData())).
	getValue().get();
    }
    tdb = *impl_->epTDBp;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getUT1(Double &tdb, const MeasFrame& frame) {
  if (frame.epoch()) {
    if (!impl_->epUT1p) {
      impl_->epUT1p = impl_->epConvUT1(*dynamic_cast<const MVEpoch *>(frame.epoch()->getData())).
	getValue().get();
    }
    tdb = *impl_->epUT1p;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getTT(Double &tdb, const MeasFrame& frame) {
  if (frame.epoch()) {
    if (!impl_->epTTp) {
      impl_->epTTp = impl_->epConvTT(*dynamic_cast<const MVEpoch *>(frame.epoch()->getData())).
	getValue().get();
    }
    tdb = *impl_->epTTp;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLong(Double &tdb, const MeasFrame& frame) {
  if (frame.position()) {
    if (impl_->posLongp.empty()) {
      impl_->posITRFp = impl_->posConvLong(*dynamic_cast<const MVPosition *>(frame.position()->getData())).
	getValue();
      impl_->posLongp = impl_->posITRFp.get();
    }
    tdb = MVAngle(impl_->posLongp(1))(-0.5);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLat(Double &tdb, const MeasFrame& frame) {
  if (frame.position()) {
    if (impl_->posLongp.empty()) {
      impl_->posITRFp = impl_->posConvLong(*dynamic_cast<const MVPosition *>(frame.position()->getData())).
	getValue();
      impl_->posLongp = impl_->posITRFp.get();
    }
    tdb = impl_->posLongp(2);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLatGeo(Double &tdb, const MeasFrame& frame) {
  if (frame.position()) {
    if (impl_->posLongGeop.empty()) {
      impl_->posGeop = impl_->posConvLongGeo(*dynamic_cast<const MVPosition *>(frame.position()->getData())).
        getValue();
      impl_->posLongGeop = impl_->posGeop.get();
    }
    tdb = impl_->posLongGeop(2);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getITRF(MVPosition &tdb, const MeasFrame& frame) {
  if (frame.position()) {
    if (impl_->posLongp.empty()) {
      impl_->posITRFp = impl_->posConvLong(*dynamic_cast<const MVPosition *>(frame.position()->getData())).
	getValue();
      impl_->posLongp = impl_->posITRFp.get();
    }
    tdb = impl_->posITRFp;
    return True;
  }
  tdb = MVPosition(0.0);
  return False;
}

Bool MCFrame::getRadius(Double &tdb, const MeasFrame& frame) {
  if (frame.position()) {
    if (impl_->posLongp.empty()) {
      impl_->posITRFp = impl_->posConvLong(*dynamic_cast<const MVPosition *>(frame.position()->getData())).
	getValue();
      impl_->posLongp = impl_->posITRFp.get();
    }
    tdb = impl_->posLongp(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLAST(Double &tdb, const MeasFrame& frame) {
  if (frame.epoch()) {
    if (!impl_->epLASTp) {
      impl_->epLASTp = impl_->epConvLAST(*dynamic_cast<const MVEpoch *>(frame.epoch()->getData())).
	getValue().get();
    }
    tdb = fmod(*impl_->epLASTp, 1.0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLASTr(Double &tdb, const MeasFrame& frame) {
  Bool tmp = MCFrame::getLAST(tdb, frame);
  tdb *= C::circle;
  return tmp;
}

Bool MCFrame::getJ2000Long(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (impl_->j2000Longp.empty()) {
      impl_->dirJ2000p = impl_->dirConvJ2000(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->j2000Longp = impl_->dirJ2000p.get();
    }
    tdb = impl_->j2000Longp(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getJ2000Lat(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (impl_->j2000Longp.empty()) {
      impl_->dirJ2000p = impl_->dirConvJ2000(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->j2000Longp = impl_->dirJ2000p.get();
    }
    tdb = impl_->j2000Longp(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getJ2000(MVDirection &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (impl_->j2000Longp.empty()) {
      impl_->dirJ2000p = impl_->dirConvJ2000(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->j2000Longp = impl_->dirJ2000p.get();
    }
    tdb = impl_->dirJ2000p;
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getB1950Long(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (impl_->b1950Longp.empty()) {
      impl_->dirB1950p = impl_->dirConvB1950(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->b1950Longp = impl_->dirB1950p.get();
    }
    tdb = impl_->b1950Longp(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getB1950Lat(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (impl_->b1950Longp.empty()) {
      impl_->dirB1950p = impl_->dirConvB1950(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->b1950Longp = impl_->dirB1950p.get();
    }
    tdb = impl_->b1950Longp(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getB1950(MVDirection &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (impl_->b1950Longp.empty()) {
      impl_->dirB1950p = impl_->dirConvB1950(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->b1950Longp = impl_->dirB1950p.get();
    }
    tdb = impl_->dirB1950p;
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getAppLong(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (impl_->appLongp.empty()) {
      impl_->dirAppp = impl_->dirConvApp(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->appLongp = impl_->dirAppp.get();
    }
    tdb = impl_->appLongp(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getAppLat(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (impl_->appLongp.empty()) {
      impl_->dirAppp = impl_->dirConvApp(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->appLongp = impl_->dirAppp.get();
    }
    tdb = impl_->appLongp(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getApp(MVDirection &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (impl_->appLongp.empty()) {
      impl_->dirAppp = impl_->dirConvApp(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->appLongp = impl_->dirAppp.get();
    }
    tdb = impl_->dirAppp;
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getLSR(Double &tdb, const MeasFrame& frame) {
  if (frame.radialVelocity()) {
    if (!impl_->radLSRp) {
      impl_->radLSRp = impl_->radConvLSR(*dynamic_cast<const MVRadialVelocity *>(frame.radialVelocity()->
						      getData())).
	getValue();
    }
    tdb = *impl_->radLSRp;
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getCometType(uInt &tdb, const MeasFrame& frame) {
  if (frame.comet()) {
    tdb = static_cast<uInt>(frame.comet()->getType());
    return True;
  }
  tdb = 0;
  return False;
}

Bool MCFrame::getComet(MVPosition &tdb, const MeasFrame& frame) {
  if (frame.comet()) {
    Double x(0);
    if (getTDB(x, frame) && frame.comet()->get(tdb, x)) return True;
  }
  tdb = MVPosition(0.0);
  return False;
}

void MCFrame::makeEpoch(const MeasFrame& frame) {
  const MEpoch::Ref REFTDB = MEpoch::Ref(MEpoch::TDB);
  const MEpoch::Ref REFUT1 = MEpoch::Ref(MEpoch::UT1);
  const MEpoch::Ref REFTT  = MEpoch::Ref(MEpoch::TT);
  impl_->epConvTDB = MEpoch::Convert(*(frame.epoch()), REFTDB);
  impl_->epConvUT1 = MEpoch::Convert(*(frame.epoch()), REFUT1);
  impl_->epConvTT  = MEpoch::Convert(*(frame.epoch()), REFTT);
  impl_->epTDBp.reset();
  impl_->epUT1p.reset();
  impl_->epTTp.reset();
  impl_->epConvLAST = MEpoch::Convert(*(frame.epoch()),
				   MEpoch::Ref(MEpoch::LAST, frame));
  impl_->epLASTp.reset();
  impl_->appLongp = casacore::Vector<Double>();
  impl_->dirAppp = MVDirection();
  impl_->radLSRp.reset();
}

void MCFrame::makePosition(const MeasFrame& frame) {
  static const MPosition::Ref REFLONG 
    = MPosition::Ref(MPosition::ITRF);
  impl_->posConvLong = MPosition::Convert(*frame.position(),
				       REFLONG);
  impl_->posLongp = casacore::Vector<Double>();
  impl_->posITRFp = MVPosition();
  impl_->epLASTp.reset();
  impl_->radLSRp.reset();
  static const MPosition::Ref REFGEO
    = MPosition::Ref(MPosition::WGS84);
  impl_->posConvLongGeo = MPosition::Convert(*frame.position(),
					  REFGEO);
  impl_->posLongGeop = casacore::Vector<Double>();
  impl_->posGeop = MVPosition();
}

void MCFrame::makeDirection(const MeasFrame& frame) {
  static const MDirection::Ref REFJ2000 = MDirection::Ref(MDirection::J2000);
  impl_->dirConvJ2000 = MDirection::Convert(*frame.direction(),
					 MDirection::Ref(MDirection::J2000,
							 frame));

  static const MDirection::Ref REFB1950 = MDirection::Ref(MDirection::B1950);
  impl_->dirConvB1950 = MDirection::Convert(*frame.direction(),
					 MDirection::Ref(MDirection::B1950,
							 frame));
  impl_->dirConvApp = MDirection::Convert(*frame.direction(),
				       MDirection::Ref(MDirection::APP,
						       frame));
  impl_->j2000Longp = casacore::Vector<Double>();
  impl_->dirJ2000p = MVDirection();
  impl_->b1950Longp = casacore::Vector<Double>();
  impl_->dirB1950p = MVDirection();
  impl_->appLongp = casacore::Vector<Double>();
  impl_->dirAppp = MVDirection();
  impl_->radLSRp.reset();
}

void MCFrame::makeRadialVelocity(const MeasFrame& frame) {
  static const MRadialVelocity::Ref REFLSR 
    = MRadialVelocity::Ref(MRadialVelocity::LSRK);
  impl_->radConvLSR = MRadialVelocity::Convert(*frame.radialVelocity(),
					    REFLSR);
  impl_->radLSRp.reset();
}

void MCFrame::makeComet() {}

} //# NAMESPACE CASACORE - END

