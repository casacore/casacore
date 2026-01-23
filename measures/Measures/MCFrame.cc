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
  std::optional< MeasConvert<MEpoch> > epConvTDB;
  // TDB time
  std::optional<Double> epTDBp;
  // Conversion to UT1 time
  std::optional< MeasConvert<MEpoch> > epConvUT1;
  // UT1 time
  std::optional<Double> epUT1p;
  // Conversion to TT time
  std::optional< MeasConvert<MEpoch> > epConvTT;
  // TT time
  std::optional<Double> epTTp;
  // Conversion to LAST time
  std::optional< MeasConvert<MEpoch> > epConvLAST;
  // LAST time
  std::optional<Double> epLASTp;
  // Conversion to ITRF longitude/latitude
  std::optional< MeasConvert<MPosition> > posConvLong;
  // Longitude
  std::optional< Vector<Double> > posLongp;
  // Position
  std::optional< MVPosition> posITRFp;
  // Conversion to geodetic longitude/latitude
  std::optional< MeasConvert<MPosition> > posConvLongGeo;
  // Latitude
  std::optional< Vector<Double> > posLongGeop;
  // Position
  std::optional< MVPosition> posGeop;
  // Conversion to J2000
  std::optional< MeasConvert<MDirection> > dirConvJ2000;
  // Longitude
  std::optional< Vector<Double> > j2000Longp;
  // J2000 coordinates
  std::optional< MVDirection> dirJ2000p;
  // Conversion to B1950
  std::optional< MeasConvert<MDirection> > dirConvB1950;
  // Longitude
  std::optional< Vector<Double> > b1950Longp;
  // B1950 coordinates
  std::optional< MVDirection> dirB1950p;
  // Conversion to apparent coordinates
  std::optional< MeasConvert<MDirection> > dirConvApp;
  // Longitude
  std::optional< Vector<Double> > appLongp;
  // Apparent coordinates
  std::optional<MVDirection> dirAppp;
  // Conversion to LSR radial velocity
  std::optional< MeasConvert<MRadialVelocity> > radConvLSR;
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
  impl_->appLongp.reset();
  impl_->dirAppp.reset();
  impl_->radLSRp.reset();
}

void MCFrame::resetPosition() {
  impl_->posLongp.reset();
  impl_->posITRFp.reset();
  impl_->posLongGeop.reset();
  impl_->posGeop.reset();
  impl_->epLASTp.reset();
}

void MCFrame::resetDirection() {
  impl_->j2000Longp.reset();
  impl_->dirJ2000p.reset();
  impl_->b1950Longp.reset();
  impl_->dirB1950p.reset();
  impl_->appLongp.reset();
  impl_->dirAppp.reset();
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
      impl_->epTDBp = impl_->epConvTDB.value()(*dynamic_cast<const MVEpoch *>(frame.epoch()->getData())).
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
      impl_->epUT1p = impl_->epConvUT1.value()(*dynamic_cast<const MVEpoch *>(frame.epoch()->getData())).
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
      impl_->epTTp = impl_->epConvTT.value()(*dynamic_cast<const MVEpoch *>(frame.epoch()->getData())).
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
    if (!(impl_->posLongp.has_value())) {
      impl_->posITRFp = impl_->posConvLong.value()(*dynamic_cast<const MVPosition *>(frame.position()->getData())).
	getValue();
      impl_->posLongp = impl_->posITRFp.value().get();
    }
    tdb = MVAngle(impl_->posLongp.value()(1))(-0.5);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLat(Double &tdb, const MeasFrame& frame) {
  if (frame.position()) {
    if (!(impl_->posLongp.has_value())) {
      impl_->posITRFp = impl_->posConvLong.value()(*dynamic_cast<const MVPosition *>(frame.position()->getData())).
	getValue();
      impl_->posLongp = impl_->posITRFp.value().get();
    }
    tdb = impl_->posLongp.value()(2);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLatGeo(Double &tdb, const MeasFrame& frame) {
  if (frame.position()) {
    if (!(impl_->posLongGeop.has_value())) {
      impl_->posGeop = impl_->posConvLongGeo.value()(*dynamic_cast<const MVPosition *>(frame.position()->getData())).
        getValue();
      impl_->posLongGeop = impl_->posGeop.value().get();
    }
    tdb = impl_->posLongGeop.value()(2);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getITRF(MVPosition &tdb, const MeasFrame& frame) {
  if (frame.position()) {
    if (!(impl_->posLongp.has_value())) {
      impl_->posITRFp = impl_->posConvLong.value()(*dynamic_cast<const MVPosition *>(frame.position()->getData())).
	getValue();
      impl_->posLongp = impl_->posITRFp.value().get();
    }
    tdb = impl_->posITRFp.value();
    return True;
  }
  tdb = MVPosition(0.0);
  return False;
}

Bool MCFrame::getRadius(Double &tdb, const MeasFrame& frame) {
  if (frame.position()) {
    if (!(impl_->posLongp.has_value())) {
      impl_->posITRFp = impl_->posConvLong.value()(*dynamic_cast<const MVPosition *>(frame.position()->getData())).
	getValue();
      impl_->posLongp = impl_->posITRFp.value().get();
    }
    tdb = impl_->posLongp.value()(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getLAST(Double &tdb, const MeasFrame& frame) {
  if (frame.epoch()) {
    if (!impl_->epLASTp) {
      impl_->epLASTp = impl_->epConvLAST.value()(*dynamic_cast<const MVEpoch *>(frame.epoch()->getData())).
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
    if (!(impl_->j2000Longp.has_value())) {
      impl_->dirJ2000p = impl_->dirConvJ2000.value()(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->j2000Longp = impl_->dirJ2000p.value().get();
    }
    tdb = impl_->j2000Longp.value()(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getJ2000Lat(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (!(impl_->j2000Longp.has_value())) {
      impl_->dirJ2000p = impl_->dirConvJ2000.value()(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->j2000Longp = impl_->dirJ2000p.value().get();
    }
    tdb = impl_->j2000Longp.value()(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getJ2000(MVDirection &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (!(impl_->j2000Longp.has_value())) {
      impl_->dirJ2000p = impl_->dirConvJ2000.value()(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->j2000Longp = impl_->dirJ2000p.value().get();
    }
    tdb = impl_->dirJ2000p.value();
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getB1950Long(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (!(impl_->b1950Longp.has_value())) {
      impl_->dirB1950p = impl_->dirConvB1950.value()(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->b1950Longp = impl_->dirB1950p.value().get();
    }
    tdb = impl_->b1950Longp.value()(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getB1950Lat(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (!(impl_->b1950Longp.has_value())) {
      impl_->dirB1950p = impl_->dirConvB1950.value()(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->b1950Longp = impl_->dirB1950p.value().get();
    }
    tdb = impl_->b1950Longp.value()(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getB1950(MVDirection &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (!(impl_->b1950Longp.has_value())) {
      impl_->dirB1950p = impl_->dirConvB1950.value()(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->b1950Longp = impl_->dirB1950p.value().get();
    }
    tdb = impl_->dirB1950p.value();
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getAppLong(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (!(impl_->appLongp.has_value())) {
      impl_->dirAppp = impl_->dirConvApp.value()(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->appLongp = impl_->dirAppp.value().get();
    }
    tdb = impl_->appLongp.value()(0);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getAppLat(Double &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (!(impl_->appLongp.has_value())) {
      impl_->dirAppp = impl_->dirConvApp.value()(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->appLongp = impl_->dirAppp.value().get();
    }
    tdb = impl_->appLongp.value()(1);
    return True;
  }
  tdb = 0.0;
  return False;
}

Bool MCFrame::getApp(MVDirection &tdb, const MeasFrame& frame) {
  if (frame.direction()) {
    if (!(impl_->appLongp.has_value())) {
      impl_->dirAppp = impl_->dirConvApp.value()(*dynamic_cast<const MVDirection *>(frame.direction()->getData())).
	getValue();
      impl_->appLongp = impl_->dirAppp.value().get();
    }
    tdb = impl_->dirAppp.value();
    return True;
  }
  tdb = MVDirection(0.0);
  return False;
}

Bool MCFrame::getLSR(Double &tdb, const MeasFrame& frame) {
  if (frame.radialVelocity()) {
    if (!impl_->radLSRp) {
      impl_->radLSRp = impl_->radConvLSR.value()(*dynamic_cast<const MVRadialVelocity *>(frame.radialVelocity()->
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

void MCFrame::makeEpoch(MeasFrame& frame) {
  const MEpoch::Ref REFTDB = MEpoch::Ref(MEpoch::TDB);
  const MEpoch::Ref REFUT1 = MEpoch::Ref(MEpoch::UT1);
  const MEpoch::Ref REFTT  = MEpoch::Ref(MEpoch::TT);
  impl_->epConvTDB = MEpoch::Convert(*(frame.epoch()), REFTDB);
  impl_->epConvUT1 = MEpoch::Convert(*(frame.epoch()), REFUT1);
  impl_->epConvTT  = MEpoch::Convert(*(frame.epoch()), REFTT);
  impl_->epTDBp.reset();
  impl_->epUT1p.reset();
  impl_->epTTp.reset();
  // Initializing the MeasConvert may cause a cycle:
  // MeasConvert -> MeasRef -> MeasFrame and
  // MeasConvert -> Measure -> MeasRef -> MeasFrame
  const details::CyclicState state = frame.rep.Freeze();
  impl_->epConvLAST = MEpoch::Convert(*(frame.epoch()),
				   MEpoch::Ref(MEpoch::LAST, frame));
  frame.rep.Unfreeze(state);
  impl_->epLASTp.reset();
  impl_->appLongp.reset();
  impl_->dirAppp.reset();
  impl_->radLSRp.reset();
}

void MCFrame::makePosition(const MeasFrame& frame) {
  static const MPosition::Ref REFLONG
    = MPosition::Ref(MPosition::ITRF);
  impl_->posConvLong = MPosition::Convert(*frame.position(),
				       REFLONG);
  impl_->posLongp.reset();
  impl_->posITRFp.reset();
  impl_->epLASTp.reset();
  impl_->radLSRp.reset();
  static const MPosition::Ref REFGEO
    = MPosition::Ref(MPosition::WGS84);
  impl_->posConvLongGeo = MPosition::Convert(*frame.position(),
					  REFGEO);
  impl_->posLongGeop.reset();
  impl_->posGeop.reset();
}

void MCFrame::makeDirection(MeasFrame& frame) {
  static const MDirection::Ref REFJ2000 = MDirection::Ref(MDirection::J2000);
  static const MDirection::Ref REFB1950 = MDirection::Ref(MDirection::B1950);

  // Initializing the MeasConvert may cause a cycle (see makeEpoch).
  details::CyclicState state = frame.rep.Freeze();
  impl_->dirConvJ2000 = MDirection::Convert(*frame.direction(),
          MDirection::Ref(MDirection::J2000, frame));
  impl_->dirConvB1950 = MDirection::Convert(*frame.direction(),
					 MDirection::Ref(MDirection::B1950,
							 frame));
  impl_->dirConvApp = MDirection::Convert(*frame.direction(),
				       MDirection::Ref(MDirection::APP,
						       frame));
  frame.rep.Unfreeze(state);
  impl_->j2000Longp.reset();
  impl_->dirJ2000p.reset();
  impl_->b1950Longp.reset();
  impl_->dirB1950p.reset();
  impl_->appLongp.reset();
  impl_->dirAppp.reset();
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

