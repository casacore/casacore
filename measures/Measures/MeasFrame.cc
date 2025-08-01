//# MeasFrame.cc: Container for Measure frame
//# Copyright (C) 1996-2003,2007
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
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/measures/Measures/MCFrame.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/measures/Measures/MeasComet.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Representation class
class FrameRep {
public:
  FrameRep() = default;
  ~FrameRep() = default;
 FrameRep(const FrameRep& source) :
    epval(source.epval ? source.epval->clone() : nullptr),
    posval(source.posval ? source.posval->clone() : nullptr),
    dirval(source.dirval ? source.dirval->clone() : nullptr),
    radval(source.radval ? source.radval->clone() : nullptr),
    comval(source.comval ? source.comval->clone() : nullptr),
    mymcf(source.mymcf ? new MCFrame(*source.mymcf) : nullptr) {}

  // The actual measures
  // <group>
  // Epoch in time
  std::unique_ptr<Measure> epval;
  // Position
  std::unique_ptr<Measure> posval;
  // Direction
  std::unique_ptr<Measure> dirval;
  // Radial velocity
  std::unique_ptr<Measure> radval;
  // Comet
  std::unique_ptr<MeasComet> comval;
  // Pointer to belonging conversion frame
  std::unique_ptr<MCFrame> mymcf;
};

// MeasFrame class

//# Constructors
MeasFrame::MeasFrame() {
  create();
}

MeasFrame::MeasFrame(const Measure &meas1) {
  create();
  fill(&meas1);
}

MeasFrame::MeasFrame(const Measure &meas1, const Measure &meas2) {
  create();
  fill(&meas1);
  fill(&meas2);
}

MeasFrame::MeasFrame(const Measure &meas1, const Measure &meas2,
		     const Measure &meas3) {
  create();
  fill(&meas1);
  fill(&meas2);
  fill(&meas3);
}

MeasFrame::MeasFrame(details::CyclicPtr<FrameRep> new_rep) :
  rep(std::move(new_rep))
{
}

// These must be declared here and not in the header because FrameRep
// needs to be defined.
MeasFrame::MeasFrame(const MeasFrame &other) = default;
MeasFrame::MeasFrame(MeasFrame &&other) = default;
MeasFrame &MeasFrame::operator=(const MeasFrame &other) = default;
MeasFrame &MeasFrame::operator=(MeasFrame &&other) = default;

// Destructor
MeasFrame::~MeasFrame() = default;

Bool MeasFrame::operator==(const MeasFrame &other) const {
  return (rep == other.rep);
}

Bool MeasFrame::operator!=(const MeasFrame &other) const{
  return (rep != other.rep);
}

// General member functions
Bool MeasFrame::empty() const{
  return ( !(rep && (rep->epval || rep->posval || 
		     rep->dirval || rep->radval)) );
}

void MeasFrame::set(const Measure &meas1) {
  fill(&meas1);
}

void MeasFrame::set(const Measure &meas1, const Measure &meas2) {
  fill(&meas1);
  fill(&meas2);
}

void MeasFrame::set(const Measure &meas1, const Measure &meas2,
		    const Measure &meas3) {
  fill(&meas1);
  fill(&meas2);
  fill(&meas3);
}

void MeasFrame::set(const MeasComet &meas) {
  fill(&meas);
}

void MeasFrame::resetEpoch(Double val) {
  resetEpoch(MVEpoch(val));
}

void MeasFrame::resetEpoch(const Vector<Double> &val) {
  resetEpoch(MVEpoch(val));
}

void MeasFrame::resetEpoch(const Quantum<Double> &val) {
  resetEpoch(MVEpoch(val));
}

void MeasFrame::resetEpoch(const Quantum<Vector<Double> > &val) {
  resetEpoch(MVEpoch(val));
}

void MeasFrame::resetEpoch(const MVEpoch &val) {
  if (rep && rep->epval) {
    rep->epval->set(val);
    rep->mymcf->resetEpoch();
  } else {
    errorReset(String("Epoch"));
  }
}

void MeasFrame::resetEpoch(const Measure &val) {
  if (rep && rep->epval) {
    const details::CyclicState state = rep.Freeze();
    rep->epval.reset(val.clone());
    rep.Unfreeze(state);
    makeEpoch();
  } else {
    errorReset(String("Epoch"));
  }
}

void MeasFrame::resetPosition(const Vector<Double> &val) {
  resetPosition(MVPosition(val));
}

void MeasFrame::resetPosition(const Quantum<Vector<Double> > &val) {
  resetPosition(MVPosition(val));
}

void MeasFrame::resetPosition(const MVPosition  &val) {
  if (rep && rep->posval) {
    rep->posval->set(val);
    rep->mymcf->resetPosition();
  } else {
    errorReset(String("Position"));
  }
}

void MeasFrame::resetPosition(const Measure &val) {
  if (rep && rep->posval) {
    const details::CyclicState state = rep.Freeze();
    rep->posval.reset(val.clone());
    rep.Unfreeze(state);
    makePosition();
  } else {
    errorReset(String("Position"));
  }
}

void MeasFrame::resetDirection(const Vector<Double> &val) {
  resetDirection(MVDirection(val));
}

void MeasFrame::resetDirection(const Quantum<Vector<Double> > &val) {
  resetDirection(MVDirection(val));
}

void MeasFrame::resetDirection(const MVDirection  &val) {
  if (rep && rep->dirval) {
    rep->dirval->set(val);
    rep->mymcf->resetDirection();
  } else {
    errorReset(String("Direction"));
  }
}

void MeasFrame::resetDirection(const Measure &val) {
  if (rep && rep->dirval) {
    const details::CyclicState state = rep.Freeze();
    rep->dirval.reset(val.clone());
    rep.Unfreeze(state);
    makeDirection();
  } else {
    errorReset(String("Direction"));
  }
}

void MeasFrame::resetRadialVelocity(const Vector<Double> &val) {
  resetRadialVelocity(MVRadialVelocity(val));
}

void MeasFrame::resetRadialVelocity(const Quantum<Vector<Double> > &val) {
  resetRadialVelocity(MVRadialVelocity(val));
}

void MeasFrame::resetRadialVelocity(const MVRadialVelocity  &val) {
  if (rep && rep->radval) {
    rep->radval->set(val);
    rep->mymcf->resetRadialVelocity();
  } else {
    errorReset(String("RadialVelocity"));
  }
}

void MeasFrame::resetRadialVelocity(const Measure &val) {
  if (rep && rep->radval) {
    const details::CyclicState state = rep.Freeze();
    rep->radval.reset(val.clone());
    rep.Unfreeze(state);
    makeRadialVelocity();
  } else {
    errorReset(String("RadialVelocity"));
  }
}

void MeasFrame::resetComet(const MeasComet &val) {
  if (rep && rep->comval) {
    fill(&val);
  } else {
    errorReset(String("Comet"));
  }
}

const Measure* MeasFrame::epoch() const{
  if (rep) return rep->epval.get();
  return nullptr;
}

const Measure* MeasFrame::position() const{
  if (rep) return rep->posval.get();
  return nullptr;
}

const Measure* MeasFrame::direction() const{
  if (rep) return rep->dirval.get();
  return nullptr;
}

const Measure* MeasFrame::radialVelocity() const{
  if (rep) return rep->radval.get();
  return nullptr;
}

const MeasComet* MeasFrame::comet() const{
  if (rep) return rep->comval.get();
  return nullptr;
}

Bool MeasFrame::getTDB(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getTDB(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getUT1(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getUT1(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getTT(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getTT(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLong(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLong(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLat(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLat(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getITRF(MVPosition &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getITRF(tdb, *this));
  tdb = MVPosition(0.0);
  return False; 
}

Bool MeasFrame::getRadius(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getRadius(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLatGeo(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLatGeo(tdb, *this));
  tdb = 0;
  return False;
}

Bool MeasFrame::getLAST(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLAST(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLASTr(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLASTr(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getJ2000(MVDirection &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getJ2000(tdb, *this));
  tdb = Double(0.0);
  return False; 
}

Bool MeasFrame::getJ2000Long(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getJ2000Long(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getJ2000Lat(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getJ2000Lat(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getB1950(MVDirection &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getB1950(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getB1950Long(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getB1950Long(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getB1950Lat(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getB1950Lat(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getApp(MVDirection &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getApp(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getAppLong(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getAppLong(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getAppLat(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getAppLat(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLSR(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLSR(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getCometType(uInt &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getCometType(tdb, *this));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getComet(MVPosition &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getComet(tdb, *this));
  tdb = MVPosition(0.0);
  return False; 
}

void MeasFrame::create() {
  if (!rep) {
    rep = details::MakeCyclic<FrameRep>();
    rep->mymcf = std::make_unique<MCFrame>();
  }
}

void MeasFrame::fill(const Measure *in) {
  // The Measure values may own a MeasRef that owns this MeasFrame, causing a
  // cycle. The Freezing/Unfreezing calls prevent counting these links.
  if (in) {
    if (dynamic_cast<const MEpoch*>(in)) {
      const details::CyclicState state = rep.Freeze();
      rep->epval.reset(in->clone());
      rep.Unfreeze(state);
      makeEpoch();
    } else if (dynamic_cast<const MPosition*>(in)) {
      const details::CyclicState state = rep.Freeze();
      rep->posval.reset(in->clone());
      rep.Unfreeze(state);
      makePosition();
    } else if (dynamic_cast<const MDirection*>(in)) {
      const details::CyclicState state = rep.Freeze();
      rep->dirval.reset(in->clone());
      rep.Unfreeze(state);
      makeDirection();
    } else if (dynamic_cast<const MRadialVelocity*>(in)) {
      const details::CyclicState state = rep.Freeze();
      rep->radval.reset(in->clone());
      rep.Unfreeze(state);
      makeRadialVelocity();
    } else {
      throw(AipsError("Unknown MeasFrame Measure type " +
		      in->tellMe()));
    }
  }
}

void MeasFrame::fill(const MeasComet *in) {
  if (in) {
    rep->comval.reset();
    if (in->ok()) {
      rep->comval.reset(in->clone());
      if (!rep->comval->ok()) {
        rep->comval.reset();
      }
    }
    if (rep->comval) {
      makeComet();
    } else {
      throw(AipsError("Unknown or illegal MeasComet given for MeasFrame"));
    }
  }
}

void MeasFrame::makeEpoch() {
  rep->mymcf->makeEpoch(*this);
}

void MeasFrame::makePosition() {
  rep->mymcf->makePosition(*this);
}

void MeasFrame::makeDirection() {
  rep->mymcf->makeDirection(*this);
}

void MeasFrame::makeRadialVelocity() {
  rep->mymcf->makeRadialVelocity(*this);
}

void MeasFrame::makeComet() {
  rep->mymcf->makeComet();
}

void MeasFrame::errorReset(const String &txt) {
  throw(AipsError("Attempt to reset non-existent frame member "+txt));
}

ostream &operator<<(ostream &os, MeasFrame &mf) {
  os << "Frame: ";
  Double tmp, tmp1, tmp2;
  if (mf.rep && mf.rep->epval) {
    os << *(mf.rep->epval);
    if (mf.getTDB(tmp) && mf.getUT1(tmp1) && mf.getTT(tmp2)) 
      os << " (TDB = " << tmp << ", UT1 = " << tmp1 << ", TT = " << tmp2 <<
	")";
  }
  if (mf.rep && mf.rep->posval) {
    if (mf.rep && mf.rep->epval) os << endl << "       ";
    os << *(mf.rep->posval);
    if (mf.getLong(tmp)) {
      os << endl << "        (Longitude = " << tmp;
      mf.getLat(tmp);
      os << " Latitude = " << tmp << ")";
    }
  }
  if (mf.rep && mf.rep->dirval) {
    if (mf.rep && (mf.rep->epval || mf.rep->posval)) 
      os << endl << "       ";
    os << *(mf.rep->dirval);
    MVDirection tmp;    
    if (mf.getJ2000(tmp)) {
      os << endl << "        (J2000 = " << 
	tmp.getAngle("deg") << ")";
    }
  }
  if (mf.rep && mf.rep->radval) {
    if (mf.rep && (mf.rep->epval || mf.rep->posval ||
		   mf.rep->dirval)) {
      os << endl << "       ";
    }
    os << *(mf.rep->radval);
    if (mf.getLSR(tmp)) {
      tmp /= 1000.;
      os << endl << "        (LSR velocity = " << 
	Quantity(tmp,"km/s") << ")";
    }
  }
  if (mf.rep && mf.rep->comval) {
    if (mf.rep && (mf.rep->epval || mf.rep->posval ||
		   mf.rep->dirval || mf.rep->radval)) {
      os << endl << "       ";
    }
    os << mf.rep->comval->getName() << " comet between MJD " <<
      mf.rep->comval->getStart() << " and " <<
      mf.rep->comval->getEnd();
  }
  return os;
}

MeasFrame MeasFrame::independentCopy() const {
  MeasFrame new_frame(details::MakeCyclic<FrameRep>(*rep));
  if (new_frame.rep->epval)
    new_frame.makeEpoch();
  if (new_frame.rep->posval)
    new_frame.makePosition();
  if (new_frame.rep->dirval)
    new_frame.makeDirection();
  if (new_frame.rep->radval)
    new_frame.makeRadialVelocity();
  return new_frame;
}

} //# NAMESPACE CASACORE - END

