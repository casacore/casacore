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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Includes
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/measures/Measures/MCFrame.h>
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
  // Constructor
  FrameRep() :
    epval(0), posval(0), dirval(0), radval(0), comval(0),
    mymcf(0), cnt(1) {}
  // Destructor
  ~FrameRep() {
    delete epval;
    delete posval;
    delete dirval;
    delete radval;
    delete comval;
    delete mymcf;		// delete conversion frame data
  }
  
  // The actual measures
  // <group>
  // Epoch in time
  Measure *epval;
  // Position
  Measure *posval;
  // Direction
  Measure *dirval;
  // Radial velocity
  Measure *radval;
  // Comet
  MeasComet *comval;
  // Pointer to belonging conversion frame
  MCFrame *mymcf;
  // Usage count
  Int cnt;
};

// MeasFrame class

//# Constructors
MeasFrame::MeasFrame() : rep(0) {
  create();
}

MeasFrame::MeasFrame(const Measure &meas1) : rep(0) {
  create();
  fill(&meas1);
}

MeasFrame::MeasFrame(const Measure &meas1, const Measure &meas2) : rep(0) {
  create();
  fill(&meas1);
  fill(&meas2);
}

MeasFrame::MeasFrame(const Measure &meas1, const Measure &meas2,
		     const Measure &meas3) : rep(0) {
  create();
  fill(&meas1);
  fill(&meas2);
  fill(&meas3);
}

MeasFrame::MeasFrame(const MeasFrame &other) {
  rep = other.rep;
  if (rep) rep->cnt++;
}

// Destructor
MeasFrame::~MeasFrame() {
  if (rep && rep->cnt && --rep->cnt == 0) delete rep;
}

// Operators
MeasFrame &MeasFrame::operator=(const MeasFrame &other) {
  if (this != &other) {
    if (other.rep) other.rep->cnt++;
    if (rep && rep->cnt && --rep->cnt == 0) delete rep;
    rep = other.rep;
  }
  return *this;
}

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
    uInt locker = 0;
    lock(locker);
    delete rep->epval;
    rep->epval = val.clone();
    unlock(locker);
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
    uInt locker = 0;
    lock(locker);
    delete rep->posval;
    rep->posval = val.clone();
    unlock(locker);
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
    uInt locker = 0;
    lock(locker);
    delete rep->dirval;
    rep->dirval = val.clone();
    unlock(locker);
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
    uInt locker = 0;
    lock(locker);
    delete rep->radval;
    rep->radval = val.clone();
    unlock(locker);
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
  if (rep) return rep->epval;
  return 0;
}

const Measure* MeasFrame::position() const{
  if (rep) return rep->posval;
  return 0;
}

const Measure* MeasFrame::direction() const{
  if (rep) return rep->dirval;
  return 0;
}

const Measure* MeasFrame::radialVelocity() const{
  if (rep) return rep->radval;
  return 0;
}

const MeasComet* MeasFrame::comet() const{
  if (rep) return rep->comval;
  return 0;
}

void MeasFrame::lock(uInt &locker) {
  locker = 1;
  if (rep) locker = rep->cnt++;
}

void MeasFrame::unlock(const uInt locker) {
  if (rep) rep->cnt = locker;
}

Bool MeasFrame::getTDB(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getTDB(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getUT1(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getUT1(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getTT(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getTT(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLong(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLong(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLat(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLat(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getITRF(MVPosition &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getITRF(tdb));
  tdb = MVPosition(0.0);
  return False; 
}

Bool MeasFrame::getRadius(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getRadius(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLatGeo(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLatGeo(tdb));
  tdb = 0;
  return False;
}

Bool MeasFrame::getLAST(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLAST(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLASTr(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLASTr(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getJ2000(MVDirection &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getJ2000(tdb));
  tdb = Double(0.0);
  return False; 
}

Bool MeasFrame::getJ2000Long(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getJ2000Long(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getJ2000Lat(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getJ2000Lat(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getB1950(MVDirection &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getB1950(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getB1950Long(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getB1950Long(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getB1950Lat(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getB1950Lat(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getApp(MVDirection &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getApp(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getAppLong(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getAppLong(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getAppLat(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getAppLat(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLSR(Double &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getLSR(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getCometType(uInt &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getCometType(tdb));
  tdb = 0;
  return False; 
}

Bool MeasFrame::getComet(MVPosition &tdb) const {
  if (rep && rep->mymcf) return (rep->mymcf->getComet(tdb));
  tdb = MVPosition(0.0);
  return False; 
}

void MeasFrame::create() {
  if (!rep) {
    rep = new FrameRep();
    uInt locker = 0;
    lock(locker);
    rep->mymcf = new MCFrame(*this);
    unlock(locker);
  }
}

void MeasFrame::fill(const Measure *in) {
  if (in) {
    uInt locker = 0;
    if (in->type() == Register(static_cast<MEpoch *>(0))) {
      lock(locker);
      delete rep->epval;
      rep->epval = in->clone();
      unlock(locker);
      makeEpoch();
    } else if (in->type() == Register(static_cast<MPosition *>(0))) {
      lock(locker);
      delete rep->posval;
      rep->posval = in->clone();
      unlock(locker);
      makePosition();
    } else if (in->type() == Register(static_cast<MDirection *>(0))) {
      lock(locker);
      delete rep->dirval;
      rep->dirval = in->clone();
      unlock(locker);
      makeDirection();
    } else if (in->type() == Register(static_cast<MRadialVelocity *>(0))) {
      lock(locker);
      delete rep->radval;
      rep->radval = in->clone();
      unlock(locker);
      makeRadialVelocity();
    } else {
      throw(AipsError("Unknown MeasFrame Measure type " +
		      in->tellMe()));
    }
  }
}

void MeasFrame::fill(const MeasComet *in) {
  if (in) {
    delete rep->comval; rep->comval = 0;
    if (in->ok()) {
      rep->comval = in->clone();
      if (!rep->comval->ok()) {
	delete rep->comval; rep->comval = 0;
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
  rep->mymcf->makeEpoch();
}

void MeasFrame::makePosition() {
  rep->mymcf->makePosition();
}

void MeasFrame::makeDirection() {
  rep->mymcf->makeDirection();
}

void MeasFrame::makeRadialVelocity() {
  rep->mymcf->makeRadialVelocity();
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

} //# NAMESPACE CASACORE - END

