//# MeasFrame.cc: Container for Measure frame
//# Copyright (C) 1996,1997,1998
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
#ifdef __GNUG__
#include <aips/Quanta/Quantum.h>
typedef Quantum<Double> gpp_MeasFrame_bug1;
#endif
#include <aips/Exceptions/Error.h>
#include <aips/RTTI/Register.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Measures/MeasFrame.h>

// Representation class
class FrameRep {
public:
  // Constructor
  FrameRep() :
    epval(0), epset(False), epreset(False),
    posval(0), posset(False), posreset(False),
    dirval(0), dirset(False), dirreset(False),
    radval(0), radset(False), radreset(False),
    mymcf(0), delmcf(0),
    getdbl(0), getmvdir(0), getmvpos(0),
    cnt(1) {;};
  // Destructor
  ~FrameRep() {
    delete epval; 
    delete posval;
    delete dirval;
    delete radval;
    if (mymcf) delmcf(mymcf);		// delete conversion frame data
  }
  
  // The actual measures
  // <group>
  // Epoch in time
  Measure *epval;
  Bool epset;
  Bool epreset;
  // Position
  Measure *posval;
  Bool posset;
  Bool posreset;
  // Direction
  Measure *dirval;
  Bool dirset;
  Bool dirreset;
  // Radial velocity
  Measure *radval;
  Bool radset;
  Bool radreset;
  // Pointer to belonging conversion frame
  void *mymcf;
  // Pointer to conversion frame deletion
  void (*delmcf)(void*);
  // Pointer to get a double
  Bool (*getdbl)(void*, uInt, Double &);
  // Pointer to get an MVDirection
  Bool (*getmvdir)(void*, uInt, MVDirection &);
  // Pointer to get an MVPosition
  Bool (*getmvpos)(void*, uInt, MVPosition &);
  // Usage count
  Int cnt;
};

// MeasFrame class

//# Constructors
MeasFrame::MeasFrame() :
  rep(0) {
    create();
  }

MeasFrame::MeasFrame(const Measure &meas1) :
  rep(0) {
    create();
    fill(&meas1);
  }

MeasFrame::MeasFrame(const Measure &meas1, const Measure &meas2) :
  rep(0) {
    create();
    fill(&meas1);
    fill(&meas2);
  }

MeasFrame::MeasFrame(const Measure &meas1, const Measure &meas2,
		     const Measure &meas3) :
  rep(0) {
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
  if (rep && --rep->cnt == 0) {
    delete rep;
  };
}

// Operators
MeasFrame &MeasFrame::operator=(const MeasFrame &other) {
  if (this != &other) {
    if (other.rep) other.rep->cnt++;
    if (rep && --rep->cnt == 0) {
      delete rep;
    }
    rep = other.rep;
  }
  return *this;
}

Bool MeasFrame::operator==(const MeasFrame &other) const{
  return ToBool(rep == other.rep);
}

Bool MeasFrame::operator!=(const MeasFrame &other) const{
  return ToBool(rep != other.rep);
}

// General member functions
Bool MeasFrame::empty() const{
  return ToBool( !(rep && (rep->epval || rep->posval || 
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

void MeasFrame::resetEpoch(const MVEpoch  &val) {
  if (rep && rep->epval) {
    rep->epval->set(val);
    rep->epreset = True;
  } else {
    errorReset(String("Epoch"));
  };
}

void MeasFrame::resetEpoch(const Measure &val) {
  if (rep && rep->epval) {
    delete rep->epval;
    rep->epval = val.clone();
    makeEpoch();
  } else {
    errorReset(String("Epoch"));
  };
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
    rep->posreset = True;
  } else {
    errorReset(String("Position"));
  };
}

void MeasFrame::resetPosition(const Measure &val) {
  if (rep && rep->posval) {
    delete rep->posval;
    rep->posval = val.clone();
    makePosition();
  } else {
    errorReset(String("Position"));
  };
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
    rep->dirreset = True;
  } else {
    errorReset(String("Direction"));
  };
}

void MeasFrame::resetDirection(const Measure &val) {
  if (rep && rep->dirval) {
    delete rep->dirval;
    rep->dirval = val.clone();
    makeDirection();
  } else {
    errorReset(String("Direction"));
  };
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
    rep->radreset = True;
  } else {
    errorReset(String("RadialVelocity"));
  };
}

void MeasFrame::resetRadialVelocity(const Measure &val) {
  if (rep && rep->radval) {
    delete rep->radval;
    rep->radval = val.clone();
    makeRadialVelocity();
  } else {
    errorReset(String("RadialVelocity"));
  };
}

const Measure *const MeasFrame::epoch() const{
  if (rep) return rep->epval;
  return 0;
}

const Measure *const MeasFrame::position() const{
  if (rep) return rep->posval;
  return 0;
}

const Measure *const MeasFrame::direction() const{
  if (rep) return rep->dirval;
  return 0;
}

const Measure *const MeasFrame::radialVelocity() const{
  if (rep) return rep->radval;
  return 0;
}

Bool MeasFrame::getEpset() const {
  if (rep) return rep->epset;
  return False;
}

Bool MeasFrame::getPosset() const {
  if (rep) return rep->posset;
  return False;
}

Bool MeasFrame::getDirset() const {
  if (rep) return rep->dirset;
  return False;
}

Bool MeasFrame::getRadset() const {
  if (rep) return rep->radset;
  return False;
}

Bool MeasFrame::getEpreset() const {
  if (rep) return rep->epreset;
  return False;
}

Bool MeasFrame::getPosreset() const {
  if (rep) return rep->posreset;
  return False;
}

Bool MeasFrame::getDirreset() const {
  if (rep) return rep->dirreset;
  return False;
}

Bool MeasFrame::getRadreset() const {
  if (rep) return rep->radreset;
  return False;
}

void MeasFrame::setEpset(Bool in) {
  if (rep) rep->epset = in;
}

void MeasFrame::setPosset(Bool in) {
  if (rep) rep->posset = in;
}

void MeasFrame::setDirset(Bool in) {
  if (rep) rep->dirset = in;
}

void MeasFrame::setRadset(Bool in) {
  if (rep) rep->radset = in;
}

void MeasFrame::setEpreset(Bool in) {
  if (rep) rep->epreset = in;
}

void MeasFrame::setPosreset(Bool in) {
  if (rep) rep->posreset = in;
}

void MeasFrame::setDirreset(Bool in) {
  if (rep) rep->dirreset = in;
}

void MeasFrame::setRadreset(Bool in) {
  if (rep) rep->radreset = in;
}

void MeasFrame::setMCFramePoint(void *in) {
  if (rep) rep->mymcf = in;
}

void MeasFrame::setMCFrameDelete(void (*in)(void*)) {
  if (rep) rep->delmcf = in;
}

void MeasFrame::setMCFrameGetdbl(Bool (*in)(void *, uInt, Double &)) {
  if (rep) rep->getdbl = in;
}

void MeasFrame::setMCFrameGetmvdir(Bool (*in)(void *, uInt, MVDirection &)) {
  if (rep) rep->getmvdir = in;
}

void MeasFrame::setMCFrameGetmvpos(Bool (*in)(void *, uInt, MVPosition &)) {
  if (rep) rep->getmvpos = in;
}

void *MeasFrame::getMCFramePoint() const {
  if (rep) return rep->mymcf;
  return 0;
}

void MeasFrame::lock() {
  if (rep) rep->cnt++;
}

void MeasFrame::unlock() {
  if (rep) rep->cnt--;
}

Bool MeasFrame::getTDB(Double &tdb) {
  if (rep && rep->mymcf) return rep->getdbl(rep->mymcf, GetTDB, tdb);
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLong(Double &tdb) {
  if (rep && rep->mymcf) return rep->getdbl(rep->mymcf, GetLong, tdb);
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLat(Double &tdb) {
  if (rep && rep->mymcf) return rep->getdbl(rep->mymcf, GetLat, tdb);
  tdb = 0;
  return False; 
}

Bool MeasFrame::getITRF(MVPosition &tdb) {
  if (rep && rep->mymcf) return rep->getmvpos(rep->mymcf, GetITRF, tdb);
  tdb = 0;
  return False; 
}

Bool MeasFrame::getRadius(Double &tdb) {
  if (rep && rep->mymcf) return rep->getdbl(rep->mymcf, GetRadius, tdb);
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLAST(Double &tdb) {
  if (rep && rep->mymcf) return rep->getdbl(rep->mymcf, GetLAST, tdb);
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLASTr(Double &tdb) {
  if (rep && rep->mymcf) return rep->getdbl(rep->mymcf, GetLASTr, tdb);
  tdb = 0;
  return False; 
}

Bool MeasFrame::getJ2000(MVDirection &tdb) {
  if (rep && rep->mymcf) return rep->getmvdir(rep->mymcf, GetJ2000, tdb);
  tdb = Double(0.0);
  return False; 
}

Bool MeasFrame::getB1950(MVDirection &tdb) {
  if (rep && rep->mymcf) return rep->getmvdir(rep->mymcf, GetB1950, tdb);
  tdb = 0;
  return False; 
}

Bool MeasFrame::getApp(MVDirection &tdb) {
  if (rep && rep->mymcf) return rep->getmvdir(rep->mymcf, GetApp, tdb);
  tdb = 0;
  return False; 
}

Bool MeasFrame::getLSR(Double &tdb) {
  if (rep && rep->mymcf) return rep->getdbl(rep->mymcf, GetLSR, tdb);
  tdb = 0;
  return False; 
}

void MeasFrame::create() {
  if (!rep) rep = new FrameRep();
}

void MeasFrame::fill(const Measure *in) {
  if (in) {
    if (in->type() == Register((MEpoch *)0)) {
      delete rep->epval;
      rep->epval = in->clone();
      makeEpoch();
    } else if (in->type() == Register((MPosition *)0)) {
      delete rep->posval;
      rep->posval = in->clone();
      makePosition();
    } else if (in->type() == Register((MDirection *)0)) {
      delete rep->dirval;
      rep->dirval = in->clone();
      makeDirection();
    } else if (in->type() == Register((MRadialVelocity *)0)) {
      delete rep->radval;
      rep->radval = in->clone();
      makeRadialVelocity();
    } else {
      throw(AipsError("Unknown MeasFrame Measure type " +
		      in->tellMe()));
    };
  };
}

void MeasFrame::makeEpoch() {
  rep->epset = True;
}

void MeasFrame::makePosition() {
  rep->posset = True;
}

void MeasFrame::makeDirection() {
  rep->dirset = True;
}

void MeasFrame::makeRadialVelocity() {
  rep->radset = True;
}

void MeasFrame::errorReset(const String &txt) {
  throw(AipsError("Attempt to reset non-existent frame member "+txt));
}

ostream &operator<<(ostream &os, MeasFrame &mf) {
  os << "Frame: ";
  Double tmp;
  if (mf.rep && mf.rep->epval) {
    os << *(mf.rep->epval);
    if (mf.getTDB(tmp)) 
      os << " (TDB = " << tmp << ")";
  };
  if (mf.rep && mf.rep->posval) {
    if (mf.rep && mf.rep->epval) os << endl << "       ";
    os << *(mf.rep->posval);
    if (mf.getLong(tmp)) {
      os << endl << "        (Longitude = " << tmp;
      mf.getLat(tmp);
      os << " Latitude = " << tmp << ")";
    };
  };
  if (mf.rep && mf.rep->dirval) {
    if (mf.rep && (mf.rep->epval || mf.rep->posval)) 
      os << endl << "       ";
    os << *(mf.rep->dirval);
    MVDirection tmp;    
    if (mf.getJ2000(tmp)) {
      os << endl << "        (J2000 = " << 
	tmp.getAngle("deg") << ")";
    };
  };
  if (mf.rep && mf.rep->radval) {
    if (mf.rep && (mf.rep->epval || mf.rep->posval ||
		   mf.rep->dirval)) {
      os << endl << "       ";
    };
    os << *(mf.rep->radval);
    if (mf.getLSR(tmp)) {
      tmp /= 1000.;
      os << endl << "        (LSR velocity = " << 
	Quantity(tmp,"km/s") << ")";
    };
  };
  return os;
}
