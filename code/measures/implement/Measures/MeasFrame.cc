//# MeasFrame.cc: Container for Measure frame
//# Copyright (C) 1996,1997
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
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_measframe_bug1;
#endif
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MeasConvert.h>

// Representation class
class FrameRep {
public:
  // Constructor
  FrameRep() :
    epval(0), posval(0), dirval(0), radval(0),
    epConvTDB(0), epTDBp(0), 
    epConvLAST(0), epLASTp(0), 
    posConvLong(0), posLongp(0),
    dirConvJ2000(0), dirJ2000p(0),
    dirConvB1950(0), dirB1950p(0),
    dirConvApp(0), dirAppp(0),
    radConvLSR(0), radLSRp(0),
    cnt(1) {;};
  // Destructor
  ~FrameRep() {
    delete epval; 
    delete posval;
    delete dirval;
    delete radval;
    delete (MEpoch::Convert *) epConvTDB;
    delete epTDBp;
    delete (MEpoch::Convert *) epConvLAST;
    delete epLASTp;
    delete (MPosition::Convert *) posConvLong;
    delete posLongp;
    delete (MDirection::Convert *) dirConvJ2000;
    delete dirJ2000p;
    delete (MDirection::Convert *) dirConvB1950;
    delete dirB1950p;
    delete (MDirection::Convert *) dirConvApp;
    delete dirAppp;
    delete (MRadialVelocity::Convert *) radConvLSR;
    delete radLSRp;
  }

  // The actual measures
  // <group>
  // Epoch in time
  MEpoch *epval;
  // Position
  MPosition *posval;
  // Direction
  MDirection *dirval;
  // Radial velocity
  MRadialVelocity *radval;
  // Conversion to TDB time (due to some (for me) unsolvable dependency errors
  // not the proper MeasConvert* here)
  void *epConvTDB;
  // TDB time
  Double *epTDBp;
  // Conversion to LAST time
  void *epConvLAST;
  // LAST time
  Double *epLASTp;
  // Conversion to astronomical longitude/latitude
  void *posConvLong;
  // Longitude
  Vector<Double> *posLongp;
  // Conversion to J2000
  void *dirConvJ2000;
  // J2000 coordinates
  MVDirection *dirJ2000p;
  // Conversion to B1950
  void *dirConvB1950;
  // B1950 coordinates
  MVDirection *dirB1950p;
  // Conversion to apparent coordinates
  void *dirConvApp;
  // Apparent coordinates
  MVDirection *dirAppp;
  // Conversion to LSR radial velocity
  void *radConvLSR;
  // Radial velocity
  Double *radLSRp;
  // </group>
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
	if (rep->epTDBp) {
	    delete rep->epTDBp; rep->epTDBp = 0;
	};
	if (rep->epLASTp) {
	    delete rep->epLASTp; rep->epLASTp = 0;
	};
	if (rep->dirAppp) {
	    delete rep->dirAppp; rep->dirAppp = 0;
	};
	if (rep->radLSRp) {
	    delete rep->radLSRp; rep->radLSRp = 0;
	};
    } else {
	errorReset(MEpoch::showMe());
    };
}

void MeasFrame::resetEpoch(const MEpoch &val) {
    if (rep && rep->epval) {
	delete rep->epval;
	rep->epval = (MEpoch *) val.clone();
	makeEpoch();
    } else {
	errorReset(MEpoch::showMe());
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
	if (rep->posLongp) {
	    delete rep->posLongp; rep->posLongp = 0;
	};
	if (rep->epLASTp) {
	    delete rep->epLASTp; rep->epLASTp = 0;
	};
    } else {
	errorReset(MPosition::showMe());
    };
}

void MeasFrame::resetPosition(const MPosition &val) {
    if (rep && rep->posval) {
	delete rep->posval;
	rep->posval = (MPosition *) val.clone();
	makePosition();
    } else {
	errorReset(MPosition::showMe());
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
	if (rep->dirJ2000p) {
	    delete rep->dirJ2000p; rep->dirJ2000p = 0;
	};
	if (rep->dirB1950p) {
	    delete rep->dirB1950p; rep->dirB1950p = 0;
	};
	if (rep->dirAppp) {
	    delete rep->dirAppp; rep->dirAppp = 0;
	};
	if (rep->radLSRp) {
	    delete rep->radLSRp; rep->radLSRp = 0;
	};
    } else {
	errorReset(MDirection::showMe());
    };
}

void MeasFrame::resetDirection(const MDirection &val) {
    if (rep && rep->dirval) {
	delete rep->dirval;
	rep->dirval = (MDirection *) val.clone();
	makeDirection();
    } else {
	errorReset(MDirection::showMe());
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
	if (rep->radLSRp) {
	    delete rep->radLSRp; rep->radLSRp = 0;
	};
    } else {
	errorReset(MRadialVelocity::showMe());
    };
}

void MeasFrame::resetRadialVelocity(const MRadialVelocity &val) {
    if (rep && rep->radval) {
	delete rep->radval;
	rep->radval = (MRadialVelocity *) val.clone();
	makeRadialVelocity();
    } else {
	errorReset(MRadialVelocity::showMe());
    };
}

Bool MeasFrame::getTDB(Double &tdb) const {
    if (rep && rep->epval) {
	if (!rep->epTDBp) {
	    rep->epTDBp = new Double;
	    *(rep->epTDBp) = (*(MEpoch::Convert *)
			      (rep->epConvTDB))().getValue().get();
	};
	tdb = *(rep->epTDBp);
	return True;
    };
    tdb = 0;
    return False;
}

Bool MeasFrame::getLong(Double &tdb) const {
    if (rep && rep->posval) {
	if (!rep->posLongp) {
	    rep->posLongp = new Vector<Double>(3);
	    *(rep->posLongp) = (*(MPosition::Convert *)
				(rep->posConvLong))().getValue().get();
	};
	tdb = MVAngle(rep->posLongp->operator()(1))(-0.5);
	return True;
    };
    tdb = 0;
    return False;
}

Bool MeasFrame::getLat(Double &tdb) const {
    if (rep && rep->posval) {
	if (!rep->posLongp) {
	    rep->posLongp = new Vector<Double>(3);
	    *(rep->posLongp) = (*(MPosition::Convert *)
				(rep->posConvLong))().getValue().get();
	};
	tdb = rep->posLongp->operator()(2);
	return True;
    };
    tdb = 0;
    return False;
}

Bool MeasFrame::getRadius(Double &tdb) const {
    if (rep && rep->posval) {
	if (!rep->posLongp) {
	    rep->posLongp = new Vector<Double>(3);
	    *(rep->posLongp) = (*(MPosition::Convert *)
				(rep->posConvLong))().getValue().get();
	};
	tdb = rep->posLongp->operator()(0);
	return True;
    };
    tdb = 0;
    return False;
}

Bool MeasFrame::getLAST(Double &tdb) const {
    if (rep && rep->epval) {
	if (!rep->epLASTp) {
	    rep->epLASTp = new Double;
	    *(rep->epLASTp) = (*(MEpoch::Convert *)
			      (rep->epConvLAST))().getValue().get();
	};
	tdb = fmod(*(rep->epLASTp),1.0);
	return True;
    };
    tdb = 0;
    return False;
}

Bool MeasFrame::getLASTr(Double &tdb) const {
    Bool tmp = MeasFrame::getLAST(tdb);
    tdb *= C::circle;
    return tmp;
}

Bool MeasFrame::getJ2000(MVDirection &tdb) const {
    if (rep && rep->dirval) {
	if (!rep->dirJ2000p) {
	    rep->dirJ2000p = new MVDirection;
	    *(rep->dirJ2000p) = (*(MDirection::Convert *)
				(rep->dirConvJ2000))().getValue();
	};
	tdb = *(rep->dirJ2000p);
	return True;
    };
    tdb = (Double) 0.0;
    return False;
}

Bool MeasFrame::getB1950(MVDirection &tdb) const {
    if (rep && rep->dirval) {
	if (!rep->dirB1950p) {
	    rep->dirB1950p = new MVDirection;
	    *(rep->dirB1950p) = (*(MDirection::Convert *)
				(rep->dirConvB1950))().getValue();
	};
	tdb = *(rep->dirB1950p);
	return True;
    };
    tdb = (Double) 0.0;
    return False;
}

Bool MeasFrame::getApp(MVDirection &tdb) const {
    if (rep && rep->dirval) {
	if (!rep->dirAppp) {
	    rep->dirAppp = new MVDirection;
	    *(rep->dirAppp) = (*(MDirection::Convert *)
				(rep->dirConvApp))().getValue();
	};
	tdb = *(rep->dirAppp);
	return True;
    };
    tdb = (Double) 0.0;
    return False;
}

Bool MeasFrame::getLSR(Double &tdb) const {
    if (rep && rep->radval) {
	if (!rep->radLSRp) {
	    rep->radLSRp = new Double;
	    *(rep->radLSRp) = (*(MRadialVelocity::Convert *)
				(rep->radConvLSR))().getValue();
	};
	tdb = *(rep->radLSRp);
	return True;
    };
    tdb = (Double) 0.0;
    return False;
}

const MEpoch *const MeasFrame::epoch() const{
    if (rep) return rep->epval;
    return 0;
}

const MPosition *const MeasFrame::position() const{
    if (rep) return rep->posval;
    return 0;
}

const MDirection *const MeasFrame::direction() const{
    if (rep) return rep->dirval;
    return 0;
}

const MRadialVelocity *const MeasFrame::radialVelocity() const{
    if (rep) return rep->radval;
    return 0;
}

void MeasFrame::create() {
    if (!rep) rep = new FrameRep();
}

void MeasFrame::fill(const Measure *in) {
    if (in) {
	if (in->areYou("EPOCH")) {
	    delete rep->epval;
	    rep->epval = (MEpoch *) in->clone();
	    makeEpoch();
	} else if (in->areYou("Position")) {
	    delete rep->posval;
	    rep->posval = (MPosition *) in->clone();
	    makePosition();
	} else if (in->areYou("Direction")) {
	    delete rep->dirval;
	    rep->dirval = (MDirection *) in->clone();
	    makeDirection();
	} else if (in->areYou("Radialvelocity")) {
	    delete rep->radval;
	    rep->radval = (MRadialVelocity *) in->clone();
	    makeRadialVelocity();
	} else {
	    throw(AipsError("Unknown MeasFrame Measure type " +
			    in->tellMe()));
	};
    };
}

void MeasFrame::makeEpoch() {
    static const MEpoch::Ref REFTDB 
	= MEpoch::Ref(MEpoch::TDB);
    delete (MEpoch::Convert *) rep->epConvTDB;
    rep->epConvTDB = new MEpoch::Convert(*(rep->epval),
					 REFTDB);
    if (rep->epTDBp) {
	delete rep->epTDBp; rep->epTDBp = 0;
    };
    if (rep->epConvLAST) {
      rep->cnt++;
      delete (MEpoch::Convert *) rep->epConvLAST;
      rep->epConvLAST = 0;
    };
    rep->epConvLAST = new MEpoch::Convert(*(rep->epval),
					 MEpoch::Ref(MEpoch::LAST, *this));
    if (rep->epConvLAST) {
      --rep->cnt;
    };
    if (rep->epLASTp) {
	delete rep->epLASTp; rep->epLASTp = 0;
    };
    if (rep->dirAppp) {
	delete rep->dirAppp; rep->dirAppp = 0;
    };
    if (rep->radLSRp) {
      delete rep->radLSRp; rep->radLSRp = 0;
    };
}

void MeasFrame::makePosition() {
    static const MPosition::Ref REFLONG 
	= MPosition::Ref(MPosition::ITRF);
    delete (MPosition::Convert *) rep->posConvLong;
    rep->posConvLong = new MPosition::Convert(*(rep->posval),
					      REFLONG);
    if (rep->posLongp) {
	delete rep->posLongp; rep->posLongp = 0;
    };
    if (rep->epLASTp) {
	delete rep->epLASTp; rep->epLASTp = 0;
    };
    if (rep->radLSRp) {
      delete rep->radLSRp; rep->radLSRp = 0;
    };
}

void MeasFrame::makeDirection() {
    static const MDirection::Ref REFJ2000 
	= MDirection::Ref(MDirection::J2000);
    delete (MDirection::Convert *) rep->dirConvJ2000;
    rep->dirConvJ2000 = new MDirection::Convert(*(rep->dirval),
					      REFJ2000);
    static const MDirection::Ref REFB1950 
	= MDirection::Ref(MDirection::B1950);
    delete (MDirection::Convert *) rep->dirConvB1950;
    rep->dirConvB1950 = new MDirection::Convert(*(rep->dirval),
					      REFB1950);
    if (rep->dirConvApp) {
      rep->cnt++;
      delete (MDirection::Convert *) rep->dirConvApp;
      rep->dirConvApp = 0;
    };
    rep->dirConvApp = new MDirection::Convert(*(rep->dirval),
					      MDirection::Ref(MDirection::APP,
							      *this));
    if (rep->dirConvApp) {
      --rep->cnt;
    };
    if (rep->dirJ2000p) {
	delete rep->dirJ2000p; rep->dirJ2000p = 0;
    };
    if (rep->dirB1950p) {
	delete rep->dirB1950p; rep->dirB1950p = 0;
    };
    if (rep->dirAppp) {
	delete rep->dirAppp; rep->dirAppp = 0;
    };
    if (rep->radLSRp) {
      delete rep->radLSRp; rep->radLSRp = 0;
    };
}

void MeasFrame::makeRadialVelocity() {
    static const MRadialVelocity::Ref REFLSR 
	= MRadialVelocity::Ref(MRadialVelocity::LSR);
    delete (MRadialVelocity::Convert *) rep->radConvLSR;
    rep->radConvLSR = new MRadialVelocity::Convert(*(rep->radval),
					      REFLSR);
    if (rep->radLSRp) {
	delete rep->radLSRp; rep->radLSRp = 0;
    };
}

void MeasFrame::errorReset(const String &txt) {
    throw(AipsError("Attempt to reset non-existent frame member "+txt));
}

ostream &operator<<(ostream &os, const MeasFrame &mf) {
    os << "Frame: ";
    Double tmp;
    if (mf.rep && mf.rep->epval) {
	mf.getTDB(tmp);
	os << (MEpoch) *(mf.rep->epval);
	os << " (TDB = " << tmp << ")";
    };
    if (mf.rep && mf.rep->posval) {
	if (mf.rep && mf.rep->epval) {
	    os << endl << "       ";
	};
	os << (MPosition) *(mf.rep->posval);
	mf.getLong(tmp);
	os << endl << "        (Longitude = " << tmp;
	mf.getLat(tmp);
	os << " Latitude = " << tmp << ")";
    };
    if (mf.rep && mf.rep->dirval) {
	if (mf.rep && (mf.rep->epval || mf.rep->posval)) {
	    os << endl << "       ";
	};
	os << (MDirection) *(mf.rep->dirval);
	MVDirection tmp;    
	mf.getJ2000(tmp);
	os << endl << "        (J2000 = " << 
	    tmp.getAngle("deg") << ")";
    };
    if (mf.rep && mf.rep->radval) {
	if (mf.rep && (mf.rep->epval || mf.rep->posval ||
		       mf.rep->dirval)) {
	    os << endl << "       ";
	};
	os << (MRadialVelocity) *(mf.rep->radval);
	mf.getLSR(tmp); tmp /= 1000.;
	os << endl << "        (LSR velocity = " << 
	    Quantity(tmp,"km/s") << ")";
    };
    return os;
}
