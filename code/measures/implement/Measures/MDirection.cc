//# MDirection.cc:  A Measure: astronomical direction
//# Copyright (C) 1995,1996,1997
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
typedef Quantum<Double> gpp_direction_bug1;
#endif
#include <aips/Exceptions.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/RotMatrix.h>
#include <aips/Measures/MVPosition.h>
#include <aips/Measures/SolarPos.h>
#include <aips/Measures/Aberration.h>
#include <aips/Measures/Precession.h>
#include <aips/Measures/Nutation.h>
#include <aips/Measures/MeasData.h>

//# Constructors
MDirection::MDirection() :
  MeasBase<MVDirection,MDirection::Ref>() {}

MDirection::MDirection(const MVDirection &dt) : 
  MeasBase<MVDirection,MDirection::Ref>(dt,MDirection::DEFAULT) {}

MDirection::MDirection(const MVDirection &dt, const MDirection::Ref &rf) : 
  MeasBase<MVDirection,MDirection::Ref>(dt,rf) {}

MDirection::MDirection(const MVDirection &dt, uInt rf) : 
  MeasBase<MVDirection,MDirection::Ref>(dt,rf) {}

MDirection::MDirection(const Quantity &dt, const Quantity &dt1) : 
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt,dt1),
					MDirection::DEFAULT) {}

MDirection::MDirection(const Quantity &dt, const Quantity &dt1,
		       const MDirection::Ref &rf) : 
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt,dt1),rf) {}

MDirection::MDirection(const Quantity &dt, const Quantity &dt1,
		       uInt rf) : 
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt,dt1),rf) {}

MDirection::MDirection(const Quantum<Vector<Double> > &dt) :
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt),
					MDirection::DEFAULT) {}

MDirection::MDirection(const Quantum<Vector<Double> > &dt,
		       const MDirection::Ref &rf) : 
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt),rf) {}

MDirection::MDirection(const Quantum<Vector<Double> > &dt,
		       uInt rf) : 
  MeasBase<MVDirection,MDirection::Ref>(MVDirection(dt),rf) {}

//# Destructor
MDirection::~MDirection() {}

//# Operators

//# Member functions

const String &MDirection::tellMe() const {
    return MDirection::showMe();
}

const String &MDirection::showMe() {
    static const String name("Direction");
    return name;
}

const String &MDirection::showType(uInt tp) {
    static const String tname[MDirection::N_Types] = {
	"J2000",
	"JMEAN",
	"JTRUE",
	"APP",
	"B1950",
	"BMEAN",
	"BTRUE",
	"GALACTIC",
	"HADEC",
	"AZEL"};
    DebugAssert(tp < MDirection::N_Types, AipsError);
    return tname[tp];
}

Bool MDirection::giveMe(const String &in, MDirection::Ref &mr) {
    static const Int N_name = 10;
    static const String tname[N_name] = {
	"J2000",
	"JMEAN",
	"JTRUE",
	"APP",
	"B1950",
	"BMEAN",
	"BTRUE",
	"GALACTIC",
	"HADEC",
	"AZEL"};

    static const uInt oname[N_name] = {
	MDirection::J2000,
	MDirection::JMEAN,
	MDirection::JTRUE,
	MDirection::APP,
	MDirection::B1950,
	MDirection::BMEAN,
	MDirection::BTRUE,
	MDirection::GALACTIC,
	MDirection::HADEC,
	MDirection::AZEL};

    uInt i = Measure::giveMe(in, N_name, tname);

    if (i>=N_name) {
	mr = MDirection::Ref();
	return False;
    } else {
	mr = MDirection::Ref(oname[i]);
    };
    return True;
}

Quantum<Vector<Double> > MDirection::getAngle() const {
    return (data.getAngle());
}

Quantum<Vector<Double> > MDirection::getAngle(const Unit &inunit) const {
    return (data.getAngle(inunit));
}

void *MDirection::clone() const {
    return ((void *) new MDirection(*this));
}

void MDirection::getConvert(MDirection::Convert &mc,
			    const MDirection::Ref &inref,
			    const MDirection::Ref &outref) {

// Array of conversion routines to call
    static const MDirection::Routes 
	FromTo[MDirection::N_Types][MDirection::N_Types] = {
	{ MDirection::N_Routes,
	  MDirection::J2000_JMEAN,
	  MDirection::J2000_JMEAN,
	  MDirection::J2000_APP, 
	  MDirection::J2000_B1950,
	  MDirection::J2000_B1950, 
	  MDirection::J2000_B1950,
	  MDirection::J2000_GAL,
	  MDirection::J2000_APP,
	  MDirection::J2000_APP},
    { MDirection::JMEAN_J2000, 
      MDirection::N_Routes,    
      MDirection::JMEAN_JTRUE,
      MDirection::JMEAN_J2000, 
      MDirection::JMEAN_J2000, 
      MDirection::JMEAN_J2000, 
      MDirection::JMEAN_J2000,
      MDirection::JMEAN_J2000,
      MDirection::JMEAN_J2000, 
      MDirection::JMEAN_J2000},
    { MDirection::JTRUE_JMEAN, 
      MDirection::JTRUE_JMEAN, 
      MDirection::N_Routes,
      MDirection::JTRUE_JMEAN,  
      MDirection::JTRUE_JMEAN, 
      MDirection::JTRUE_JMEAN, 
      MDirection::JTRUE_JMEAN,
      MDirection::JTRUE_JMEAN,
      MDirection::JTRUE_JMEAN,  
      MDirection::JTRUE_JMEAN},
    { MDirection::APP_J2000,  
      MDirection::APP_J2000,  
      MDirection::APP_J2000,  
      MDirection::N_Routes,    
      MDirection::APP_B1950,  
      MDirection::APP_B1950,  
      MDirection::APP_B1950,
      MDirection::APP_J2000,
      MDirection::APP_HADEC, 
      MDirection::APP_HADEC},
    { MDirection::B1950_J2000, 
      MDirection::B1950_J2000, 
      MDirection::B1950_J2000, 
      MDirection::B1950_APP, 
      MDirection::N_Routes,    
      MDirection::B1950_BMEAN,
      MDirection::B1950_BMEAN,
      MDirection::B1950_GAL,
      MDirection::B1950_APP, 
      MDirection::B1950_APP},
    { MDirection::BMEAN_B1950, 
      MDirection::BMEAN_B1950, 
      MDirection::BMEAN_B1950,
      MDirection::BMEAN_B1950, 
      MDirection::BMEAN_B1950, 
      MDirection::N_Routes,
      MDirection::BMEAN_BTRUE,
      MDirection::BMEAN_B1950,
      MDirection::BMEAN_B1950, 
      MDirection::BMEAN_B1950},
    { MDirection::BTRUE_BMEAN, 
      MDirection::BTRUE_BMEAN, 
      MDirection::BTRUE_BMEAN,
      MDirection::BTRUE_BMEAN, 
      MDirection::BTRUE_BMEAN, 
      MDirection::BTRUE_BMEAN, 
      MDirection::N_Routes,   
      MDirection::BTRUE_BMEAN,
      MDirection::BTRUE_BMEAN, 
      MDirection::BTRUE_BMEAN},
    { MDirection::GAL_J2000,   
      MDirection::GAL_J2000,   
      MDirection::GAL_J2000, 
      MDirection::GAL_J2000,   
      MDirection::GAL_B1950,   
      MDirection::GAL_B1950,
      MDirection::GAL_B1950,
      MDirection::N_Routes,
      MDirection::GAL_J2000,   
      MDirection::GAL_J2000},
    { MDirection::HADEC_APP,   
      MDirection::HADEC_APP,   
      MDirection::HADEC_APP, 
      MDirection::HADEC_APP,   
      MDirection::HADEC_APP,   
      MDirection::HADEC_APP,
      MDirection::HADEC_APP,
      MDirection::HADEC_APP,
      MDirection::N_Routes,    
      MDirection::HADEC_AZEL},
    { MDirection::AZEL_HADEC,  
      MDirection::AZEL_HADEC,  
      MDirection::AZEL_HADEC,  
      MDirection::AZEL_HADEC,  
      MDirection::AZEL_HADEC,  
      MDirection::AZEL_HADEC,  
      MDirection::AZEL_HADEC,  
      MDirection::AZEL_HADEC,  
      MDirection::AZEL_HADEC,  
      MDirection::N_Routes}
    };

// List of codes converted to
    static const MDirection::Types ToRef[MDirection::N_Routes] = {
	MDirection::J2000,    	MDirection::B1950,
	MDirection::GALACTIC, 	MDirection::GALACTIC,
	MDirection::B1950,    	MDirection::J2000,
	MDirection::JMEAN,    	MDirection::BMEAN,
	MDirection::J2000,    	MDirection::JTRUE,
	MDirection::B1950,    	MDirection::BTRUE,
	MDirection::JMEAN,	MDirection::BMEAN,    	
	MDirection::APP,      	MDirection::J2000,
	MDirection::APP,      	MDirection::B1950,
	MDirection::HADEC,    	MDirection::AZEL,
	MDirection::HADEC,    	MDirection::APP
	};

    Int iout = outref.getType();
    Int iin  = inref.getType();
    Int tmp;
    while (iin != iout) {
	tmp = FromTo[iin][iout];
	iin = ToRef[tmp];
	mc.addMethod(tmp);
	initConvert(tmp, mc);
    }
}

void MDirection::clearConvert(MDirection::Convert &mc) {
  delete (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
  delete (Euler *) mc.getStruct(MDirection::EULER1);
  delete (MVPosition *) mc.getStruct(MDirection::MVPOS1);
  delete (MVPosition *) mc.getStruct(MDirection::MVPOS2);
  delete (MVPosition *) mc.getStruct(MDirection::MVPOS3);
  delete (SolarPos *) mc.getStruct(MDirection::SOLPOSFROM);
  delete (SolarPos *) mc.getStruct(MDirection::SOLPOSTO);
  delete (Aberration *) mc.getStruct(MDirection::ABERFROM);
  delete (Aberration *) mc.getStruct(MDirection::ABERTO);
  delete (Nutation *) mc.getStruct(MDirection::NUTATFROM);
  delete (Nutation *) mc.getStruct(MDirection::NUTATTO);
  delete (Precession *) mc.getStruct(MDirection::PRECESFROM);
  delete (Precession *) mc.getStruct(MDirection::PRECESTO);
}

//# Conversion routines
void MDirection::initConvert(uInt which, MDirection::Convert &mc) {
    if (!(mc.getStruct(MDirection::ROTMAT1))) {
	mc.addStruct(MDirection::ROTMAT1,
		     (void *) new RotMatrix());
    };
    if (!(mc.getStruct(MDirection::MVPOS1))) {
	mc.addStruct(MDirection::MVPOS1,
		     (void *) new MVPosition());
    };
    if (!(mc.getStruct(MDirection::MVPOS2))) {
	mc.addStruct(MDirection::MVPOS2,
		     (void *) new MVPosition());
    };
    if (!(mc.getStruct(MDirection::MVPOS3))) {
	mc.addStruct(MDirection::MVPOS3,
		     (void *) new MVPosition());
    };
    if (!(mc.getStruct(MDirection::EULER1))) {
	mc.addStruct(MDirection::EULER1,
		     (void *) new Euler());
    };

    switch (which) {

	case GAL_J2000:
	break;

	case GAL_B1950:
	break;

	case J2000_GAL:
	break;

	case B1950_GAL:
	break;

	case J2000_B1950:
	break;

	case B1950_J2000:
	break;

	case J2000_JMEAN:
	mc.addStruct(MDirection::PRECESFROM,
		     (void *) new Precession(Precession::STANDARD));
	break;

	case B1950_BMEAN:
	mc.addStruct(MDirection::PRECESFROM,
		     (void *) new Precession(Precession::B1950));
	break;

	case JMEAN_J2000:
	mc.addStruct(MDirection::PRECESTO,
		     (void *) new Precession(Precession::STANDARD));
	break;

	case JMEAN_JTRUE:
	mc.addStruct(MDirection::NUTATFROM,
		     (void *) new Nutation(Nutation::STANDARD));
	break;

	case BMEAN_B1950:
	mc.addStruct(MDirection::PRECESTO,
		     (void *) new Precession(Precession::B1950));
	break;

	case BMEAN_BTRUE:
	mc.addStruct(MDirection::NUTATFROM,
		     (void *) new Nutation(Nutation::B1950));
	break;

	case JTRUE_JMEAN:
	mc.addStruct(MDirection::NUTATTO,
		     (void *) new Nutation(Nutation::STANDARD));
	break;

	case BTRUE_BMEAN:
	mc.addStruct(MDirection::NUTATTO,
		     (void *) new Nutation(Nutation::B1950));
	break;

	case J2000_APP:
	mc.addStruct(MDirection::SOLPOSFROM,
		     (void *) new SolarPos(SolarPos::STANDARD));
	mc.addStruct(MDirection::ABERFROM,
		     (void *) new Aberration(Aberration::STANDARD));
	mc.addStruct(MDirection::NUTATFROM,
		     (void *) new Nutation(Nutation::STANDARD));
	mc.addStruct(MDirection::PRECESFROM,
		     (void *) new Precession(Precession::STANDARD));
	break;

	case APP_J2000:
	mc.addStruct(MDirection::SOLPOSTO,
		     (void *) new SolarPos(SolarPos::STANDARD));
	mc.addStruct(MDirection::ABERTO,
		     (void *) new Aberration(Aberration::STANDARD));
	mc.addStruct(MDirection::NUTATTO,
		     (void *) new Nutation(Nutation::STANDARD));
	mc.addStruct(MDirection::PRECESTO,
		     (void *) new Precession(Precession::STANDARD));
	break;

	case B1950_APP:
	mc.addStruct(MDirection::ABERFROM,
		     (void *) new Aberration(Aberration::B1950));
	mc.addStruct(MDirection::NUTATFROM,
		     (void *) new Nutation(Nutation::B1950));
	mc.addStruct(MDirection::PRECESFROM,
		     (void *) new Precession(Precession::B1950));
	break;

	case APP_B1950:
	mc.addStruct(MDirection::ABERTO,
		     (void *) new Aberration(Aberration::B1950));
	mc.addStruct(MDirection::NUTATTO,
		     (void *) new Nutation(Nutation::B1950));
	mc.addStruct(MDirection::PRECESTO,
		     (void *) new Precession(Precession::B1950));
	break;

	case APP_HADEC:
	break;

	case HADEC_AZEL:
	break;

	case AZEL_HADEC:
	break;

	case HADEC_APP:
	break;

	default:
	break;

    }
}

void MDirection::doConvert(MVDirection &in,
			   const MDirection::Ref &inref,
			   const MDirection::Ref &outref,
			   const MDirection::Convert &mc) {
    Double g1, g2, g3, lengthE, tdbTime;
    MVPosition *solpos;
    MVPosition *trypos;
    MVPosition *respos;
    RotMatrix *prot;
    Euler *eul;

    for (Int i=0; i<mc.nMethod(); i++) {

      switch (mc.getMethod(i)) {
	
      case GAL_J2000:
	in = MeasData::GALtoJ2000() * in;
	break;

      case GAL_B1950:
	in = MeasData::GALtoB1950() * in;
	break;

      case J2000_GAL:
	in = MeasData::J2000toGAL() * in;
	break;

      case B1950_GAL:
	in = MeasData::B1950toGAL() * in;
	break;

      case J2000_B1950: {
	solpos = (MVPosition *) mc.getStruct(MDirection::MVPOS1);
	trypos = (MVPosition *) mc.getStruct(MDirection::MVPOS2);
	respos = (MVPosition *) mc.getStruct(MDirection::MVPOS3);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Frame rotation
	*prot = MeasData::MToB1950(4);
	in *= *prot;
	in.adjust();
// E-terms
// Iterate
	*solpos = MeasData::AberETerm(0);
	*trypos = in;
	do {
	  g1 = *trypos * *solpos;
	  *respos = *trypos - *solpos + (g1 * *trypos);
	  respos->adjust();
	  *respos -= in;
	  *trypos -= *respos;
	} 
	while (respos->radius() > 1e-10);
	in = *trypos;
      }
      break;

      case B1950_J2000: {
	solpos = (MVPosition *) mc.getStruct(MDirection::MVPOS1);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// E-terms
	*solpos = MeasData::AberETerm(0);
	g1 = in * *solpos;
	in += g1 * in;
	in -= *solpos;
	in.adjust();
// Frame rotation
	*prot = MeasData::MToJ2000(0);
	in *= *prot;
	in.adjust();
      }	
      break;

      case J2000_JMEAN: {
	MDirection::Ref::frameEpoch(outref, inref).getTDB(tdbTime);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Precession
	*prot = (((Precession *) 
		  mc.getStruct(MDirection::PRECESFROM))->
		 operator()(tdbTime));
	in *= *prot;
      }
      break;

      case B1950_BMEAN: {
	MDirection::Ref::frameEpoch(outref, inref).getTDB(tdbTime);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Precession
	*prot = (((Precession *) 
		  mc.getStruct(MDirection::PRECESFROM))->
		 operator()(tdbTime));
	in *= *prot;
      }
      break;

      case JMEAN_J2000: {
	MDirection::Ref::frameEpoch(inref, outref).getTDB(tdbTime);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Precession
	*prot = (((Precession *) 
		  mc.getStruct(MDirection::PRECESTO))->
		 operator()(tdbTime));
	in = *prot * in;
      }
      break;

      case JMEAN_JTRUE: {
	MDirection::Ref::frameEpoch(outref, inref).getTDB(tdbTime);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Nutation
	*prot *= (((Nutation *) 
		   mc.getStruct(MDirection::NUTATFROM))->
		  operator()(tdbTime));
	in *= *prot;
      }
      break;

      case BMEAN_B1950: {
	MDirection::Ref::frameEpoch(inref, outref).getTDB(tdbTime);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Precession
	*prot = (((Precession *) 
		  mc.getStruct(MDirection::PRECESTO))->
		 operator()(tdbTime));
	in = *prot * in;
      }
      break;

      case BMEAN_BTRUE: {
	MDirection::Ref::frameEpoch(outref, inref).getTDB(tdbTime);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Nutation
	*prot *= (((Nutation *) 
		   mc.getStruct(MDirection::NUTATFROM))->
		  operator()(tdbTime));
	in *= *prot;
      }
      break;

      case JTRUE_JMEAN: {
	MDirection::Ref::frameEpoch(outref, inref).getTDB(tdbTime);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Nutation
	*prot *= (((Nutation *) 
		   mc.getStruct(MDirection::NUTATTO))->
		  operator()(tdbTime));
	in = *prot * in;
      }
      break;

      case BTRUE_BMEAN: {
	MDirection::Ref::frameEpoch(outref, inref).getTDB(tdbTime);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Nutation
	*prot *= (((Nutation *) 
		   mc.getStruct(MDirection::NUTATTO))->
		  operator()(tdbTime));
	in = *prot * in;
      }
      break;

      case J2000_APP: {
	MDirection::Ref::frameEpoch(outref, inref).getTDB(tdbTime);
	solpos = (MVPosition *) mc.getStruct(MDirection::MVPOS1);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Solar position in ecliptic coordinates
	*solpos = ((SolarPos *) 
		   mc.getStruct(MDirection::SOLPOSFROM))->
	  operator()(tdbTime);
// Solar position in rectangular coordinates
	*solpos = MeasData::posToRect() * *solpos;
// Get length and unit vector
	solpos->adjust(lengthE);
	g1 = -1.974e-8 / lengthE;
	g2 = in * *solpos;
// Check if near sun
	if (!nearAbs(g2, 1.0,
		     1.0-cos(MeasData::SunSemiDiameter()/lengthE))) {
	  *solpos -= g2 * in;
	  *solpos *= (g1 / (1.0 - g2));
	  in += *solpos;
	};
// Aberration
	*solpos =  ((Aberration *) 
		    mc.getStruct(MDirection::ABERFROM))->
	  operator()(tdbTime);
// Get length
	lengthE = solpos->radius();
// Beta^-1 (g1)
	g1 = sqrt(1 - lengthE * lengthE);
	g2 = in * *solpos;
	in = (g1*in + (1+g2/(1+g1)) * *solpos)*(1.0/(1.0+g2));
// Precession
	*prot = (((Precession *) 
		  mc.getStruct(MDirection::PRECESFROM))->
		 operator()(tdbTime));
// Nutation
	*prot *= (((Nutation *) 
		   mc.getStruct(MDirection::NUTATFROM))->
		  operator()(tdbTime));
	in *= *prot;
      }
      break;

      case APP_J2000: {
	MDirection::Ref::frameEpoch(inref, outref).getTDB(tdbTime);
	solpos = (MVPosition *) mc.getStruct(MDirection::MVPOS1);
	trypos = (MVPosition *) mc.getStruct(MDirection::MVPOS2);
	respos = (MVPosition *) mc.getStruct(MDirection::MVPOS3);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Nutation
	*prot = (((Nutation *) 
		  mc.getStruct(MDirection::NUTATTO))->
		 operator()(tdbTime));
// Precession
	*prot *= (((Precession *) 
		   mc.getStruct(MDirection::PRECESTO))->
		  operator()(tdbTime));
	in = *prot * in;
// Aberration
	*solpos =  ((Aberration *) 
		    mc.getStruct(MDirection::ABERTO))->
	  operator()(tdbTime);
// Get length
	lengthE = solpos->radius();
// Beta^-1 (g1)
	g1 = sqrt(1 - lengthE * lengthE);
// First guess
	*trypos = in - *solpos;
// Solve for aberration solution
	do {
	  g2 = *trypos * *solpos;
	  *respos = ((g1 * *trypos + 
		      (1+g2/(1+g1)) * *solpos)*(1.0/(1.0+g2)));
	  respos->adjust();
	  for (Int j=0; j<3; j++) {
	    g3 = solpos->operator()(j);
	    trypos->operator()(j) -= 
	      (respos->operator()(j) - in(j))/
	      (((g1+g3*g3/(1+g1))-
		g3*respos->operator()(j))/(1+g2));
	  };
	  *respos -= in;
	}
	while (respos->radius() > 1e-10);
	in = *trypos;
// Solar position in ecliptic coordinates
	*solpos = ((SolarPos *) 
		   mc.getStruct(MDirection::SOLPOSTO))->
	  operator()(tdbTime);
// Solar position in rectangular coordinates
	*solpos = MeasData::posToRect() * *solpos;
// Get length and unit vector
	solpos->adjust(lengthE);
	g1 = -1.974e-8 / lengthE;
// Check if near sun
	g2 = in * *solpos;
	if (!nearAbs(g2, 1.0,
		     1.0-cos(MeasData::SunSemiDiameter()/lengthE))) {
// First guess
	  *trypos = in;
	  do {
	    *respos = (*solpos - g2 * *trypos) * (g1/(1.0 - g2));
	    respos->adjust();
	    for (Int j=0; j<3; j++) {
	      g3 = solpos->operator()(j);
	      trypos->operator()(j) -= 
		(respos->operator()(j) + 
		 trypos->operator()(j) - in(j))/
		(1 + (g3 * respos->operator()(j) -
		      g1 * (g2 + g3 *
			    trypos->operator()(j)))/(1-g2));
	    };
	    g2 = *trypos * *solpos;
	    *respos += *trypos;
	    *respos -= in;
	  }
	  while (respos->radius() > 1e-10);
	  in = *trypos;
	};
      }
      break;

      case B1950_APP: {
	MDirection::Ref::frameEpoch(outref, inref).getTDB(tdbTime);
	solpos = (MVPosition *) mc.getStruct(MDirection::MVPOS1);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// E-terms
	*solpos = MeasData::AberETerm(0);
	g1 = in * *solpos;
	in += g1 * in;
	in -= *solpos;
	in.adjust();
// Precession
	*prot = (((Precession *) 
		  mc.getStruct(MDirection::PRECESFROM))->
		 operator()(tdbTime));
// Nutation
	*prot *= (((Nutation *) 
		   mc.getStruct(MDirection::NUTATFROM))->
		  operator()(tdbTime));
	in *= *prot;
// Aberration
	*solpos =  ((Aberration *) 
		    mc.getStruct(MDirection::ABERFROM))->
	  operator()(tdbTime);
	in += *solpos;
	in.adjust();
      }
      break;

      case APP_B1950: {
	MDirection::Ref::frameEpoch(inref, outref).getTDB(tdbTime);
	solpos = (MVPosition *) mc.getStruct(MDirection::MVPOS1);
	trypos = (MVPosition *) mc.getStruct(MDirection::MVPOS2);
	respos = (MVPosition *) mc.getStruct(MDirection::MVPOS3);
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
// Aberration
	*solpos =  ((Aberration *) 
		    mc.getStruct(MDirection::ABERTO))->
	  operator()(tdbTime);
	in -= *solpos;
	in.adjust();
// Nutation
	*prot = (((Nutation *) 
		  mc.getStruct(MDirection::NUTATTO))->
		 operator()(tdbTime));
// Precession
	*prot *= (((Precession *) 
		   mc.getStruct(MDirection::PRECESTO))->
		  operator()(tdbTime));
	in = *prot * in;
// E-terms
	*solpos = MeasData::AberETerm(0);
	g1 = in * *solpos;
	in += g1 * in;
	in += *solpos;
	in.adjust();
      }
      break;

      case APP_HADEC: {
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
	eul = (Euler *) mc.getStruct(MDirection::EULER1);
	MDirection::Ref::frameEpoch(outref, inref).getLASTr(g1);
	MDirection::Ref::framePosition(outref, inref).getRadius(lengthE);
	MDirection::Ref::frameEpoch(outref, inref).getTDB(tdbTime);
	MDirection::Ref::framePosition(outref, inref).getLat(g3);
	respos = (MVPosition *) mc.getStruct(MDirection::MVPOS3);
	g2 = MeasData::diurnalAber(lengthE, tdbTime);
	*respos = MVDirection(g1, g3);
	respos->readjust(g2);
	in += *respos;
	*eul = MeasData::polarMotion(tdbTime);
	eul->operator()(2) = g1;
	*prot = RotMatrix(*eul);
	in *= *prot;
	in(1) = -in(1);
      }
      break;

      case HADEC_AZEL:
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
	MDirection::Ref::framePosition(outref, inref).getLat(g1);
	*prot = RotMatrix(Euler(C::pi_2-g1 ,(uInt) 2));
	in *= *prot;
	break;

      case AZEL_HADEC:
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
	MDirection::Ref::framePosition(inref, outref).getLat(g1);
	*prot = RotMatrix(Euler(C::pi_2-g1 ,(uInt) 2));
	in = *prot * in;
	break;
	
      case HADEC_APP: {
	prot = (RotMatrix *) mc.getStruct(MDirection::ROTMAT1);
	MDirection::Ref::frameEpoch(inref, outref).getLASTr(g1);
	MDirection::Ref::framePosition(inref, outref).getRadius(lengthE);
	MDirection::Ref::frameEpoch(inref, outref).getTDB(tdbTime);
	MDirection::Ref::framePosition(inref, outref).getLat(g3);
	respos = (MVPosition *) mc.getStruct(MDirection::MVPOS3);
	g2 = MeasData::diurnalAber(lengthE, tdbTime);
	*respos = MVDirection(g1, g3);
	respos->readjust(g2);
	in(1) = -in(1);
	*prot = RotMatrix(Euler(g1 ,(uInt) 3));
	in = *prot * in;
	in -= *respos;
      }
      break;

      default:
	break;
	
      }
    }
}
