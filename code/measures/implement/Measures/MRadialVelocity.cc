//# MRadialVelocity.cc: A Measure: radial velocity
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
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/QC.h>
#include <aips/Measures/QMath.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MVPosition.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/Aberration.h>
#include <aips/Measures/MeasData.h>
#include <aips/Measures/MDoppler.h>

//# Constructors
MRadialVelocity::MRadialVelocity() :
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>() {}

MRadialVelocity::MRadialVelocity(const MVRadialVelocity &dt) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,
						  MRadialVelocity::DEFAULT) {}

MRadialVelocity::MRadialVelocity(const MVRadialVelocity &dt, 
				 const MRadialVelocity::Ref &rf) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const MVRadialVelocity &dt, uInt rf) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const Quantity &dt) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,
						  MRadialVelocity::DEFAULT) {}

MRadialVelocity::MRadialVelocity(const Quantity &dt, const MRadialVelocity::Ref &rf) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const Quantity &dt, uInt rf) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,rf) {}

//# Destructor
MRadialVelocity::~MRadialVelocity() {}

//# Operators

//# Member functions

const String &MRadialVelocity::tellMe() const {
    return MRadialVelocity::showMe();
}

const String &MRadialVelocity::showMe() {
    static const String name("Radialvelocity");
    return name;
}

const String &MRadialVelocity::showType(uInt tp) {
    static const String tname[MRadialVelocity::N_Types] = {
	"LSR",
	"LSRK",
	"BARY",
	"GEO",	    
	"TOPO",
	"GALACTO"}; 
    DebugAssert(tp < MRadialVelocity::N_Types, AipsError);
    return tname[tp];
}

Bool MRadialVelocity::giveMe(const String &in, MRadialVelocity::Ref &mr) {
    static const Int N_name = 6;
    static const String tname[N_name] = {
	"LSR",
	"LSRK",
	"BARY",
	"GEO",	    
	"TOPO",
	"GALACTO"}; 

    static const uInt oname[N_name] = {
	MRadialVelocity::LSR,
	MRadialVelocity::LSRK,
	MRadialVelocity::BARY,
	MRadialVelocity::GEO,
	MRadialVelocity::TOPO,
	MRadialVelocity::GALACTO};

    uInt i = Measure::giveMe(in, N_name, tname);

    if (i>=N_name) {
	mr = MRadialVelocity::Ref();
	return False;
    } else {
	mr = MRadialVelocity::Ref(oname[i]);
    };
    return True;
}

Quantity MRadialVelocity::get(const Unit &un) const {
    return data.get(un);
}

MDoppler MRadialVelocity::toDoppler() {
    Double t = data.getValue() / C::c;
    return MDoppler( MVDoppler(t), MDoppler::BETA);
}

MRadialVelocity MRadialVelocity::fromDoppler(const MDoppler &dop) {
    Double t = C::c * MDoppler::Convert(dop, MDoppler::BETA)()
	.getValue().getValue();
    return MRadialVelocity(MVRadialVelocity(t),
		      MRadialVelocity::LSR);
}

MRadialVelocity MRadialVelocity::fromDoppler(const MDoppler &dop,
					     MRadialVelocity::Types typ) {
    Double t = C::c * MDoppler::Convert(dop, MDoppler::BETA)()
	.getValue().getValue();
    return MRadialVelocity(MVRadialVelocity(t),
			   typ);
}
    
void *MRadialVelocity::clone() const {
    return ((void *) new MRadialVelocity(*this));
}

//# Conversion routines
void MRadialVelocity::getConvert(MRadialVelocity::Convert &mc,
			    const MRadialVelocity::Ref &inref, 
			    const MRadialVelocity::Ref &outref) {
// Array of conversion routines to call
    static const MRadialVelocity::Routes 
	FromTo[MRadialVelocity::N_Types][MRadialVelocity::N_Types] = {
    { MRadialVelocity::N_Routes,
      MRadialVelocity::LSR_BARY,
      MRadialVelocity::LSR_BARY,
      MRadialVelocity::LSR_BARY,
      MRadialVelocity::LSR_BARY,
      MRadialVelocity::LSR_GALACTO},
    { MRadialVelocity::LSRK_BARY,
      MRadialVelocity::N_Routes,
      MRadialVelocity::LSRK_BARY,
      MRadialVelocity::LSRK_BARY,
      MRadialVelocity::LSRK_BARY,
      MRadialVelocity::LSRK_BARY},
    { MRadialVelocity::BARY_LSR,
      MRadialVelocity::BARY_LSRK,
      MRadialVelocity::N_Routes,
      MRadialVelocity::BARY_GEO,
      MRadialVelocity::BARY_GEO,
      MRadialVelocity::BARY_LSR},
    { MRadialVelocity::GEO_BARY,
      MRadialVelocity::GEO_BARY,
      MRadialVelocity::GEO_BARY,
      MRadialVelocity::N_Routes,
      MRadialVelocity::GEO_TOPO,
      MRadialVelocity::GEO_BARY},
    { MRadialVelocity::TOPO_GEO,
      MRadialVelocity::TOPO_GEO,
      MRadialVelocity::TOPO_GEO,
      MRadialVelocity::TOPO_GEO,
      MRadialVelocity::N_Routes,
      MRadialVelocity::TOPO_GEO},
    { MRadialVelocity::GALACTO_LSR,
      MRadialVelocity::GALACTO_LSR,
      MRadialVelocity::GALACTO_LSR,
      MRadialVelocity::GALACTO_LSR,
      MRadialVelocity::GALACTO_LSR,
      MRadialVelocity::N_Routes}
};

// List of codes converted to
    static const MRadialVelocity::Types ToRef[MRadialVelocity::N_Routes] = {
        MRadialVelocity::BARY,
        MRadialVelocity::LSR,
        MRadialVelocity::GEO,
        MRadialVelocity::TOPO,
	MRadialVelocity::BARY,
        MRadialVelocity::GEO,
	MRadialVelocity::GALACTO,
        MRadialVelocity::LSR,
	MRadialVelocity::BARY,
	MRadialVelocity::LSRK
	};

    Int iin  = inref.getType();
    Int iout = outref.getType();
    Int tmp;
    while (iin != iout) {
	tmp = FromTo[iin][iout];
	iin = ToRef[tmp];
	mc.addMethod(tmp);
	initConvert(tmp, mc);
    }
}

void MRadialVelocity::clearConvert(MRadialVelocity::Convert &mc) {
  delete (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
  delete (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
  delete (Aberration *) mc.getStruct(MRadialVelocity::ABERFROM);
  delete (Aberration *) mc.getStruct(MRadialVelocity::ABERTO);
}

//# Conversion routines
void MRadialVelocity::initConvert(uInt which, MRadialVelocity::Convert &mc) {
    if (!(mc.getStruct(MRadialVelocity::MVPOS1))) {
	mc.addStruct(MRadialVelocity::MVPOS1,
		     (void *) new MVPosition());
    };

    if (!(mc.getStruct(MRadialVelocity::MVDIR1))) {
	mc.addStruct(MRadialVelocity::MVDIR1,
		     (void *) new MVDirection());
    };

    switch (which) {
      
        case LSR_BARY:
        break;

	case BARY_LSR:
	break;

	case BARY_GEO:
	  mc.addStruct(MRadialVelocity::ABERFROM,
		     (void *) new Aberration(Aberration::STANDARD));
	break;

	case GEO_TOPO:
	break;

	case GEO_BARY:
	  mc.addStruct(MRadialVelocity::ABERTO,
		     (void *) new Aberration(Aberration::STANDARD));
	break;

	case TOPO_GEO:
	break;

        case LSR_GALACTO:
	break;

	case GALACTO_LSR:
	break;

	case LSRK_BARY:
	break;

	case BARY_LSRK:
	break;

	default:
	break;
    }
}

void MRadialVelocity::doConvert(MVRadialVelocity &in,
				const MRadialVelocity::Ref &inref,
				const MRadialVelocity::Ref &outref,
				const MRadialVelocity::Convert &mc) {
    Double g1, g2, g3, lengthE, tdbTime;
    MVPosition *solpos;
    MVDirection *respos;

    for (Int i=0; i<mc.nMethod(); i++) {

      switch (mc.getMethod(i)) {

      case LSR_BARY: {
	solpos = (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSR(0));
	respos = (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
	MRadialVelocity::Ref::frameDirection(outref, inref).
	  getJ2000(*respos);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue()/C::c;
	in = (g2 + g1)/(1 + g2 * g1) * C::c;
      }
      break;

      case BARY_LSR: {
	solpos = (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSR(0));
	respos = (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
	MRadialVelocity::Ref::frameDirection(inref, outref).
	  getJ2000(*respos);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue()/C::c;
	in = (g2 - g1)/(1 - g2 * g1) * C::c;
      }
      break;

      case BARY_GEO: {
	MRadialVelocity::Ref::frameEpoch(outref, inref).
	  getTDB(tdbTime);
	solpos = (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
	*solpos = ((Aberration *)
		   mc.getStruct(MRadialVelocity::ABERFROM))->
	  operator()(tdbTime);
	respos = (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
	MRadialVelocity::Ref::frameDirection(outref, inref).
	  getJ2000(*respos);
	g1 = *solpos * *respos;
	g2 = in.getValue()/C::c;
	in = (g2 - g1)/(1 - g2 * g1) * C::c;
      }	
      break;

      case GEO_TOPO: {
	MRadialVelocity::Ref::frameEpoch(outref, inref).
	  getLASTr(g1);
	MRadialVelocity::Ref::framePosition(outref, inref).
	  getRadius(lengthE);
	MRadialVelocity::Ref::frameEpoch(outref, inref).
	  getTDB(tdbTime);
	MRadialVelocity::Ref::framePosition(outref, inref).
	  getLat(g3);
	solpos = (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
	g2 = MeasData::diurnalAber(lengthE, tdbTime);
	*solpos = MVDirection(C::pi_2 + g1, 0.0);
	solpos->readjust(g2 * cos(g3));
	respos = (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
	MRadialVelocity::Ref::frameDirection(outref, inref).
	  getApp(*respos);
	g1 = *solpos * *respos;
	g2 = in.getValue()/C::c;
	in = (g2 - g1)/(1 - g2 * g1) * C::c;
      }
      break;

      case GEO_BARY: {
	MRadialVelocity::Ref::frameEpoch(inref, outref).
	  getTDB(tdbTime);
	solpos = (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
	*solpos = ((Aberration *)
		   mc.getStruct(MRadialVelocity::ABERTO))->
	  operator()(tdbTime);
	respos = (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
	MRadialVelocity::Ref::frameDirection(outref, inref).
	  getApp(*respos);
	g1 = *solpos * *respos;
	g2 = in.getValue()/C::c;
	in = (g2 + g1)/(1 + g2 * g1) * C::c;
      }	
      break;

      case TOPO_GEO: {
	MRadialVelocity::Ref::frameEpoch(inref, outref).
	  getLASTr(g1);
	MRadialVelocity::Ref::framePosition(inref, outref).
	  getRadius(lengthE);
	MRadialVelocity::Ref::frameEpoch(inref, outref).
	  getTDB(tdbTime);
	MRadialVelocity::Ref::framePosition(inref, outref).
	  getLat(g3);
	solpos = (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
	g2 = MeasData::diurnalAber(lengthE, tdbTime);
	*solpos = MVDirection(C::pi_2 + g1, 0.0);
	solpos->readjust(g2 * cos(g3));
	respos = (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
	MRadialVelocity::Ref::frameDirection(outref, inref).
	  getApp(*respos);
	g1 = *solpos * *respos;
	g2 = in.getValue()/C::c;
	in = (g2 + g1)/(1 + g2 * g1) * C::c;
      }
      break;

      case LSR_GALACTO:
	solpos = (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSRGal(0));
	respos = (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
	MRadialVelocity::Ref::frameDirection(inref, outref).
	  getJ2000(*respos);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue()/C::c;
	in = (g2 - g1)/(1 - g2 * g1) * C::c;
	break;

      case GALACTO_LSR:
	solpos = (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSRGal(0));
	respos = (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
	MRadialVelocity::Ref::frameDirection(outref, inref).
	  getJ2000(*respos);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue()/C::c;
	in = (g2 + g1)/(1 + g2 * g1) * C::c;
	break;

      case LSRK_BARY:
	solpos = (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSRK(0));
	respos = (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
	MRadialVelocity::Ref::frameDirection(outref, inref).
	  getJ2000(*respos);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue()/C::c;
	in = (g2 + g1)/(1 + g2 * g1) * C::c;
	break;

      case BARY_LSRK:
	solpos = (MVPosition *) mc.getStruct(MRadialVelocity::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSRK(0));
	respos = (MVDirection *) mc.getStruct(MRadialVelocity::MVDIR1);
	MRadialVelocity::Ref::frameDirection(inref, outref).
	  getJ2000(*respos);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue()/C::c;
	in = (g2 - g1)/(1 - g2 * g1) * C::c;
	break;

      default:
	break;
      }
    }
}
