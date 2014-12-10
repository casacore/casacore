//# MCPosition.cc:  MPosition conversion routines 
//# Copyright (C) 1995,1996,1997,1998,2000,2001
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
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MeasTable.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Statics
uInt MCPosition::ToRef_p[N_Routes][3] = {
  {MPosition::ITRF,	MPosition::WGS84,	0},
  {MPosition::WGS84,	MPosition::ITRF,	0} };
uInt MCPosition::FromTo_p[MPosition::N_Types][MPosition::N_Types];
MutexedInit MCPosition::theirMutexedInit (MCPosition::doFillState);

//# Constructors
MCPosition::MCPosition() :
  DVEC1(0) {
    fillState();
}

//# Destructor
MCPosition::~MCPosition() {
  clearConvert();
}

//# Operators

//# Member functions

void MCPosition::getConvert(MConvertBase &mc,
			    const MRBase &inref, 
			    const MRBase &outref) {

  Int iin  = inref.getType();
  Int iout = outref.getType();
  Int tmp;
  while (iin != iout) {
    tmp = FromTo_p[iin][iout];
    iin = ToRef_p[tmp][1];
    mc.addMethod(tmp);
    initConvert(tmp, mc);
  }
}

void MCPosition::clearConvert() {
  delete DVEC1; DVEC1 = 0;
}

//# Conversion routines
void MCPosition::initConvert(uInt which, MConvertBase &mc) {

  if (False) initConvert(which, mc);	// Stop warning

  if (!DVEC1) DVEC1 = new Vector<Double>(3);

  switch (which) {

  default:
    break;
  }
}

void MCPosition::doConvert(MeasValue &in,
			   MRBase &inref,
			   MRBase &outref,
			   const MConvertBase &mc) {
  doConvert(*(MVPosition*)&in,
	    inref, outref, mc);
}

void MCPosition::doConvert(MVPosition &in, 
			   MRBase &inref,
			   MRBase &outref,
			   const MConvertBase &mc) {
    
  if (False) { inref.getType(); outref.getType(); } // to stop warnings

  Double g1, g2, g3, g4;

  for (Int i=0; i<mc.nMethod(); i++) {

    switch (mc.getMethod(i)) {
	
    case ITRF_WGS84: {
      Double d1, d2;
      // Get angles
      *DVEC1 = in.get();
      // Get flattening
      g1 = MeasTable::WGS84(1); g1 = 1.0/g1;
      g1 = 2*g1 - g1*g1;
      // Iterate
      d2 = (*DVEC1)(0) * cos((*DVEC1)(2));
      do {
	g2 = (*DVEC1)(2);
	d1 = sin(g2);
	g3 = 1.0/sqrt(1 - g1 * d1 * d1);
	(*DVEC1)(2) = in(2) + MeasTable::WGS84(0) * g3 * g1 * d1;
	if (d2 != 0.0) {
	  (*DVEC1)(2) = atan(((*DVEC1)(2))/d2);
	} else {
	  (*DVEC1)(2) = ((*DVEC1)(2) >= 0) ? C::pi_2 : - C::pi_2;
	}
      }
      while ( !nearAbs((*DVEC1)(2), g2, 1e-10));
      (*DVEC1)(0) = d2/cos((*DVEC1)(2)) - MeasTable::WGS84(0) * g3;
      in = MVPosition(Quantity((*DVEC1)(0),"m"),
		      (*DVEC1)(1), (*DVEC1)(2));
    }
    break;

    case WGS84_ITRF: {
      // Equatorial radius
      g1 = MeasTable::WGS84(0);
      // Flattening
      g2 = MeasTable::WGS84(1);
      g2 = 1.0 - 1.0/g2; g2 *= g2;
      // h
      g4 = in.radius();
      if (g4 != 0.0) {
	// aC
	g3 = (in(0)*in(0) + in(1)*in(1) + g2 * in(2)*in(2));
	g3 = g1 * sqrt(1.0/g3);
	// aS
	g2 *= g3;
	// Apply
	g4 = in.get()[0]/g4;
	in(0) *= (g4 + g3);
	in(1) *= (g4 + g3);
	in(2) *= (g4 + g2);
      } else {
	// C
	g3 = g2;
	g3 = g1 * sqrt(1.0/g3);
	// S
	g2 *= g3;
	// Apply
	in(2) = g2;
      }
    }	
    break;

    default:
      break;
	
    } //switch
  } // for
}

String MCPosition::showState() {
  fillState();
  return MCBase::showState(MCPosition::FromTo_p[0],
			   MPosition::N_Types, MCPosition::N_Routes,
			   MCPosition::ToRef_p);
}

void MCPosition::doFillState (void*) {
  MPosition::checkMyTypes();
  MCBase::makeState(FromTo_p[0], MPosition::N_Types, N_Routes, ToRef_p);
}

} //# NAMESPACE CASACORE - END

