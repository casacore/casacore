//# tMBaseline.cc: This program tests MBaseline class
//# Copyright (C) 1998,1999,2000,2002,2007
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
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
#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/measures/Measures.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Quanta/Euler.h>
#include <casacore/measures/Measures/Aberration.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCBaseline.h>
#include <casacore/measures/Measures/MBaseline.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
    try {
      	cout << "Test measure class MBaseline" << endl;
	cout << "--------------------------------------" << endl;

	cout << endl << "MBaseline state transition matrix:\n" << endl;
	cout << MCBaseline::showState() << endl;

	MEpoch tbm(Quantity(50927.92931, "d"));
	MPosition pos(MVPosition(-4750915.84032, 2792906.17778, 
				 -3200483.75028), 
		      MPosition::ITRF);
	MeasFrame mf(tbm, pos);
	mf.set(MDirection(Quantity(10, "deg"), Quantity(80, "deg"),
			  MDirection::HADEC));
	MVBaseline mvb0(100 ,10, 1e-8); // to stop Intel problems
	cout << "Baseline: " << mvb0 << endl;

	MBaseline::Ref mbref0(MBaseline::ITRF, mf);
	MBaseline mb0(mvb0, mbref0);

	cout << "Baseline: " << mb0 << endl;
	cout << "Baseline reference: " << mbref0 << endl;

	MBaseline::Ref mbref1(MBaseline::J2000);

	cout << "Baseline reference: " << mbref1 << endl;

	cout << "Test Baseline conversion ..." << endl;
	cout << "--------------------------------------" << endl;
	MBaseline::Convert bconv(mb0, mbref1);
	cout << "Converted " << mb0 << endl <<
	  " to " << mbref1 << endl <<
	  " as " << bconv() << endl;

	MBaseline::Convert bconvb(mbref1, mbref0);
	if (allNearAbs(mb0.getValue().getValue(),
		       bconvb(bconv()).getValue().getValue(),
		       1e-7)) {
	  cout << "Back      " << mb0 << " : ok" << endl;
	} else {
	  cout << "Back      " << mb0 << " : not ok" << endl <<
	  " as " << bconvb(bconv()) << endl;
	};

	cout << "--------------------------------------" << endl;
	cout << "Testing all conversions forward/backward" << endl;

	Bool isok = True;
	Vector<Double> tvec(3);
	tvec = 0.0;
	for (uInt i=MBaseline::J2000; i<MBaseline::N_Types; i++) {
	  for (uInt j=MBaseline::J2000; j<MBaseline::N_Types; j++) {
	    MBaseline::Ref rin(i, mf);
	    MBaseline::Ref rout(j, mf);
	    MBaseline mb0(mvb0, rin);
	    MBaseline::Convert forw(rin, rout);
	    MBaseline::Convert backw(rout, rin);
	    if (!allNearAbs(mb0.getValue().getValue() -
			 backw(forw(mb0)).getValue().getValue(), 
			 tvec, 1e-6)) {
	      cout << MBaseline::showType(i) << " to " <<
		MBaseline::showType(j) << ": " <<
		mb0.getValue().getValue() -
		backw(forw(mb0)).getValue().getValue() << endl;
	      isok = False;
	    };
	  };
	};
	if (isok) {
	  cout << "All forward/backward Baseline conversions: ok" << endl;
	} else {
	  cout << "Some forward/backward Baseline conversions wrong" << endl;
	};
	cout << "------------------------------------" << endl;

	cout << "Exercise all MVBaseline function" << endl;
	{
	  MVBaseline x(mvb0);
	  if (x != mvb0) cout << "Copy constructor error" << endl;
	  x = mvb0;
	  if (x != mvb0) cout << "Assignment error" << endl;
	  Vector<Quantum<Double> > vq(3);
	  vq = Quantity(23, "m");
	  x.putValue(vq);
	  cout << "putValue:       " << vq << ", " << x << endl;
	  cout << "BaselineAngle:  " << x.BaselineAngle(mvb0) << endl;
	  cout << "BaselineAngle:  " << x.BaselineAngle(mvb0, "deg") << endl;
	  cout << "get:            " << x.get() << endl;
	  cout << "getRecordValue: " << x.getRecordValue() << endl;
	  cout << "separation:     " << x.separation(mvb0) << endl;
	  cout << "separation:     " << x.separation(mvb0, "deg") << endl;
	  cout << "crossProduct:   " << x.crossProduct(mvb0) << endl;
	  cout << "getAngle:       " << x.getAngle() << endl;
	  cout << "getAngle:       " << x.getAngle("deg") << endl;
	  cout << "getlength:      " << x.getLength("cm") << endl;
	  cout << "radius:         " << x.radius() << endl;
	  cout << "getXRecordValue:" << x.getXRecordValue() << endl;
	  Vector<Double> x1(3);
	  x1(0) = 30;
	  x1(1) = 40;
	  x1(2) = 0;
	  x.putVector(x1);
	  cout << "putVector:      " << x1 << ", " << x << endl;
	  MVBaseline x2(vq);
	  cout << "VQ constructor: " << x2 << endl;
	  cout << "Pos constructor:" << MVBaseline(x, x2) << endl;
	  cout << "Q constructor:  " << MVBaseline(Quantity(50, "m")) << endl;
	  cout << "QV constructor: " << MVBaseline(x2.getAngle()) << endl;
	  cout << "QV constructor: " << MVBaseline(Quantity(34,"m"),
						   x2.getAngle()) << endl;
	  cout << "V constructor:  " << MVBaseline(x1) << endl;
	  cout << "D constructor:  " << MVBaseline(Double(78)) << endl;
	  cout << "operator+:      " << x+x2 << endl;
	  cout << "operator-:      " << x-x2 << endl;
	  cout << "operator-pre-:  " << -x2 << endl;
	  RotMatrix rm(Euler(25, 1, 0, 0));
	  cout << "operator*:      " << x2*rm << endl;
	  cout << "operator*:      " << x2*2 << endl;
	  MVBaseline::assure(x);
	  cout << "assure:         " << "ok" << endl;
	  cout << "getLength:      " << x.getLength() << endl;
	  cout << "operator*:      " << x*x1 << endl;
	  cout << "operator*       " << x*x2 << endl;
	  cout << "operator*:      " << x1*x << endl;
	  MeasValue *xc = x.clone();
	  cout << "clone:          " << *xc << endl;
	  cout << "getVector:      " << x.getVector() << endl;
	  cout << "near:           " << x.near(x2) << endl;
	  cout << "near:           " << x.near(x2, Quantity(1, "deg")) << endl;
	  cout << "nearAbs:        " << x.nearAbs(x2) << endl;
	  cout << "!=:             " << (x != x2) << endl;
	  cout << "==:             " << (x == x2) << endl;
	  cout << "type:           " << x.type() << endl;

	  cout << "All MVBaseline functions: ok" << endl;
	  cout << "----------------------------" << endl;
	  delete xc;
	}

 	cout << "Exercise all MBaseline function" << endl;
	{
	  MBaseline mb(mvb0, MBaseline::B1950);
	  String s0("azel");
	  MBaseline::Types tp;
	  MBaseline::Ref mr;
	  cout << "getType:        " << MBaseline::getType(tp, s0) << ", ";
	  // next () to stop egcs warning
	  cout << (uInt)tp << endl;
	  cout << "giveMe:         " << mb.giveMe(mr, s0) << ", ";
	  cout << mr << endl;
	  cout << "setRefString:   " << mb.setRefString("hadec") << ", ";
	  cout << mb << endl;
	  MBaseline::assure(mb);
	  cout << "assure:         " << "ok" << endl;
	  Measure *mbc = mb.clone();
	  cout << "clone:          " << *mbc << endl;
	  cout << "get:            " << mb.get("cm") << endl;
	  cout << "getAngle:       " << mb.getAngle("deg") << endl;
	  cout << "getDefaultType: " << mb.getDefaultType() << endl;
	  cout << "getRefString:   " << mb.getRefString() << endl;
	  cout << "myType:         " << mb.myType() << endl;
	  cout << "type:           " << mb.type() << endl;

	  cout << "All MBaseline functions: ok" << endl;
	  cout << "---------------------------" << endl;
	  delete mbc;
	}
      
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    return 0;
}
