//# tMBaseline.cc: This program tests MBaseline class
//# Copyright (C) 1998
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
#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures.h>
#include <aips/Measures/RotMatrix.h>
#include <aips/Measures/Euler.h>
#include <aips/Measures/Aberration.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MPosition.h>
#include <trial/Measures/MBaseline.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>

main()
{
    try {
      	cout << "Test measure class MBaseline" << endl;
	cout << "--------------------------------------" << endl;


	MEpoch tbm(Quantity(50927.92931, "d"));
	MPosition pos(MVPosition(-4750915.84032, 2792906.17778, 
				 -3200483.75028), 
		      MPosition::ITRF);
	MeasFrame mf(tbm, pos);
	MVBaseline mvb0(100 ,10, 0);
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
	cout << "Back      " << bconv() << endl <<
	  " as " << bconvb(bconv()) << endl;

	cout << "--------------------------------------" << endl;
	cout << "Testing all conversions forward/backward" << endl;

	Vector<Double> tvec(3);
	tvec = 0.0;
	for (uInt i=MBaseline::ITRF; i<MBaseline::N_Types; i++) {
	  for (uInt j=MBaseline::ITRF; j<MBaseline::N_Types; j++) {
	    MBaseline::Ref rin(i, mf);
	    MBaseline::Ref rout(j, mf);
	    MBaseline mb0(mvb0, rin);
	    MBaseline::Convert forw(rin, rout);
	    MBaseline::Convert backw(rout, rin);
	    if (!allNearAbs(mb0.getValue().getValue().ac() -
			 backw(forw(mb0)).getValue().getValue().ac(), 
			 tvec.ac(), 1e-7)) {
	      cout << MBaseline::showType(i) << " to " <<
		MBaseline::showType(j) << ": " <<
		mb0.getValue().getValue().ac() -
		backw(forw(mb0)).getValue().getValue().ac() << endl;
	    };
	  };
	};

	cout << "All forward/backward conversions: ok" << endl;
	cout << "------------------------------------" << endl;

	cout << "Testing all MDirection conversions forward/backward" << endl;

 	{
	  MVDirection mvd0(0.5, 0.5, 0.5);
	  Double tp;
	  for (uInt i=MDirection::J2000; i<MDirection::N_Types; i++) {
	    for (uInt j=MDirection::J2000; j<MDirection::N_Types; j++) {
	      if (i == MDirection::B1950 || i == MDirection::BMEAN ||
		  i == MDirection::BTRUE ||
		  j == MDirection::B1950 || j == MDirection::BMEAN ||
                  j == MDirection::BTRUE) tp = 1.7e-8;
	      else tp = 1e-9;
	      MDirection::Ref rin(i, mf);
	      MDirection::Ref rout(j, mf);
	      MDirection mb0(mvd0, rin);
	      MDirection::Convert forw(rin, rout);
	      MDirection::Convert backw(rout, rin);
	      if (!allNearAbs(mb0.getValue().getValue().ac() -
			      backw(forw(mb0)).getValue().getValue().ac(), 
			      tvec.ac(), tp)) {
		cout << MDirection::showType(i) << " to " <<
		  MDirection::showType(j) << ": " <<
		  mb0.getValue().getValue().ac() -
		  backw(forw(mb0)).getValue().getValue().ac() << endl;
	      };
	    };
	  };
	}

	cout << "All forward/backward Direction conversions: ok" << endl;
	cout << "----------------------------------------------" << endl;
	cout << "Exercise all MVBaseline function" << endl;
	{
	  MVBaseline x(mvb0);
	  if (x != mvb0) cout << "Copy constructor error" << endl;
	  x = mvb0;
	  if (x != mvb0) cout << "Assignment error" << endl;
	  Vector<Quantum<Double> > vq(3);
	  vq = Quantity(23, "m");
	  x.putValue(vq);
	  cout << "putValue:       " << vq.ac() << ", " << x << endl;
	  cout << "BaselineAngle:  " << x.BaselineAngle(mvb0) << endl;
	  cout << "BaselineAngle:  " << x.BaselineAngle(mvb0, "deg") << endl;
	  cout << "get:            " << x.get().ac() << endl;
	  cout << "getRecordValue: " << x.getRecordValue().ac() << endl;
	  cout << "separation:     " << x.separation(mvb0) << endl;
	  cout << "separation:     " << x.separation(mvb0, "deg") << endl;
	  cout << "crossProduct:   " << x.crossProduct(mvb0) << endl;
	  cout << "getAngle:       " << x.getAngle() << endl;
	  cout << "getAngle:       " << x.getAngle("deg") << endl;
	  cout << "getlength:      " << x.getLength("cm") << endl;
	  cout << "radius:         " << x.radius() << endl;
	  cout << "getXRecordValue:" << x.getXRecordValue().ac() << endl;
	  Vector<Double> x1(3);
	  x1(0) = 30;
	  x1(1) = 40;
	  x1(2) = 0;
	  x.putVector(x1);
	  cout << "putVector:      " << x1.ac() << ", " << x << endl;
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
	  MVBaseline::assert(x);
	  cout << "assert:         " << "ok" << endl;
	  cout << "getLength:      " << x.getLength() << endl;
	  cout << "operator*:      " << x*x1 << endl;
	  cout << "operator*       " << x*x2 << endl;
	  cout << "operator*:      " << x1*x << endl;
	  cout << "clone:          " << *(x.clone()) << endl;
	  cout << "getVector:      " << x.getVector().ac() << endl;
	  cout << "near:           " << x.near(x2) << endl;
	  cout << "near:           " << x.near(x2, Quantity(1, "deg")) << endl;
	  cout << "nearAbs:        " << x.nearAbs(x2) << endl;
	  cout << "!=:             " << (x != x2) << endl;
	  cout << "==:             " << (x == x2) << endl;
	  cout << "type:           " << x.type() << endl;

	  cout << "All MVBaseline functions: ok" << endl;
	  cout << "----------------------------" << endl;
	}

 	cout << "Exercise all MBaseline function" << endl;
	{
	  MBaseline mb(mvb0, MBaseline::B1950);
	  String s0("azel");
	  MBaseline::Types tp;
	  MBaseline::Ref mr;
	  cout << "getType:        " << MBaseline::getType(tp, s0) << ", ";
	  cout << tp << endl;
	  cout << "giveMe:         " << mb.giveMe(mr, s0) << ", ";
	  cout << mr << endl;
	  cout << "setRefString:   " << mb.setRefString("hadec") << ", ";
	  cout << mb << endl;
	  MBaseline::assert(mb);
	  cout << "assert:         " << "ok" << endl;
	  cout << "clone:          " << *(mb.clone()) << endl;
	  cout << "get:            " << mb.get("cm") << endl;
	  cout << "getAngle:       " << mb.getAngle("deg") << endl;
	  cout << "getDefaultType: " << mb.getDefaultType() << endl;
	  cout << "getRefString:   " << mb.getRefString() << endl;
	  cout << "myType:         " << mb.myType() << endl;
	  cout << "type:           " << mb.type() << endl;

	  cout << "All MBaseline functions: ok" << endl;
	  cout << "---------------------------" << endl;
	}
      
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } end_try;

    exit(0);
}
