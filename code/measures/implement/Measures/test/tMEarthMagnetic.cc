//# tMEarthMagnetic.cc: This program test Measure functions
//# Copyright (C) 1995,1996,1997,1998
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
#include <aips/Quanta/RotMatrix.h>
#include <aips/Quanta/Euler.h>
#include <trial/Measures/MEarthMagnetic.h>
#include <trial/Measures/MCEarthMagnetic.h>
#include <trial/Measures/EarthField.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>

Int main() {

    try {
	cout << "Test Earth Magnetic field" << endl;
	cout << "--------------------------------------" << endl;
	MEarthMagnetic vl;
	MVTime dat(1998,5,18);
	MPosition obs(MVPosition(Quantity(3828488.86, "m").getBaseValue(),
				 Quantity(443253.42, "m").getBaseValue(),
				 Quantity(5064977.78, "m").getBaseValue()));
	MeasFrame frame(MEpoch(MVEpoch(dat.day())),obs);

	cout << "Date:      " << dat.string(MVTime::YMD +
					    MVTime::NO_TIME, 6) <<
	  endl;
	cout << "Position:  " << obs.getValue().get().ac() << endl;
	cout << "           " << obs.getAngle("deg") << endl;

	EarthField ef(EarthField::STANDARD, dat.day());
	cout << "Result:    " << ef(obs.getValue()).ac() << endl;

	cout << "------------------------------------------" << endl;
	MEarthMagnetic::Convert cv(MEarthMagnetic::Ref(MEarthMagnetic::ITRF,
						       frame),
				   MEarthMagnetic::Ref(MEarthMagnetic::AZEL));
	MEarthMagnetic::Convert cv1(MEarthMagnetic::Ref(MEarthMagnetic::ITRF,
							frame),
				    MEarthMagnetic::Ref(MEarthMagnetic::HADEC));
	MVEarthMagnetic res(ef(obs.getValue()));
	cout << "In ITRF:   " << MEarthMagnetic(res) << endl;
	cout << "           " << MEarthMagnetic(res).getAngle("deg") << endl;
	cout << "In AZEL:   " << cv(res) << endl;
	cout << "           " << cv(res).getAngle("deg") << endl;
	cout << "In HADEC:  " << cv1(res) << endl;
	cout << "           " << cv1(res).getAngle("deg") << endl;

	cout << "------------------------------------------" << endl;
	MEarthMagnetic::Convert cv2(MEarthMagnetic::Ref(MEarthMagnetic::AZEL,
							frame),
				    MEarthMagnetic::Ref(MEarthMagnetic::HADEC));
	MEarthMagnetic::Convert cv3(MEarthMagnetic::Ref(MEarthMagnetic::AZEL,
							frame),
				    MEarthMagnetic::Ref(MEarthMagnetic::ITRF));
	MVEarthMagnetic res1(18312.1, -382.004, 45184.5);
	cout << "In AZEL:   " << MEarthMagnetic(res1) << endl;
	cout << "           " << MEarthMagnetic(res1).getAngle("deg") << endl;
	cout << "In HADEC:  " << cv2(res1) << endl;
	cout << "           " << cv2(res1).getAngle("deg") << endl;
	cout << "In ITRF:   " << cv3(res1) << endl;
	cout << "           " << cv3(res1).getAngle("deg") << endl;

	cout << "------------------------------------------" << endl;
	MEarthMagnetic::Convert cv4(MEarthMagnetic::Ref(MEarthMagnetic::IGRF,
							frame),
				    MEarthMagnetic::Ref(MEarthMagnetic::ITRF));
	MEarthMagnetic::Convert cv5(MEarthMagnetic::Ref(MEarthMagnetic::IGRF,
							frame),
				    MEarthMagnetic::Ref(MEarthMagnetic::AZEL));
	MEarthMagnetic::Convert cv6(MEarthMagnetic::Ref(MEarthMagnetic::IGRF,
							frame),
				    MEarthMagnetic::Ref(MEarthMagnetic::HADEC));
	cout << "Model ITRF:   " << cv4(res) << endl; 
	cout << "              " << cv4(res).getAngle("deg") <<
	  ", " << cv4(res).getValue().getLength("G") << endl; 
	cout << "Model AZEL:   " << cv5(res) << endl; 
	cout << "              " << cv5(res).getAngle("deg") <<
	  ", " << cv5(res).getValue().getLength("G") << endl; 
	cout << "Model HADEC:  " << cv6(res) << endl; 
	cout << "              " << cv6(res).getAngle("deg") << 
	  ", " << cv6(res).getValue().getLength("G") << endl; 

	cout << "--------------------------------------" << endl;
	cout << "Testing all conversions forward/backward" << endl;

	Vector<Double> tvec(3);
	tvec = 0.0;
	Int ecnt = 0;
	for (uInt i=MEarthMagnetic::ITRF; i<MEarthMagnetic::N_Types; i++) {
	  for (uInt j=MEarthMagnetic::ITRF; j<MEarthMagnetic::N_Types; j++) {
	    MEarthMagnetic::Ref rin(i, frame);
	    MEarthMagnetic::Ref rout(j, frame);
	    MEarthMagnetic mb0(res, rin);
	    MEarthMagnetic::Convert forw(rin, rout);
	    MEarthMagnetic::Convert backw(rout, rin);
	    if (!allNearAbs(mb0.getValue().getValue().ac() -
			    backw(forw(mb0)).getValue().getValue().ac(), 
			    tvec.ac(), 1.5e-3)) {
	      cout << MEarthMagnetic::showType(i) << " to " <<
		MEarthMagnetic::showType(j) << ": " <<
		mb0.getValue().getValue().ac() -
		backw(forw(mb0)).getValue().getValue().ac() << endl;
	      ecnt++;
	    };
	  };
	};
	if (ecnt == 0) {
	  cout << "All forward/backward conversions: ok" << endl;
	};

	cout << "--------------------------------------" << endl;
	cout << "Exercise all MVEarthMagnetic function" << endl;
	MVEarthMagnetic mvb0(res);
	{
	  MVEarthMagnetic x(mvb0);
	  if (x != mvb0) cout << "Copy constructor error" << endl;
	  x = mvb0;
	  if (x != mvb0) cout << "Assignment error" << endl;
	  Vector<Quantum<Double> > vq(3);
	  vq = Quantity(23, "G");
	  x.putValue(vq);
	  cout << "putValue:       " << vq.ac() << ", " << x << endl;
	  cout << "earthMagneticAngle:  " << x.earthMagneticAngle(mvb0) << endl;
	  cout << "earthMagneticAngle:  " << x.earthMagneticAngle(mvb0, "deg") << endl;
	  cout << "get:            " << x.get().ac() << endl;
	  cout << "getRecordValue: " << x.getRecordValue().ac() << endl;
	  cout << "separation:     " << x.separation(mvb0) << endl;
	  cout << "separation:     " << x.separation(mvb0, "deg") << endl;
	  cout << "crossProduct:   " << x.crossProduct(mvb0) << endl;
	  cout << "getAngle:       " << x.getAngle() << endl;
	  cout << "getAngle:       " << x.getAngle("deg") << endl;
	  cout << "getlength:      " << x.getLength("G") << endl;
	  cout << "radius:         " << x.radius() << endl;
	  Vector<Double> x1(3);
	  x1(0) = 30;
	  x1(1) = 40;
	  x1(2) = 0;
	  x.putVector(x1);
	  cout << "putVector:      " << x1.ac() << ", " << x << endl;
	  MVEarthMagnetic x2(vq);
	  cout << "VQ constructor: " << x2 << endl;
	  cout << "Q constructor:  " << MVEarthMagnetic(Quantity(50, "G")) << endl;
	  cout << "QV constructor: " << MVEarthMagnetic(x2.getAngle()) << endl;
	  cout << "QV constructor: " << MVEarthMagnetic(Quantity(34,"G"),
						   x2.getAngle()) << endl;
	  cout << "V constructor:  " << MVEarthMagnetic(x1) << endl;
	  cout << "D constructor:  " << MVEarthMagnetic(Double(78)) << endl;
	  cout << "operator+:      " << x+x2 << endl;
	  cout << "operator-:      " << x-x2 << endl;
	  cout << "operator-pre-:  " << -x2 << endl;
	  RotMatrix rm(Euler(25, 1, 0, 0));
	  cout << "operator*:      " << x2*rm << endl;
	  cout << "operator*:      " << x2*2 << endl;
	  MVEarthMagnetic::assert(x);
	  cout << "assert:         " << "ok" << endl;
	  cout << "getLength:      " << x.getLength() << endl;
	  cout << "operator*:      " << x*x1 << endl;
	  cout << "operator*       " << x*x2 << endl;
	  cout << "operator*:      " << x1*x << endl;
	  MeasValue *y = x.clone();
	  cout << "clone:          " << *y << endl;
	  delete y;
	  cout << "getVector:      " << x.getVector().ac() << endl;
	  cout << "near:           " << x.near(x2) << endl;
	  cout << "near:           " << x.near(x2, Quantity(1, "deg")) << endl;
	  cout << "nearAbs:        " << x.nearAbs(x2) << endl;
	  cout << "!=:             " << (x != x2) << endl;
	  cout << "==:             " << (x == x2) << endl;
	  cout << "type:           " << x.type() << endl;

	  cout << "All MVEarthMagnetic functions: ok" << endl;
	  cout << "----------------------------" << endl;
	}

 	cout << "Exercise all MEarthMagnetic function" << endl;
	{
	  MEarthMagnetic mb(mvb0, MEarthMagnetic::B1950);
	  String s0("azel");
	  MEarthMagnetic::Types tp;
	  MEarthMagnetic::Ref mr;
	  cout << "getType:        " << MEarthMagnetic::getType(tp, s0) << ", ";
	  cout << tp << endl;
	  cout << "giveMe:         " << mb.giveMe(mr, s0) << ", ";
	  cout << mr << endl;
	  cout << "setRefString:   " << mb.setRefString("hadec") << ", ";
	  cout << mb << endl;
	  MEarthMagnetic::assert(mb);
	  cout << "assert:         " << "ok" << endl;
	  Measure *y = mb.clone();
	  cout << "clone:          " << *y << endl;
	  delete y;
	  cout << "get:            " << mb.get("cm") << endl;
	  cout << "getAngle:       " << mb.getAngle("deg") << endl;
	  cout << "getDefaultType: " << mb.getDefaultType() << endl;
	  cout << "getRefString:   " << mb.getRefString() << endl;
	  cout << "myType:         " << mb.myType() << endl;
	  cout << "type:           " << mb.type() << endl;

	  cout << "All MEarthMagnetic functions: ok" << endl;
	  cout << "---------------------------" << endl;
	}
	cout << "------------------------------------------" << endl;

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } end_try;

    exit(0);
}
