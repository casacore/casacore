//# tMuvw.cc: This program tests Muvw class
//# Copyright (C) 1998-2000,2002,2007
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
#include <casacore/measures/Measures/Aberration.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Quanta/Euler.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/MVBaseline.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCuvw.h>
#include <casacore/measures/Measures/Muvw.h>
#include <casacore/casa/Quanta/MVuvw.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
    try {
      	cout << "Test measure class Muvw" << endl;
	cout << "--------------------------------------" << endl;

	cout << endl << "Muvw state transition matrix:\n" << endl;
	cout << MCuvw::showState() << endl;

	MEpoch tbm(Quantity(50927.92931, "d"));
	MPosition pos(MVPosition(-4750915.84032, 2792906.17778, 
				 -3200483.75028), 
		      MPosition::ITRF);
	MeasFrame mf(tbm, pos);
	mf.set(MDirection(Quantity(10, "deg"), Quantity(80, "deg"),
			  MDirection::HADEC));
	MVuvw mvb0(100 ,10, 0);
	cout << "uvw: " << mvb0 << endl;

	Muvw::Ref mbref0(Muvw::ITRF, mf);
	Muvw mb0(mvb0, mbref0);

	cout << "uvw: " << mb0 << endl;
	cout << "uvw reference: " << mbref0 << endl;

	Muvw::Ref mbref1(Muvw::J2000);

	cout << "uvw reference: " << mbref1 << endl;

	cout << "Test uvw conversion ..." << endl;
	cout << "--------------------------------------" << endl;
	Muvw::Convert bconv(mb0, mbref1);
	cout << "Converted " << mb0 << endl <<
	  " to " << mbref1 << endl <<
	  " as " << bconv() << endl;

	Muvw::Convert bconvb(mbref1, mbref0);
	if (allNearAbs(mb0.getValue().getValue(),
		       bconvb(bconv()).getValue().getValue(),
		       1e-8)) {
	  cout << "Back      " << mb0 << " : ok" << endl;
	} else {
	  cout << "Back      " << mb0 << " : not ok" << endl <<
	  " as " << bconvb(bconv()) << endl;
	};

	cout << "--------------------------------------" << endl;
	cout << "Make uvw from Baseline" << endl;
	MVBaseline mvb1(100, 10 ,1);
	MVDirection mvd(Quantity(30, "deg"), Quantity(45, "deg"));
	MVuvw mvu0(mvb1, mvd);
	cout << "Baseline: " << mvb1 << mvb1.getAngle("deg") << endl;
	cout << "Direction:" << mvd << mvd.getAngle("deg") << endl;
	cout << "uvw:      " << mvu0 << mvu0.getAngle("deg") << endl;

	cout << "--------------------------------------" << endl;
	cout << "Testing all conversions forward/backward" << endl;

	Vector<Double> tvec(3);
	tvec = 0.0;
	Bool isok = True;
	for (uInt i=Muvw::J2000; i<Muvw::N_Types; i++) {
	  for (uInt j=Muvw::J2000; j<Muvw::N_Types; j++) {
	    Muvw::Ref rin(i, mf);
	    Muvw::Ref rout(j, mf);
	    Muvw mb0(mvb0, rin);
	    Muvw::Convert forw(rin, rout);
	    Muvw::Convert backw(rout, rin);
	    if (!allNearAbs(mb0.getValue().getValue() -
			 backw(forw(mb0)).getValue().getValue(), 
			 tvec, 1e-4)) {
	      cout << Muvw::showType(i) << " to " <<
		Muvw::showType(j) << ": " <<
		mb0.getValue().getValue() -
		backw(forw(mb0)).getValue().getValue() << endl;
	      isok = False;
	    };
	  };
	};
      if (isok) {
	cout << "All forward/backward uvw conversions: ok" << endl;
      } else {
	cout << "Some forward/backward uvw conversions wrong" << endl;
      };
	cout << "--------------------------------------" << endl;

	cout << "Exercise all MVuvw functions" << endl;
	{
	  MVuvw x(mvb0);
	  if (x != mvb0) cout << "Copy constructor error" << endl;
	  x = mvb0;
	  if (x != mvb0) cout << "Assignment error" << endl;
	  Vector<Quantum<Double> > vq(3);
	  vq = Quantity(23, "m");
	  x.putValue(vq);
	  cout << "putValue:       " << vq << ", " << x << endl;
	  cout << "uvwAngle:  " << x.uvwAngle(mvb0) << endl;
	  cout << "uvwAngle:  " << x.uvwAngle(mvb0, "deg") << endl;
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
	  MVuvw x2(vq);
	  cout << "VQ constructor: " << x2 << endl;
	  cout << "Pos constructor:" << MVuvw(x, x2) << endl;
	  cout << "Q constructor:  " << MVuvw(Quantity(50, "m")) << endl;
	  cout << "QV constructor: " << MVuvw(x2.getAngle()) << endl;
	  cout << "QV constructor: " << MVuvw(Quantity(34,"m"),
						   x2.getAngle()) << endl;
	  cout << "V constructor:  " << MVuvw(x1) << endl;
	  cout << "D constructor:  " << MVuvw(Double(78)) << endl;
	  cout << "operator+:      " << x+x2 << endl;
	  cout << "operator-:      " << x-x2 << endl;
	  cout << "operator-pre-:  " << -x2 << endl;
	  RotMatrix rm(Euler(25, 1, 0, 0));
	  cout << "operator*:      " << x2*rm << endl;
	  cout << "operator*:      " << x2*2 << endl;
	  MVuvw::assure(x);
	  cout << "assure:         " << "ok" << endl;
	  cout << "getLength:      " << x.getLength() << endl;
	  cout << "operator*:      " << x*x1 << endl;
	  cout << "operator*       " << x*x2 << endl;
	  cout << "operator*:      " << x1*x << endl;
	  MeasValue *xmvu = x.clone();
	  cout << "clone:          " << *xmvu << endl;
	  delete xmvu;
	  cout << "getVector:      " << x.getVector() << endl;
	  cout << "near:           " << x.near(x2) << endl;
	  cout << "near:           " << x.near(x2, Quantity(1, "deg")) << endl;
	  cout << "nearAbs:        " << x.nearAbs(x2) << endl;
	  cout << "!=:             " << (x != x2) << endl;
	  cout << "==:             " << (x == x2) << endl;
	  cout << "type:           " << x.type() << endl;
	  cout << "Original:       " << x << endl;
	  Double xa;
	  x.adjust(xa);
	  cout << "adjust:         " << x << endl;
	  x.readjust(xa);
	  cout << "readjust:       " << x << endl;

	  cout << "All MVuvw functions: ok" << endl;
	  cout << "----------------------------" << endl;
	}

 	cout << "Exercise all Muvw function" << endl;
	{
	  Muvw mb(mvb0, Muvw::B1950);
	  String s0("azel");
	  Muvw::Types tp;
	  Muvw::Ref mr;
	  cout << "getType:        " << Muvw::getType(tp, s0) << ", ";
	  // next () to stop egcs warning
	  cout << (uInt)tp << endl;
	  cout << "giveMe:         " << mb.giveMe(mr, s0) << ", ";
	  cout << mr << endl;
	  cout << "setRefString:   " << mb.setRefString("hadec") << ", ";
	  cout << mb << endl;
	  Muvw::assure(mb);
	  cout << "assure:         " << "ok" << endl;
	  Measure *xmu = mb.clone();
	  cout << "clone:          " << *xmu << endl;
	  delete xmu;
	  cout << "get:            " << mb.get("cm") << endl;
	  cout << "getAngle:       " << mb.getAngle("deg") << endl;
	  cout << "getDefaultType: " << mb.getDefaultType() << endl;
	  cout << "getRefString:   " << mb.getRefString() << endl;
	  cout << "myType:         " << mb.myType() << endl;
	  cout << "Original:       " << mb << endl;
	  cout << "type:           " << mb.type() << endl;
	  Muvw cpc(mb);
	  cout << "Ctor copy:      " << cpc << endl;
	  Muvw cpas;
	  cpas = mb;
	  cout << "Assign:         " << cpas << endl;
	  cout << "tellMe:         " << mb.tellMe() << endl;
	  cout << "showMe:         " << mb.showMe() << endl;

	  cout << "All Muvw functions: ok" << endl;
	  cout << "---------------------------" << endl;
	}

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    try {
      cout << "Test Muvw exception" << endl;
      cout << "---------------------------" << endl;
      MEpoch x;
      Muvw::assure(x);

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    try {
      cout << "Test MVuvw exception" << endl;
      cout << "---------------------------" << endl;
      MVEpoch x;
      MVuvw::assure(x);

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    cout << "---------------------------" << endl;

    return 0;
}
