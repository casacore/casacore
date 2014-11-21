//# tMeasMath.cc: This program test MeasMath functions
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2002
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
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/measures/Measures.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Quanta/Euler.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/Precession.h>
#include <casacore/measures/Measures/Nutation.h>
#include <casacore/measures/Measures/Aberration.h>
#include <casacore/measures/Measures/SolarPos.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
    try {
	cout << "Test measure math (MeasMath) class ..." << endl;
	cout << "--------------------------------------" << endl;

	cout << "Euler and RotMatrix ..." << endl;
	cout << "--------------------------------------" << endl;

	Euler eul1(0.1,0.2,0.3);
	RotMatrix rot1, rot2, rot3;
	MVDirection dc1,dc2,dc3;
	MVPosition pdc1,pdc2,pdc3;
	
	cout << "Euler(0.1,0.2,0.3):  " << eul1 << endl;
	cout << "-Euler(0.1,0.2,0.3): " << -eul1 << endl;
	cout << "Euler(0.1,0.2,0.3):  " << eul1 << endl;
	cout << "Rotation none:    " << rot1 << endl;
	cout << "Rotation squared: " << rot1*rot1 << endl;

	cout << "Rotation from Euler:   " << RotMatrix(eul1) << endl;
	cout << "I-Rotation from Euler: " << RotMatrix(-eul1) << endl;
	rot2 = RotMatrix(eul1);
	rot3 = RotMatrix(-eul1);

	cout << "Euler(1,2):     " << Euler(1.0,2.0) << endl;
	cout << "Euler(10 deg, 20 deg): " << Euler(Quantity(10,"deg"),
						   Quantity(20,"deg")) << endl;
	Vector<Double> vec2(2);
	vec2(0)=30; vec2(1)=40;
	Quantum<Vector<Double> > qu2(vec2,"arcsec");
	cout << "Euler(30, 40 arcsec):  " << Euler(qu2) << endl;

	cout << "Direction cosines  (MVDirection)..." << endl;
	cout << "--------------------------------------" << endl;
	
	cout << "MVDirection default: " << dc1 << endl;
	dc2 = MVDirection(1,2,3);
	cout << "MVDirection(1,2,3):  " << dc2 << endl;
	dc2.adjust();
	cout << "Normalised:          " << dc2 << endl;
	dc3 = MVDirection(0.1,0.2);
	cout << "MVDirection(.1,.2):  " << dc3 << endl;
	cout << "Last 2 *:            " << dc2*dc3 << endl;
	cout << "Re-angle:            " << dc3.get() << endl;
	cout << "10 deg, 20 deg:      " << MVDirection(Quantity(10.,"deg"),
					     Quantity(20.,"deg")) << endl;
	cout << "Re-angle:            " << dc3.getAngle("deg") << endl;

	cout << "Shifts  (MVDirection)..." << endl;
	cout << "--------------------------------------" << endl;
	
	dc2 = MVDirection(Quantity(0, "deg"), Quantity(0, "deg"));
	cout << "Start:       " << dc2 << endl;
	dc3 = dc2;
	dc3.shiftLongitude(Quantity(10, "deg"));
	cout << "dl = 10 deg:  " << dc3 << endl;
	dc3 = dc2;
	dc3.shiftLongitude(Quantity(10, "deg"), True);
	cout << "... true:    " << dc3 << endl;
	dc3 = dc2;
	dc3.shiftLatitude(Quantity(10, "deg"));
	cout << "db = 10 deg:  " << dc3 << endl;
	dc3 = dc2;
	dc3.shiftLatitude(Quantity(10, "deg"), True);
	cout << "... true:    " << dc3 << endl;
	dc3 = dc2;
	dc3.shift(Quantity(10, "deg"), Quantity(10, "deg"));
	cout << "dl,b = 10 deg:" << dc3 << endl;
	dc3 = dc2;
	dc3.shift(Quantity(10, "deg"), Quantity(10, "deg"), True);
	cout << "... true:    " << dc3 << endl;
	dc2 = MVDirection(Quantity(0, "deg"), Quantity(60, "deg"));
	cout << "Start:       " << dc2 << endl;
	dc3 = dc2;
	dc3.shiftLongitude(Quantity(10, "deg"));
	cout << "dl = 10 deg:  " << dc3 << endl;
	dc3 = dc2;
	dc3.shiftLongitude(Quantity(10, "deg"), True);
	cout << "... true:    " << dc3 << endl;
	dc3 = dc2;
	dc3.shiftLatitude(Quantity(10, "deg"));
	cout << "db = 10 deg:  " << dc3 << endl;
	dc3 = dc2;
	dc3.shiftLatitude(Quantity(10, "deg"), True);
	cout << "... true:    " << dc3 << endl;
	dc3 = dc2;
	dc3.shift(Quantity(10, "deg"), Quantity(10, "deg"));
	cout << "dl,b = 10 deg:" << dc3 << endl;
	dc3 = dc2;
	dc3.shift(Quantity(10, "deg"), Quantity(10, "deg"), True);
	cout << "... true:    " << dc3 << endl;
	dc2 = MVDirection(Quantity(30, "deg"), Quantity(60, "deg"));
	cout << "Start:       " << dc2 << endl;
	dc3 = dc2;
	dc3.shiftLongitude(Quantity(10, "deg"));
	cout << "dl = 10 deg:  " << dc3 << endl;
	dc3 = dc2;
	dc3.shiftLongitude(Quantity(10, "deg"), True);
	cout << "... true:    " << dc3 << endl;
	dc3 = dc2;
	dc3.shiftLatitude(Quantity(10, "deg"));
	cout << "db = 10 deg:  " << dc3 << endl;
	dc3 = dc2;
	dc3.shiftLatitude(Quantity(10, "deg"), True);
	cout << "... true:    " << dc3 << endl;
	dc3 = dc2;
	dc3.shift(Quantity(10, "deg"), Quantity(10, "deg"));
	cout << "dl,b = 10 deg:" << dc3 << endl;
	dc3 = dc2;
	dc3.shift(Quantity(10, "deg"), Quantity(10, "deg"), True);
	cout << "... true:    " << dc3 << endl;

	cout << "Positions (MVPosition)..." << endl;
	cout << "--------------------------------------" << endl;
	
	cout << "MVPosition default:   " << pdc1 << endl;
	pdc2 = MVPosition(1,2,3);
	cout << "MVPosition(1,2,3):    " << pdc2 << endl;
	pdc2.adjust();
	cout << "Normalised:           " << pdc2 << endl;
	Quantity pdcqu(5,"km");
	pdc3 = MVPosition(pdcqu,0.1,0.2);
	cout << "MVPosition(5km.1,.2): " << pdc3 << endl;
	Quantity pqdc3(1,"m");
	MVPosition ppdc3(pqdc3,0.1,0.2);
	cout << "Last * DC(0.1,0.2):   " << pdc2*ppdc3 << endl;
	cout << "Re-angle:             " << pdc3.get() << endl;
	cout << "6 mm, 10 deg, 20 deg: " << MVPosition(Quantity(6.,"mm"),
						      Quantity(10.,"deg"),
						      Quantity(20.,"deg")) 
	    << endl;
	cout << "Re-angle:             " << pdc3.getAngle("deg") << endl;
	cout << "Length:               " << pdc3.getLength() << endl;
	cout << "Length in dam:        " << pdc3.getLength("dam") << endl;


    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    try {
	cout << "Euler(10 deg, 20 m): ";
	cout << Euler(Quantity(10,"deg"), Quantity(20,"m")) << endl;
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    try {
	Euler eul10(Quantity(0,"deg"),Quantity(0,"deg"),Quantity(30,"deg"));
	RotMatrix rot10(eul10);
	MVDirection dc10(Quantity(10,"deg"),Quantity(20,"deg"));
	cout << "Rotate (10,20 deg) over 0,0,30 deg: " 
	    << (rot10*dc10).getAngle("deg") << endl;
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    try {
	Precession pc2;
	Precession pc1(Precession::B1950);
	Nutation nt1;
	Nutation nt2(Nutation::B1950);
	Aberration ab1;
	Aberration ab2(Aberration::B1950);
	SolarPos sp1;

	UnitVal AUperDay(1e-8,"AU/d");
	Double factor = AUperDay.getFac();

	Double facAU = 1.;

	cout << "B1950 precession MJD 45700(1984/01/01): " << 
	     pc1(45700.).getAngle("''") << endl;
	cout << "B1950 precession MJD 45700 using derivative from 45701: " <<
	     pc1(45701.).getAngle("''") - 
		 pc1.derivative(45701).getAngle("''") << endl;
	cout << "with rotation matrix: " <<
	    RotMatrix(pc1(45700)) << endl;
	cout << "J2000 precession J1984.5: " << 
	     pc2(45883.125).getAngle("''") << endl;
	cout << "with rotation matrix: " <<
	    RotMatrix(pc2(45883.125)) << endl;
	cout << "J2000 nutation J1984.5: " << 
	    nt1(45883.125).getAngle("\"") << 
	    (nt1(45883.125)(0)+nt1(45883.125)(2))/C::arcsec << endl;
	Vector<Double> tenth(3); tenth = Double(0.1);
	Vector<Double> hun4(3); hun4 = Double(0.04);
	cout << "J2000 nutation J1984.5 derivative from +0.1 day: " << 
	    nt1(45883.225).getAngle("\"") - tenth *
		nt1.derivative(45883.225).getAngle("\"") << endl;
	cout << "J2000 nutation J1984.5 derivative from +0.04 day: " << 
	    nt1(45883.165).getAngle("\"") - hun4 *
		nt1.derivative(45883.165).getAngle("\"") << endl;
	cout << "with rotation matrix: " <<
	    RotMatrix(nt1(45883.125)) << endl;
	cout << "with rotation matrix combined 45882.5: " <<
	    RotMatrix(nt1(45882)) * RotMatrix(pc2(45882)) << "or:" <<
	    RotMatrix(pc2(45882)) * RotMatrix(nt1(45882)) << endl;
	cout << " equation of equinoxes 45882.5: " <<
	    nt1.getEqoxAngle(45882,"''") << endl;
	cout << " equation at 45882.5 from derivative at 45882.7: " <<
	    nt1.getEqoxAngle(45882.2,"''") - (const Double) 0.2 *
		Quantity(nt1.derivativeEqox(45882.2)/C::arcsec,"''") << endl;
	cout << " equation at 45882.5 from derivative at 45882.54: " <<
	    nt1.getEqoxAngle(45882.04,"''") - (const Double) 0.04 *
		Quantity(nt1.derivativeEqox(45882.04)/C::arcsec,"''") << endl;
	cout << "J2000 nutation: " << endl;
	Double eq;
	for (eq=45837.; eq<45884.; eq++) {
	    cout << eq+0.5 << ": " << 
		nt1(eq).getAngle("\"") << 
		    (nt1(eq)(0)+nt1(eq)(2))/C::arcsec << endl;
	}
	cout << "B1950 nutation: " << endl;
	for (eq=40632.; eq<40678.; eq++) {
	    cout << eq+0.5 << ": " << 
		nt2(eq).getAngle("\"") << 
		    (nt2(eq)(0)+nt2(eq)(2))/C::arcsec << " " <<
			nt2.getEqoxAngle(eq,"''")/Quantity(15.,"''/s") <<
			    endl;
	}
	cout << "J2000 nutation 50449.5: " <<
	    nt1(50449.).getAngle("''") <<
	    (nt1(50449.)(0)+nt1(50449.)(2))/C::arcsec << endl;

	cout << "J2000 aberration for 45837: " << endl;
	cout << ab1(45837.) * (C::c / factor) << endl;

	cout << "J2000 aberration for 45837 from derivative at +0.1: " << endl;
	cout << (ab1(45837.1) - 0.1 * ab1.derivative(45837.1)) 
	    * (C::c / factor) << endl;

	cout << "J2000 aberration for 45837 from derivative at +0.04: " << endl;
	cout << (ab1(45837.04) - 0.04 * ab1.derivative(45837.04)) 
	    * (C::c / factor) << endl;

	cout << "B1950 aberration for 44238: " << endl;
	cout << ab2(44238.) * (C::c / factor) << endl;

	cout << "B1950 aberration for 44238 from derivative at +0.1: " << endl;
	cout << (ab2(44238.1) - 0.1 * ab2.derivative(44238.1)) 
	    * (C::c / factor) << endl;

	cout << "B1950 aberration for 44238 from derivative at +0.04: " << endl;
	cout << (ab2(44238.04) - 0.04 * ab2.derivative(44238.04)) 
	    * (C::c / factor) << endl;

	cout << "J2000 aberration: " << endl;
	for (eq=45837.; eq<45884.; eq++) {
	    cout << eq+0.5 << ": " << 
		ab1(eq) * (C::c / factor) << endl;
	}

	RotMatrix fromE = MeasTable::posToRect();
	cout << "Rotation matrix from ecliptic: " << fromE << endl;

	cout << "J2000 barycentre Earth: " << endl;
	for (eq=45837.; eq<45884.; eq++) {
	    MVPosition mypcd;
	    mypcd = sp1.baryEarth(eq) * facAU;
	    cout << eq+0.5 << ": " << 
		mypcd << endl;
	}

	cout << "J2000 barycentre Sun: " << endl;
	for (eq=45837.; eq<45884.; eq++) {
	    MVPosition mypcd;
	    mypcd = sp1.barySun(eq) * facAU;
	    cout << eq+0.5 << ": " << 
		mypcd << endl;
	}

	cout << "J2000 geocentric Sun at 45837: " <<
	    sp1(45837.) * facAU << endl;

	cout << "J2000 geocentric Sun at 45837 from derivative at +0.1: " <<
	    (sp1(45837.1) - 0.1 * sp1.derivative(45837.1)) * facAU << endl;

	cout << "J2000 geocentric Sun at 45837 from derivative at +0.04: " <<
	    (sp1(45837.04) - 0.04 * sp1.derivative(45837.04)) * facAU << endl;

	cout << "J2000 geocentric Sun: " << endl;
	for (eq=45837.; eq<45884.; eq++) {
	    MVPosition mypcd;
	    mypcd = sp1(eq) * facAU;
	    cout << eq+0.5 << ": " << 
		mypcd << endl;
	}

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    try {
	cout << "MVEpoch checks ------------------------" << endl;
	cout << "5.3: " << MVEpoch(5.3).get() << " -- " << MVEpoch(5.3) << endl;
	cout << "5.3 + 10.9: " << MVEpoch(5.3,10.9) << endl;
	cout << "5.3 + 10.9: " << MVEpoch(5.3)+MVEpoch(10.9) << endl;
	cout << "1.123 years: " << MVEpoch(Quantity(1.123,"a")) << endl;

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    try {
      cout << "Separation and near checks -----------" << endl;
      MVDirection dc1(0.1, 0.2);
      MVDirection dc2(0.1, 0.20001);
      MVDirection dc3(0.1000001, 0.2);
      cout << "Separation between (0.1, 0.2) and (0.1, 0.20001): " <<
	dc1.separation(dc2) << endl;
      cout << "Separation between (0.1, 0.2) and (0.1000001, 0.2): " <<
	dc1.separation(dc3) << endl;
      cout << "Separation between (0.1, 0.2) and (0.1000001, 0.2): " <<
	dc1.separation(dc3,"arcsec") << endl;
      cout << "Near 1.00 \" (0.1,0.2) and (0.1000001, 0.2): " <<
	dc1.near(dc3, Quantity(1.0, "arcsec")) << endl;
      cout << "Near 0.01 \" (0.1,0.2) and (0.1000001, 0.2): " <<
	dc1.near(dc3, Quantity(0.01, "arcsec")) << endl;

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    return(0);
}
