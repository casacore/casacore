//# tMSSimulator.cc: Test the synthesis MeasurementSet Simulator
//# Copyright (C) 1995,1996,1998,1999,2000
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

#include <trial/MeasurementSets/MSSimulator.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <aips/Tables.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>
#include <fstream.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Measures/MPosition.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Measures/MDirection.h>
#include <aips/Mathematics/Constants.h>

int main()
{
    try {

	MSSimulator sim2;

	// make config information===================================================
	Vector<Double> x(6);
	Vector<Double> y(6);
	Vector<Double> z(6);
	y.set(0.0);
	z.set(0.0);
	x(0) = 0;
	x(1) = 40;
	x(2) = 100;
	x(3) = 180;
	x(4) = 280;
	x(5) = 400;
	Vector<Float> diam(6);
	diam.set(12.0);
	Vector<String> mount(6);
	mount.set("AZ/EL");
	Vector<String> name(6);
	name.set("ANT1");
	for (uInt i=0; i<6;i++) {
	  ostrstream cbuf; cbuf << flush << "ANT" << i <<ends;
	  char* pChar=cbuf.str(); name(i)=*pChar; delete pChar;
	}
	String coordsys = "local";	
	// position of ATCA
	MPosition mRef( Quantity( 6372960.26, "m"),
			Quantity( 2.61014232, "rad"),
			Quantity( -0.52613792, "rad"),
			MPosition::Ref(MPosition::ITRF));

	sim2.initAnt("atca", x, y, z, diam, mount, name, coordsys, mRef);
	cout << "Just set antenna config info"<<endl;




	// Make the field information=================================================
	uInt nSources = 3;
	Vector<String> sourceName(3);
	sourceName(0) = "SouthPole";
	sourceName(1) = "SagA";
	sourceName(2) = "CENA";
	Vector<MDirection> sourceDirection(3);
	sourceDirection(0) = MDirection(Quantity(0.0, "deg"), 
					Quantity(-90.0, "deg"),   
					MDirection::Ref(MDirection::B1950)) ;
	sourceDirection(1) = MDirection(Quantity(265.0,"deg"), 
					Quantity(-29.0, "deg"),   
					MDirection::Ref(MDirection::B1950)) ;
	sourceDirection(2) = MDirection(Quantity(200.5,"deg"), 
					Quantity(-42.75, "deg"),   
					MDirection::Ref(MDirection::B1950)) ;
	Vector<Int> intsPerPointing(3);
	intsPerPointing(0) = 2;
	intsPerPointing(1) = 1;
	intsPerPointing(2) = 10;
	Vector<Int> mosPointingsX(3);
	Vector<Int> mosPointingsY(3);
	mosPointingsX(0) = 3;
	mosPointingsY(0) = 3;
	mosPointingsX(1) = 1;
	mosPointingsY(1) = 1;
	mosPointingsX(2) = 1;
	mosPointingsY(2) = 1;
	Vector<Float> mosSpacing(3);
	mosSpacing(0) = 0.0;
	mosSpacing(1) = 0.0;
	mosSpacing(2) = 0.0;

	sim2.initFields( nSources, sourceName, sourceDirection,
			 intsPerPointing, mosPointingsX, mosPointingsY,
			 mosSpacing);
	cout << "Just set field info"<<endl;



	// Make the Spectral Window info=============================================
	uInt nSpWindows = 2;
	Vector<String> spWindowName(2);
	Vector<Int> nChan(2);
	Vector<Quantity> startFreq(2);
	Vector<Quantity> freqInc(2);
	Vector<Quantity> freqRes(2);
	Vector<String> stokesString(2);

	spWindowName(0) = "XBAND";
	spWindowName(1) = "QBAND";

	nChan(0) = 1;
	nChan(1) = 16;

	startFreq(0) = Quantity(8.0, "GHz");
	startFreq(1) = Quantity(43.0, "GHz");

	freqInc(0) = Quantity(50, "MHz");
	freqInc(0) = Quantity(0.5, "MHz");

	freqRes(0) = Quantity(50, "MHz");
	freqRes(0) = Quantity(0.5, "MHz");

	stokesString(0) = "RR RL LR LL";
	stokesString(1) = "RR LL";

	sim2.initSpWindows(nSpWindows, spWindowName, nChan, startFreq, 
			   freqInc, freqRes, stokesString);
	cout << "Just set spectral window info" << endl;



	// Make the Feed info========================================================
	// (Brain Dead Version!)
	String mode = "perfect R L";
	sim2.initFeeds(mode);
	cout << "Just set feed info" << endl;



	// Set the times=============================================================
	Quantity tint(60.0, "s");
	Quantity tgap(10.0, "s");
	Bool useHA=False;
	Quantity tstart(0.0, "h");
	Quantity tstop(1.0, "h");
        MEpoch tref(MVEpoch(51483.18946969),MEpoch::UTC);

	sim2.setTimes(tint, tgap, useHA, tstart, tstop, tref);



	// NOW:  make and write the MS tables========================================
	cout << "Write MS with no obs.params!"<<endl;
	sim2.writeMS("fakems");
	cout << "MS in fakems" << endl;

    } catch (AipsError x) {
	cout << "Caught exception " << endl;
	cout << x.getMesg() << endl;
	cout << "Thrown from file : " << x.thrownFile() << endl;
	cout << "at line : " << x.thrownLine() << endl;
	return 1;
    } 
    cout << "Done." << endl;
    return 0;
}








