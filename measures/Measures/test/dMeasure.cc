//# dMeasure.cc: This program demonstrates Measures to calculate filed rotation
//# Copyright (C) 1995,1996,2000,2002
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
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
    try {
	cout << "Demonstrate measure class to provide field rotations" << endl;
	cout << "----------------------------------------------------" << endl;

	String epoch;
	while (epoch != "B1950" && epoch != "J2000") {
	    cout << "Specify the base epoch (B1950, J2000) [B1950]: ";
// The following and other flush() are necessary for cfront (although
// theoretically cin should auto flush)
	    cout.flush();
	    if (cin.peek() == '\n') {
		cin.get();
		epoch = "";
	    } else {
		cin >> epoch;
	    };
	    epoch.capitalize();
	    if (epoch.empty()) epoch = "B1950";
	};

	MVDirection coord;
	MVDirection pole;
	MEpoch tim;
	MeasFrame frame(tim);
	MDirection::Ref appref(MDirection::APP, frame);
	MDirection mycoord(coord, appref);
	MDirection newcoord;
	MDirection::Convert toward;
	MDirection::Convert from;
	if (epoch == "J2000") {
	    toward = MDirection::Convert(mycoord, MDirection::J2000);
	    newcoord = MDirection(coord, MDirection::J2000);
	    from = MDirection::Convert(newcoord, appref);
	} else {
	    toward = MDirection::Convert(mycoord, MDirection::B1950);
	    newcoord = MDirection(coord, MDirection::J2000);
	    from = MDirection::Convert(newcoord, appref);
	};

	Double ra, dec;
	while (True) {
	    cout << "Specify RA in degrees:  ";
	    cout.flush();
	    if (cin.peek() == '\n') {
		break;
	    };
	    cin >> ra;
	    cout << "Specify DEC in degrees: ";
	    cout.flush();
	    cin >> dec;
	    cin.get();
	    coord = MVDirection(Quantity(ra,"deg"), Quantity(dec,"deg"));
	    
	    Double mytim;
	    while (True) {
		cout << "Specify time in MJD: ";
		cout.flush();
		if (cin.peek() == '\n') {
		    cin.get();
		    break;
		};
		cin >> mytim;
		cin.get();
		tim = MEpoch(Quantity(mytim,"d"));
		frame.set(tim);
		MDirection ncoord(toward(coord));
		cout << epoch << " coordinates: " <<
		    ncoord.getAngle("deg") << endl;
		MDirection npole(toward(pole));
		cout << "Rotation angle: " <<
		    -ncoord.getValue().positionAngle(npole.getValue(), "deg") <<
			endl;
		cout << "Apparent" << " coordinates: " <<
		    from(ncoord).getAngle("deg") << endl;
		MDirection apole(MVDirection(0.,0.,1.),appref);
		MDirection mpole(from(pole));
		cout << "Rotation angle: " <<
		    from(ncoord).getValue().positionAngle(mpole.getValue(), "deg") <<
			endl;
	    };
	};	    
	    
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    
    return(0);
}
