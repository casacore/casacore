//# tMEarthMagnetic.cc: This program test Measure functions
//# Copyright (C) 1995,1996,1997
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
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MPosition.h>
#include <trial/Measures/MEarthMagnetic.h>

main()
{
    try {
	cout << "Test Earth Magnetic field" << endl;
	cout << "--------------------------------------" << endl;
	MEarthMagnetic vl;
	MPosition obs(MVPosition(Quantity(3828488.86, "m").getBaseValue(),
				 Quantity(443253.42, "m").getBaseValue(),
				 Quantity(5064977.78, "m").getBaseValue()));
	cout << "Result:    " << vl(obs.getValue()) << endl;
	cout << "Strength:  " << vl(obs.getValue()).getLength("G") << endl;
	cout << "Direction: " << vl(obs.getValue()).getAngle("deg") << endl;
	

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } end_try;

    exit(0);
}
