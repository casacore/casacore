//# tMEarthMagnetic.cc: This program test Measure functions
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
#include <trial/Measures/EarthField.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Arrays/Vector.h>

Int main() {

    try {
	cout << "Test Earth Magnetic field values" << endl;
	cout << "--------------------------------------" << endl;
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

	cout << "----- IGRF coefficients" << endl;
	cout << "Field      " <<
	  MeasTable::IGRF(dat.day()).ac() << endl;
	cout << "             " <<
	  MeasTable::IGRF(dat.day()).nelements() << endl;

	EarthField ef(EarthField::STANDARD, dat.day());
	cout << "Result:    " << ef(obs.getValue()).ac() << endl;
	cout << "Derivatives: " << endl;
	for (Int i0=0; i0<3; i0++) {
	  cout << "     " << ef.derivative(obs.getValue())[i0].ac() << endl;
	};

	cout << "--------- From derivatives ----------" << endl;
	cout << "+10km X:   " <<
	  ef((obs.getValue()+MVPosition(10000,0,0))).ac() << endl;
	cout << "-10km X:   " <<
	  ef((obs.getValue()+MVPosition(-10000,0,0))).ac() << endl;
	cout << "+10km Y:   " <<
	  ef((obs.getValue()+MVPosition(0,10000,0))).ac() << endl;
	cout << "-10km Y:   " <<
	  ef((obs.getValue()+MVPosition(0,-10000,0))).ac() << endl;
	cout << "+10km Z:   " <<
	  ef((obs.getValue()+MVPosition(0,0,10000))).ac() << endl;
	cout << "-10km Z:   " <<
	  ef((obs.getValue()+MVPosition(0,0,-10000))).ac() << endl;

	cout << "--------- From scratch ----------" << endl;
	ef.init(EarthField::STANDARD, dat.day());
	cout << "+10km X:   " <<
	  ef((obs.getValue()+MVPosition(10000,0,0))).ac() << endl;
	ef.init(EarthField::STANDARD, dat.day());
	cout << "-10km X:   " <<
	  ef((obs.getValue()+MVPosition(-10000,0,0))).ac() << endl;
	ef.init(EarthField::STANDARD, dat.day());
	cout << "+10km Y:   " <<
	  ef((obs.getValue()+MVPosition(0,10000,0))).ac() << endl;
	ef.init(EarthField::STANDARD, dat.day());
	cout << "-10km Y:   " <<
	  ef((obs.getValue()+MVPosition(0,-10000,0))).ac() << endl;
	ef.init(EarthField::STANDARD, dat.day());
	cout << "+10km Z:   " <<
	  ef((obs.getValue()+MVPosition(0,0,10000))).ac() << endl;
	ef.init(EarthField::STANDARD, dat.day());
	cout << "-10km Z:   " <<
	  ef((obs.getValue()+MVPosition(0,0,-10000))).ac() << endl;

	cout << "--------- From refresh ----------" << endl;
	ef.refresh();
	cout << "+10km X:   " <<
	  ef((obs.getValue()+MVPosition(10000,0,0))).ac() << endl;
	ef.refresh();
	cout << "-10km X:   " <<
	  ef((obs.getValue()+MVPosition(-10000,0,0))).ac() << endl;
	ef.refresh();
	cout << "+10km Y:   " <<
	  ef((obs.getValue()+MVPosition(0,10000,0))).ac() << endl;
	ef.refresh();
	cout << "-10km Y:   " <<
	  ef((obs.getValue()+MVPosition(0,-10000,0))).ac() << endl;
	ef.refresh();
	cout << "+10km Z:   " <<
	  ef((obs.getValue()+MVPosition(0,0,10000))).ac() << endl;
	ef.refresh();
	cout << "-10km Z:   " <<
	  ef((obs.getValue()+MVPosition(0,0,-10000))).ac() << endl;

	cout << "------------------------------------------" << endl;

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } end_try;

    exit(0);
}
