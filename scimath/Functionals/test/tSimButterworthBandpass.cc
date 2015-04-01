//# tSimButterworthBandpass: test the SimButterworthBandpass class
//# Copyright (C) 2001,2002,2003
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

#define DIAGNOSTICS
#ifdef DEBUG 
#define DIAGNOSTICS
#endif

#include <casacore/scimath/Functionals/SimButterworthBandpass.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Containers/Record.h>

#include <casacore/casa/namespace.h>
int main() {
    SimButterworthBandpass<Double> butt(1,1,-1.0,1.0,0.0,1.0);

    AlwaysAssertExit(butt.getCenter()    ==  0.0 &&
		     butt.getPeak()      ==  1.0   );
    AlwaysAssertExit(butt.getMinOrder()  ==  1.0 &&
		     butt.getMaxOrder()  ==  1.0   );
    AlwaysAssertExit(butt.getMinCutoff() == -1.0 &&
		     butt.getMaxCutoff() ==  1.0   );

//      AlwaysAssertExit(butt.getCenter() - 0.0 < DBL_EPSILON &&
//  		     butt.getPeak()   - 1.0 < DBL_EPSILON   );
//      AlwaysAssertExit(butt.getMinOrder()  - 1.0 < DBL_EPSILON &&
//  		     butt.getMaxOrder()  - 1.0 < DBL_EPSILON   );
//      AlwaysAssertExit(butt.getMinCutoff() + 1.0 < DBL_EPSILON &&
//  		     butt.getMaxCutoff() - 1.0 < DBL_EPSILON   );

    butt.setPeak(10.0);
    AlwaysAssertExit(butt.getPeak() == 10.0);
    butt.setCenter(5.0);
    AlwaysAssertExit(butt.getCenter() == 5.0);
    butt.setMinOrder(4);
    AlwaysAssertExit(butt.getMinOrder() == 4);
    butt.setMaxOrder(5);
    AlwaysAssertExit(butt.getMaxOrder() == 5);
    butt.setMinCutoff(1.0);
    AlwaysAssertExit(butt.getMinCutoff() == 1.0);
    butt.setMaxCutoff(9.0);
    AlwaysAssertExit(butt.getMaxCutoff() == 9.0);

    Double pk = butt.getPeak();
    Double cen = butt.getCenter();
    //Double irt2 = 1.0/sqrt(2.0);
    AlwaysAssertExit(butt(cen) == pk);
    //AlwaysAssertExit(butt(fabs(butt.getMinCutoff()) - irt2*pk) < DBL_EPSILON && 
//	             butt(fabs(butt.getMaxCutoff()) - irt2*pk) < DBL_EPSILON);
    AlwaysAssertExit(butt(6*butt.getMinCutoff()-5*cen) < 1e-2*pk);
    AlwaysAssertExit(butt(6*butt.getMaxCutoff()+5*cen) < 1e-2*pk);

    // test get/setMode()
    AlwaysAssertExit(butt.hasMode());
    Record rec;
    rec.define(RecordFieldId("minOrder"), 2);
    rec.define(RecordFieldId("maxOrder"), 3);

    butt.setMode(rec);
    AlwaysAssertExit(butt.getMinOrder() == 2 &&
		     butt.getMaxOrder() == 3);

    Record rec2;
    butt.getMode(rec2);
    try {
	uInt mino, maxo;
	rec2.get(RecordFieldId("minOrder"), mino);
	rec2.get(RecordFieldId("maxOrder"), maxo);
	AlwaysAssertExit(mino == 2 && maxo ==3);
    }
    catch (AipsError ex) {
	cerr << "Exception: " << ex.getMesg() << endl;
	exit(1);
    }

    cout << "OK" << endl;
    return 0;
}
