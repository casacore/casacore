//# tMSReader.cc: This program tests the MSReader class
//# Copyright (C) 2000,2002
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

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSOper/MSReader.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/OS/Timer.h>

#include <casacore/casa/namespace.h>
int main(int argc, const char* argv[]) 
{
    try {
	if (argc<2) {
	    cout << "Usage: " << argv[0] << " ms_name" << endl; 
	    return 1;
	}
	String msName(argv[1]);
	cout << "MS Name = " << msName << endl;

	// construct the MS
	MeasurementSet ms(msName);

	// start up a reader
	Timer timer;
	timer.mark();
	MSReader reader(ms);
	timer.show("Construction : ");

	cout << "Tables : " << reader.tables() << endl;
	Vector<String> tables(reader.tables());

	timer.mark();

	for (uInt i=0;i<ms.nrow();i++) {
	    reader.gotoRow(i);
	    cout << i << " : ";
	    for (uInt j=0;j<tables.nelements();j++) {
		if (j > 0) cout << " | ";
		cout << reader.rowNumber(tables(j));
	    }
	    cout << endl;
	}
	timer.show("read to end : ");
	cout << "done" << endl;
    } catch (AipsError x) {
	cerr << "Exception : " << x.getMesg() << endl;
    } 
 
    return 0;
}
