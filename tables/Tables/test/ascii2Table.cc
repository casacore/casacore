//# ascii2Table.cc: This program loads an ASCII file into a table
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2002
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

#include <casacore/tables/Tables/ReadAsciiTable.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// program to load an ASCII file into a table
// </summary>

// This programs fills a table with data from an ASCII catalogue.
// It calculates mean, median, etc., of the given column, shows them
// and stores them as keywords.

void calc(Table&, const String&);

int main (int argc, const char* argv[])
{
    try {
	if (argc < 3) {
	    cout << ">>>" << endl;
	    cout << "Needs arguments: filein tablename [column]" << endl;
	    cout << "filein:    input file in AsciiFileIO format" << endl;
            cout << "tablename: name of resulting table" << endl;
	    cout << "column:    optional column for which to compute mean, etc." << endl;
	    cout << "<<<" << endl;
	    return 0;
	}

	// Convert the ASCII file into a table with name argv[2]
	cout << "Loading " << argv[1] << " into table " << argv[2] << endl;
	readAsciiTable (argv[1], "", argv[2]);

	// Compute the minimum, maximum, mean, stddev and median
	// show them and store into the table.
	if (argc < 4) {
	    Table tab(argv[2], Table::Old);
	    cout << "Loaded " << tab.nrow() << " rows" << endl;
	}else{
	    Table tab(argv[2], Table::Update);
	    cout << "Loaded " << tab.nrow() << " rows" << endl;
	    calc (tab, argv[3]);
	}
    } catch (AipsError x) {
	cout << "\nCaught an exception: " << x.getMesg() << endl;
        return 1;
    } 
    return 0;          // successfully executed
}


void calc(Table& tab, const String& name)
{
    cout << "Computing mean " << name << ", etc." << endl;
    if (! tab.tableDesc().isColumn(name)) {
	cout << "Column " << name << " does not exist" << endl;
	return;
    }
    TableColumn tabcol (tab, name);
    const ColumnDesc& cd = tabcol.columnDesc();
    if (cd.dataType() == TpBool    || cd.dataType() == TpString
    ||  cd.dataType() == TpComplex || cd.dataType() == TpDComplex
    ||  cd.ndim() != 0) {
	cout << "Column " << name << " should be a numeric scalar" << endl;
	return;
    }

    uInt nrrow = tab.nrow();
    Vector<double> vec(nrrow);
    for (uInt i=0; i<nrrow; i++) {
	tabcol.getScalar (i, vec(i));
    }
    double vmean = mean (vec);
    double vmin  = min (vec);
    double vmax  = max (vec);
    double vadev = avdev (vec, vmean);
    double vsdev = stddev (vec, vmean);
    double vmed  = median (vec, False);
    cout << "Min    " << name << ":  " << vmin << endl;
    cout << "Max    " << name << ":  " << vmax << endl;
    cout << "Mean   " << name << ":  " << vmean << endl;
    cout << "AvDev  " << name << ":  " << vadev << endl;
    cout << "StdDev " << name << ":  " << vsdev << endl;
    cout << "Median " << name << ":  " << vmed << endl;

    // Add keywords Mean, etc.
    TableRecord& keys = tab.rwKeywordSet();
    keys.define ("ColumnUsed", name);
    keys.define ("Min", vmin);
    keys.define ("Max", vmax);
    keys.define ("Mean", vmean);
    keys.define ("AvDev", vadev);
    keys.define ("StdDev", vsdev);
    keys.define ("Median", vmed);
    keys.define ("Comment",
		 "Mean, etc. of column " + name + " have been calculated");
}
