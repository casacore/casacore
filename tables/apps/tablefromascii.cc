//# tablefromascii.cc: Convert an ASCII file to a table
//# Copyright (C) 2015
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

#include <casacore/tables/Tables/ReadAsciiTable.h>
#include <casacore/tables/Tables/TableProxy.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Inputs/Input.h>

#include <iostream>

using namespace casacore;
using namespace std;

// <summary> Convert an ASCII file to a table </summary>

// <synopsis>
// This program converts an ASCII table to a table using the class
// ReadAsciiTable. It can only handle scalar columns.
// The separator between the fields in the ASCII file can be specified.
// </synopsis>

int main (int argc, char* argv[])
{
  try {
    Input inputs(1);
    inputs.version("2016Jan08GvD");
    inputs.create("in", "", "Input ASCII file", "string");
    inputs.create("out", "", "Output table name", "string");
    inputs.create("headerfile", "", "Name of optional file containing headers",
                  "string");
    inputs.create("autoheader", "F", "Determine header automatically?", "bool");
    inputs.create("autoshape", "[]",
                  "Shape if all columns are treated as one array", "string");
    inputs.create("columnnames", "", "Comma separated names of the columns",
                  "string");
    inputs.create("datatypes", "", "Comma separated data types of the columns",
                  "string");
    inputs.create("sep", " ", "One character separator between values",
                  "string");
    inputs.create("commentmarker", "", "Regex indicating comments", "string");
    inputs.create("firstline", "1", "First line to process", "int");
    inputs.create("lastline", "-1", "Last line to process (<0 is till end)",
                  "int");
    inputs.readArguments(argc, argv);

    // Get and check the input specification.
    String in (inputs.getString("in"));
    String out (inputs.getString("out"));
    String hdrfile (inputs.getString("headerfile"));
    Bool autohdr (inputs.getBool("autoheader"));
    String autoshp (inputs.getString("autoshape"));
    String colnm (inputs.getString("columnnames"));
    String dtype (inputs.getString("datatypes"));
    String sep (inputs.getString("sep"));
    String comm (inputs.getString("commentmarker"));
    Int first (inputs.getInt("firstline"));
    Int last  (inputs.getInt("lastline"));
    if (in.empty()) {
      throw AipsError(" an input file name must be given");
    }
    if (out.empty()) {
      throw AipsError(" an output table name must be given");
    }
    if (sep.size() != 1) {
      throw AipsError(" separator must be a single character");
    }
    // Split column names and datatypes.
    Vector<String> cols (stringToVector(colnm));
    Vector<String> typs (stringToVector(dtype));
    // Get the auto-shape (if given).
    Vector<Int> vec;
    std::istringstream is(autoshp);
    if (! read(is, vec, 0, False)) {
      throw AipsError (" '" + autoshp +
                       "' is an invalid autoshape (maybe enclose in [])");
    }
    IPosition shp(vec);
    // Convert the text file to a table and get table object.
    TableProxy tp(in, hdrfile, out, autohdr, shp, sep, comm,
                  first, last, cols, typs);
    cout << "Created table " << tp.table().tableName() << " with "
         << tp.table().nrow() << " rows" << endl;
    cout << " Used format = " << tp.getAsciiFormat() << endl;
  } catch (const std::exception& x) {
    cerr << "Exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
