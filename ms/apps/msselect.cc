//# msselect.cc: Create a persistent selection of an MS
//# Copyright (C) 2005
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/ms/MSSel/MSSelection.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/DirectoryIterator.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/SymLink.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <iostream>

using namespace casacore;
using namespace std;

void select (const String& msin, const String& out, const String& baseline,
             bool deep)
{
  MeasurementSet ms(msin);
  MSSelection select;
  // Set given selection strings.
  if (!baseline.empty()) {
    select.setAntennaExpr (baseline);
  }
  // Create the selection table expression for the MS.
  TableExprNode node = select.toTableExprNode (&ms);
  // Make the selection and write the resulting RefTable.
  // If no selection was made, create it explicitly from all rows
  // to be sure there is a RefTable.
  Table mssel = ms(node);
  if (mssel.nrow() == ms.nrow()) {
    Vector<uInt> allRows(ms.nrow());
    indgen (allRows);
    mssel = ms(allRows);
  }
  if (deep) {
    mssel.deepCopy (out, Table::New);
    cout << "Created MeasurementSet " << out;
  } else {
    mssel.rename (out, Table::New);
    cout << "Created RefTable " << out;
  }
  cout << " containing " << mssel.nrow() << " rows (out of "
       << ms.nrow() << ')' << endl;
}

// Copy (or symlink) directories that are not a subtable.
// In that way possible instrument and sky model tables can be copied.
void copyOtherDirs (const String& msName, const String& outName, bool deep)
{
  // Get all table keywords.
  Table tab(msName);
  const TableRecord& keys = tab.keywordSet();
  // Loop over all files in the MS directory.
  Directory dir(msName);
  DirectoryIterator iter(dir);
  for (DirectoryIterator iter(dir); !iter.pastEnd(); ++iter) {
    // Test if a directory basename (also via a symlink) is a subtable.
    // If not, it is an extra directory that needs to be copied.
    if (iter.file().isDirectory()) {
      String bname = iter.file().path().baseName();
      if (!(keys.isDefined(bname)  &&  keys.dataType(bname) == TpTable)) {
        if (deep) {
          Directory sdir(iter.file());
          sdir.copyRecursive (outName + '/' + bname);
          cout << "Copied subdirectory " << bname << endl;
        } else {
          // Resolve a possible symlink created by another msselect.
          // Do it only one deep.
          Path newName;
          if (iter.file().isSymLink()) {
            newName = SymLink(iter.file()).readSymLink();
          } else {
            newName = iter.file().path();
          }
          // Create a symlink to the directory.
          SymLink slink(outName + '/' + bname);
          slink.create (newName.absoluteName(), False);
          cout << "Created symlink to subdirectory " << bname << endl;
        }
      }
    }
  }
}

int main (int argc, char* argv[])
{
  try {
    // enable input in no-prompt mode
    Input inputs(1);
    // define the input structure
    inputs.version("20130819GvD");
    inputs.create ("in", "",
		   "Name of input MeasurementSet",
		   "string");
    inputs.create ("out", "",
		   "Name of output table",
		   "string");
    inputs.create ("deep", "false",
		   "Is the output a deep copy of the MeasurementSet selection?",
		   "bool");
    inputs.create ("baseline", "",
                   "Selection string for antennae and baselines",
                   "string");
    /*
    inputs.create ("time", "",
                   "selection string for times",
                   "string");
    inputs.create ("uv", "",
                   "selection string for uv distance",
                   "string");
    */
    // Fill the input structure from the command line.
    inputs.readArguments (argc, argv);

    // Get and check the input specification.
    String msin (inputs.getString("in"));
    if (msin.empty()) {
      throw AipsError(" an input MeasurementSet must be given");
    }
    // Get the output name.
    String out(inputs.getString("out"));
    if (out.empty()) {
      throw AipsError(" an output table name must be given");
    }
    // Get the deep option.
    bool deep = inputs.getBool("deep");
    // Get the baseline selection string.
    string baseline(inputs.getString("baseline"));
    // Do the selection and copying.
    select (msin, out, baseline, deep);
    copyOtherDirs (msin, out, deep);
  } catch (std::exception& x) {
    cerr << "Error: " << x.what() << endl;
    return 1;
  }
  return 0;
}
