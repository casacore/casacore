//# tConcatRows.cc: This program tests class ConcatRows
//# Copyright (C) 2008
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

#include <casacore/tables/Tables/ConcatRows.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>

// <summary>
// Test program for class ConcatRows.
// </summary>


void doIt()
{
  // Construct from 2 tables.
  ConcatRows rows;
  AlwaysAssertExit (rows.nrow() == 0);
  AlwaysAssertExit (rows.ntable() == 0);
  rows.add (10);
  AlwaysAssertExit (rows.nrow() == 10);
  AlwaysAssertExit (rows.ntable() == 1);
  rows.add (15);
  AlwaysAssertExit (rows.nrow() == 25);
  AlwaysAssertExit (rows.ntable() == 2);

  // Check if rownr mapping is fine.
  uInt tabnr;
  uInt rownr;
  for (uInt i=0; i<10; ++i) {
    rows.mapRownr (tabnr, rownr, i);
    AlwaysAssertExit (tabnr == 0);
    AlwaysAssertExit (rownr == i);
  }
  for (uInt i=10; i<25; ++i) {
    rows.mapRownr (tabnr, rownr, i);
    AlwaysAssertExit (tabnr == 1);
    AlwaysAssertExit (rownr == i-10);
  }
  // Check if it fails if rownr out of bounds.
  Bool ok = True;
  try {
    rows.mapRownr (tabnr, rownr, rows.nrow());
  } catch (AipsError& x) {
    ok = False;
  }
  AlwaysAssertExit (!ok);

  // Check if iteration is fine.
  {
    // Check for an empty object.
    ConcatRowsIter iter ((ConcatRows()));
    AlwaysAssertExit (iter.pastEnd());
    iter.next();
    AlwaysAssertExit (iter.pastEnd());
  }    
  {
    // Check for an empty object.
    ConcatRowsIter iter ((ConcatRows()), 0, 1000);
    AlwaysAssertExit (iter.pastEnd());
    iter.next();
    AlwaysAssertExit (iter.pastEnd());
  }    
  {
    // Check for the full iteration.
    ConcatRowsIter iter (rows);
    AlwaysAssertExit (! iter.pastEnd());
    RefRows chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 0);
    AlwaysAssertExit (chunk.nrow() == 10);
    iter.next();
    chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 0);
    AlwaysAssertExit (chunk.nrow() == 15);
    iter.next();
    AlwaysAssertExit (iter.pastEnd());
  }
  {
    // Check for the full iteration.
    ConcatRowsIter iter (rows, 0, 1000);
    AlwaysAssertExit (! iter.pastEnd());
    RefRows chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 0);
    AlwaysAssertExit (chunk.nrow() == 10);
    AlwaysAssertExit (iter.tableNr() == 0);
    iter.next();
    chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 0);
    AlwaysAssertExit (chunk.nrow() == 15);
    AlwaysAssertExit (iter.tableNr() == 1);
    iter.next();
    AlwaysAssertExit (iter.pastEnd());
  }    
  {
    // Check for the empty iteration.
    ConcatRowsIter iter (rows, 2, 1);
    AlwaysAssertExit (iter.pastEnd());
  }    
  {
    // Check for partial iteration.
    ConcatRowsIter iter (rows, 2, 8);
    AlwaysAssertExit (! iter.pastEnd());
    RefRows chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 2);
    AlwaysAssertExit (chunk.nrow() == 7);
    AlwaysAssertExit (iter.tableNr() == 0);
    iter.next();
    AlwaysAssertExit (iter.pastEnd());
  }    
  {
    // Check for partial iteration.
    ConcatRowsIter iter (rows, 12, 18);
    AlwaysAssertExit (! iter.pastEnd());
    RefRows chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 2);
    AlwaysAssertExit (chunk.nrow() == 7);
    AlwaysAssertExit (iter.tableNr() == 1);
    iter.next();
    AlwaysAssertExit (iter.pastEnd());
  }    
  {
    // Check for partial iteration.
    ConcatRowsIter iter (rows, 2, 18);
    AlwaysAssertExit (! iter.pastEnd());
    RefRows chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 2);
    AlwaysAssertExit (chunk.nrow() == 8);
    AlwaysAssertExit (iter.tableNr() == 0);
    iter.next();
    chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 0);
    AlwaysAssertExit (chunk.nrow() == 9);
    AlwaysAssertExit (iter.tableNr() == 1);
    iter.next();
    AlwaysAssertExit (iter.pastEnd());
  }    
  {
    // Check for iteration with steps.
    ConcatRowsIter iter (rows, 2, 25, 4);
    AlwaysAssertExit (! iter.pastEnd());
    RefRows chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 2);
    AlwaysAssertExit (chunk.nrow() == 2);
    AlwaysAssertExit (iter.tableNr() == 0);
    iter.next();
    chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 0);
    AlwaysAssertExit (chunk.nrow() == 4);
    AlwaysAssertExit (iter.tableNr() == 1);
    iter.next();
    AlwaysAssertExit (iter.pastEnd());
  }    
  {
    // Check for iteration with steps.
    ConcatRowsIter iter (rows, 2, 18, 5);
    AlwaysAssertExit (! iter.pastEnd());
    RefRows chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 2);
    AlwaysAssertExit (chunk.nrow() == 2);
    AlwaysAssertExit (iter.tableNr() == 0);
    iter.next();
    chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 2);
    AlwaysAssertExit (chunk.nrow() == 2);
    AlwaysAssertExit (iter.tableNr() == 1);
    iter.next();
    AlwaysAssertExit (iter.pastEnd());
  }    
  {
    // Check for iteration with steps.
    ConcatRowsIter iter (rows, 1, 22, 7);
    AlwaysAssertExit (! iter.pastEnd());
    RefRows chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 1);
    AlwaysAssertExit (chunk.nrow() == 2);
    AlwaysAssertExit (iter.tableNr() == 0);
    iter.next();
    chunk = iter.getChunk();
    AlwaysAssertExit (chunk.firstRow() == 5);
    AlwaysAssertExit (chunk.nrow() == 2);
    AlwaysAssertExit (iter.tableNr() == 1);
    iter.next();
    AlwaysAssertExit (iter.pastEnd());
  }    
}

int main()
{
  try {
    doIt();
  } catch (AipsError& x) {
    cout << "\nCaught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;               // successfully executed
}
