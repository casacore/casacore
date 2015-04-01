//# tSSMAddRemove.cc: Test program when adding/removing rows
//# Copyright (C) 2012
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

#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>

using namespace casacore;

// This test program was created due to a problem in BucketCache found
// on 31-Aug-2012. When a bucket was added and removed before being flushed,
// only the free list (4 bytes) was written instead of the entire bucket.
// When opening the file again, it found that the nr of buckets in the file
// was one short and tried to write it, which failed if opened as readonly.
//
// The table used is tarred in tSSMAddRemove.in and unpacked by the .run file.

void addRemove()
{
  Table tab ("tSSMAddRemove_tmp.tab", Table::Update);
  cout << "nrow=" << tab.nrow() << endl;
  tab.addRow (14);
  Vector<uInt> rows(14);
  indgen (rows, 28u);
  tab.removeRow (rows);
}

int main()
{
  try {
    addRemove();
    Table tab ("tSSMAddRemove_tmp.tab");
    ScalarColumn<Int> idcol(tab, "SOURCE_ID");
    idcol(27);
  } catch (std::exception& x) {
    cout << x.what() << endl;
    return 1;
  }
  return 0;
}
