//# tArrayColumnSlices.cc: Test program for the ArrayColumn slices functions
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

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

using namespace casacore;

uInt nRows=7;
uInt nChannels=5;
uInt nCorrelations=3;
Array<Int> referenceArray (IPosition (3, nCorrelations, nChannels, nRows));

// Create the table.
void createTab()
{
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<Int>("arr1",IPosition(2, nCorrelations, nChannels),
                                       ColumnDesc::FixedShape));
    // Now create a new table from the description.
    SetupNewTable newtab("tArrayColumnSlices_tmp.data", td, Table::New);
    Table tab(newtab, nRows, False, Table::LocalEndian);
    ArrayColumn<Int> arr1(tab, "arr1");
    Array<Int> arrf(IPosition(2,nCorrelations, nChannels));
    indgen (arrf);
    for (uInt i=0; i<nRows; i++) {
	for (uInt j=0; j < nCorrelations; j++){
	    for (uInt k=0; k < nChannels; k++){
		arrf (IPosition (2, j, k)) = i * 100 + k * 10 + j;
		referenceArray (IPosition (3, j, k, i)) = arrf (IPosition (2, j, k));
	    }
	    arr1.put(i, arrf);
	}
    }
}

void 
compareToReferenceArray (const Array<Int> other)
{

    AlwaysAssertExit (other.shape().isEqual (referenceArray.shape()));

    for (uInt i=0; i<nRows; i++) {
	for (uInt j=0; j < nCorrelations; j++){
	    for (uInt k=0; k < nChannels; k++){
		AlwaysAssertExit (referenceArray (IPosition (3, j, k, i)) == other (IPosition (3, j, k, i)));
	    }
	}
    }   
}

void readCellSlices()
{
  Table tab("tArrayColumnSlices_tmp.data");
  ArrayColumn<Int> arr1(tab, "arr1");
  {
      // Check ColumnSlicer validation logic
      //
      // No slicers

      Vector <Slicer *> dataSlicer;
      Vector <Slicer *> destinationSlicer;
      IPosition shape (1, 0);


      try {
	  
	  ColumnSlicer columnSlicer (shape, dataSlicer, destinationSlicer);
	  AlwaysAssertExit (False); // shouldn't get here
      }
      catch (AipsError & e){
      }
  }

  {
      // Check ColumnSlicer validation logic II
      //
      // Slicer lists must be same length

      Vector <Slicer *> dataSlicer (3, 0);
      Vector <Slicer *> destinationSlicer (2, 0);
      IPosition shape (1, 0);


      try {
	  
	  ColumnSlicer columnSlicer (shape, dataSlicer, destinationSlicer);
	  AlwaysAssertExit (False); // shouldn't get here
      }
      catch (AipsError & e){
      }
  }

  {
      // Check ColumnSlicer validation logic III
      //
      // Slicer elements must have same length

      Vector <Slicer *> dataSlicer (1, 0);
      Vector <Slicer *> destinationSlicer (1, 0);
      IPosition shape (1, 0);

      dataSlicer (0) = new Slicer (IPosition (1, 0), IPosition (1, 2), IPosition (1, 1));
      destinationSlicer (0) = new Slicer (IPosition (1, 0), IPosition (1, 3), IPosition (1, 1));

      try {
	  
	  ColumnSlicer columnSlicer (shape, dataSlicer, destinationSlicer);
	  AlwaysAssertExit (False); // shouldn't get here
      }
      catch (AipsError & e){
      }
  }
  
  {
      // Check the actual I/O now
      //
      // Read the whole thing at once.

      Vector <Slicer *> dataSlicer (1, 0);
      Vector <Slicer *> destinationSlicer (1, 0);
      IPosition shape (2, nCorrelations, nChannels);

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, nChannels), IPosition (2, 1));
      destinationSlicer (0) = new Slicer (IPosition (2, 0), IPosition (2, nCorrelations, nChannels), IPosition (2, 1));

      ColumnSlicer columnSlicer (shape, dataSlicer, destinationSlicer);
      RefRows refRows (0, nRows - 1);
      Array<Int> destination (IPosition (3, nCorrelations, nChannels, nRows));

      arr1.getColumnCells (refRows, columnSlicer, destination);

      compareToReferenceArray (destination);

  }

  {
      // Check the actual I/O now
      //
      // Do read in two halves

      Vector <Slicer *> dataSlicer (2, 0);
      Vector <Slicer *> destinationSlicer (2, 0);
      IPosition shape (2, nCorrelations, nChannels);

      uInt n1 = nChannels / 2;
      uInt n2 = nChannels / 2 + nChannels % 2;

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, n1), IPosition (2, 1, 1));
      destinationSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, n1), IPosition (2, 1, 1));

      dataSlicer (1) = new Slicer (IPosition (2, 0, n1), IPosition (2, nCorrelations, n2), IPosition (2, 1, 1));
      destinationSlicer (1) = new Slicer (IPosition (2, 0, n1), IPosition (2, nCorrelations, n2), IPosition (2, 1, 1));

      ColumnSlicer columnSlicer (shape, dataSlicer, destinationSlicer);
      RefRows refRows (0, nRows - 1);
      Array<Int> destination (IPosition (3, nCorrelations, nChannels, nRows));

      arr1.getColumnCells (refRows, columnSlicer, destination);

      compareToReferenceArray (destination);

  }

  {
      // Check the actual I/O now
      //
      // Do read in two halves the other way.

      Vector <Slicer *> dataSlicer (2, 0);
      Vector <Slicer *> destinationSlicer (2, 0);
      IPosition shape (2, nCorrelations, nChannels);

      uInt n1 = nCorrelations / 2;
      uInt n2 = nCorrelations / 2 + nCorrelations % 2;

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, n1, nChannels), IPosition (2, 1, 1));
      destinationSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, n1, nChannels), IPosition (2, 1, 1));

      dataSlicer (1) = new Slicer (IPosition (2, n1, 0), IPosition (2, n2, nChannels), IPosition (2, 1, 1));
      destinationSlicer (1) = new Slicer (IPosition (2, n1, 0), IPosition (2, n2, nChannels), IPosition (2, 1, 1));

      ColumnSlicer columnSlicer (shape, dataSlicer, destinationSlicer);
      RefRows refRows (0, nRows - 1);
      Array<Int> destination (IPosition (3, nCorrelations, nChannels, nRows));

      arr1.getColumnCells (refRows, columnSlicer, destination);

      compareToReferenceArray (destination);

  }



  {
      // Check the actual I/O now
      //
      // Do it with two interleaving slices.

      Vector <Slicer *> dataSlicer (2, 0);
      Vector <Slicer *> destinationSlicer (2, 0);
      IPosition shape (2, nCorrelations, nChannels);

      uInt nChannels1 = nChannels / 2;
      uInt nChannels2 = nChannels / 2 + nChannels % 2;

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, nChannels2), IPosition (2, 1, 2));
      destinationSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, nChannels2), IPosition (2, 1, 2));

      dataSlicer (1) = new Slicer (IPosition (2, 0, 1), IPosition (2, nCorrelations, nChannels1), IPosition (2, 1, 2));
      destinationSlicer (1) = new Slicer (IPosition (2, 0, 1), IPosition (2, nCorrelations, nChannels1), IPosition (2, 1, 2));

      ColumnSlicer columnSlicer (shape, dataSlicer, destinationSlicer);
      RefRows refRows (0, nRows - 1);

      Array<Int> destination (IPosition (3, nCorrelations, nChannels, nRows));

      arr1.getColumnCells (refRows, columnSlicer, destination);

      compareToReferenceArray (destination);

  }

  {
      // Check the actual I/O now
      //
      // Do it with two interleaving slices doing it the other way

      Vector <Slicer *> dataSlicer (2, 0);
      Vector <Slicer *> destinationSlicer (2, 0);
      IPosition shape (2, nCorrelations, nChannels);

      uInt nCorrelations1 = nCorrelations / 2;
      uInt nCorrelations2 = nCorrelations / 2 + nCorrelations % 2;

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations2, nChannels), IPosition (2, 2, 1));
      destinationSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations2, nChannels), IPosition (2, 2, 1));

      dataSlicer (1) = new Slicer (IPosition (2, 1, 0), IPosition (2, nCorrelations1, nChannels), IPosition (2, 2, 1));
      destinationSlicer (1) = new Slicer (IPosition (2, 1, 0), IPosition (2, nCorrelations1, nChannels), IPosition (2, 2, 1));

      ColumnSlicer columnSlicer (shape, dataSlicer, destinationSlicer);
      RefRows refRows (0, nRows - 1);

      Array<Int> destination (IPosition (3, nCorrelations, nChannels, nRows));

      arr1.getColumnCells (refRows, columnSlicer, destination);

      compareToReferenceArray (destination);

  }

}


int main()
{
  try {
    createTab();
    readCellSlices();
  } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
