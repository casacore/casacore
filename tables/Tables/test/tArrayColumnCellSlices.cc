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
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
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
void createTable (DataManager& dataMan,
                  const Array<Int> array,
                  Bool useDirect)
{
    IPosition shape = array.shape();
    int nCorrelations = shape(0);
    int nChannels = shape (1);
    int nRows = shape(2);

    // Build the table description.

    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<Int>("testArrayColumn",IPosition(2, nCorrelations, nChannels),
                                       useDirect ? ColumnDesc::Direct : ColumnDesc::FixedShape));

    // Now create a new table from the description.

    SetupNewTable newtab("tArrayColumnCellSlices_tmp.data", td, Table::New);
    newtab.bindAll (dataMan);
    Table tab(newtab, nRows, False, Table::LocalEndian);
    ArrayColumn<Int> arrayColumn (tab, "testArrayColumn");

    //indgen (arrf);

    RefRows rows (0, nRows-1); // all rows
    arrayColumn.putColumnCells (rows, referenceArray);

}

void
clearValues (ArrayColumn<Int> & arrayColumn, Int value = 0)
{
    int nRows = referenceArray.shape().last();

    Array<Int> array (referenceArray.shape(), value);

    RefRows rows (0, nRows-1); // all rows

    arrayColumn.putColumnCells (rows, array);
}

void
createReferenceArray (int nCorrelations, int nChannels, int nRows)
{
   for (Int i=0; i<nRows; i++) {
	for (Int j=0; j < nCorrelations; j++){
	    for (Int k=0; k < nChannels; k++){
		referenceArray (IPosition (3, j, k, i)) = i * 100 + k * 10 + j;;
	    }
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

void
readAndCompareArray (ArrayColumn<Int> & arrayColumn)
{
    Array<Int> array (referenceArray.shape(), -2);
    RefRows rows (0, referenceArray.shape().last()-1);
    arrayColumn.getColumnCells (rows, array);

    compareToReferenceArray (array);
}

void readCellSlices()
{
  Table tab("tArrayColumnCellSlices_tmp.data");
  ArrayColumn<Int> arrayColumn(tab, "testArrayColumn");
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

      Vector <Slicer *> dataSlicer (3, nullptr);
      Vector <Slicer *> destinationSlicer (2, nullptr);
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

      Vector <Slicer *> dataSlicer (1, nullptr);
      Vector <Slicer *> destinationSlicer (1, nullptr);
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

      Vector <Slicer *> dataSlicer (1, nullptr);
      Vector <Slicer *> destinationSlicer (1, nullptr);
      IPosition shape (2, nCorrelations, nChannels);

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, nChannels), IPosition (2, 1));
      destinationSlicer (0) = new Slicer (IPosition (2, 0), IPosition (2, nCorrelations, nChannels), IPosition (2, 1));

      ColumnSlicer columnSlicer (shape, dataSlicer, destinationSlicer);
      RefRows refRows (0, nRows - 1);
      Array<Int> destination (IPosition (3, nCorrelations, nChannels, nRows));

      arrayColumn.getColumnCells (refRows, columnSlicer, destination);

      compareToReferenceArray (destination);

  }

  {
      // Check the actual I/O now
      //
      // Do read in two halves

      Vector <Slicer *> dataSlicer (2, nullptr);
      Vector <Slicer *> destinationSlicer (2, nullptr);
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

      arrayColumn.getColumnCells (refRows, columnSlicer, destination);

      compareToReferenceArray (destination);

  }

  {
      // Check the actual I/O now
      //
      // Do read in two halves the other way.

      Vector <Slicer *> dataSlicer (2, nullptr);
      Vector <Slicer *> destinationSlicer (2, nullptr);
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

      arrayColumn.getColumnCells (refRows, columnSlicer, destination);

      compareToReferenceArray (destination);

  }



  {
      // Check the actual I/O now
      //
      // Do it with two interleaving slices.

      Vector <Slicer *> dataSlicer (2, nullptr);
      Vector <Slicer *> destinationSlicer (2, nullptr);
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

      arrayColumn.getColumnCells (refRows, columnSlicer, destination);

      compareToReferenceArray (destination);

  }

  {
      // Check the actual I/O now
      //
      // Do it with two interleaving slices doing it the other way

      Vector <Slicer *> dataSlicer (2, nullptr);
      Vector <Slicer *> destinationSlicer (2, nullptr);
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

      arrayColumn.getColumnCells (refRows, columnSlicer, destination);

      compareToReferenceArray (destination);

  }

}

void writeCellSlices()
{
    Table tab("tArrayColumnCellSlices_tmp.data", Table::Update);
    ArrayColumn<Int> arrayColumn(tab, "testArrayColumn");
  
  {
      // Check the actual I/O now
      //
      // Read the whole thing at once.

      Vector <Slicer *> dataSlicer (1, nullptr);
      Vector <Slicer *> sourceSlicer (1, nullptr);
      IPosition shape (2, nCorrelations, nChannels);

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, nChannels), IPosition (2, 1));
      sourceSlicer (0) = new Slicer (IPosition (2, 0), IPosition (2, nCorrelations, nChannels), IPosition (2, 1));

      ColumnSlicer columnSlicer (shape, dataSlicer, sourceSlicer);
      RefRows refRows (0, nRows - 1);
      Array<Int> source = referenceArray.copy();

      arrayColumn.putColumnCells (refRows, columnSlicer, source);

      readAndCompareArray (arrayColumn);

  }

  {
      // Check the actual I/O now
      //
      // Do read in two halves

      Vector <Slicer *> dataSlicer (2, nullptr);
      Vector <Slicer *> sourceSlicer (2, nullptr);
      IPosition shape (2, nCorrelations, nChannels);

      uInt n1 = nChannels / 2;
      uInt n2 = nChannels / 2 + nChannels % 2;

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, n1), IPosition (2, 1, 1));
      sourceSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, n1), IPosition (2, 1, 1));

      dataSlicer (1) = new Slicer (IPosition (2, 0, n1), IPosition (2, nCorrelations, n2), IPosition (2, 1, 1));
      sourceSlicer (1) = new Slicer (IPosition (2, 0, n1), IPosition (2, nCorrelations, n2), IPosition (2, 1, 1));

      ColumnSlicer columnSlicer (shape, dataSlicer, sourceSlicer);
      RefRows refRows (0, nRows - 1);
      Array<Int> source = referenceArray.copy();

      arrayColumn.putColumnCells (refRows, columnSlicer, source);

      readAndCompareArray (arrayColumn);

  }

  {
      // Check the actual I/O now
      //
      // Do read in two halves the other way.

      Vector <Slicer *> dataSlicer (2, nullptr);
      Vector <Slicer *> sourceSlicer (2, nullptr);
      IPosition shape (2, nCorrelations, nChannels);

      uInt n1 = nCorrelations / 2;
      uInt n2 = nCorrelations / 2 + nCorrelations % 2;

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, n1, nChannels), IPosition (2, 1, 1));
      sourceSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, n1, nChannels), IPosition (2, 1, 1));

      dataSlicer (1) = new Slicer (IPosition (2, n1, 0), IPosition (2, n2, nChannels), IPosition (2, 1, 1));
      sourceSlicer (1) = new Slicer (IPosition (2, n1, 0), IPosition (2, n2, nChannels), IPosition (2, 1, 1));

      ColumnSlicer columnSlicer (shape, dataSlicer, sourceSlicer);
      RefRows refRows (0, nRows - 1);
      Array<Int> source = referenceArray.copy();

      arrayColumn.putColumnCells (refRows, columnSlicer, source);

      readAndCompareArray (arrayColumn);

  }



  {
      // Check the actual I/O now
      //
      // Do it with two interleaving slices.

      Vector <Slicer *> dataSlicer (2, nullptr);
      Vector <Slicer *> sourceSlicer (2, nullptr);
      IPosition shape (2, nCorrelations, nChannels);

      uInt nChannels1 = nChannels / 2;
      uInt nChannels2 = nChannels / 2 + nChannels % 2;

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, nChannels2), IPosition (2, 1, 2));
      sourceSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations, nChannels2), IPosition (2, 1, 2));

      dataSlicer (1) = new Slicer (IPosition (2, 0, 1), IPosition (2, nCorrelations, nChannels1), IPosition (2, 1, 2));
      sourceSlicer (1) = new Slicer (IPosition (2, 0, 1), IPosition (2, nCorrelations, nChannels1), IPosition (2, 1, 2));

      ColumnSlicer columnSlicer (shape, dataSlicer, sourceSlicer);
      RefRows refRows (0, nRows - 1);

      Array<Int> source = referenceArray.copy();

      arrayColumn.putColumnCells (refRows, columnSlicer, source);

      readAndCompareArray (arrayColumn);

  }

  {
      // Check the actual I/O now
      //
      // Do it with two interleaving slices doing it the other way

      Vector <Slicer *> dataSlicer (2, nullptr);
      Vector <Slicer *> sourceSlicer (2, nullptr);
      IPosition shape (2, nCorrelations, nChannels);

      uInt nCorrelations1 = nCorrelations / 2;
      uInt nCorrelations2 = nCorrelations / 2 + nCorrelations % 2;

      dataSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations2, nChannels), IPosition (2, 2, 1));
      sourceSlicer (0) = new Slicer (IPosition (2, 0, 0), IPosition (2, nCorrelations2, nChannels), IPosition (2, 2, 1));

      dataSlicer (1) = new Slicer (IPosition (2, 1, 0), IPosition (2, nCorrelations1, nChannels), IPosition (2, 2, 1));
      sourceSlicer (1) = new Slicer (IPosition (2, 1, 0), IPosition (2, nCorrelations1, nChannels), IPosition (2, 2, 1));

      ColumnSlicer columnSlicer (shape, dataSlicer, sourceSlicer);
      RefRows refRows (0, nRows - 1);

      Array<Int> source = referenceArray.copy();

      arrayColumn.putColumnCells (refRows, columnSlicer, source);

      readAndCompareArray (arrayColumn);

  }

}


int main()
{
  try {
      createReferenceArray (nCorrelations, nChannels, nRows);
      {
        StandardStMan dataMan;
        createTable (dataMan, referenceArray, True);
        readCellSlices();
        writeCellSlices();
      } 
      {
        StandardStMan dataMan;
        createTable (dataMan, referenceArray, False);
        readCellSlices();
        writeCellSlices();
      }
     {
        IncrementalStMan dataMan;
        createTable (dataMan, referenceArray, True);
        readCellSlices();
        writeCellSlices();
      }
     {
        IncrementalStMan dataMan;
        createTable (dataMan, referenceArray, False);
        readCellSlices();
        writeCellSlices();
      }
 } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}

