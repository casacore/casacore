//# tTiledShapeStM_2.cc: Test program for parallel write access of the TiledShapeStMan classes
//# Copyright (C) 2023
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
#include <casacore/tables/DataMan/TiledShapeStMan.h>
//#include <casacore/tables/DataMan/DataManAccessor.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/OS/LECanonicalConversion.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for parallel write access of Tiled column data
// </summary>

// This program tests the class TiledShapeStMan and related classes.

// Outcomment and uncomment the correct typedef and define.
//typedef Int Type;

typedef Float Type;
#define ARRINIT indgen(array)
#define ARRINCR array += (Type)1

//typedef Bool Type;
//#define ARRINIT array = False
//#define ARRINCR array = !array


TSMOption makeAcc (int acc, Bool read=True)
{
  if (!read) {
    acc = acc>>2;
  }
  if ((acc&3) == 1) {
    cout << "use mmapped TSM access" << endl;
    return TSMOption (TSMOption::MMap, 0, 0);
  } else if ((acc&3) == 2) {
    cout << "use buffered TSM access" << endl;
    return TSMOption (TSMOption::Buffer, 0, 0);
  }
  cout << "use cached TSM access" << endl;
  return TSMOption (TSMOption::Cache, 0, 0);
}

Bool readTable (int acc, Bool chk, const IPosition& shape, uInt nrrow, Bool extrainc = false)
{
  Bool ok = True;
  Table table("tTiledShapeStM_2_tmp.data", Table::Old, makeAcc(acc));
  if (table.nrow() != nrrow) {
    cout << "Table has " << table.nrow() << " rows; expected "
         << nrrow << endl;
    return False;
  }
  ArrayColumn<Type> data (table, "Data");
  Array<Type> result;
  Array<Type> array(shape);
  ARRINIT;
  if (extrainc) {
      ARRINCR;
  }
  Timer timer;
  for (uInt i=0; i<nrrow; i++) {
    data.get (i, result);
    if (chk) {
      if (! allEQ (array, result)) {
        cout << "mismatch in data row " << i << endl;
        ok = False;
      }
      ARRINCR;
    }
  }
  timer.show("Read cell ");
  if (chk && ok) {
    cout << "  readVar checks successfull" << endl;
  }
  return ok;
}


void writeVar (int acc, Bool chk, const IPosition& shape,
               const IPosition& tileShape, uInt nrrow)
{
  // Build the table description.
  TableDesc td ("", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<Type> ("Data", shape.nelements()));
  td.defineHypercolumn ("TSMExample",
                        shape.nelements()+1,
                        stringToVector ("Data"));

  // Now create a new table from the description.
  SetupNewTable newtab("tTiledShapeStM_2_tmp.data", td, Table::New);
  // Create a storage manager for it.
  TiledShapeStMan sm1 ("TSMExample", tileShape);
  newtab.bindAll (sm1);
  Table table(newtab, 0, False, Table::AipsrcEndian, makeAcc(acc, False));
  ArrayColumn<Type> data (table, "Data");
  Array<Type> array(shape);
  uInt i;
  ARRINIT;
  Timer timer;
  try {
    for (i=0; i<nrrow; i++) {
      table.addRow();
      data.put (i, array);
      if (chk) {
        ARRINCR;
      }
    }
    // Sync to measure true IO.
    table.flush(True);
  } catch (std::exception& x) {
    cout << "Caught an exception: " << x.what() << endl;
  }
  timer.show("Write     ");
}

void updateVar (int acc, Bool chk, Bool tiledAccess, const IPosition& shape,
               const IPosition& tileShape, uInt nrrow, int rank, int numRank)
{
  Table table("tTiledShapeStM_2_tmp.data", TableLock::NoLocking, Table::Old, makeAcc(acc));
  if (table.nrow() != nrrow) {
    cout << "Table has " << table.nrow() << " rows; expected "
       << nrrow << endl;
  }
  ArrayColumn<Type> data (table, "Data");
  Array<Type> result;
  Array<Type> array(shape);
  //ARRINIT;
  Timer timer;
  uint startRow, numRows;
  if (tiledAccess) {
      uint nTiles = nrrow / tileShape(2);
      if (nTiles * tileShape(2) < nrrow) {
          nTiles++;
      }
      uint div = nTiles / numRank;
      uint rem = nTiles % numRank;
      // Simple round-robin: the first `rem` ranks receive an extra item
      uint firstTile = rank * div + (uint(rank) < rem ? rank : rem);
      uint numTiles = div + (uint(rank) < rem);
      startRow = firstTile * tileShape(2);
      numRows = numTiles * tileShape(2);
  } else {
      uint div = nrrow / numRank;
      uint rem = nrrow % numRank;
      // Simple round-robin: the first `rem` ranks receive an extra item
      startRow = rank * div + (uint(rank) < rem ? rank : rem);
      numRows = div + (uint(rank) < rem);
  }
  if (startRow + numRows > nrrow) {
      if (nrrow >= startRow) {
          numRows = nrrow - startRow;
      } else {
          numRows = 0;
      }
  }
  cout << "Rank "<< rank << " updates rows "<<startRow << " to "<< startRow + numRows<< endl;
  table.dataOnly();
  table.reopenRW();
  try {
    for (uint i=startRow; i<startRow + numRows; i++) {
      data.get (i, array);
      if (chk) {
        ARRINCR;
      }
      data.put(i, array);
    }
    // Sync to measure true IO.
    table.flush(True);
  } catch (std::exception& x) {
    cout << "Caught an exception: " << x.what() << endl;
  }
  //RODataManAccessor(table, "TSMExample", False).showCacheStatistics (cout);

  timer.show("Update     ");
}

int main (int argc, char* argv[])
{
    int rank = 0;
    int numRank = 1;

#ifdef HAVE_MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &numRank);
    if (rank == 1) {
        cout << " Running test with "<< numRank << " ranks" << endl;
    }
#endif
  Bool ok = True;
  try {
      if (argc < 6) {
        if (rank == 0) {
            cout << "Run as  tTiledShapeStM_2 acc mode nrow nx ny [tx ty tz]" << endl;
            cout << "Acc&3 = 0: use cache on read" << endl;
            cout << "Acc&3 = 1: use mmap on read" << endl;
            cout << "Acc&3 = 2: use buffer on read" << endl;
            cout << "Acc&12 = 0: use cache on write" << endl;
            cout << "Acc&12 = 4: use mmap on write" << endl;
            cout << "Acc&12 = 8: use buffer on write" << endl;
            cout << "Mode&1 = 1: check data read" << endl;
            cout << "    &2 = 1: distribute by tiles instead of rows" << endl;
        }
        return 0;
      }
      uInt acc, mode, nrow, nx, ny;
      {
        istringstream istr(argv[1]);
        istr >> acc;
      }
      {
        istringstream istr(argv[2]);
        istr >> mode;
      }
      {
        istringstream istr(argv[3]);
        istr >> nrow;
      }
      {
        istringstream istr(argv[4]);
        istr >> nx;
      }
      {
        istringstream istr(argv[5]);
        istr >> ny;
      }
      uInt tx = nx;
      if (argc >= 7) {
        istringstream istr(argv[6]);
        istr >> tx;
      }
      uInt ty = ny;
      if (argc >= 8) {
        istringstream istr(argv[7]);
        istr >> ty;
      }
      uInt tz = 1;
      if (argc >= 9) {
        istringstream istr(argv[8]);
        istr >> tz;
      }
      uInt sx = 0;
      uInt sy = 0;
      uInt ex = nx-1;
      uInt ey = ny-1;
      uInt ix = 1;
      uInt iy = 1;
      IPosition shape(2,nx,ny);
      IPosition tileShape(3,tx,ty,tz);
      IPosition blc(2,sx,sy);
      IPosition trc(2,ex,ey);
      IPosition inc(2,ix,iy);
      if (rank == 0) {
          cout << "acc:       " << acc << endl;
          cout << "mode:      " << mode << endl;
          cout << "nrow:      " << nrow << endl;
          cout << "shape:     " << shape << endl;
          cout << "tileShape: " << tileShape << endl;
          cout << "sei:       " << blc << trc << inc << endl;
          cout << ">>>" << endl;
      }
      // Write table and test with one rank
      if (rank == 0) {
          writeVar (acc, mode%2==1, shape, tileShape, nrow);
          if (! readTable (acc, mode%2==1, shape, nrow)) {
            ok = False;
          }
      }
      // all ranks wait for table to be written
      #ifdef HAVE_MPI
        MPI_Barrier(MPI_COMM_WORLD);
      #endif

      // now update table with all ranks
      updateVar(acc, mode%2==1, (mode&2)==2, shape, tileShape, nrow, rank, numRank);

      // all ranks wait for table to be updated
      #ifdef HAVE_MPI
        MPI_Barrier(MPI_COMM_WORLD);
      #endif

      // check read back with rank 0
      if (rank == 0) {
          if (! readTable (acc, mode%2==1, shape, nrow, true)) {
            ok = False;
          }
      }
  } catch (std::exception& x) {
    cout << "Caught an exception: " << x.what() << endl;
    return 1;
  }
  if (rank == 0) {
      cout << "<<<" << endl;
  }
#ifdef HAVE_MPI
  MPI_Finalize();
#endif
  if (!ok) {
    return 1;
  }
  return 0;                           // exit with success status
}
