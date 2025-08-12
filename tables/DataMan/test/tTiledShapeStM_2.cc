//# tTiledShapeStM_2.cc: Test program for parallel write access of the TiledShapeStMan classes

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
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


TSMOption makeAccessType (int accessType, Bool read=True)
{
  if (!read) {
    accessType = accessType>>2;
  }
  if ((accessType&3) == 1) {
    cout << "use mmapped TSM access" << endl;
    return TSMOption (TSMOption::MMap, 0, 0);
  } else if ((accessType&3) == 2) {
    cout << "use buffered TSM access" << endl;
    return TSMOption (TSMOption::Buffer, 0, 0);
  }
  cout << "use cached TSM access" << endl;
  return TSMOption (TSMOption::Cache, 0, 0);
}

Bool readTable (int accessType, Bool chk, const IPosition& shape, uInt nrrow, Bool extrainc = false)
{
  Bool ok = True;
  Table table("tTiledShapeStM_2_tmp.data", Table::Old, makeAccessType(accessType));
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


void writeVar (int accessType, Bool chk, const IPosition& shape,
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
  Table table(newtab, 0, False, Table::AipsrcEndian, makeAccessType(accessType, False));
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

void updateVar (int accessType, Bool chk, Bool tiledAccess, const IPosition& shape,
               const IPosition& tileShape, uInt nrrow, int rank, int numRank)
{
  Table table("tTiledShapeStM_2_tmp.data", TableLock::NoLocking, Table::Old, makeAccessType(accessType));
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
  table.changeTiledDataOnly();
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
            cout << "Run as  tTiledShapeStM_2 accessType mode nrow nx ny [tx ty tz]" << endl;
            cout << "accessType&3 = 0: use cache on read" << endl;
            cout << "accessType&3 = 1: use mmap on read" << endl;
            cout << "accessType&3 = 2: use buffer on read" << endl;
            cout << "accessType&12 = 0: use cache on write" << endl;
            cout << "accessType&12 = 4: use mmap on write" << endl;
            cout << "accessType&12 = 8: use buffer on write" << endl;
            cout << "Mode&1 = 1: check data read" << endl;
            cout << "    &2 = 1: distribute by tiles instead of rows" << endl;
            cout << "nx ny = size of data array in each row" << endl;
            cout << "tx ty tz = size of tiles, where tz is number of rows per tile" << endl;
        }
        return 0;
      }
      uInt accessType, mode, nrow, nx, ny;
      accessType = std::stoi(argv[1]);
      mode = std::stoi(argv[2]);
      nrow = std::stoi(argv[3]);
      nx = std::stoi(argv[4]);
      ny = std::stoi(argv[5]);
      uInt tx = nx;
      if (argc >= 7) {
        tx = std::stoi(argv[6]);
      }
      uInt ty = ny;
      if (argc >= 8) {
        ty = std::stoi(argv[7]);
      }
      uInt tz = 1;
      if (argc >= 9) {
        tz = stoi(argv[8]);
      }
      IPosition shape(2,nx,ny);
      IPosition tileShape(3,tx,ty,tz);
      if (rank == 0) {
          cout << "accessType: " << accessType << endl;
          cout << "mode:       " << mode << endl;
          cout << "nrow:       " << nrow << endl;
          cout << "shape:      " << shape << endl;
          cout << "tileShape:  " << tileShape << endl;
          cout << ">>>" << endl;
      }
      // Write table and test with one rank
      if (rank == 0) {
          writeVar (accessType, mode%2==1, shape, tileShape, nrow);
          if (! readTable (accessType, mode%2==1, shape, nrow)) {
            ok = False;
          }
      }
      // all ranks wait for table to be written
      #ifdef HAVE_MPI
        MPI_Barrier(MPI_COMM_WORLD);
      #endif

      // now update table with all ranks
      updateVar(accessType, mode%2==1, (mode&2)==2, shape, tileShape, nrow, rank, numRank);

      // all ranks wait for table to be updated
      #ifdef HAVE_MPI
        MPI_Barrier(MPI_COMM_WORLD);
      #endif

      // check read back with rank 0
      if (rank == 0) {
          if (! readTable (accessType, mode%2==1, shape, nrow, true)) {
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
