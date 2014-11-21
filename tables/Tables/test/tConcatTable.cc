//# tConcatTable.cc: Test program for class ConcatTable
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
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdio.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for class tConcatTable
// </summary>

void doIt (const Table& tab)
{
  cout << ">>> -------------------" << endl;
  cout << "partNamesF " << tab.getPartNames() << endl;
  cout << "partNamesT " << tab.getPartNames(True) << endl;
  cout << "<<<" << endl;
  ScalarColumn<Int> ab2(tab,"ab");
  ScalarColumn<Int> ac (tab,"ac");
  ScalarColumn<uInt> ad(tab,"ad");
  ScalarColumn<float> ae(tab,"ae");
  ScalarColumn<String> af(tab,"af");
  ScalarColumn<DComplex> ag(tab,"ag");
  ArrayColumn<float> arr1(tab,"arr1");
  ArrayColumn<float> arr2(tab,"arr2");
  ArrayColumn<float> arr3(tab,"arr3");
  uInt i;
  Vector<String> names = tab.tableDesc().columnNames();
  for (i=0; i<names.nelements(); i++) {
    cout << names(i) << endl;
  }
  Int abval, acval;
  uInt adval;
  float aeval;
  String afval;
  DComplex agval;
  char str[8];
  Cube<float> arrf(IPosition(3,2,3,4));
  Cube<float> arrval(IPosition(3,2,3,4));
  Cube<float> arrvalslice(arrval(Slice(0,1),Slice(0,1,2),Slice(0,2,2)));
  Slice tmp;
  Slicer nslice (tmp, tmp, tmp, Slicer::endIsLength);
  Slicer nslice2(Slice(0,1), Slice(0,1,2), Slice(0,2,2),
		 Slicer::endIsLength);
  indgen (arrf);
  for (i=0; i<10; i++) {
    cout << "get scalar row " << i << endl;
    ab2.get (i, abval);
    ac.get (i, acval);
    ad.get (i, adval);
    ae.get (i, aeval);
    af.get (i, afval);
    ag.get (i, agval);
    sprintf (str, "V%i", i);
    if (abval != Int(i)  ||  acval != Int(i+1)  ||  adval != i+2
        ||  aeval != i+3  ||  afval != str  ||  agval != DComplex(i+2)) {
      cout << "error in row " << i << ": " << abval
	   << ", " << acval << ", " << adval
	   << ", " << aeval << ", " << afval
	   << ", " << agval << endl;
    }
    arr1.get (i, arrval);
    if (!allEQ (arrval, arrf)) {
      cout << "error in arr1 in row " << i << endl;
    }
    arr2.get (i, arrval);
    if (!allEQ (arrval, arrf)) {
      cout << "error in arr2 in row " << i << endl;
    }
    cout << "get array row " << i << endl;
    arr3.get (i, arrval);
    if (!allEQ (arrval, arrf)) {
      cout << "error in arr3 in row " << i << endl;
    }
    arr2.getSlice (i, nslice, arrval);
    if (!allEQ (arrval, arrf)) {
      cout << "error in arr2 (entire slice) in row " << i << endl;
    }
    arr2.getSlice (i, nslice2, arrvalslice);
    if (!allEQ (arrval, arrf)) {
      cout << "error in arr2 (partial slice) in row " << i << endl;
    }
    arrf += (float)(arrf.nelements());
  }
  Vector<Int> abvec = ab2.getColumn();
  cout << tab.nrow() << " " << abvec.nelements() << endl;
  for (i=0; i<10; i++) {
    if (abvec(i) != Int(i)) {
      cout << "error in getColumn " << i << ": " << abvec(i) << endl;
    }
  }
  Array<float> arr1a = arr1.getColumn();
  if (arr1a.ndim() != 4) {
    cout << "arr1a not 4-dim" << endl;
  }
  i=0;
  uInt j0,j1,j2,j3;
  for (j3=0; j3<10; j3++)
    for (j2=0; j2<4; j2++)
      for (j1=0; j1<3; j1++)
	for (j0=0; j0<2; j0++) {
	  if (arr1a(IPosition(4,j0,j1,j2,j3)) != i++) {
	    cout <<"arr1a error at " <<j0<<" "<<j1<<" "<<j2<<" "
		 <<j3<<" should be: "<<i<<endl;
	  }
	}
  Array<float> arr1b = arr1.getColumn(nslice);
  if (arr1b.ndim() != 4) {
    cout << "arr1b not 4-dim" << endl;
  }
  i=0;
  for (j3=0; j3<10; j3++)
    for (j2=0; j2<4; j2++)
      for (j1=0; j1<3; j1++)
	for (j0=0; j0<2; j0++) {
	  if (arr1b(IPosition(4,j0,j1,j2,j3)) != i++) {
	    cout <<"arr1b error at " <<j0<<" "<<j1<<" "<<j2<<" "
		 <<j3<<" should be: "<<i<<endl;
	  }
	}

  // Sort the table.
  Table sortab = tab.sort ("ae", Sort::Descending);
  if (sortab.nrow() != 10) {
    cout << "sortab does not contain 10 rows" << endl;
  }
  ScalarColumn<float> sorae(sortab, "ae");
  cout << sorae.getColumn() << endl;
  cout << "#columns in sortab: " << sortab.tableDesc().ncolumn() << endl;

  Table sortab2 = sortab.sort ("ad");
  if (sortab2.nrow() != 10) {
    cout << "sortab2 does not contain 10 rows" << endl;
  }
  ScalarColumn<uInt> sorad(sortab2, "ad");
  cout << sorad.getColumn() << endl;
  cout << "#columns in sortab2: " << sortab2.tableDesc().ncolumn() << endl;

  // Get a subset of the table via row numbers.
  Vector<uInt> rownrs(4);
  rownrs(0)=3;
  rownrs(1)=1;
  rownrs(2)=9;
  rownrs(3)=6;
  Table seltab1 = sortab(rownrs);
  if (seltab1.nrow() != 4) {
    cout << "seltab1 does not contain 4 rows" << endl;
  }
  ScalarColumn<Int> sel1ab(seltab1, "ab");
  cout << sel1ab.getColumn() << endl;
  cout << "#columns in seltab1: " << seltab1.tableDesc().ncolumn() << endl;

  // Project the table.
  Block<String> projname(3);
  projname[0] = "ae";
  projname[1] = "ab";
  projname[2] = "arr1";
  Table seltab2 = seltab1.project (projname);
  if (seltab2.nrow() != 4) {
    cout << "seltab2 does not contain 4 rows" << endl;
  }
  ScalarColumn<Int> sel2ab(seltab2, "ab");
  cout << sel2ab.getColumn() << endl;
  cout << "#columns in seltab2: " << seltab2.tableDesc().ncolumn() << endl;

  // Get a subset via a mask.
  Block<Bool> mask(4,True);
  mask[0] = False;
  mask[3] = False;
  Table seltab3 = seltab2(mask);
  if (seltab3.nrow() != 2) {
    cout << "seltab3 does not contain 2 rows" << endl;
  }
  ScalarColumn<Int> sel3ab(seltab3, "ab");
  cout << sel3ab.getColumn() << endl;
  cout << "#columns in seltab3: " << seltab3.tableDesc().ncolumn() << endl;
  cout << seltab3.tableDesc().columnNames() << endl;

  Table xortab = sortab ^ seltab1;
  if (xortab.nrow() != 6) {
    cout << "xortab does not contain 6 rows" << endl;
  }
  ScalarColumn<Int> xorab(xortab, "ab");
  cout << xorab.getColumn() << endl;
  cout << "#columns in xortab: " << xortab.tableDesc().ncolumn() << endl;

  Table or1tab = xortab | seltab3;
  if (or1tab.nrow() != 8) {
    cout << "or1tab does not contain 8 rows" << endl;
  }
  ScalarColumn<Int> or1ab(or1tab, "ab");
  cout << or1ab.getColumn() << endl;
  cout << "#columns in or1tab: " << or1tab.tableDesc().ncolumn() << endl;

  Table or2tab = seltab3 | xortab;
  if (or2tab.nrow() != 8) {
    cout << "or2tab does not contain 8 rows" << endl;
  }
  ScalarColumn<Int> or2ab(or2tab, "ab");
  cout << or2ab.getColumn() << endl;
  cout << "#columns in or2tab: " << or2tab.tableDesc().ncolumn() << endl;

  Table exprtab = sortab(sortab.col("ab") >= 5);
  if (exprtab.nrow() != 5) {
    cout << "exprtab does not contain 5 rows" << endl;
  }
  ScalarColumn<Int> exprab(exprtab, "ab");
  cout << exprab.getColumn() << endl;

  Table expr2tab = tab(tab.col("af") == "V3"  ||
		       (tab.col("ab") >= 5  &&  tab.col("ab") < 8));
  if (expr2tab.nrow() != 4) {
    cout << "expr2tab does not contain 4 rows" << endl;
  }
  ScalarColumn<Int> expr2ab(expr2tab, "ab");
  cout << expr2ab.getColumn() << endl;
  // Add a column.
  Table tabrw(tab);
  tabrw.reopenRW();
  tabrw.addColumn (ScalarColumnDesc<Int>("newcol"));
  AlwaysAssertExit (tab.tableDesc().isColumn ("newcol"));
}

void doIt1 (const String& tableName)
{
  cout << ">>> -------------------" << endl;
  cout << "start reading Tables: " << tableName << endl;
  cout << "<<<" << endl;
  Table tab(tableName);
  cout << "end reading Tables" << endl;
  uInt nr = tab.nrow();
  if (nr < 3) {
    cout << "Table must have at least 3 rows" << endl;
    return;
  }
  Block<Table> tabs(2);
  tabs[0] = tab(tab.nodeRownr() < nr/3);
  tabs[1] = tab(tab.nodeRownr() >= nr/3);
  Table ctab(tabs);
  doIt (ctab);
}

void doItMult (const Block<String>& tableNames)
{
  cout << ">>> -------------------" << endl;
  cout << "start reading Tables: " << tableNames << endl;
  cout << "<<<" << endl;
  Table tab(tableNames);
  cout << "end reading Tables" << endl;
  doIt (tab);
}

int main (int argc, const char* argv[])
{
  // Only execute when table names have been given.
  try {
    if (argc > 1) {
      if (argc == 2) {
	doIt1 (argv[1]);
      } else {
	Block<String> names(argc-1);
	for (int i=1; i<argc; i++) {
	  names[i-1] = argv[i];
	}
	doItMult (names);
      }
    }
  } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
