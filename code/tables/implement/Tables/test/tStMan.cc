//# tStMan.cc: Test program for the various storage managers
//# Copyright (C) 2000
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

#include <aips/Tables/TableDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableLock.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/StandardStMan.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>
#include <strstream.h>

// <summary>
// Test program for the various storage managers.
// </summary>


// Create and fill a new table.
void newtab (uInt nrrow, const DataManager& stman)
{
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ScalarColumnDesc<String>("str1"));
  ScalarColumnDesc<String> dstr2("str2");
  dstr2.setMaxLength (20);
  td.addColumn (dstr2);
  td.addColumn (ArrayColumnDesc<String>("stra1",IPosition(2,2,3),
					ColumnDesc::Direct));
  ArrayColumnDesc<String> dstra2("stra2",IPosition(2,2,3),
				 ColumnDesc::Direct);
  dstra2.setMaxLength (20);
  td.addColumn (dstra2);
  td.addColumn (ArrayColumnDesc<String>("stra3", -1, ColumnDesc::FixedShape));
  ArrayColumnDesc<String> dstra4("stra4");
  dstra4.setMaxLength (20);
  td.addColumn (dstra4);

  td.addColumn (ScalarColumnDesc<Bool>("b1"));
  td.addColumn (ArrayColumnDesc<Bool>("ba1",IPosition(2,2,3),
				      ColumnDesc::Direct));
  td.addColumn (ArrayColumnDesc<Bool>("ba2", -1, ColumnDesc::FixedShape));
  td.addColumn (ArrayColumnDesc<Bool>("ba3"));

  td.addColumn (ScalarColumnDesc<Float>("f1"));
  td.addColumn (ArrayColumnDesc<Float>("fa1",IPosition(2,2,3),
				       ColumnDesc::Direct));
  td.addColumn (ArrayColumnDesc<Float>("fa2", -1, ColumnDesc::FixedShape));
  td.addColumn (ArrayColumnDesc<Float>("fa3"));

  td.addColumn (ScalarColumnDesc<DComplex>("dc1"));
  td.addColumn (ArrayColumnDesc<DComplex>("dca1",IPosition(2,2,3),
					  ColumnDesc::Direct));
  td.addColumn (ArrayColumnDesc<DComplex>("dca2", -1, ColumnDesc::FixedShape));
  td.addColumn (ArrayColumnDesc<DComplex>("dca3"));

  // Now create a new table from the description.
  SetupNewTable newtab("tStMan_tmp.data", td, Table::New);
  // Create a storage manager for it.
  newtab.bindAll (stman);
  newtab.setShapeColumn("stra3",IPosition(2,2,3));
  newtab.setShapeColumn("ba2",IPosition(2,2,3));
  newtab.setShapeColumn("fa2",IPosition(2,2,3));
  newtab.setShapeColumn("dca2",IPosition(2,2,3));
  Table tab(newtab, nrrow);

  Array<String> emptyArray(IPosition(2,2,3));

  ScalarColumn<String> str1 (tab, "str1");
  ScalarColumn<String> str2 (tab, "str2");
  ArrayColumn<String> stra1 (tab, "stra1");
  ArrayColumn<String> stra2 (tab, "stra2");
  ArrayColumn<String> stra3 (tab, "stra3");
  ArrayColumn<String> stra4 (tab, "stra4");
  ScalarColumn<Bool> b1 (tab, "b1");
  ArrayColumn<Bool> ba1 (tab, "ba1");
  ArrayColumn<Bool> ba2 (tab, "ba2");
  ArrayColumn<Bool> ba3 (tab, "ba3");
  ScalarColumn<Float> f1 (tab, "f1");
  ArrayColumn<Float> fa1 (tab, "fa1");
  ArrayColumn<Float> fa2 (tab, "fa2");
  ArrayColumn<Float> fa3 (tab, "fa3");
  ScalarColumn<DComplex> dc1 (tab, "dc1");
  ArrayColumn<DComplex> dca1 (tab, "dca1");
  ArrayColumn<DComplex> dca2 (tab, "dca2");
  ArrayColumn<DComplex> dca3 (tab, "dca3");
  for (uInt i=0; i<nrrow; i++) {
    AlwaysAssertExit (str1(i) == "");
    AlwaysAssertExit (str2(i) == "");
    AlwaysAssertExit (allEQ (stra1(i), emptyArray));
    AlwaysAssertExit (allEQ (stra2(i), emptyArray));
    AlwaysAssertExit (allEQ (stra3(i), emptyArray));
    AlwaysAssertExit (! stra4.isDefined(i));
    AlwaysAssertExit (  b1.isDefined(i));
    AlwaysAssertExit (  ba1.isDefined(i));
    AlwaysAssertExit (  ba2.isDefined(i));
    AlwaysAssertExit (! ba3.isDefined(i));
    AlwaysAssertExit (  f1.isDefined(i));
    AlwaysAssertExit (  fa1.isDefined(i));
    AlwaysAssertExit (  fa2.isDefined(i));
    AlwaysAssertExit (! fa3.isDefined(i));
    AlwaysAssertExit (  dc1.isDefined(i));
    AlwaysAssertExit (  dca1.isDefined(i));
    AlwaysAssertExit (  dca2.isDefined(i));
    AlwaysAssertExit (! dca3.isDefined(i));
  }
}

void checktab1()
{
  Table tab("tStMan_tmp.data");

  Array<String> emptyArray(IPosition(2,2,3));

  ROScalarColumn<String> str1 (tab, "str1");
  ROScalarColumn<String> str2 (tab, "str2");
  ROArrayColumn<String> stra1 (tab, "stra1");
  ROArrayColumn<String> stra2 (tab, "stra2");
  ROArrayColumn<String> stra3 (tab, "stra3");
  ROArrayColumn<String> stra4 (tab, "stra4");
  ROScalarColumn<Bool> b1 (tab, "b1");
  ROArrayColumn<Bool> ba1 (tab, "ba1");
  ROArrayColumn<Bool> ba2 (tab, "ba2");
  ROArrayColumn<Bool> ba3 (tab, "ba3");
  ROScalarColumn<Float> f1 (tab, "f1");
  ROArrayColumn<Float> fa1 (tab, "fa1");
  ROArrayColumn<Float> fa2 (tab, "fa2");
  ROArrayColumn<Float> fa3 (tab, "fa3");
  ROScalarColumn<DComplex> dc1 (tab, "dc1");
  ROArrayColumn<DComplex> dca1 (tab, "dca1");
  ROArrayColumn<DComplex> dca2 (tab, "dca2");
  ROArrayColumn<DComplex> dca3 (tab, "dca3");
  uInt nrrow = tab.nrow();
  for (uInt i=0; i<nrrow; i++) {
    AlwaysAssertExit (str1(i) == "");
    AlwaysAssertExit (str2(i) == "");
    AlwaysAssertExit (allEQ (stra1(i), emptyArray));
    AlwaysAssertExit (allEQ (stra2(i), emptyArray));
    AlwaysAssertExit (allEQ (stra3(i), emptyArray));
    AlwaysAssertExit (! stra4.isDefined(i));
    AlwaysAssertExit (  b1.isDefined(i));
    AlwaysAssertExit (  ba1.isDefined(i));
    AlwaysAssertExit (  ba2.isDefined(i));
    AlwaysAssertExit (! ba3.isDefined(i));
    AlwaysAssertExit (  f1.isDefined(i));
    AlwaysAssertExit (  fa1.isDefined(i));
    AlwaysAssertExit (  fa2.isDefined(i));
    AlwaysAssertExit (! fa3.isDefined(i));
    AlwaysAssertExit (  dc1.isDefined(i));
    AlwaysAssertExit (  dca1.isDefined(i));
    AlwaysAssertExit (  dca2.isDefined(i));
    AlwaysAssertExit (! dca3.isDefined(i));
  }
}

void checktab (const String& prefix)
{
  Table tab("tStMan_tmp.data");

  Array<String> emptyArray(IPosition(2,2,3));
  Array<String> filledArray(IPosition(2,2,3));
  filledArray(IPosition(2,0,0)) = prefix + "str_00_";
  filledArray(IPosition(2,0,1)) = "str_01_";
  filledArray(IPosition(2,0,2)) = "str_02_";
  filledArray(IPosition(2,1,0)) = "str_10_";
  filledArray(IPosition(2,1,1)) = "str_11_";
  filledArray(IPosition(2,1,2)) = "str_12_";

  Array<Float> arrf(IPosition(2,2,3));
  indgen (arrf);
  Array<Double> arrd(IPosition(2,4,3));
  indgen (arrd);
  Array<DComplex> arrdc = RealToComplex (arrd);
  Array<Bool> arrb = (fmod(arrf,float(4)) == float(0));

  ROScalarColumn<String> str1 (tab, "str1");
  ROScalarColumn<String> str2 (tab, "str2");
  ROArrayColumn<String> stra1 (tab, "stra1");
  ROArrayColumn<String> stra2 (tab, "stra2");
  ROArrayColumn<String> stra3 (tab, "stra3");
  ROArrayColumn<String> stra4 (tab, "stra4");
  ROScalarColumn<Bool> b1 (tab, "b1");
  ROArrayColumn<Bool> ba1 (tab, "ba1");
  ROArrayColumn<Bool> ba2 (tab, "ba2");
  ROArrayColumn<Bool> ba3 (tab, "ba3");
  ROScalarColumn<Float> f1 (tab, "f1");
  ROArrayColumn<Float> fa1 (tab, "fa1");
  ROArrayColumn<Float> fa2 (tab, "fa2");
  ROArrayColumn<Float> fa3 (tab, "fa3");
  ROScalarColumn<DComplex> dc1 (tab, "dc1");
  ROArrayColumn<DComplex> dca1 (tab, "dca1");
  ROArrayColumn<DComplex> dca2 (tab, "dca2");
  ROArrayColumn<DComplex> dca3 (tab, "dca3");

  uInt nrrow = tab.nrow();
  char buf[8];
  {
    String s1(prefix + "str1_");
    String s2(prefix + "str2_");
    for (uInt i=0; i<nrrow; i++) {
      sprintf (buf, "%d", i);
      s1 += buf;
      s2 += buf;
      if (s2.length() > 20) {
	s2 = prefix + "str2_";
      }
      AlwaysAssertExit (str1(i) == s1);
      AlwaysAssertExit (str2(i) == s2);
      AlwaysAssertExit (allEQ (stra1(i),
			       String("a1_")+filledArray+String(buf)));
      AlwaysAssertExit (allEQ (stra1.getSlice (i, Slicer(IPosition(2,1,0),
							 IPosition(2,1,2))),
			       (String("a1_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (stra2(i),
			       String("a2_")+filledArray+String(buf)));
      AlwaysAssertExit (allEQ (stra2.getSlice (i, Slicer(IPosition(2,1,0),
							 IPosition(2,1,2))),
			       (String("a2_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (stra3(i),
			       String("a3_")+filledArray+String(buf)));
      AlwaysAssertExit (allEQ (stra3.getSlice (i, Slicer(IPosition(2,1,0),
							 IPosition(2,1,2))),
			       (String("a3_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      if (i%2 == 0) {
	AlwaysAssertExit (allEQ (stra4(i),
				 String("a4_")+filledArray+String(buf)));
	AlwaysAssertExit (allEQ (stra4.getSlice (i, Slicer(IPosition(2,1,0),
							   IPosition(2,1,2))),
				 (String("a4_")+filledArray+String(buf))
				 (IPosition(2,1,0), IPosition(2,1,1))));
      } else {
	AlwaysAssertExit (allEQ (stra4(i), emptyArray));
	AlwaysAssertExit (allEQ (stra4.getSlice (i, Slicer(IPosition(2,1,0),
							   IPosition(2,1,2))),
				 emptyArray
				 (IPosition(2,1,0), IPosition(2,1,1))));
      }

      AlwaysAssertExit (b1(i) == (i%2==0));
      AlwaysAssertExit (allEQ (ba1(i), arrb));
      AlwaysAssertExit (allEQ (ba1.getSlice (i, Slicer(IPosition(2,1,0),
						       IPosition(2,1,2))),
			       arrb(IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (ba2(i), !arrb));
      AlwaysAssertExit (allEQ (ba2.getSlice (i, Slicer(IPosition(2,1,0),
						       IPosition(2,1,2))),
			       (!arrb)(IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (ba3(i), arrb));
      AlwaysAssertExit (allEQ (ba3.getSlice (i, Slicer(IPosition(2,1,0),
						       IPosition(2,1,2))),
			       arrb(IPosition(2,1,0), IPosition(2,1,1))));

      AlwaysAssertExit (f1(i) == i+1);
      AlwaysAssertExit (allEQ (fa1(i), arrf));
      AlwaysAssertExit (allEQ (fa1.getSlice (i, Slicer(IPosition(2,1,0),
						       IPosition(2,1,2))),
			       arrf(IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (fa2(i), arrf+float(1)));
      AlwaysAssertExit (allEQ (fa2.getSlice (i, Slicer(IPosition(2,1,0),
						       IPosition(2,1,2))),
			       (arrf+float(1))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (fa3(i), arrf+float(2)));
      AlwaysAssertExit (allEQ (fa3.getSlice (i, Slicer(IPosition(2,1,0),
						       IPosition(2,1,2))),
			       (arrf+float(2))
			       (IPosition(2,1,0), IPosition(2,1,1))));

      AlwaysAssertExit (dc1(i) == DComplex(i+1,i+2));
      AlwaysAssertExit (allEQ (dca1(i), arrdc));
      AlwaysAssertExit (allEQ (dca1.getSlice (i, Slicer(IPosition(2,1,0),
							IPosition(2,1,2))),
			       arrdc(IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (dca2(i), arrdc+DComplex(1,2)));
      AlwaysAssertExit (allEQ (dca2.getSlice (i, Slicer(IPosition(2,1,0),
							IPosition(2,1,2))),
			       (arrdc+DComplex(1,2))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (dca3(i), arrdc+DComplex(3,4)));
      AlwaysAssertExit (allEQ (dca3.getSlice (i, Slicer(IPosition(2,1,0),
							IPosition(2,1,2))),
			       (arrdc+DComplex(3,4))
			       (IPosition(2,1,0), IPosition(2,1,1))));

      arrb = !arrb;
      arrf += float(10);
      arrdc += DComplex(10,20);
    }
  }
  {
    String s1(prefix + "str1_");
    String s2(prefix + "str2_");
    indgen (arrf);
    arrdc = RealToComplex (arrd);
    arrb = (fmod(arrf,float(4)) == float(0));
    Vector<String> vec1 = str1.getColumn();
    Vector<String> vec2 = str2.getColumn();
    Array<String> arr1 = stra1.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<String> arr2 = stra2.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<String> arr3 = stra3.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<String> arr4 = stra4.getColumn().reform(IPosition(2,2,3*nrrow));
    Vector<Bool> bvec1 = b1.getColumn();
    Array<Bool> barr1 = ba1.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<Bool> barr2 = ba2.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<Bool> barr3 = ba3.getColumn().reform(IPosition(2,2,3*nrrow));
    Vector<Float> fvec1 = f1.getColumn();
    Array<Float> farr1 = fa1.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<Float> farr2 = fa2.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<Float> farr3 = fa3.getColumn().reform(IPosition(2,2,3*nrrow));
    Vector<DComplex> dcvec1 = dc1.getColumn();
    Array<DComplex> dcarr1 = dca1.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<DComplex> dcarr2 = dca2.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<DComplex> dcarr3 = dca3.getColumn().reform(IPosition(2,2,3*nrrow));
    for (uInt i=0; i<nrrow; i++) {
      sprintf (buf, "%d", i);
      s1 += buf;
      s2 += buf;
      if (s2.length() > 20) {
	s2 = prefix + "str2_";
      }
      AlwaysAssertExit (vec1(i) == s1);
      AlwaysAssertExit (vec2(i) == s2);
      AlwaysAssertExit (allEQ (arr1(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       String("a1_")+filledArray+String(buf)));
      AlwaysAssertExit (allEQ (arr2(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       String("a2_")+filledArray+String(buf)));
      AlwaysAssertExit (allEQ (arr3(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       String("a3_")+filledArray+String(buf)));
      if (i%2 == 0) {
	AlwaysAssertExit (allEQ (arr4(IPosition(2,0,3*i),IPosition(2,1,3*i+2)),
				 String("a4_")+filledArray+String(buf)));
      } else {
	AlwaysAssertExit (allEQ (arr4(IPosition(2,0,3*i),IPosition(2,1,3*i+2)),
				 emptyArray));
      }

      AlwaysAssertExit (bvec1(i) == (i%2==0));
      AlwaysAssertExit (allEQ (barr1(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       arrb));
      AlwaysAssertExit (allEQ (barr2(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       !arrb));
      AlwaysAssertExit (allEQ (barr3(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       arrb));

      AlwaysAssertExit (fvec1(i) == i+1);
      AlwaysAssertExit (allEQ (farr1(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       arrf));
      AlwaysAssertExit (allEQ (farr2(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       arrf+float(1)));
      AlwaysAssertExit (allEQ (farr3(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       arrf+float(2)));

      AlwaysAssertExit (dcvec1(i) == DComplex(i+1,i+2));
      AlwaysAssertExit (allEQ(dcarr1(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			      arrdc));
      AlwaysAssertExit (allEQ(dcarr2(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			      arrdc+DComplex(1,2)));
      AlwaysAssertExit (allEQ(dcarr3(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			      arrdc+DComplex(3,4)));

      arrb = !arrb;
      arrf += float(10);
      arrdc += DComplex(10,20);
    }
  }
  {
    String s1(prefix + "str1_");
    String s2(prefix + "str2_");
    indgen (arrf);
    arrdc = RealToComplex (arrd);
    arrb = (fmod(arrf,float(4)) == float(0));
    Array<String> arr1 = stra1.getColumn(Slicer(IPosition(2,1,0),
						IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    Array<String> arr2 = stra2.getColumn(Slicer(IPosition(2,1,0),
						IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    Array<String> arr3 = stra3.getColumn(Slicer(IPosition(2,1,0),
						IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    Array<String> arr4 = stra4.getColumn(Slicer(IPosition(2,1,0),
						IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    Array<Bool> barr1 = ba1.getColumn(Slicer(IPosition(2,1,0),
					     IPosition(2,1,2))).
                        reform(IPosition(2,1,2*nrrow));
    Array<Bool> barr2 = ba2.getColumn(Slicer(IPosition(2,1,0),
					     IPosition(2,1,2))).
                        reform(IPosition(2,1,2*nrrow));
    Array<Bool> barr3 = ba3.getColumn(Slicer(IPosition(2,1,0),
					     IPosition(2,1,2))).
                        reform(IPosition(2,1,2*nrrow));
    Array<Float> farr1 = fa1.getColumn(Slicer(IPosition(2,1,0),
					      IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    Array<Float> farr2 = fa2.getColumn(Slicer(IPosition(2,1,0),
					      IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    Array<Float> farr3 = fa3.getColumn(Slicer(IPosition(2,1,0),
					      IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    Array<DComplex> dcarr1 = dca1.getColumn(Slicer(IPosition(2,1,0),
						   IPosition(2,1,2))).
                        reform(IPosition(2,1,2*nrrow));
    Array<DComplex> dcarr2 = dca2.getColumn(Slicer(IPosition(2,1,0),
						   IPosition(2,1,2))).
                             reform(IPosition(2,1,2*nrrow));
    Array<DComplex> dcarr3 = dca3.getColumn(Slicer(IPosition(2,1,0),
						   IPosition(2,1,2))).
                             reform(IPosition(2,1,2*nrrow));
    for (uInt i=0; i<nrrow; i++) {
      sprintf (buf, "%d", i);
      s1 += buf;
      s2 += buf;
      if (s2.length() > 20) {
	s2 = prefix + "str2_";
      }
      AlwaysAssertExit (allEQ (arr1(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       (String("a1_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (arr2(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       (String("a2_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (arr3(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       (String("a3_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      if (i%2 == 0) {
	AlwaysAssertExit (allEQ (arr4(IPosition(2,0,2*i),IPosition(2,0,2*i+1)),
				 (String("a4_")+filledArray+String(buf))
			         (IPosition(2,1,0), IPosition(2,1,1))));
      } else {
	AlwaysAssertExit (allEQ (arr4(IPosition(2,0,2*i),IPosition(2,0,2*i+1)),
				 emptyArray
	                         (IPosition(2,1,0), IPosition(2,1,1))));
      }

      AlwaysAssertExit (allEQ (barr1(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       arrb(IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (barr2(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       (!arrb)(IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (barr3(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       arrb(IPosition(2,1,0), IPosition(2,1,1))));

      AlwaysAssertExit (allEQ (farr1(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       arrf(IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (farr2(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       (arrf+float(1))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (farr3(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       (arrf+float(2))
			       (IPosition(2,1,0), IPosition(2,1,1))));

      AlwaysAssertExit (allEQ(dcarr1(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			      arrdc(IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ(dcarr2(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			      (arrdc+DComplex(1,2))
			      (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ(dcarr3(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			      (arrdc+DComplex(3,4))
			      (IPosition(2,1,0), IPosition(2,1,1))));

      arrb = !arrb;
      arrf += float(10);
      arrdc += DComplex(10,20);
    }
  }
}

void extab (const String& prefix)
{
  Table tab("tStMan_tmp.data", Table::Update);

  Array<String> filledArray(IPosition(2,2,3));
  filledArray(IPosition(2,0,0)) = prefix + "str_00_";
  filledArray(IPosition(2,0,1)) = "str_01_";
  filledArray(IPosition(2,0,2)) = "str_02_";
  filledArray(IPosition(2,1,0)) = "str_10_";
  filledArray(IPosition(2,1,1)) = "str_11_";
  filledArray(IPosition(2,1,2)) = "str_12_";

  Array<Float> arrf(IPosition(2,2,3));
  indgen (arrf);
  Array<Double> arrd(IPosition(2,4,3));
  indgen (arrd);
  Array<DComplex> arrdc = RealToComplex (arrd);
  Array<Bool> arrb = (fmod(arrf,float(4)) == float(0));

  ScalarColumn<String> str1 (tab, "str1");
  ScalarColumn<String> str2 (tab, "str2");
  ArrayColumn<String> stra1 (tab, "stra1");
  ArrayColumn<String> stra2 (tab, "stra2");
  ArrayColumn<String> stra3 (tab, "stra3");
  ArrayColumn<String> stra4 (tab, "stra4");
  ScalarColumn<Bool> b1 (tab, "b1");
  ArrayColumn<Bool> ba1 (tab, "ba1");
  ArrayColumn<Bool> ba2 (tab, "ba2");
  ArrayColumn<Bool> ba3 (tab, "ba3");
  ScalarColumn<Float> f1 (tab, "f1");
  ArrayColumn<Float> fa1 (tab, "fa1");
  ArrayColumn<Float> fa2 (tab, "fa2");
  ArrayColumn<Float> fa3 (tab, "fa3");
  ScalarColumn<DComplex> dc1 (tab, "dc1");
  ArrayColumn<DComplex> dca1 (tab, "dca1");
  ArrayColumn<DComplex> dca2 (tab, "dca2");
  ArrayColumn<DComplex> dca3 (tab, "dca3");

  char buf[8];
  String s1(prefix + "str1_");
  String s2(prefix + "str2_");
  uInt nrrow = tab.nrow();
  for (uInt i=0; i<nrrow; i++) {
    sprintf (buf, "%d", i);
    s1 += buf;
    s2 += buf;
    if (s2.length() > 20) {
      s2 = prefix + "str2_";
    }
    str1.put (i, s1);
    str2.put (i, s2);
    stra1.put (i, String("a1_")+filledArray+String(buf));
    stra2.put (i, String("a2_")+filledArray+String(buf));
    stra3.put (i, String("a3_")+filledArray+String(buf));
    if (i%2 == 0) {
      stra4.put (i, String("a4_")+filledArray+String(buf));
    } else {
      stra4.setShape (i, filledArray.shape());
    }

    b1.put(i, i%2==0);
    ba1.put (i, arrb);
    ba2.put (i, !arrb);
    ba3.put (i, arrb);

    f1.put(i, i+1);
    fa1.put (i, arrf);
    fa2.put (i, arrf+float(1));
    fa3.put (i, arrf+float(2));

    dc1.put(i, DComplex(i+1,i+2));
    dca1.put (i, arrdc);
    dca2.put (i, arrdc+DComplex(1,2));
    dca3.put (i, arrdc+DComplex(3,4));

    arrb = !arrb;
    arrf += float(10);
    arrdc += DComplex(10,20);
  }

  checktab(prefix);
}

void doTest (uInt nrrow, const DataManager& stman)
{
  newtab (nrrow, stman);
  checktab1();
  for (uInt i=0; i<4; i++) {
    extab ("");
    checktab ("");
  }
  extab ("p");
  checktab ("p");
}

main (int argc, const char* argv[])
{
  uInt nrrow = 10;
  uInt bucketSize = 500;
  if (argc > 1) {
    istrstream istr(argv[1]);
    istr >> nrrow;
  }
  if (argc > 2) {
    istrstream istr(argv[2]);
    istr >> bucketSize;
  }

  try {
    doTest (nrrow, StManAipsIO());
    doTest (nrrow, StandardStMan (max(bucketSize,500u)));
    doTest (nrrow, IncrementalStMan (max(bucketSize,1000u), False));
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
