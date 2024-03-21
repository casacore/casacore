//# tRecordGram.cc: Test program for the record selection grsmmar
//# Copyright (C) 2000,2001
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/RecordGram.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for record selection.
// </summary>

// This program tests the class RecordGram to select records from a group
// of records.


void doIt()
{
  // Check if it handles a normal record field.
  TableRecord rec;
  rec.define ("fld1", Int(1));
  TableExprNode expr (RecordGram::parse(rec, "fld1 == 1."));
  Bool result;
  expr.get (rec, result);
  AlwaysAssertExit (result);
  // Check if it can also handle a record where fld1 is e.g. a float.
  rec.removeField ("fld1");
  rec.define ("fld1", Float(2));
  expr.get (rec, result);
  AlwaysAssertExit (!result);

  // Check if it handles fields in subrecords.
  TableRecord subrec1, subrec2;
  subrec2.define ("fld1", 1);
  subrec1.defineRecord ("sub2", subrec2);
  rec.defineRecord ("sub1", subrec1);
  expr.get (rec, result);
  AlwaysAssertExit (!result);
  TableExprNode expr1 (RecordGram::parse(rec, "sub1.sub2.fld1 == 1"));
  expr1.get (rec, result);
  AlwaysAssertExit (result);
  TableExprNode expr2 (RecordGram::parse(rec, "sub1.sub2.fld1 == 2"));
  expr2.get (rec, result);
  AlwaysAssertExit (!result);
  TableExprNode expr3 (RecordGram::parse(rec,
					 "sub1.sub2.fld1 == 1 && fld1 > 1"));
  expr3.get (rec, result);
  AlwaysAssertExit (result);
  rec.define ("fld1", Float(1));
  expr3.get (rec, result);
  AlwaysAssertExit (!result);

  // Check if ifDefined behaves correctly.
  TableExprNode expr4a (RecordGram::parse (rec,
					   "isdefined (sub1.sub2.fld1)"));
  expr4a.get (rec, result);
  AlwaysAssertExit (result);
  TableExprNode expr4b (RecordGram::parse (rec, "isdefined (fld1)"));
  expr4b.get (rec, result);
  AlwaysAssertExit (result);
  // Undefined when used on an empty record.
  TableRecord rect;
  TableRecord subrect1, subrect2;
  expr4a.get (rect, result);
  AlwaysAssertExit (!result);
  // Still undefined.
  rect.define ("fld2", True);
  rect.defineRecord ("sub1", subrect1);
  expr4a.get (rect, result);
  AlwaysAssertExit (!result);
  // Still undefined because field has incorrect type.
  subrect2.define ("fld1", True);
  subrect1.defineRecord ("sub2", subrect2);
  rect.defineRecord ("sub1", subrect1);
  expr4a.get (rect, result);
  AlwaysAssertExit (!result);
  // Now it should be defined.
  subrect2.removeField ("fld1");
  subrect2.define ("fld1", 1);
  subrect1.defineRecord ("sub2", subrect2);
  rect.defineRecord ("sub1", subrect1);
  expr4a.get (rect, result);
  AlwaysAssertExit (result);
  // Now undefined again (because sub1 has not correct fieldNumber anymore).
  rect.removeField ("fld2");
  expr4a.get (rect, result);
  AlwaysAssertExit (!result);

  // Check ndim and shape function for a scalar.
  TableExprNode expr5a (RecordGram::parse (rec, "ndim (fld1) == 0"));
  expr5a.get (rec, result);
  AlwaysAssertExit (result);
  TableExprNode expr5b (RecordGram::parse (rec,
					   "nelements (shape (fld1)) == 0"));
  expr5b.get (rec, result);
  AlwaysAssertExit (result);

  // Check if array fields are handled correctly.
  Array<Int> arr(IPosition(3,6,8,12));
  indgen (arr);
  rec.define ("arr1", arr);
  TableExprNode expr6a (RecordGram::parse (rec, "max (arr1) > 6*8*12"));
  expr6a.get (rec, result);
  AlwaysAssertExit (!result);
  TableExprNode expr6b (RecordGram::parse (rec, "max (arr1) >= 6*8*12-1"));
  expr6b.get (rec, result);
  AlwaysAssertExit (result);
  // Check shape and ndim function.
  TableExprNode expr6c (RecordGram::parse (rec, "7 in shape (arr1)"));
  expr6c.get (rec, result);
  AlwaysAssertExit (!result);
  TableExprNode expr6d (RecordGram::parse (rec, "7 in shape(arr1) - 1"));
  expr6d.get (rec, result);
  AlwaysAssertExit (result);

  // Check if an array in an array works fine.
  TableExprNode expr7a (RecordGram::parse (rec,
					   "all(shape(arr1) in shape(arr1))"));
  expr7a.get (rec, result);
  AlwaysAssertExit (result);
  TableExprNode expr7b (RecordGram::parse (rec,
					"any(shape(arr1) in shape(arr1)-1)"));
  expr7b.get (rec, result);
  AlwaysAssertExit (!result);
  // Check if an array in a set works fine.
  TableExprNode expr7c (RecordGram::parse (rec,
					   "all(shape(arr1) in [6,8,12])"));
  expr7c.get (rec, result);
  AlwaysAssertExit (result);
  TableExprNode expr7d (RecordGram::parse (rec,
					   "any(shape(arr1) in [5,7,11])"));
  expr7d.get (rec, result);
  AlwaysAssertExit (!result);
  // Check if an array in a set works fine.
  TableExprNode expr7e (RecordGram::parse (rec,
					   "all([6,8,12] in shape(arr1))"));
  expr7e.get (rec, result);
  AlwaysAssertExit (result);
  TableExprNode expr7f (RecordGram::parse (rec,
					   "any([5,7,11] in shape(arr1))"));
  expr7f.get (rec, result);
  AlwaysAssertExit (!result);
  // Check if a set in a set works fine.
  TableExprNode expr7g (RecordGram::parse (rec,
					   "all([6,8,12] in [6,8,12])"));
  expr7g.get (rec, result);
  AlwaysAssertExit (result);
  TableExprNode expr7h (RecordGram::parse (rec,
					   "any([5,7,11] in [6,8,12])"));
  expr7h.get (rec, result);
  AlwaysAssertExit (!result);

  // Check if rownumber is indeed an invalid function.
  {
    Bool err = False;
    try {
      TableExprNode expr8 (RecordGram::parse (rec, "rownumber() > 3"));
    } catch (std::exception& x) {
      cout << "Expected exception:\n" << x.what() << endl;
      err = True;
    }
    AlwaysAssertExit (err);
  }
}


// Test the expr2 functions.
void testExpr2()
{
  Record vars;
  AlwaysAssertExit (RecordGram::expr2Bool("T||F") == True);
  AlwaysAssertExit (RecordGram::expr2Int("2*2") == 4);
  AlwaysAssertExit (RecordGram::expr2Double("4") == 4);
  AlwaysAssertExit (RecordGram::expr2Complex("4") == DComplex(4,0));
  AlwaysAssertExit (RecordGram::expr2String("'ab'+'cd'") == "abcd");
  AlwaysAssertExit (near (RecordGram::expr2Date("12Mar2017/12:34:56.7").second(),
                          MVTime(Time(2017,3,12,12,34,56.7)).second()));
  AlwaysAssertExit (RecordGram::expr2Double("4 kHz", vars, "Hz") == 4000);
  AlwaysAssertExit (RecordGram::expr2Double("1.2m", vars, "m") == 1.2);
  Array<Bool> arrb;
  Array<Int64> arri;
  Array<double> arrd;
  Array<DComplex> arrc;
  Array<String> arrs;
  Array<MVTime> arrm;
  vars.define ("i", 10);
  vars.define ("s", "xy");
  arrb = RecordGram::expr2ArrayBool("T", vars);
  arri = RecordGram::expr2ArrayInt("i", vars);
  arrd = RecordGram::expr2ArrayDouble("i cm", vars, "m");
  arrc = RecordGram::expr2ArrayComplex("i + i*1i", vars);
  arrs = RecordGram::expr2ArrayString("'str'", vars);
  arrm = RecordGram::expr2ArrayDate("12Mar2017/12:34:56.7", vars);
  AlwaysAssertExit (arrb.shape() == IPosition(1,1)  &&
                    arrb.data()[0] == True);
  AlwaysAssertExit (arri.shape() == IPosition(1,1)  &&
                    arri.data()[0] == 10);
  AlwaysAssertExit (arrd.shape() == IPosition(1,1)  &&
                    arrd.data()[0] == 0.1);
  AlwaysAssertExit (arrc.shape() == IPosition(1,1)  &&
                    arrc.data()[0] == DComplex(10,10));
  AlwaysAssertExit (arrs.shape() == IPosition(1,1)  &&
                    arrs.data()[0] == "str");
  AlwaysAssertExit (arrm.shape() == IPosition(1,1)  &&
                    near(arrm.data()[0].second(),
                         MVTime(Time(2017,3,12,12,34,56.7)).second()));
  arrb.reference (RecordGram::expr2ArrayBool("[T,F]", vars));
  arri.reference (RecordGram::expr2ArrayInt("[i,i+1]", vars));
  arrd.reference (RecordGram::expr2ArrayDouble("[10cm, 10+2dm]", vars, "m"));
  arrc.reference (RecordGram::expr2ArrayComplex("[i, i*1i]", vars));
  arrs.reference (RecordGram::expr2ArrayString("['str', s]", vars));
  arrm.reference (RecordGram::expr2ArrayDate("[12Mar2017/12:34:56.7, "
                                             "12Mar2017/12:34:56.7 + 2d]"));
  AlwaysAssertExit (arrb.shape() == IPosition(1,2)  &&
                    arrb.data()[0] == True  &&  arrb.data()[1] == False);
  AlwaysAssertExit (arri.shape() == IPosition(1,2)  &&
                    arri.data()[0] == 10  &&  arri.data()[1] == 11);
  AlwaysAssertExit (arrd.shape() == IPosition(1,2)  &&
                    arrd.data()[0] == 0.1  &&  arrd.data()[1] == 1.2);
  AlwaysAssertExit (arrc.shape() == IPosition(1,2)  &&
                    arrc.data()[0] == DComplex(10,0)  &&
                    arrc.data()[1] == DComplex(0,10));
  AlwaysAssertExit (arrs.shape() == IPosition(1,2)  &&
                    arrs.data()[0] == "str"  &&  arrs.data()[1] == "xy");
  AlwaysAssertExit (arrm.shape() == IPosition(1,2)  &&
                    near(arrm.data()[0].second(),
                         MVTime(Time(2017,3,12,12,34,56.7)).second())  &&
                    near(arrm.data()[1].second(),
                         MVTime(Time(2017,3,14,12,34,56.7)).second()));
}


int main()
{
  try {
    doIt();
    testExpr2();
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  } catch (...) {
    cout << "Unexpected unknown exception" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
