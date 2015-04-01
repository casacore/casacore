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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/RecordGram.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
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
    } catch (AipsError x) {
      cout << "Expected exception:\n" << x.getMesg() << endl;
      err = True;
    }
    AlwaysAssertExit (err);
  }
}


int main()
{
  try {
    doIt();
  } catch (AipsError x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  } catch (...) {
    cout << "Unexpected unknown exception" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
