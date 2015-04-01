//# tRecordExpr.cc: Test program for the record selection
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
#include <casacore/tables/TaQL/RecordExpr.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for record selection.
// </summary>

// This program tests the class RecordExpr to select records from a group
// of records.


void doIt()
{
  // Check if it handles a normal record field.
  TableRecord rec;
  rec.define ("fld1", Int(1));
  TableExprNode expr (makeRecordExpr(rec, "fld1") == 1.);
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
  TableExprNode expr1 (makeRecordExpr(rec, "sub1.sub2.fld1") == 1);
  expr1.get (rec, result);
  AlwaysAssertExit (result);
  TableExprNode expr2 (makeRecordExpr(rec, "sub1.sub2.fld1") == 2);
  expr2.get (rec, result);
  AlwaysAssertExit (!result);
  TableExprNode expr3 (makeRecordExpr(rec, "sub1.sub2.fld1") == 1
		       && makeRecordExpr(rec, "fld1") > 1);
  expr3.get (rec, result);
  AlwaysAssertExit (result);
  rec.define ("fld1", Float(1));
  expr3.get (rec, result);
  AlwaysAssertExit (!result);

  // Check if ifDefined behaves correctly.
  TableExprNode expr4a (isdefined (makeRecordExpr(rec, "sub1.sub2.fld1")));
  expr4a.get (rec, result);
  AlwaysAssertExit (result);
  TableExprNode expr4b (isdefined (makeRecordExpr(rec, "fld1")));
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
  TableExprNode expr5a (ndim (makeRecordExpr(rec, "fld1")) == 0);
  expr5a.get (rec, result);
  AlwaysAssertExit (result);
  TableExprNode expr5b (nelements (shape (makeRecordExpr(rec, "fld1"))) == 0);
  expr5b.get (rec, result);
  AlwaysAssertExit (result);

  // Check if array fields are handled correctly.
  Array<Int> arr(IPosition(3,6,8,12));
  indgen (arr);
  rec.define ("arr1", arr);
  TableExprNode expr6a (max (makeRecordExpr(rec, "arr1")) > 6*8*12);
  expr6a.get (rec, result);
  AlwaysAssertExit (!result);
  TableExprNode expr6b (max (makeRecordExpr(rec, "arr1")) >= 6*8*12-1);
  expr6b.get (rec, result);
  AlwaysAssertExit (result);
  // Check shape and ndim function.
  TableExprNode expr6c (TableExprNode(7).in
                          (shape (makeRecordExpr(rec, "arr1"))));
  expr6c.get (rec, result);
  AlwaysAssertExit (!result);
  TableExprNode expr6d (TableExprNode(7).in
                          (shape (makeRecordExpr(rec, "arr1")) - 1));
  expr6d.get (rec, result);
  AlwaysAssertExit (result);
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
  return 0;
}
