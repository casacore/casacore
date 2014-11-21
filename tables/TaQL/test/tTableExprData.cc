//# tTableExprData.cc: Test program for class tTableExprData
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

#include <casacore/tables/TaQL/TableExprData.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/RecordExpr.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for class TableExprData.
// </summary>

// This program tests the class TableExprData.
// This example shows how a data set consisting of two vectors
// of scalars can be used.


// Write a class derived from TableExprData to handle the vectors.
class MyTestClass : public TableExprData
{
public:
  // Constructor checks if both vectors have equal length.
  MyTestClass (const Vector<Int>& fld1, const Vector<String>& fld2)
    : itsFld1(fld1), itsFld2(fld2), itsEntry(0)
    { AlwaysAssert (fld1.nelements() == fld2.nelements(), AipsError); }
  virtual ~MyTestClass()
    {}
  void next()
    { itsEntry++; }
  // Note that only the get functions for the possible types are needed.
  // The exception should never be thrown unless things are screwed up.
  virtual Int64 getInt (const Block<Int>& fieldNrs) const
    { switch (fieldNrs[0]) {
      case 0:
        return itsFld1(itsEntry);
      default:
        throw AipsError();
      }
    }
  virtual String getString (const Block<Int>& fieldNrs) const
    { switch (fieldNrs[0]) {
      case 1:
        return itsFld2(itsEntry);
      default:
        throw AipsError();
      }
    }
  virtual DataType dataType (const Block<Int>& fieldNrs) const
    { switch (fieldNrs[0]) {
      case 0:
        return TpInt;
      case 1:
        return TpString;
      default:
        throw AipsError();
      }
    }
  // Make a Record to give to vectors a name.
  // The order in which the fields are defined determines the fieldnrs
  // passed to the get functions.
  static Record makeRecord()
    { RecordDesc desc;
      desc.addField ("fld1", TpInt);
      desc.addField ("fld2", TpString);
      return Record(desc);
    }
private:
  Vector<Int>    itsFld1;
  Vector<String> itsFld2;
  uInt           itsEntry;
};
  
Vector<uInt> findMatches (const Vector<Int>& fld1,
                          const Vector<String>& fld2)
{
  // Make some expression.
  // First create a Record to make the names and types known.
  Record rec(MyTestClass::makeRecord());
  TableExprNode expr (makeRecordExpr(rec,"fld1") > 10 &&
                      makeRecordExpr(rec,"fld2") != pattern("*xxx*"));
  // Now evaluate the expression for each entry in the vector.
  // Make a MyTestClass object to handle the vectors and put it in
  // a TableExprId object for the TaQL evaluator.
  // Note that TableExprId holds a pointer to the original MyTestClass
  // object, so the TaQL evaluator 'sees' the changes we make by
  // using the its next() function.
  MyTestClass subj(fld1, fld2);
  TableExprId eid(subj);
  // The matching entry numbers are stored in a vector.
  Vector<uInt> result(fld1.nelements());
  uInt nr=0;
  Bool valb;
  for (uInt i=0; i<fld1.nelements(); i++) {
    expr.get (eid, valb);
    if (valb) {
      result(nr++) = i;
    }
    subj.next();         // Next time the next entry must be used
  }
  result.resize (nr, True);
  return result;
}


int main()
{
  try {
    Vector<Int> fld1(4);
    fld1(0) = 4; fld1(1) = 10; fld1(2) = 11; fld1(3) = 20;
    Vector<String> fld2(4);
    fld2(0) = "xxx"; fld2(1) = ""; fld2(2) = "axxxa"; fld2(3) = "axxax";
    Vector<uInt> m = findMatches (fld1, fld2);
    AlwaysAssertExit (m.nelements() == 1);
    AlwaysAssertExit (m(0) == 3);
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
