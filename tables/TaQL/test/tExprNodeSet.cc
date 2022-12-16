//# tExprNodeSet.cc: Test program for the ExprNodeSet selection classes
//# Copyright (C) 2009
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

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for the ExprNodeSet selection classes.
// </summary>


void doSetBool()
{
  TableExprNodeSetElem elem((TableExprNode(True)));
  TableExprNodeSet set;
  set.add (elem);
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.size() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, True));
  AlwaysAssertExit (!set.contains (0, False));
}

void doSetInt()
{
  TableExprNodeSetElem elem((TableExprNode(1)));
  TableExprNodeSet set;
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.size() == 1);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, 1));
  AlwaysAssertExit (!set.contains (0, 2));
  TableExprNode st(3);
  TableExprNode end(10);
  TableExprNode incr(3);
  set.add (TableExprNodeSetElem(&st, &end, &incr, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.size() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, 1));
  AlwaysAssertExit (!set.contains (0, 2));
  AlwaysAssertExit (set.contains (0, 3));
  AlwaysAssertExit (!set.contains (0, 4));
  set.add (TableExprNodeSetElem(&st, 0, 0, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (!set.isBounded());
  AlwaysAssertExit (set.size() == 3);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, 1));
  AlwaysAssertExit (!set.contains (0, 2));
  AlwaysAssertExit (set.contains (0, 3));
  AlwaysAssertExit (set.contains (0, 4));
}

void doSetDouble()
{
  TableExprNodeSetElem elem((TableExprNode(1.)));
  TableExprNodeSet set;
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.size() == 1);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, 1.));
  AlwaysAssertExit (!set.contains (0, 2.));
  TableExprNode st(3.);
  TableExprNode end(10);
  TableExprNode incr(3);
  set.add (TableExprNodeSetElem(&st, &end, &incr, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.size() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, 1.));
  AlwaysAssertExit (!set.contains (0, 2.));
  AlwaysAssertExit (set.contains (0, 3.));
  AlwaysAssertExit (!set.contains (0, 4.));
  set.add (TableExprNodeSetElem(&st, 0, 0, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (!set.isBounded());
  AlwaysAssertExit (set.size() == 3);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, 1.));
  AlwaysAssertExit (!set.contains (0, 2.));
  AlwaysAssertExit (set.contains (0, 3.));
  AlwaysAssertExit (set.contains (0, 4.));
}

void doSetDComplex()
{
  {
    // Test using a scalar as a single element.
    TableExprNodeSetElem elem((TableExprNode(DComplex(1,2))));
    TableExprNodeSet set;
    set.add (elem);
    set.add (elem);
    AlwaysAssertExit (set.isSingle());
    AlwaysAssertExit (set.isDiscrete());
    AlwaysAssertExit (set.isBounded());
    AlwaysAssertExit (set.size() == 2);
    AlwaysAssertExit (!set.hasArrays());
    AlwaysAssertExit (set.contains (0, DComplex(1,2)));
    AlwaysAssertExit (!set.contains (0, DComplex(1,3)));
  }
  {
    // Test using an array as a single element.
    // This can only be done by converting the set to an array.
    Vector<DComplex> vec({DComplex(1,2), DComplex(3,4)});
    TableExprNodeSetElem elem(vec);
    TableExprNodeSet set;
    set.add (elem);
    set.add (TableExprNodeSetElem (TableExprNode(DComplex(1,4))));
    AlwaysAssertExit (set.isDiscrete());
    AlwaysAssertExit (set.isBounded());
    AlwaysAssertExit (set.size() == 2);
    AlwaysAssertExit (set.hasArrays());
    AlwaysAssertExit (set.contains (0, DComplex(1,2)));
    AlwaysAssertExit (!set.contains (0, DComplex(1,3)));
    AlwaysAssertExit (set.contains (0, DComplex(3,4)));
    AlwaysAssertExit (set.contains (0, DComplex(1,4)));
  }
}

void doSetString()
{
  TableExprNodeSetElem elem((TableExprNode("ger")));
  TableExprNodeSet set;
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.size() == 1);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, "ger"));
  AlwaysAssertExit (!set.contains (0, "Ger"));
  TableExprNode st("ger1");
  TableExprNode end("ger9");
  set.add (TableExprNodeSetElem(True, st, end, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (!set.isDiscrete());
  AlwaysAssertExit (!set.isBounded());
  AlwaysAssertExit (set.size() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, "ger"));
  AlwaysAssertExit (!set.contains (0, "Ger"));
  AlwaysAssertExit (set.contains (0, "ger1"));
  AlwaysAssertExit (!set.contains (0, "ger99"));
  set.add (TableExprNodeSetElem(True, st));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (!set.isDiscrete());
  AlwaysAssertExit (!set.isBounded());
  AlwaysAssertExit (set.size() == 3);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, "ger"));
  AlwaysAssertExit (!set.contains (0, "Ger"));
  AlwaysAssertExit (set.contains (0, "ger1"));
  AlwaysAssertExit (set.contains (0, "ger99"));
}

void doSetDate()
{
  TableExprNodeSetElem elem(datetime("5Apr09/12:"));     // MJD 54926.5
  TableExprNodeSet set;
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.size() == 1);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, 54926.5));
  AlwaysAssertExit (!set.contains (0, 2.));
  TableExprNode st(datetime("5Apr09/12:"));    // MJD 54926.5
  TableExprNode end(datetime("7May09/12:"));   // MJD 54958.5
  TableExprNode incr(3);
  set.add (TableExprNodeSetElem(&st, &end, &incr, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.size() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, 54926.5));
  AlwaysAssertExit (!set.contains (0, 54927.5));
  AlwaysAssertExit (set.contains (0, 54929.5));
  AlwaysAssertExit (!set.contains (0, 54957.5));
  set.add (TableExprNodeSetElem(&st, 0, 0, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (!set.isBounded());
  AlwaysAssertExit (set.size() == 3);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, 54926.5));
  AlwaysAssertExit (set.contains (0, 54927.5));
  AlwaysAssertExit (set.contains (0, 54929.5));
  AlwaysAssertExit (set.contains (0, 54957.5));
}

void doIPosition()
{
  TableExprNodeSet set(IPosition(3,4,5,6));
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.dataType() == TableExprNodeRep::NTInt);
  AlwaysAssertExit (set.size() == 3);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.contains (0, 4));
  AlwaysAssertExit (set.contains (0, 5));
  AlwaysAssertExit (set.contains (0, 6));
  AlwaysAssertExit (!set.contains (0, 3));
  // Form an index node.
  TableExprNodeIndex inx(set);
  AlwaysAssertExit (inx.isSingle());
  AlwaysAssertExit (inx.getSlicer(0).start() == IPosition(3,4,5,6));
}

void doSlicer()
{
  {
    Slicer sl(IPosition(2,1,2), IPosition(2,10,12), IPosition(2,2,3),
              Slicer::endIsLast);
    TableExprNodeSet set(sl);
    AlwaysAssertExit (!set.isSingle());
    AlwaysAssertExit (set.isDiscrete());
    AlwaysAssertExit (set.isBounded());
    AlwaysAssertExit (set.dataType() == TableExprNodeRep::NTInt);
    AlwaysAssertExit (set.size() == 2);
    AlwaysAssertExit (!set.hasArrays());
    AlwaysAssertExit (set[0]->start()->getInt(0) == 1);
    AlwaysAssertExit (set[0]->end()->getInt(0) == 10);
    AlwaysAssertExit (set[0]->increment()->getInt(0) == 2);
    AlwaysAssertExit (set[1]->start()->getInt(0) == 2);
    AlwaysAssertExit (set[1]->end()->getInt(0) == 12);
    AlwaysAssertExit (set[1]->increment()->getInt(0) == 3);
    // Form an index node.
    TableExprNodeIndex inx(set);
    AlwaysAssertExit (!inx.isSingle());
    AlwaysAssertExit (inx.getSlicer(0).start() == IPosition(2,1,2));
    AlwaysAssertExit (inx.getSlicer(0).end() == IPosition(2,10,12));
    AlwaysAssertExit (inx.getSlicer(0).stride() == IPosition(2,2,3));
  }
  {
    Slicer sl(IPosition(2,Slicer::MimicSource,2),
              IPosition(2,10,Slicer::MimicSource),
              Slicer::endIsLast);

    TableExprNodeSet set(sl);
    AlwaysAssertExit (!set.isSingle());
    AlwaysAssertExit (set.isDiscrete());
    AlwaysAssertExit (set.isBounded());
    AlwaysAssertExit (set.dataType() == TableExprNodeRep::NTInt);
    AlwaysAssertExit (set.size() == 2);
    AlwaysAssertExit (!set.hasArrays());
    AlwaysAssertExit (! set[0]->start());
    AlwaysAssertExit (set[0]->end()->getInt(0) == 10);
    AlwaysAssertExit (set[0]->increment()->getInt(0) == 1);
    AlwaysAssertExit (set[1]->start()->getInt(0) == 2);
    AlwaysAssertExit (! set[1]->end());
    AlwaysAssertExit (set[1]->increment()->getInt(0) == 1);
  }
}

void doEmpty()
{
  TableExprNodeSet set;
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.dataType() == TableExprNodeRep::NTNumeric);
  AlwaysAssertExit (set.size() == 0);
  AlwaysAssertExit (!set.hasArrays());
}

#define tryError(CMD) \
  { \
    Bool ok = True;                            \
    try { \
      CMD; \
      ok = False; \
    } catch (const std::exception& x) { \
      cout << "Expected: " << x.what() << endl; \
    } \
    AlwaysAssertExit (ok); \
  }

void doErrors()
{
  TableExprNode intNode(1);
  TableExprNode dblNode(1.);
  dblNode.useUnit ("m");
  TableExprNode db2Node(1.);
  db2Node.useUnit ("Hz");
  TableExprNode strNode("a");
  TableExprNodeSetElem intElem(intNode);
  TableExprNodeSetElem rangeElem(nullptr, &intNode, nullptr);
  TableExprNodeSetElem dblElem(dblNode);
  TableExprNodeSetElem db2Elem(db2Node);
  TableExprNodeSetElem strElem(strNode);
  TableExprNodeSet set;
  set.add (intElem, True);
  set.add (dblElem, True);
  tryError (intNode.in (set));          // mismatching data types
  TableExprNode setarr(set.setOrArray());
  AlwaysAssertExit (setarr.getRep()->valueType() == TableExprNodeRep::VTArray);
  intNode.in (setarr);
  tryError (set.add (strElem, True));   // data type cannot be coerced
  {
    TableExprNodeSet set2(set);
    set2.add (db2Elem, True);
    TableExprNode setarr2(set2.setOrArray());
    AlwaysAssertExit (setarr2.getRep()->valueType() == TableExprNodeRep::VTSet);
  }
  {
    TableExprNodeSet set2(set);
    set2.add (rangeElem, True);
    tryError (set2.setOrArray());       // no start in rangeElem
  }
}

int main()
{
  try {
    doSetBool();
    doSetInt();
    doSetDouble();
    doSetDComplex();
    doSetString();
    doSetDate();
    doIPosition();
    doSlicer();
    doEmpty();
    doErrors();
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
