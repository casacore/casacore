//# tExprNode.cc: Test program for the selection classes
//# Copyright (C) 2001
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

#include <aips/Containers/Record.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/RecordExpr.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Utilities/Assert.h>
#include <aips/iostream.h>

// <summary>
// Test program for class TableExprNode.
// </summary>

Bool foundError = False;


void checkScaBool (TableExprId& exprid, const TableExprNode& expr,
		   const Bool& value)
{
  AlwaysAssertExit (expr.dataType() == TpBool);
  Bool val;
  expr.get (exprid, val);
  if (val != value) {
    foundError = True;
    cout << "Found value " << val << "; expected " << value << endl;
  }
}

void checkScaDouble (TableExprId& exprid, const TableExprNode& expr,
		     const Double& value)
{
  AlwaysAssertExit (expr.dataType() == TpDouble);
  Double val;
  expr.get (exprid, val);
  if (!near (val,  value, 1.e-10)) {
    foundError = True;
    cout << "Found value " << val << "; expected " << value << endl;
  }
}

void checkScaDComplex (TableExprId& exprid, const TableExprNode& expr,
		       const DComplex& value)
{
  AlwaysAssertExit (expr.dataType() == TpDComplex);
  DComplex val;
  expr.get (exprid, val);
  if (!near (val,  value, 1.e-10)) {
    foundError = True;
    cout << "Found value " << val << "; expected " << value << endl;
  }
}

void checkScaString (TableExprId& exprid, const TableExprNode& expr,
		     const String& value)
{
  AlwaysAssertExit (expr.dataType() == TpString);
  String val;
  expr.get (exprid, val);
  if (val != value) {
    foundError = True;
    cout << "Found value " << val << "; expected " << value << endl;
  }
}

void checkArrBool (TableExprId& exprid, const TableExprNode& expr,
		   const Array<Bool>& value)
{
  AlwaysAssertExit (expr.dataType() == TpBool);
  Array<Bool> val;
  expr.get (exprid, val);
  if (! allEQ (val, value)) {
    foundError = True;
    cout << "Found value " << val << "; expected " << value << endl;
  }
}

void checkArrDouble (TableExprId& exprid, const TableExprNode& expr,
		     const Array<Double>& value)
{
  AlwaysAssertExit (expr.dataType() == TpDouble);
  Array<Double> val;
  expr.get (exprid, val);
  if (! allNear (val, value, 1.e-10)) {
    foundError = True;
    cout << "Found value " << val << "; expected " << value << endl;
  }
}

void checkArrDComplex (TableExprId& exprid, const TableExprNode& expr,
		       const Array<DComplex>& value)
{
  AlwaysAssertExit (expr.dataType() == TpDComplex);
  Array<DComplex> val;
  expr.get (exprid, val);
  if (! allNear (val, value, 1.e-10)) {
    foundError = True;
    cout << "Found value " << val << "; expected " << value << endl;
  }
}

void checkArrString (TableExprId& exprid, const TableExprNode& expr,
		     const Array<String>& value)
{
  AlwaysAssertExit (expr.dataType() == TpString);
  Array<String> val;
  expr.get (exprid, val);
  if (! allEQ (val, value)) {
    foundError = True;
    cout << "Found value " << val << "; expected " << value << endl;
  }
}


void doIt()
{
  // Define various arrays for the record.
  // Define arrays for various data types with the same values,
  // so they can be used when checking results.
  IPosition shp(2,4,5);
  Matrix<Bool> arrb1(shp);
  arrb1 = True;
  arrb1(2,3) = False;
  Matrix<Bool> arrb2(shp);
  arrb2 = True;
  arrb2(2,3) = False;
  arrb2(1,4) = False;
  Matrix<Int>  arri1(shp);
  Matrix<Double> arrid1(shp);
  Matrix<DComplex> arriz1(shp);
  indgen (arri1);
  indgen (arrid1);
  indgen (arriz1);
  Matrix<Int>  arri2(shp);
  Matrix<Double> arrid2(shp);
  Matrix<DComplex> arriz2(shp);
  indgen (arri2, 100, 2);
  indgen (arrid2, 100., 2.);
  indgen (arriz2, DComplex(100.,0.), DComplex(2.,0.));
  Matrix<Double> arrd1(shp);
  Matrix<DComplex> arrdz1(shp);
  indgen (arrd1, 200., 2.);
  indgen (arrdz1, DComplex(200.,0.), DComplex(2.,0.));
  Matrix<Double> arrd2(shp);
  Matrix<DComplex> arrdz2(shp);
  indgen (arrd2, 300., 2.);
  indgen (arrdz2, DComplex(300.,0.), DComplex(2.,0.));
  Matrix<Complex> arrc1(shp);
  Matrix<DComplex> arrcz1(shp);
  indgen (arrc1, Complex(1,2), Complex(1,1));
  indgen (arrcz1, DComplex(1,2), DComplex(1,1));
  Matrix<Complex> arrc2(shp);
  Matrix<DComplex> arrcz2(shp);
  indgen (arrc2, Complex(100,200), Complex(2,2));
  indgen (arrcz2, DComplex(100,200), DComplex(2,2));
  Matrix<DComplex> arrz1(shp);
  indgen (arrz1, DComplex(200,300), DComplex(2,2));
  Matrix<DComplex> arrz2(shp);
  indgen (arrz2, DComplex(300,400), DComplex(2,2));
  Vector<String> arrs1 = stringToVector("a1 a12 a123 a1234 a12345");
  Vector<String> arrs2 = stringToVector("s1 s12 s123 s1234 s12345");
  // Do the same for scalars.
  Bool sb1 = True;
  Bool sb2 = False;
  Int si1 = 1;
  Double sid1 = si1;
  DComplex siz1 = sid1;
  Int si2 = 2;
  Double sid2 = si2;
  DComplex siz2 = sid2;
  Double sd1 = 3;
  DComplex sdz1(sd1);
  Double sd2 = -4;
  DComplex sdz2(sd2);
  Complex sc1(1,2);
  DComplex scz1(sc1);
  Complex sc2(3,4);
  DComplex scz2(sc2);
  DComplex sz1(4,5);
  DComplex sz2(5,6);
  String ss1 ("ss1");
  String ss2 ("ss2");
  // Define all kind of fields in a record.
  // These fields are used for the selection.
  Record rec;
  rec.define ("sb1", sb1);
  rec.define ("sb2", sb2);
  rec.define ("si1", si1);
  rec.define ("si2", si2);
  rec.define ("sd1", sd1);
  rec.define ("sd2", sd2);
  rec.define ("sc1", sc1);
  rec.define ("sc2", sc2);
  rec.define ("sz1", sz1);
  rec.define ("sz2", sz2);
  rec.define ("ss1", ss1);
  rec.define ("ss2", ss2);
  rec.define ("arrb1", arrb1);
  rec.define ("arrb2", arrb2);
  rec.define ("arri1", arri1);
  rec.define ("arri2", arri2);
  rec.define ("arrd1", arrd1);
  rec.define ("arrd2", arrd2);
  rec.define ("arrc1", arrc1);
  rec.define ("arrc2", arrc2);
  rec.define ("arrz1", arrz1);
  rec.define ("arrz2", arrz2);
  rec.define ("arrs1", arrs1);
  rec.define ("arrs2", arrs2);
  // Now form expression nodes from the record fields.
  TableExprNode esb1 = makeRecordExpr (rec, "sb1");
  TableExprNode esb2 = makeRecordExpr (rec, "sb2");
  TableExprNode esi1 = makeRecordExpr (rec, "si1");
  TableExprNode esi2 = makeRecordExpr (rec, "si2");
  TableExprNode esd1 = makeRecordExpr (rec, "sd1");
  TableExprNode esd2 = makeRecordExpr (rec, "sd2");
  TableExprNode esc1 = makeRecordExpr (rec, "sc1");
  TableExprNode esc2 = makeRecordExpr (rec, "sc2");
  TableExprNode esz1 = makeRecordExpr (rec, "sz1");
  TableExprNode esz2 = makeRecordExpr (rec, "sz2");
  TableExprNode ess1 = makeRecordExpr (rec, "ss1");
  TableExprNode ess2 = makeRecordExpr (rec, "ss2");
  TableExprNode earrb1 = makeRecordExpr (rec, "arrb1");
  TableExprNode earrb2 = makeRecordExpr (rec, "arrb2");
  TableExprNode earri1 = makeRecordExpr (rec, "arri1");
  TableExprNode earri2 = makeRecordExpr (rec, "arri2");
  TableExprNode earrd1 = makeRecordExpr (rec, "arrd1");
  TableExprNode earrd2 = makeRecordExpr (rec, "arrd2");
  TableExprNode earrc1 = makeRecordExpr (rec, "arrc1");
  TableExprNode earrc2 = makeRecordExpr (rec, "arrc2");
  TableExprNode earrz1 = makeRecordExpr (rec, "arrz1");
  TableExprNode earrz2 = makeRecordExpr (rec, "arrz2");
  TableExprNode earrs1 = makeRecordExpr (rec, "arrs1");
  TableExprNode earrs2 = makeRecordExpr (rec, "arrs2");

  // Use the record as the expression id.
  TableExprId exprid(rec);

  // Check if getting the data is correct.
  checkScaBool (exprid, esb1, sb1);
  checkScaBool (exprid, esb2, sb2);
  checkScaDouble (exprid, esi1, sid1);
  checkScaDouble (exprid, esi2, sid2);
  checkScaDouble (exprid, esd1, sd1);
  checkScaDouble (exprid, esd2, sd2);
  checkScaDComplex (exprid, esz1, sz1);
  checkScaDComplex (exprid, esz2, sz2);
  checkScaDComplex (exprid, esc1, scz1);
  checkScaDComplex (exprid, esc2, scz2);
  checkScaString (exprid, ess1, ss1);
  checkScaString (exprid, ess2, ss2);
  checkArrBool (exprid, earrb1, arrb1);
  checkArrBool (exprid, earrb2, arrb2);
  checkArrDouble (exprid, earri1, arrid1);
  checkArrDouble (exprid, earri2, arrid2);
  checkArrDouble (exprid, earrd1, arrd1);
  checkArrDouble (exprid, earrd2, arrd2);
  checkArrDComplex (exprid, earrz1, arrz1);
  checkArrDComplex (exprid, earrz2, arrz2);
  checkArrDComplex (exprid, earrc1, arrcz1);
  checkArrDComplex (exprid, earrc2, arrcz2);
  checkArrString (exprid, earrs1, arrs1);
  checkArrString (exprid, earrs2, arrs2);

  // Check the logical operators.
  // Form combinations of scalars and arrays (and constants).
  checkScaBool (exprid, esb1||esb2, sb1||sb2);
  checkScaBool (exprid, esb1&&esb2, sb1&&sb2);
  checkScaBool (exprid, esb1&&True, sb1&&True);
  checkScaBool (exprid, !esb1, !sb1);
  checkScaBool (exprid, !esb2, !sb2);
  checkArrBool (exprid, earrb1||earrb2, arrb1||arrb2);
  checkArrBool (exprid, earrb1||esb2, arrb1||sb2);
  checkArrBool (exprid, esb1||earrb2, sb1||arrb2);
  checkArrBool (exprid, earrb1&&earrb2, arrb1&&arrb2);
  checkArrBool (exprid, earrb1&&esb2, arrb1&&sb2);
  checkArrBool (exprid, esb1&&earrb2, sb1&&arrb2);
  checkArrBool (exprid, !earrb2, !arrb2);

  // Check the unary mathematical operators.
  // Form combinations of scalars and arrays.
  checkScaDouble (exprid, +esi1, sid1);
  checkScaDouble (exprid, -esi1, -sid1);
  checkScaDouble (exprid, +esd1, sd1);
  checkScaDouble (exprid, -esd1, -sd1);
  checkArrDouble (exprid, +earri1, arrid1);
  checkArrDouble (exprid, -earri1, -arrid1);
  checkArrDouble (exprid, +earrd1, arrd1);
  checkArrDouble (exprid, -earrd1, -arrd1);
  checkScaDComplex (exprid, +esc1, scz1);
  checkScaDComplex (exprid, -esc1, -scz1);
  checkScaDComplex (exprid, +esz1, sz1);
  checkScaDComplex (exprid, -esz1, -sz1);
  checkArrDComplex (exprid, +earrc1, arrcz1);
  checkArrDComplex (exprid, -earrc1, -arrcz1);
  checkArrDComplex (exprid, +earrz1, arrz1);
  checkArrDComplex (exprid, -earrz1, -arrz1);

  // Check the binary arithmetic operators.
  // Form combinations of different data types.
  // Form combinations of scalars and arrays (and constants).
  checkScaDouble (exprid, esd1+esi2, sd1+sid2);
  checkScaDouble (exprid, esi1-2, sid1-2.);
  checkScaDouble (exprid, esi1*esi2, sid1*sid2);
  checkScaDouble (exprid, esd1/esd2, sd1/sd2);
  checkScaDouble (exprid, esd1%esd2, fmod(sd1,sd2));
  checkScaDouble (exprid, esd1^esd2, pow(sd1,sd2));
  checkArrDouble (exprid, earrd1+earri2, arrd1+arrid2);
  checkArrDouble (exprid, earrd1+20, arrd1+20.);
  checkArrDouble (exprid, esd1+earri2, sd1+arrid2);
  checkArrDouble (exprid, earri1-earrd2, arrid1-arrd2);
  checkArrDouble (exprid, earri1-1, arrid1-1.);
  checkArrDouble (exprid, 12-earrd2, 12.-arrd2);
  checkArrDouble (exprid, earri1*earri2, arrid1*arrid2);
  checkArrDouble (exprid, earri1*esi2, arrid1*sid2);
  checkArrDouble (exprid, esd1*earri2, sd1*arrid2);
  checkArrDouble (exprid, earrd1/earrd2, arrd1/arrd2);
  checkArrDouble (exprid, earrd1/esd2, arrd1/sd2);
  checkArrDouble (exprid, esd1/earrd2, sd1/arrd2);
  checkArrDouble (exprid, earrd1%earrd2, fmod(arrd1,arrd2));
  checkArrDouble (exprid, earrd1%esd2, fmod(arrd1,sd2));
  checkArrDouble (exprid, esd1%earrd2, fmod(sd1,arrd2));
  checkArrDouble (exprid, earrd1^earrd2, pow(arrd1,arrd2));
  checkArrDouble (exprid, earrd1^esd2, pow(arrd1,sd2));
  checkArrDouble (exprid, esd1^earrd2, pow(sd1,arrd2));
  checkScaDComplex (exprid, esc1+esi2, scz1+siz2);
  checkScaDComplex (exprid, esi1-esz2, siz1-sz2);
  checkScaDComplex (exprid, esd1*esc2, sdz1*scz2);
  checkScaDComplex (exprid, esz1/esc2, sz1/scz2);
  checkScaDComplex (exprid, esz1^esz2, pow(sz1,sz2));
  checkArrDComplex (exprid, earrz1+earri2, arrz1+arriz2);
  checkArrDComplex (exprid, earrz1+Complex(10,20), arrz1+DComplex(10,20));
  checkArrDComplex (exprid, esz1+earri2, sz1+arriz2);
  checkArrDComplex (exprid, earri1-earrc2, arriz1-arrcz2);
  checkArrDComplex (exprid, earri1-esc2, arriz1-scz2);
  checkArrDComplex (exprid, 100-earrc2, DComplex(100,0)-arrcz2);
  checkArrDComplex (exprid, earrz1*earrz2, arrz1*arrz2);
  checkArrDComplex (exprid, earrz1*esz2, arrz1*sz2);
  checkArrDComplex (exprid, esz1*earrz2, sz1*arrz2);
  checkArrDComplex (exprid, earrd1/earrc2, arrdz1/arrcz2);
  checkArrDComplex (exprid, earrd1/esc2, arrdz1/scz2);
  checkArrDComplex (exprid, esd1/earrc2, sdz1/arrcz2);

  // Check the comparison operators.
  // Form combinations of different data types.
  // Form combinations of scalars and arrays (and constants).
  checkScaBool (exprid, esb1==esb2, sb1==sb2);
  checkScaBool (exprid, esb1!=esb2, sb1!=sb2);
  checkArrBool (exprid, earrb1==earrb2, arrb1==arrb2);
  checkArrBool (exprid, earrb1==True, arrb1==True);
  checkArrBool (exprid, esb1==earrb2, sb1==arrb2);
  checkArrBool (exprid, earrb1!=earrb2, arrb1!=arrb2);
  checkArrBool (exprid, earrb1!=esb2, arrb1!=sb2);
  checkArrBool (exprid, esb1!=earrb2, sb1!=arrb2);
  checkScaBool (exprid, esd1==esi2, sd1==sid2);
  checkScaBool (exprid, esi1>2, sid1>2.);
  checkScaBool (exprid, esi1>=esi2, sid1>=sid2);
  checkScaBool (exprid, esd1<esd2, sd1<sd2);
  checkScaBool (exprid, esd1<=esd1, sd1<=sd1);
  checkScaBool (exprid, esd1!=esd2, sd1!=sd2);
  checkArrBool (exprid, earrd1==earri2, arrd1==arrid2);
  checkArrBool (exprid, earrd1==202, arrd1==202.);
  checkArrBool (exprid, esd1==earri2, sd1==arrid2);
  checkArrBool (exprid, earri1>earrd2, arrid1>arrd2);
  checkArrBool (exprid, earri1>1, arrid1>1.);
  checkArrBool (exprid, 12>earrd2, 12.>arrd2);
  checkArrBool (exprid, earri1>=earri2, arrid1>=arrid2);
  checkArrBool (exprid, earri1>=esi2, arrid1>=sid2);
  checkArrBool (exprid, esd1>=earri2, sid1>=arrid2);
  checkArrBool (exprid, earrd1<earrd2, arrd1<arrd2);
  checkArrBool (exprid, earrd1<esd2, arrd1<sd2);
  checkArrBool (exprid, esd1<earrd2, sd1<arrd2);
  checkArrBool (exprid, earrd1<=earrd2, arrd1<=arrd2);
  checkArrBool (exprid, earrd1<=esd2, arrd1<=sd2);
  checkArrBool (exprid, esd1<=earrd2, sd1<=arrd2);
  checkArrBool (exprid, earrd1!=earrd2, arrd1!=arrd2);
  checkArrBool (exprid, earrd1!=esd2, arrd1!=sd2);
  checkArrBool (exprid, esd1!=earrd2, sd1!=arrd2);
  checkScaBool (exprid, esc1==esi2, scz1==siz2);
  checkScaBool (exprid, esi1>esz2, siz1>sz2);
  checkScaBool (exprid, esd1>=esc2, sdz1>=scz2);
  checkScaBool (exprid, esz1<esc2, sz1<scz2);
  checkScaBool (exprid, esz1<=esz2, sz1<=sz2);
  checkScaBool (exprid, esz1!=esz2, sz1!=sz2);
  checkArrBool (exprid, earrz1==earri2, arrz1==arriz2);
  checkArrBool (exprid, earrz1==Complex(10,20), arrz1==DComplex(10,20));
  checkArrBool (exprid, esz1==earri2, sz1==arriz2);
  checkArrBool (exprid, earri1>earrc2, arriz1>arrcz2);
  checkArrBool (exprid, earri1>esc2, arriz1>scz2);
  checkArrBool (exprid, 100>earrc2, DComplex(100,0)>arrcz2);
  checkArrBool (exprid, earrz1>=earrz2, arrz1>=arrz2);
  checkArrBool (exprid, earrz1>=esz2, arrz1>=sz2);
  checkArrBool (exprid, esz1>=earrz2, sz1>=arrz2);
  checkArrBool (exprid, earrd1<earrc2, arrdz1<arrcz2);
  checkArrBool (exprid, earrd1<esc2, arrdz1<scz2);
  checkArrBool (exprid, esd1<earrc2, sdz1<arrcz2);
  checkArrBool (exprid, earrd1<=earrc2, arrdz1<=arrcz2);
  checkArrBool (exprid, earrd1<=esc2, arrdz1<=scz2);
  checkArrBool (exprid, esd1<=earrc2, sdz1<=arrcz2);
  checkArrBool (exprid, earrd1!=earrc2, arrdz1!=arrcz2);
  checkArrBool (exprid, earrd1!=esc2, arrdz1!=scz2);
  checkArrBool (exprid, esd1!=earrc2, sdz1!=arrcz2);
  checkScaBool (exprid, ess1==ess2, ss1==ss2);
  checkScaBool (exprid, ess1>ess2, ss1>ss2);
  checkScaBool (exprid, ess1>=ess2, ss1>=ss2);
  checkScaBool (exprid, ess1<ess2, ss1<ss2);
  checkScaBool (exprid, ess1<=ess2, ss1<=ss2);
  checkScaBool (exprid, ess1!=ess2, ss1!=ss2);
  checkArrBool (exprid, earrs1==earrs2, arrs1==arrs2);
  checkArrBool (exprid, earrs1=="s12", arrs1==String("s12"));
  checkArrBool (exprid, ess1==earrs2, ss1==arrs2);
  checkArrBool (exprid, earrs1>earrs2, arrs1>arrs2);
  checkArrBool (exprid, earrs1>ess2, arrs1>ss2);
  checkArrBool (exprid, "s123">earrs2, String("s123")>arrs2);
  checkArrBool (exprid, earrs1>=earrs2, arrs1>=arrs2);
  checkArrBool (exprid, earrs1>=ess2, arrs1>=ss2);
  checkArrBool (exprid, ess1>=earrs2, ss1>=arrs2);
  checkArrBool (exprid, earrs1<earrs2, arrs1<arrs2);
  checkArrBool (exprid, earrs1<ess2, arrs1<ss2);
  checkArrBool (exprid, ess1<earrs2, ss1<arrs2);
  checkArrBool (exprid, earrs1<=earrs2, arrs1<=arrs2);
  checkArrBool (exprid, earrs1<=ess2, arrs1<=ss2);
  checkArrBool (exprid, ess1<=earrs2, ss1<=arrs2);
  checkArrBool (exprid, earrs1!=earrs2, arrs1!=arrs2);
  checkArrBool (exprid, earrs1!=ess2, arrs1!=ss2);
  checkArrBool (exprid, ess1!=earrs2, ss1!=arrs2);

  // Test the double near functions.
  checkScaBool (exprid, near(esd1,sd1), near(sd1,sd1));
  checkScaBool (exprid, nearAbs(esd1,sd1), nearAbs(sd1,sd1));
  checkScaBool (exprid, near(esd1,esd2,1.e-7), near(sd1,sd2,1.e-7));
  checkScaBool (exprid, nearAbs(esd1,esd2,1.e-7), nearAbs(sd1,sd2,1.e-7));
  checkScaBool (exprid, near(esd1,esd1-1.e-10,1.e-7),
		near(sd1,sd1-1.e-10,1.e-7));
  checkScaBool (exprid, nearAbs(esd1,esd1-1.e-10,1.e-7),
		nearAbs(sd1,sd1-1.e-10,1.e-7));
  checkScaBool (exprid, near(esd1,esd1-1.e-4,1.e-7),
		near(sd1,sd1-1.e-4,1.e-7));
  checkScaBool (exprid, nearAbs(esd1,esd1-1.e-4,1.e-7),
		nearAbs(sd1,sd1-1.e-4,1.e-7));
  checkArrBool (exprid, near(earrd1,arrd1), near(arrd1,arrd1,1.e-13));
  checkArrBool (exprid, nearAbs(earrd1,arrd1), nearAbs(arrd1,arrd1,1.e-13));
  checkArrBool (exprid, near(earrd1,earrd2,1.e-7), near(arrd1,arrd2,1.e-7));
  checkArrBool (exprid, nearAbs(earrd1,earrd2,1.e-7),
		nearAbs(arrd1,arrd2,1.e-7));
  Array<Double> tmparrd(shp);
  tmparrd = sd1;
  checkArrBool (exprid, near(earrd1,sd1), near(arrd1,tmparrd,1.e-13));
  checkArrBool (exprid, nearAbs(earrd1,sd1), nearAbs(arrd1,tmparrd,1.e-13));
  tmparrd = sd2;
  checkArrBool (exprid, near(earrd1,esd2,1.e-7), near(tmparrd,arrd1,1.e-7));
  checkArrBool (exprid, nearAbs(earrd1,esd2,1.e-7),
		nearAbs(tmparrd,arrd1,1.e-7));
  tmparrd = sd1;
  checkArrBool (exprid, near(sd1,earrd1), near(arrd1,tmparrd,1.e-13));
  checkArrBool (exprid, nearAbs(sd1,earrd1), nearAbs(arrd1,tmparrd,1.e-13));
  tmparrd = sd2;
  checkArrBool (exprid, near(esd2,earrd1,1.e-7), near(tmparrd,arrd1,1.e-7));
  checkArrBool (exprid, nearAbs(esd2,earrd1,1.e-7),
		nearAbs(tmparrd,arrd1,1.e-7));
  // Test the complex near functions.
  checkScaBool (exprid, near(esz1,sz1), near(sz1,sz1));
  checkScaBool (exprid, nearAbs(esz1,sz1), nearAbs(sz1,sz1));
  checkScaBool (exprid, near(esz1,esz2,1.e-7), near(sz1,sz2,1.e-7));
  checkScaBool (exprid, nearAbs(esz1,esz2,1.e-7), nearAbs(sz1,sz2,1.e-7));
  checkScaBool (exprid, near(esz1,esz1-1.e-10,1.e-7),
		near(sz1,sz1-1.e-10,1.e-7));
  checkScaBool (exprid, nearAbs(esz1,esz1-1.e-10,1.e-7),
		nearAbs(sz1,sz1-1.e-10,1.e-7));
  checkScaBool (exprid, near(esz1,esz1-1.e-4,1.e-7),
		near(sz1,sz1-1.e-4,1.e-7));
  checkScaBool (exprid, nearAbs(esz1,esz1-1.e-4,1.e-7),
		nearAbs(sz1,sz1-1.e-4,1.e-7));
  checkArrBool (exprid, near(earrz1,arrz1), near(arrz1,arrz1,1.e-13));
  checkArrBool (exprid, nearAbs(earrz1,arrz1), nearAbs(arrz1,arrz1,1.e-13));
  checkArrBool (exprid, near(earrz1,earrz2,1.e-7), near(arrz1,arrz2,1.e-7));
  checkArrBool (exprid, nearAbs(earrz1,earrz2,1.e-7),
		nearAbs(arrz1,arrz2,1.e-7));
  Array<DComplex> tmparrz(shp);
  tmparrz = sz1;
  checkArrBool (exprid, near(earrz1,sz1), near(arrz1,tmparrz,1.e-13));
  checkArrBool (exprid, nearAbs(earrz1,sz1), nearAbs(arrz1,tmparrz,1.e-13));
  tmparrz = sz2;
  checkArrBool (exprid, near(earrz1,esz2,1.e-7), near(tmparrz,arrz1,1.e-7));
  checkArrBool (exprid, nearAbs(earrz1,esz2,1.e-7),
		nearAbs(tmparrz,arrz1,1.e-7));
  tmparrz = sz1;
  checkArrBool (exprid, near(sz1,earrz1), near(arrz1,tmparrz,1.e-13));
  checkArrBool (exprid, nearAbs(sz1,earrz1), nearAbs(arrz1,tmparrz,1.e-13));
  tmparrz = sz2;
  checkArrBool (exprid, near(esz2,earrz1,1.e-7), near(tmparrz,arrz1,1.e-7));
  checkArrBool (exprid, nearAbs(esz2,earrz1,1.e-7),
		nearAbs(tmparrz,arrz1,1.e-7));
  // Test the mixed double,complex near functions.
  checkScaBool (exprid, near(esz1,sd1), near(sz1,sdz1));
  checkScaBool (exprid, near(esd2,esz1), near(sdz2,sz1));
  tmparrz = sdz2;
  checkArrBool (exprid, near(esd2,arrz1), near(tmparrz,arrz1,1.e-13));

  // Test various functions.
  // They are done in order of their definition in ExprFuncNode.h.
  // First the ones taking one Double or DComplex argument.
  checkScaDouble (exprid, sin(esd1), sin(sd1));
  checkArrDouble (exprid, sin(earrd1), sin(arrd1));
  checkScaDComplex (exprid, sin(esz1), sin(sz1));
  checkArrDComplex (exprid, sin(earrz1), sin(arrz1));
  checkScaDouble (exprid, sinh(esd1), sinh(sd1));
  checkArrDouble (exprid, sinh(earrd1), sinh(arrd1));
  checkScaDComplex (exprid, sinh(esz1), sinh(sz1));
  checkArrDComplex (exprid, sinh(earrz1), sinh(arrz1));
  checkScaDouble (exprid, cos(esd1), cos(sd1));
  checkArrDouble (exprid, cos(earrd1), cos(arrd1));
  checkScaDComplex (exprid, cos(esz1), cos(sz1));
  checkArrDComplex (exprid, cos(earrz1), cos(arrz1));
  checkScaDouble (exprid, cosh(esd1), cosh(sd1));
  checkArrDouble (exprid, cosh(earrd1), cosh(arrd1));
  checkScaDComplex (exprid, cosh(esz1), cosh(sz1));
  checkArrDComplex (exprid, cosh(earrz1), cosh(arrz1));
  checkScaDouble (exprid, exp(esd1), exp(sd1));
  checkArrDouble (exprid, exp(earrd1), exp(arrd1));
  checkScaDComplex (exprid, exp(esz1), exp(sz1));
  checkArrDComplex (exprid, exp(earrz1), exp(arrz1));
  checkScaDouble (exprid, log(esd1), log(sd1));
  checkArrDouble (exprid, log(earrd1), log(arrd1));
  checkScaDComplex (exprid, log(esz1), log(sz1));
  checkArrDComplex (exprid, log(earrz1), log(arrz1));
  checkScaDouble (exprid, log10(esd1), log10(sd1));
  checkArrDouble (exprid, log10(earrd1), log10(arrd1));
  checkScaDComplex (exprid, log10(esz1), log10(sz1));
  checkArrDComplex (exprid, log10(earrz1), log10(arrz1));
  checkScaDouble (exprid, square(esd1), square(sd1));
  checkArrDouble (exprid, square(earrd1), square(arrd1));
  checkScaDComplex (exprid, square(esz1), sz1*sz1);
  checkArrDComplex (exprid, square(earrz1), square(arrz1));
  checkScaDouble (exprid, sqrt(esd1), sqrt(sd1));
  checkArrDouble (exprid, sqrt(earrd1), sqrt(arrd1));
  checkScaDComplex (exprid, sqrt(esz1), sqrt(sz1));
  checkArrDComplex (exprid, sqrt(earrz1), sqrt(arrz1));
  checkScaDouble (exprid, conj(esd1), sd1);
  checkArrDouble (exprid, conj(earrd1), arrd1);
  checkScaDComplex (exprid, conj(esz1), conj(sz1));
  checkArrDComplex (exprid, conj(earrz1), conj(arrz1));

  // Test the iif functions in various ways.
  checkScaDouble (exprid, iif(True,esd1,esd2), sd1);
}


int main()
{
  try {
    doIt();
  } catch (AipsError x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    exit(1);
  } catch (...) {
    cout << "Unexpected unknown exception" << endl;
    exit(1);
  }
  if (foundError) {
    exit(1);
  }
  cout << "OK" << endl;
  exit(0);
}
