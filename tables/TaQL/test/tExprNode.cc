//# tExprNode.cc: Test program for the selection classes
//# Copyright (C) 2001,2003
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

#include <casacore/casa/Containers/Record.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/RecordExpr.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for class TableExprNode.
// </summary>

Bool foundError = False;


void checkScaBool (const String& str, TableExprId& exprid,
		   const TableExprNode& expr,
		   const Bool& value)
{
  cout << "checkScaBool " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpBool);
  Bool val;
  expr.get (exprid, val);
  if (val != value) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkScaInt (const String& str, TableExprId& exprid,
                  const TableExprNode& expr,
                  const Int& value)
{
  cout << "checkScaInt " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpInt);
  Int64 val;
  expr.get (exprid, val);
  if (val != value) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkScaDouble (const String& str, TableExprId& exprid,
		     const TableExprNode& expr,
		     const Double& value)
{
  cout << "checkScaDouble " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpDouble);
  Double val;
  expr.get (exprid, val);
  if (!near (val,  value, 1.e-10)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkScaDComplex (const String& str, TableExprId& exprid,
		       const TableExprNode& expr,
		       const DComplex& value)
{
  cout << "checkScaDComplex " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpDComplex);
  DComplex val;
  expr.get (exprid, val);
  if (!near (val,  value, 1.e-10)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkScaString (const String& str, TableExprId& exprid,
		     const TableExprNode& expr,
		     const String& value)
{
  cout << "checkScaString " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpString);
  String val;
  expr.get (exprid, val);
  if (val != value) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkScaDate (const String& str, TableExprId& exprid,
                   const TableExprNode& expr,
                   const MVTime& value)
{
  cout << "checkScaDate " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpOther);
  MVTime val;
  expr.get (exprid, val);
  if (!near (Double(val), Double(value), 1.e-10)) {
    foundError = True;
    cout << str << ": found value " << Double(val) << "; expected "
         << Double(value) << endl;
  }
}

void checkArrBool (const String& str, TableExprId& exprid,
		   const TableExprNode& expr,
		   const Array<Bool>& value)
{
  cout << "checkArrBool " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpBool);
  Array<Bool> val;
  expr.get (exprid, val);
  if (! allEQ (val, value)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkArrInt (const String& str, TableExprId& exprid,
                  const TableExprNode& expr,
                  const Array<Int>& value)
{
  cout << "checkArrInt " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpInt);
  Array<Int64> val64;
  expr.get (exprid, val64);
  Array<Int> val(val64.shape());
  convertArray (val, val64);
  if (! allEQ (val, value)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkArrDouble (const String& str, TableExprId& exprid,
		     const TableExprNode& expr,
		     const Array<Double>& value)
{
  cout << "checkArrDouble " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpDouble);
  Array<Double> val;
  expr.get (exprid, val);
  if (! allNear (val, value, 1.e-10)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkArrDComplex (const String& str, TableExprId& exprid,
		       const TableExprNode& expr,
		       const Array<DComplex>& value)
{
  cout << "checkArrDComplex " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpDComplex);
  Array<DComplex> val;
  expr.get (exprid, val);
  if (! allNear (val, value, 1.e-10)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkArrString (const String& str, TableExprId& exprid,
		     const TableExprNode& expr,
		     const Array<String>& value)
{
  cout << "checkArrString " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpString);
  Array<String> val;
  expr.get (exprid, val);
  if (! allEQ (val, value)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

#define checkFailure(STR,EXPR)\
{\
  bool failed = False;\
  try {\
    TableExprNode n(EXPR);\
  } catch (std::exception&) {\
    failed = True;\
  }\
  if (!failed) {\
    cout << STR << ": was expected to fail, but did not" << endl;\
  }\
}


void doIt()
{
  // Define various arrays for the record.
  // Define arrays for various data types with the same values,
  // so they can be used when checking results.
  IPosition shp(2,4,5);
  Vector<Int> shpVec(2);
  convertArray (shpVec, shp.asVector());
  Matrix<Bool> arrb1(shp);
  arrb1 = True;
  arrb1(2,3) = False;
  Matrix<Bool> arrb2(shp);
  arrb2 = True;
  arrb2(2,3) = False;
  arrb2(1,4) = False;
  Matrix<Int> arrbi(shp);
  arrbi=0;
  arrbi(2,3) = 1;
  Matrix<Double> arrbd(shp);
  arrbd=1.;
  arrbd(2,3)=0.;
  Matrix<Int>  arri1(shp);
  Matrix<Double> arrid1(shp);
  Matrix<DComplex> arriz1(shp);
  indgen (arri1, -1);
  indgen (arrid1, -1.);
  indgen (arriz1, DComplex(-1.,0));
  Matrix<Int> arrisign(shp);
  Matrix<Double> arridsign(shp);
  Matrix<Double> arridarg(shp);
  arrisign = 1;
  arrisign.data()[0] = -1;
  convertArray (arridarg, arrisign);
  arridarg = (arridarg-1.)*C::pi/-2.;
  arrisign.data()[1] = 0;
  convertArray (arridsign, arrisign);
  Matrix<Int>  arri2(shp);
  Matrix<Double> arrid2(shp);
  Matrix<DComplex> arriz2(shp);
  indgen (arri2, 100, 2);
  indgen (arrid2, 100., 2.);
  indgen (arriz2, DComplex(100.,0.), DComplex(2.,0.));
  Matrix<Double> arrd1(shp);
  Matrix<DComplex> arrdz1(shp);
  Matrix<Double> arrdsign(shp);
  Matrix<Int> arrdi1(shp);
  indgen (arrd1, -222.1, 20.);
  indgen (arrdz1, DComplex(-222.1,0.), DComplex(20.,0.));
  arrdsign = 1.;
  arrdsign(IPosition(2,0,0),IPosition(2,3,2)) = -1.;
  convertArray (arrdi1, arrd1);
  Matrix<Double> arrdzero(shp);
  arrdzero = 0.;
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
  indgen (arrz1, DComplex(-222,-333), DComplex(20,30));
  Matrix<DComplex> arrz2(shp);
  indgen (arrz2, DComplex(300,400), DComplex(2,2));
  Vector<String> arrs1 = stringToVector("a1, a12, a123, a1234");
  AlwaysAssertExit (arrs1.size() == 4);
  Vector<String> arrs2 = stringToVector("s1, s12, s123, s1234");
  Matrix<String> mats1(shp);
  for (Int i=0; i<shp[1]; ++i) {
    mats1[i] = arrs1;
  }
  // Do the same for scalars.
  Bool sb1 = True;
  Bool sb2 = False;
  Int si1 = 1;
  Double sid1 = si1;
  DComplex siz1 = sid1;
  Int si2 = 2;
  Double sid2 = si2;
  DComplex siz2 = sid2;
  Double sd1 = 3.1;
  Array<Double> sd1arr(shp);
  sd1arr = sd1;
  DComplex sdz1(sd1);
  Array<DComplex> sdz1arr(shp);
  sdz1arr = sdz1;
  Double sd2 = -4;
  Array<Double> sd2arr(shp);
  sd2arr = sd2;
  DComplex sdz2(sd2);
  Array<DComplex> sdz2arr(shp);
  sdz2arr = sdz2;
  Complex sc1(1,2);
  DComplex scz1(sc1);
  Complex sc2(3,4);
  DComplex scz2(sc2);
  DComplex sz1(4,5);
  DComplex sz2(5,6);
  Array<DComplex> sz1arr(shp);
  Array<DComplex> sz2arr(shp);
  sz1arr = sz1;
  sz2arr = sz2;
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
  checkScaBool ("sb1", exprid, esb1, sb1);
  checkScaBool ("sb2", exprid, esb2, sb2);
  checkScaInt ("si1", exprid, esi1, si1);
  checkScaInt ("si2", exprid, esi2, si2);
  checkScaDouble ("sd1", exprid, esd1, sd1);
  checkScaDouble ("sd2", exprid, esd2, sd2);
  checkScaDComplex ("sz1", exprid, esz1, sz1);
  checkScaDComplex ("sz2", exprid, esz2, sz2);
  checkScaDComplex ("sc1", exprid, esc1, scz1);
  checkScaDComplex ("sc2", exprid, esc2, scz2);
  checkScaString ("ss1", exprid, ess1, ss1);
  checkScaString ("ss2", exprid, ess2, ss2);
  checkArrBool ("ab1", exprid, earrb1, arrb1);
  checkArrBool ("ab2", exprid, earrb2, arrb2);
  checkArrInt ("ai1", exprid, earri1, arri1);
  checkArrInt ("ai2", exprid, earri2, arri2);
  checkArrDouble ("ad1", exprid, earrd1, arrd1);
  checkArrDouble ("ad2", exprid, earrd2, arrd2);
  checkArrDComplex ("az1", exprid, earrz1, arrz1);
  checkArrDComplex ("az2", exprid, earrz2, arrz2);
  checkArrDComplex ("ac1", exprid, earrc1, arrcz1);
  checkArrDComplex ("ac2", exprid, earrc2, arrcz2);
  checkArrString ("as1", exprid, earrs1, arrs1);
  checkArrString ("as2", exprid, earrs2, arrs2);

  // Check the logical operators.
  // Form combinations of scalars and arrays (and constants).
  checkScaBool ("|| sb-sb", exprid, esb1||esb2, sb1||sb2);
  checkScaBool ("&& sb-sb", exprid, esb1&&esb2, sb1&&sb2);
  checkScaBool ("&& sb", exprid, esb1&&True, sb1&&True);
  checkScaBool ("! sb1", exprid, !esb1, !sb1);
  checkScaBool ("! sb2", exprid, !esb2, !sb2);
  checkArrBool ("|| ab-ab", exprid, earrb1||earrb2, arrb1||arrb2);
  checkArrBool ("|| sb-ab", exprid, earrb1||esb2, arrb1||sb2);
  checkArrBool ("|| ab-sb", exprid, esb1||earrb2, sb1||arrb2);
  checkArrBool ("&& ab-ab", exprid, earrb1&&earrb2, arrb1&&arrb2);
  checkArrBool ("&& sb-ab", exprid, earrb1&&esb2, arrb1&&sb2);
  checkArrBool ("&& ab-sb", exprid, esb1&&earrb2, sb1&&arrb2);
  checkArrBool ("! ab", exprid, !earrb2, !arrb2);

  // Check the unary mathematical operators.
  // Form combinations of scalars and arrays.
  checkScaInt ("+ si", exprid, +esi1, si1);
  checkScaInt ("- si", exprid, -esi1, -si1);
  checkScaInt ("~ si", exprid, ~esi1, ~si1);
  checkScaDouble ("+ sd", exprid, +esd1, sd1);
  checkScaDouble ("- sd", exprid, -esd1, -sd1);
  checkArrInt ("+ ai", exprid, +earri1, arri1);
  checkArrInt ("- ai", exprid, -earri1, -arri1);
  checkArrInt ("~ ai", exprid, ~earri1, ~arri1);
  checkArrDouble ("+ ad", exprid, +earrd1, arrd1);
  checkArrDouble ("- ad", exprid, -earrd1, -arrd1);
  checkScaDComplex ("+ sc", exprid, +esc1, scz1);
  checkScaDComplex ("- sc", exprid, -esc1, -scz1);
  checkScaDComplex ("+ sz", exprid, +esz1, sz1);
  checkScaDComplex ("- sz", exprid, -esz1, -sz1);
  checkArrDComplex ("+ ac", exprid, +earrc1, arrcz1);
  checkArrDComplex ("- ac", exprid, -earrc1, -arrcz1);
  checkArrDComplex ("+ az", exprid, +earrz1, arrz1);
  checkArrDComplex ("- az", exprid, -earrz1, -arrz1);

  // Check the binary arithmetic operators.
  // Form combinations of different data types.
  // Form combinations of scalars and arrays (and constants).
  checkScaInt ("+ si-ci", exprid, esi1-2, si1-2);
  checkScaInt ("* si-si", exprid, esi1*esi2, si1*si2);
  checkScaInt ("& si-si", exprid, esi1&esi2, si1&si2);
  checkScaInt ("| si-si", exprid, esi1|esi2, si1|si2);
  checkScaInt ("^ si-si", exprid, esi1^esi2, si1^si2);
  checkScaInt ("% si-si", exprid, esi1%esi2, si1%si2);
  checkScaDouble ("/ si-si", exprid, esi1/esi2, sid1/si2);
  checkScaDouble ("+ sd-si", exprid, esd1+esi2, sd1+sid2);
  checkScaDouble ("/ sd-sd", exprid, esd1/esd2, sd1/sd2);
  checkScaDouble ("% sd-sd", exprid, esd1%esd2, fmod(sd1,sd2));
  checkScaDouble ("**sd-sd", exprid, pow(esd1,esd2), pow(sd1,sd2));
  checkArrDouble ("+ ad-ad", exprid, earrd1+earri2, arrd1+arrid2);
  checkArrDouble ("+ ad-ci", exprid, earrd1+20, arrd1+20.);
  checkArrDouble ("+ sd-ai", exprid, esd1+earri2, sd1+arrid2);
  checkArrDouble ("- ai-ad", exprid, earri1-earrd2, arrid1-arrd2);
  checkArrInt ("- ai-ci", exprid, earri1-1, arri1-1);
  checkArrInt ("* ai-ai", exprid, earri1*earri2, arri1*arri2);
  checkArrInt ("* ai-si", exprid, earri1*esi2, arri1*si2);
  checkArrInt ("& ai-si", exprid, earri1&esi2, arri1&si2);
  checkArrInt ("| si-ai", exprid, esi2|earri1, arri1|si2);
  checkArrInt ("^ ai-ai", exprid, earri1^earri2, arri1^arri2);
  checkArrInt ("% ai-ai", exprid, earri1%earri2, arri1%arri2);
  checkArrDouble ("/ ai-ai", exprid, earri1/earri2, arrid1/arrid2);
  checkArrDouble ("- ci-ad", exprid, 12-earrd2, 12.-arrd2);
  checkArrDouble ("* sd-ai", exprid, esd1*earri2, sd1*arrid2);
  checkArrDouble ("/ ad-ad", exprid, earrd1/earrd2, arrd1/arrd2);
  checkArrDouble ("/ ad-sd", exprid, earrd1/esd2, arrd1/sd2);
  checkArrDouble ("/ sd-ad", exprid, esd1/earrd2, sd1/arrd2);
  checkArrDouble ("% ad-ad", exprid, earrd1%earrd2, fmod(arrd1,arrd2));
  checkArrDouble ("% ad-sd", exprid, earrd1%esd2, fmod(arrd1,sd2));
  checkArrDouble ("% sd-ad", exprid, esd1%earrd2, fmod(sd1,arrd2));
  checkArrDouble ("**ad-ad", exprid, pow(earrd1,earrd2), pow(arrd1,arrd2));
  checkArrDouble ("**ad-sd", exprid, pow(earrd1,esd2), pow(arrd1,sd2));
  checkArrDouble ("**sd-ad", exprid, pow(esd1,earrd2), pow(sd1,arrd2));
  checkScaDComplex ("+ sc-si", exprid, esc1+esi2, scz1+siz2);
  checkScaDComplex ("- si-sz", exprid, esi1-esz2, siz1-sz2);
  checkScaDComplex ("* sd-sc", exprid, esd1*esc2, sdz1*scz2);
  checkScaDComplex ("/ sz-sc", exprid, esz1/esc2, sz1/scz2);
  checkScaDComplex ("**sz-sz", exprid, pow(esz1,esz2), pow(sz1,sz2));
  checkArrDComplex ("+ az-ai", exprid, earrz1+earri2, arrz1+arriz2);
  checkArrDComplex ("+ az-cc", exprid, earrz1+Complex(10,20),
		    arrz1+DComplex(10,20));
  checkArrDComplex ("+ sz-ai", exprid, esz1+earri2, sz1+arriz2);
  checkArrDComplex ("- ai-ac", exprid, earri1-earrc2, arriz1-arrcz2);
  checkArrDComplex ("- ai-sc", exprid, earri1-esc2, arriz1-scz2);
  checkArrDComplex ("- ci-ac", exprid, 100-earrc2, DComplex(100,0)-arrcz2);
  checkArrDComplex ("* az-az", exprid, earrz1*earrz2, arrz1*arrz2);
  checkArrDComplex ("* az-sz", exprid, earrz1*esz2, arrz1*sz2);
  checkArrDComplex ("* sz-az", exprid, esz1*earrz2, sz1*arrz2);
  checkArrDComplex ("/ ad-ac", exprid, earrd1/earrc2, arrdz1/arrcz2);
  checkArrDComplex ("/ ad-sc", exprid, earrd1/esc2, arrdz1/scz2);
  checkArrDComplex ("/ sd-ac", exprid, esd1/earrc2, sdz1/arrcz2);

  // Check the comparison operators.
  // Form combinations of different data types.
  // Form combinations of scalars and arrays (and constants).
  checkScaBool ("== sb-sb", exprid, esb1==esb2, sb1==sb2);
  checkScaBool ("!= sb-sb", exprid, esb1!=esb2, sb1!=sb2);
  checkArrBool ("== ab-ab", exprid, earrb1==earrb2, arrb1==arrb2);
  checkArrBool ("== ab-t", exprid, earrb1==True, arrb1==True);
  checkArrBool ("== sb-ab", exprid, esb1==earrb2, sb1==arrb2);
  checkArrBool ("!= ab-ab", exprid, earrb1!=earrb2, arrb1!=arrb2);
  checkArrBool ("!= ", exprid, earrb1!=esb2, arrb1!=sb2);
  checkArrBool ("!= sb-ab", exprid, esb1!=earrb2, sb1!=arrb2);
  checkScaBool ("== sd-si", exprid, esd1==esi2, sd1==sid2);
  checkScaBool ("> si-ci", exprid, esi1>2, si1>2);
  checkScaBool (">= si-si", exprid, esi1>=esi2, si1>=si2);
  checkScaBool ("< sd-sd", exprid, esd1<esd2, sd1<sd2);
  checkScaBool ("<= sd-sd", exprid, esd1<=esd1, sd1<=sd1);
  checkScaBool ("!= sd-sd", exprid, esd1!=esd2, sd1!=sd2);
  checkArrBool ("== ad-ai", exprid, earrd1==earri2, arrd1==arrid2);
  checkArrBool ("== ad-si", exprid, earrd1==202, arrd1==202.);
  checkArrBool ("== sd-ai", exprid, esd1==earri2, sd1==arrid2);
  checkArrBool ("> ai-ad", exprid, earri1>earrd2, arrid1>arrd2);
  checkArrBool ("> ai-ci", exprid, earri1>1, arri1>1);
  checkArrBool ("> ci-ad", exprid, 12>earrd2, 12.>arrd2);
  checkArrBool (">= ai-ai", exprid, earri1>=earri2, arri1>=arri2);
  checkArrBool (">= ai-si", exprid, earri1>=esi2, arri1>=si2);
  checkArrBool (">= sd-ai", exprid, esd1>=earri2, si1>=arri2);
  checkArrBool ("< ad-ad", exprid, earrd1<earrd2, arrd1<arrd2);
  checkArrBool ("< ad-sd", exprid, earrd1<esd2, arrd1<sd2);
  checkArrBool ("< sd-ad", exprid, esd1<earrd2, sd1<arrd2);
  checkArrBool ("<= ad-ad", exprid, earrd1<=earrd2, arrd1<=arrd2);
  checkArrBool ("<= ad-sd", exprid, earrd1<=esd2, arrd1<=sd2);
  checkArrBool ("<= sd-ad", exprid, esd1<=earrd2, sd1<=arrd2);
  checkArrBool ("!= ad-ad", exprid, earrd1!=earrd2, arrd1!=arrd2);
  checkArrBool ("!= ad-sd", exprid, earrd1!=esd2, arrd1!=sd2);
  checkArrBool ("!= sd-ad", exprid, esd1!=earrd2, sd1!=arrd2);
  checkScaBool ("== sc-si", exprid, esc1==esi2, scz1==siz2);
  checkScaBool ("> si-sz", exprid, esi1>esz2, siz1>sz2);
  checkScaBool (">= sd-sc", exprid, esd1>=esc2, sdz1>=scz2);
  checkScaBool ("< sz-sc", exprid, esz1<esc2, sz1<scz2);
  checkScaBool ("<= sz-sz", exprid, esz1<=esz2, sz1<=sz2);
  checkScaBool ("!= sz-sz", exprid, esz1!=esz2, sz1!=sz2);
  checkArrBool ("== az-ai", exprid, earrz1==earri2, arrz1==arriz2);
  checkArrBool ("== az-cc", exprid, earrz1==Complex(10,20),
		arrz1==DComplex(10,20));
  checkArrBool ("== sz-ai", exprid, esz1==earri2, sz1==arriz2);
  checkArrBool ("> ai-ac", exprid, earri1>earrc2, arriz1>arrcz2);
  checkArrBool ("> ai-sc", exprid, earri1>esc2, arriz1>scz2);
  checkArrBool ("> si-ac", exprid, 100>earrc2, DComplex(100,0)>arrcz2);
  checkArrBool (">= az-az", exprid, earrz1>=earrz2, arrz1>=arrz2);
  checkArrBool (">= az-sz", exprid, earrz1>=esz2, arrz1>=sz2);
  checkArrBool (">= sz-az", exprid, esz1>=earrz2, sz1>=arrz2);
  checkArrBool ("< ad-ac", exprid, earrd1<earrc2, arrdz1<arrcz2);
  checkArrBool ("< ad-sc", exprid, earrd1<esc2, arrdz1<scz2);
  checkArrBool ("< sd-ac", exprid, esd1<earrc2, sdz1<arrcz2);
  checkArrBool ("<= ad-ac", exprid, earrd1<=earrc2, arrdz1<=arrcz2);
  checkArrBool ("<= ad-sc", exprid, earrd1<=esc2, arrdz1<=scz2);
  checkArrBool ("<= sd-ac", exprid, esd1<=earrc2, sdz1<=arrcz2);
  checkArrBool ("!= ad-ac", exprid, earrd1!=earrc2, arrdz1!=arrcz2);
  checkArrBool ("!= ad-sc", exprid, earrd1!=esc2, arrdz1!=scz2);
  checkArrBool ("!= sd-ac", exprid, esd1!=earrc2, sdz1!=arrcz2);
  checkScaBool ("== ss-ss", exprid, ess1==ess2, ss1==ss2);
  checkScaBool ("> ss-ss", exprid, ess1>ess2, ss1>ss2);
  checkScaBool (">= ss-ss", exprid, ess1>=ess2, ss1>=ss2);
  checkScaBool ("< ss-ss", exprid, ess1<ess2, ss1<ss2);
  checkScaBool ("<= ss-ss", exprid, ess1<=ess2, ss1<=ss2);
  checkScaBool ("!= ss-ss", exprid, ess1!=ess2, ss1!=ss2);
  checkArrBool ("== as-as", exprid, earrs1==earrs2, arrs1==arrs2);
  checkArrBool ("== as-ss", exprid, earrs1=="s12", arrs1==String("s12"));
  checkArrBool ("== ss-as", exprid, ess1==earrs2, ss1==arrs2);
  checkArrBool ("> as-as", exprid, earrs1>earrs2, arrs1>arrs2);
  checkArrBool ("> as-ss", exprid, earrs1>ess2, arrs1>ss2);
  checkArrBool ("> ss-as", exprid, "s123">earrs2, String("s123")>arrs2);
  checkArrBool (">= as-as", exprid, earrs1>=earrs2, arrs1>=arrs2);
  checkArrBool (">= as-ss", exprid, earrs1>=ess2, arrs1>=ss2);
  checkArrBool (">= ss-as", exprid, ess1>=earrs2, ss1>=arrs2);
  checkArrBool ("< as-as", exprid, earrs1<earrs2, arrs1<arrs2);
  checkArrBool ("< as-ss", exprid, earrs1<ess2, arrs1<ss2);
  checkArrBool ("< ss-as", exprid, ess1<earrs2, ss1<arrs2);
  checkArrBool ("<= as-as", exprid, earrs1<=earrs2, arrs1<=arrs2);
  checkArrBool ("<= as-ss", exprid, earrs1<=ess2, arrs1<=ss2);
  checkArrBool ("<= ss-as", exprid, ess1<=earrs2, ss1<=arrs2);
  checkArrBool ("!= as-as", exprid, earrs1!=earrs2, arrs1!=arrs2);
  checkArrBool ("!= as-ss", exprid, earrs1!=ess2, arrs1!=ss2);
  checkArrBool ("!= ss-as", exprid, ess1!=earrs2, ss1!=arrs2);

  // Test the double near functions.
  checkScaBool ("near sd-si", exprid, near(esd1,si1), near(sd1,sid1));
  checkScaBool ("nearAbs sd-sd", exprid, nearAbs(esd1,sd1), nearAbs(sd1,sd1));
  checkScaBool ("near3 sd-sd", exprid, near(esd1,esd2,1.e-7),
		near(sd1,sd2,1.e-7));
  checkScaBool ("nearAbs3 sd-sd", exprid, nearAbs(esd1,esd2,1.e-7),
		nearAbs(sd1,sd2,1.e-7));
  checkScaBool ("neard3 sd-sd", exprid, near(esd1,esd1-1.e-10,1.e-7),
		near(sd1,sd1-1.e-10,1.e-7));
  checkScaBool ("nearAbsd3 sd-sd", exprid, nearAbs(esd1,esd1-1.e-10,1.e-7),
		nearAbs(sd1,sd1-1.e-10,1.e-7));
  checkScaBool ("nearD3 sd-sd", exprid, near(esd1,esd1-1.e-4,1.e-7),
		near(sd1,sd1-1.e-4,1.e-7));
  checkScaBool ("nearAbsD3 sd-sd", exprid, nearAbs(esd1,esd1-1.e-4,1.e-7),
		nearAbs(sd1,sd1-1.e-4,1.e-7));
  checkArrBool ("near ad-ad", exprid, near(earrd1,arrd1),
		near(arrd1,arrd1,1.e-13));
  checkArrBool ("nearAbs ad-ad", exprid, nearAbs(earrd1,arrd1),
		nearAbs(arrd1,arrd1,1.e-13));
  checkArrBool ("near3 ad-ad", exprid, near(earrd1,earrd2,1.e-7),
		near(arrd1,arrd2,1.e-7));
  checkArrBool ("nearAbs3 ad-ad", exprid, nearAbs(earrd1,earrd2,1.e-7),
		nearAbs(arrd1,arrd2,1.e-7));
  checkArrBool ("near ad-sd", exprid, near(earrd1,sd1),
		near(arrd1,sd1arr,1.e-13));
  checkArrBool ("nearAbs ad-sd", exprid, nearAbs(earrd1,sd1),
		nearAbs(arrd1,sd1arr,1.e-13));
  checkArrBool ("near3 ad-sd", exprid, near(earrd1,esd2,1.e-7),
		near(sd2arr,arrd1,1.e-7));
  checkArrBool ("nearAbs3 ad-sd", exprid, nearAbs(earrd1,esd2,1.e-7),
		nearAbs(sd2arr,arrd1,1.e-7));
  checkArrBool ("near sd-sd", exprid, near(sd1,earrd1),
		near(arrd1,sd1arr,1.e-13));
  checkArrBool ("nearAbs sd-ad", exprid, nearAbs(sd1,earrd1),
		nearAbs(arrd1,sd1arr,1.e-13));
  checkArrBool ("near3 sd-ad", exprid, near(esd2,earrd1,1.e-7),
		near(sd2arr,arrd1,1.e-7));
  checkArrBool ("nearAbs3 sd-ad", exprid, nearAbs(esd2,earrd1,1.e-7),
		nearAbs(sd2arr,arrd1,1.e-7));
  // Test the complex near functions.
  checkScaBool ("near sz-sz", exprid, near(esz1,sz1), near(sz1,sz1));
  checkScaBool ("nearAbs sz-sz", exprid, nearAbs(esz1,sz1), nearAbs(sz1,sz1));
  checkScaBool ("near3 sz-sz", exprid, near(esz1,esz2,1.e-7),
		near(sz1,sz2,1.e-7));
  checkScaBool ("nearAbs3 sz-sz", exprid, nearAbs(esz1,esz2,1.e-7),
		nearAbs(sz1,sz2,1.e-7));
  checkScaBool ("neard3 sz-sz", exprid, near(esz1,esz1-1.e-10,1.e-7),
		near(sz1,sz1-1.e-10,1.e-7));
  checkScaBool ("nearAbsd3 sz-sz", exprid, nearAbs(esz1,esz1-1.e-10,1.e-7),
		nearAbs(sz1,sz1-1.e-10,1.e-7));
  checkScaBool ("nearD3 sz-sz", exprid, near(esz1,esz1-1.e-4,1.e-7),
		near(sz1,sz1-1.e-4,1.e-7));
  checkScaBool ("nearAbsD3 sz-sz", exprid, nearAbs(esz1,esz1-1.e-4,1.e-7),
		nearAbs(sz1,sz1-1.e-4,1.e-7));
  checkArrBool ("near az-az", exprid, near(earrz1,arrz1),
		near(arrz1,arrz1,1.e-13));
  checkArrBool ("nearAbs az-az", exprid, nearAbs(earrz1,arrz1),
		nearAbs(arrz1,arrz1,1.e-13));
  checkArrBool ("near3 az-az", exprid, near(earrz1,earrz2,1.e-7),
		near(arrz1,arrz2,1.e-7));
  checkArrBool ("nearAbs3 az-az", exprid, nearAbs(earrz1,earrz2,1.e-7),
		nearAbs(arrz1,arrz2,1.e-7));
  checkArrBool ("near az-sz", exprid, near(earrz1,sz1),
		near(arrz1,sz1arr,1.e-13));
  checkArrBool ("nearAbs az-sz", exprid, nearAbs(earrz1,sz1),
		nearAbs(arrz1,sz1arr,1.e-13));
  checkArrBool ("near3 az-sz", exprid, near(earrz1,esz2,1.e-7),
		near(sz2arr,arrz1,1.e-7));
  checkArrBool ("nearAbs3 az-sz", exprid, nearAbs(earrz1,esz2,1.e-7),
		nearAbs(sz2arr,arrz1,1.e-7));
  checkArrBool ("near sz-az", exprid, near(sz1,earrz1),
		near(arrz1,sz1arr,1.e-13));
  checkArrBool ("nearAbs sz-az", exprid, nearAbs(sz1,earrz1),
		nearAbs(arrz1,sz1arr,1.e-13));
  checkArrBool ("near3 sz-az", exprid, near(esz2,earrz1,1.e-7),
		near(sz2arr,arrz1,1.e-7));
  checkArrBool ("nearAbs3 sz-az", exprid, nearAbs(esz2,earrz1,1.e-7),
		nearAbs(sz2arr,arrz1,1.e-7));
  // Test the mixed double,complex near functions.
  checkScaBool ("near sz-sd", exprid, near(esz1,sd1), near(sz1,sdz1));
  checkScaBool ("near sd-sz", exprid, near(esd2,esz1), near(sdz2,sz1));
  checkArrBool ("near sd-az", exprid, near(esd2,arrz1),
		near(sdz2arr,arrz1,1.e-13));
  // Test the mixed near functions with int arguments.
  checkScaBool ("near si-sd", exprid, near(esi1,sd1), near(sid1,sd1));
  checkArrBool ("near ad-si", exprid, near(earrd2,esi1), near(arrd2,sid1,1e-13));
  checkArrBool ("near ai-az", exprid, near(earri1,arrz1),
		near(arriz1,arrz1,1.e-13));

  // Test various functions.
  // They are done in order of their definition in ExprFuncNode.h.
  // First the ones taking one Double or DComplex argument.
  checkScaDouble ("sin si", exprid, sin(esi1), sin(sid1));
  checkArrDouble ("sin ai", exprid, sin(earri1), sin(arrid1));
  checkScaDouble ("sin sd", exprid, sin(esd1), sin(sd1));
  checkArrDouble ("sin ad", exprid, sin(earrd1), sin(arrd1));
  checkScaDComplex ("sin sz", exprid, sin(esz1), sin(sz1));
  checkArrDComplex ("sin az", exprid, sin(earrz1), sin(arrz1));
  checkScaDouble ("sinh si", exprid, sinh(esi1), sinh(sid1));
  checkArrDouble ("sinh ai", exprid, sinh(earri1), sinh(arrid1));
  checkScaDouble ("sinh sd", exprid, sinh(esd1), sinh(sd1));
  checkArrDouble ("sinh ad", exprid, sinh(earrd1), sinh(arrd1));
  checkScaDComplex ("sinh sz", exprid, sinh(esz1), sinh(sz1));
  checkArrDComplex ("sinh az", exprid, sinh(earrz1), sinh(arrz1));
  checkScaDouble ("cos si", exprid, cos(esi1), cos(sid1));
  checkArrDouble ("cos ai", exprid, cos(earri1), cos(arrid1));
  checkScaDouble ("cos sd", exprid, cos(esd1), cos(sd1));
  checkArrDouble ("cos ai", exprid, cos(earri1), cos(arrid1));
  checkArrDouble ("cos ad", exprid, cos(earrd1), cos(arrd1));
  checkScaDComplex ("cos sz", exprid, cos(esz1), cos(sz1));
  checkArrDComplex ("cos az", exprid, cos(earrz1), cos(arrz1));
  checkScaDouble ("cosh si", exprid, cosh(esi1), cosh(sid1));
  checkArrDouble ("cosh ai", exprid, cosh(earri1), cosh(arrid1));
  checkScaDouble ("cosh sd", exprid, cosh(esd1), cosh(sd1));
  checkArrDouble ("cosh ad", exprid, cosh(earrd1), cosh(arrd1));
  checkScaDComplex ("cosh sz", exprid, cosh(esz1), cosh(sz1));
  checkArrDComplex ("cosh az", exprid, cosh(earrz1), cosh(arrz1));
  checkScaDouble ("exp sd", exprid, exp(esd1), exp(sd1));
  checkScaDouble ("exp si", exprid, exp(esi1), exp(sid1));
  checkArrDouble ("exp ai", exprid, exp(earri1), exp(arrid1));
  checkArrDouble ("exp ad", exprid, exp(earrd1), exp(arrd1));
  checkScaDComplex ("exp sz", exprid, exp(esz1), exp(sz1));
  checkArrDComplex ("exp az", exprid, exp(earrz1), exp(arrz1));
  checkScaDouble ("log si", exprid, log(esi1), log(sid1));
  checkArrDouble ("log ai", exprid, log(earri2), log(arrid2));
  checkScaDouble ("log sd", exprid, log(esd1), log(sd1));
  checkArrDouble ("log ad", exprid, log(earrd2), log(arrd2));
  checkScaDComplex ("log sz", exprid, log(esz1), log(sz1));
  checkArrDComplex ("log az", exprid, log(earrz2), log(arrz2));
  checkScaDouble ("log10 si", exprid, log10(esi1), log10(sid1));
  checkArrDouble ("log10 ai", exprid, log10(earri2), log10(arrid2));
  checkScaDouble ("log10 sd", exprid, log10(esd1), log10(sd1));
  checkArrDouble ("log10 ad", exprid, log10(earrd2), log10(arrd2));
  checkScaDComplex ("log10 sz", exprid, log10(esz1), log10(sz1));
  checkArrDComplex ("log10 az", exprid, log10(earrz2), log10(arrz2));
  checkScaDouble ("pow si-si", exprid, pow(esi2,esi1), pow(sid2,sid1));
  checkScaDouble ("pow sd-sd", exprid, pow(esd1,esd2), pow(sd1,sd2));
  checkArrDouble ("pow ad-ad", exprid, pow(earrd2,earrd1), pow(arrd2,arrd1));
  checkArrDouble ("pow sd-ad", exprid, pow(esd2,earrd2), pow(sd2,arrd2));
  checkArrDouble ("pow ad-sd", exprid, pow(earrd1,esd2), pow(arrd1,sd2));
  checkScaDComplex ("pow sz-sz", exprid, pow(esz2,esz1), pow(sz2,sz1));
  checkArrDComplex ("pow az-az", exprid, pow(earrz2,earrz1), pow(arrz2,arrz1));
  checkArrDComplex ("pow sz-az", exprid, pow(esz2,earrz1), pow(sz2,arrz1));
  checkArrDComplex ("pow az-sz", exprid, pow(earrz1,esz2), pow(arrz1,sz2arr));
  checkScaDComplex ("pow sd-sz", exprid, pow(esd2,esz1), pow(sdz2,sz1));
  checkArrDComplex ("pow ad-az", exprid, pow(earrd2,earrz1),
		    pow(arrdz2,arrz1));
  checkArrDComplex ("pow sd-az", exprid, pow(esd2,earrz1), pow(sdz2,arrz1));
  checkArrDComplex ("pow ad-sz", exprid, pow(earrd1,esz2), pow(arrdz1,sz2arr));
  checkScaDComplex ("pow sz-sd", exprid, pow(esz2,esd1), pow(sz2,sd1));
  checkArrDComplex ("pow az-ad", exprid, pow(earrz2,earrd1),
		    pow(arrz2,arrdz1));
  checkArrDComplex ("pow sz-ad", exprid, pow(esz2,earrd1), pow(sz2,arrdz1));
  checkArrDComplex ("pow az-sd", exprid, pow(earrz1,esd2), pow(arrz1,sd2));
  checkScaInt ("sqr si", exprid, square(esi1), si1*si1);
  checkArrInt ("sqr ai", exprid, square(earri1), square(arri1));
  checkScaDouble ("sqr sd", exprid, square(esd1), sd1*sd1);
  checkArrDouble ("sqr ad", exprid, square(earrd1), square(arrd1));
  checkScaDComplex ("sqr sz", exprid, square(esz1), sz1*sz1);
  checkArrDComplex ("sqr az", exprid, square(earrz1), square(arrz1));
  checkScaInt ("cube si", exprid, cube(esi1), si1*si1*si1);
  checkArrInt ("cube ai", exprid, cube(earri1), cube(arri1));
  checkScaDouble ("cube sd", exprid, cube(esd1), sd1*sd1*sd1);
  checkArrDouble ("cube ad", exprid, cube(earrd1), cube(arrd1));
  checkScaDComplex ("cube sz", exprid, cube(esz1), sz1*sz1*sz1);
  checkArrDComplex ("cube az", exprid, cube(earrz1), cube(arrz1));
  checkScaDouble ("sqrt si", exprid, sqrt(esi1), sqrt(sid1));
  checkArrDouble ("sqrt ai", exprid, sqrt(earri2), sqrt(arrid2));
  checkScaDouble ("sqrt sd", exprid, sqrt(esd1), sqrt(sd1));
  checkArrDouble ("sqrt ad", exprid, sqrt(earrd2), sqrt(arrd2));
  checkScaDComplex ("sqrt sz", exprid, sqrt(esz1), sqrt(sz1));
  checkArrDComplex ("sqrt az", exprid, sqrt(earrz2), sqrt(arrz2));
  checkScaDouble ("conj si", exprid, conj(esi1), si1);
  checkArrDouble ("conj ai", exprid, conj(earri1), arrid1);
  checkScaDouble ("conj sd", exprid, conj(esd1), sd1);
  checkArrDouble ("conj ad", exprid, conj(earrd1), arrd1);
  checkScaDComplex ("conj sz", exprid, conj(esz1), conj(sz1));
  checkArrDComplex ("conj az", exprid, conj(earrz1), conj(arrz1));
  checkScaDouble ("real si", exprid, real(esi1), si1);
  checkArrDouble ("real ai", exprid, real(earri1), arrid1);
  checkScaDouble ("real sd", exprid, real(esd1), sd1);
  checkArrDouble ("real ad", exprid, real(earrd1), arrd1);
  checkScaDouble ("real sz", exprid, real(esz1), real(sz1));
  checkArrDouble ("real az", exprid, real(earrz1), real(arrz1));
  checkScaDouble ("imag si", exprid, imag(esi1), 0.);
  checkArrDouble ("imag ai", exprid, imag(earri1), arrdzero);
  checkScaDouble ("imag sd", exprid, imag(esd1), 0.);
  checkArrDouble ("imag ad", exprid, imag(earrd1), arrdzero);
  checkScaDouble ("imag sz", exprid, imag(esz1), imag(sz1));
  checkArrDouble ("imag az", exprid, imag(earrz1), imag(arrz1));
  checkScaInt ("integer si", exprid, integer(esi1), si1);
  checkArrInt ("integer ai", exprid, integer(earri1), arri1);
  checkScaInt ("integer sd", exprid, integer(esd1), Int(sd1));
  checkArrInt ("integer ad", exprid, integer(earrd1), arrdi1);
  checkScaDComplex ("complex si", exprid, formComplex(esi1, 0), siz1);
  checkArrDComplex ("complex ai", exprid, formComplex(earri1, 0), arriz1);
  checkScaDComplex ("complex sd", exprid, formComplex(esd1, 0), sdz1);
  checkArrDComplex ("complex ad", exprid, formComplex(earrd1,arrdzero), arrdz1);
  checkArrDComplex ("complex ad-ad", exprid,
                    formComplex(real(earrz1),imag(earrz1)), arrz1);
  checkScaInt ("norm si", exprid, norm(esi1), si1*si1);
  checkArrInt ("norm ai", exprid, norm(earri1), arri1*arri1);
  checkScaDouble ("norm sd", exprid, norm(esd1), sd1*sd1);
  checkArrDouble ("norm ad", exprid, norm(earrd1), arrd1*arrd1);
  checkScaDouble ("norm sz", exprid, norm(esz1), norm(sz1));
  checkArrDouble ("norm az", exprid, norm(earrz1), pow(amplitude(arrz1),2));
  checkScaInt ("abs si", exprid, abs(esi1), abs(si1));
  checkArrInt ("abs ai", exprid, abs(earri1), abs(arri1));
  checkScaDouble ("abs sd", exprid, abs(esd1), abs(sd1));
  checkArrDouble ("abs ad", exprid, abs(earrd1), abs(arrd1));
  checkScaDouble ("abs sz", exprid, abs(esz1), abs(sz1));
  checkArrDouble ("abs az", exprid, abs(earrz1), amplitude(arrz1));
  checkScaDouble ("arg si", exprid, arg(esi1), 0.);
  checkArrDouble ("arg ai", exprid, arg(earri1), arridarg);
  checkScaDouble ("arg sd", exprid, arg(esd1), 0.);
  checkArrDouble ("arg ad", exprid, arg(earrd1), (arrdsign-1.)*C::pi/-2.);
  checkScaDouble ("arg sz", exprid, arg(esz1), arg(sz1));
  checkArrDouble ("arg az", exprid, arg(earrz1), phase(arrz1));
  checkScaDouble ("asin si", exprid, asin(esi1/300), asin(sid1/300.));
  checkArrDouble ("asin ai", exprid, asin(earri1/300), asin(arrid1/300.));
  checkScaDouble ("asin sd", exprid, asin(esd1/300.), asin(sd1/300.));
  checkArrDouble ("asin ad", exprid, asin(earrd1/300.), asin(arrd1/300.));
  checkScaDouble ("acos si", exprid, acos(esi1/300), acos(sid1/300.));
  checkArrDouble ("acos ai", exprid, acos(earri1/300), acos(arrid1/300.));
  checkScaDouble ("acos sd", exprid, acos(esd1/300.), acos(sd1/300.));
  checkArrDouble ("acos ad", exprid, acos(earrd1/300.), acos(arrd1/300.));
  checkScaDouble ("atan si", exprid, atan(esi1), atan(sid1));
  checkArrDouble ("atan ai", exprid, atan(earri1), atan(arrid1));
  checkScaDouble ("atan sd", exprid, atan(esd1), atan(sd1));
  checkArrDouble ("atan ad", exprid, atan(earrd1), atan(arrd1));
  checkScaDouble ("atan2 si-si", exprid, atan2(esi1,esi2), atan2(sid1,sid2));
  checkScaDouble ("atan2 sd-si", exprid, atan2(esd1,esi2), atan2(sd1,sid2));
  checkScaDouble ("atan2 sd-sd", exprid, atan2(esd1,esd2), atan2(sd1,sd2));
  checkArrDouble ("atan2 ad-ad", exprid, atan2(earrd1,earrd2),
		  atan2(arrd1,arrd2));
  checkArrDouble ("atan2 ai-ad", exprid, atan2(earri1,earrd2),
		  atan2(arrid1,arrd2));
  checkArrDouble ("atan2 ad-sd", exprid, atan2(earrd2,esd1),
		  atan2(arrd2,sd1arr));
  checkArrDouble ("atan2 sd-ad", exprid, atan2(esd1,earrd2),
		  atan2(sd1arr,arrd2));
  checkScaDouble ("tan si", exprid, tan(esi1), tan(sid1));
  checkArrDouble ("tan ai", exprid, tan(earri1), tan(arrid1));
  checkScaDouble ("tan sd", exprid, tan(esd1), tan(sd1));
  checkArrDouble ("tan ad", exprid, tan(earrd1), tan(arrd1));
  checkScaDouble ("tanh si", exprid, tanh(esi1), tanh(sid1));
  checkArrDouble ("tanh ai", exprid, tanh(earri1), tanh(arrid1));
  checkScaDouble ("tanh sd", exprid, tanh(esd1), tanh(sd1));
  checkArrDouble ("tanh ad", exprid, tanh(earrd1), tanh(arrd1));
  checkScaInt ("floor si", exprid, floor(esi1), si1);
  checkArrInt ("floor ai", exprid, floor(earri1), arri1);
  checkScaDouble ("floor sd", exprid, floor(esd1), floor(sd1));
  checkArrDouble ("floor ad", exprid, floor(earrd1), floor(arrd1));
  checkScaInt ("ceil si", exprid, ceil(esi1), si1);
  checkArrInt ("ceil ai", exprid, ceil(earri1), arri1);
  checkScaDouble ("ceil sd", exprid, ceil(esd1), ceil(sd1));
  checkArrDouble ("ceil ad", exprid, ceil(earrd1), ceil(arrd1));
  checkScaInt ("round si", exprid, round(esi1), si1);
  checkArrInt ("round ai", exprid, round(earri1), arri1);
  checkScaDouble ("round sd", exprid, round(esd1), sd1-0.1);
  checkArrDouble ("round ad", exprid, round(earrd1), round(arrd1));
  checkScaInt ("sign si", exprid, sign(esi1), 1);
  checkScaInt ("sign si", exprid, sign(0), 0);
  checkScaInt ("sign si", exprid, sign(-5), -1);
  checkArrInt ("sign ai", exprid, sign(earri1), arrisign);
  checkScaDouble ("sign sd", exprid, sign(esd1), 1.);
  checkScaDouble ("sign sd", exprid, sign(0.), 0.);
  checkScaDouble ("sign sd", exprid, sign(-3.1), -1.);
  checkArrDouble ("sign ad", exprid, sign(earrd1), arrdsign);
  checkScaInt ("fmod si-si", exprid, fmod(esi1,esi2), si1%si2);
  checkScaDouble ("fmod sd-sd", exprid, fmod(esd1,esd2), fmod(sd1,sd2));
  checkArrInt ("fmod ai-ai", exprid, fmod(earri1,earri2), arri1%arri2);
  checkArrDouble ("fmod ad-ad", exprid, earrd1%earrd2, fmod(arrd1,arrd2));
  checkArrDouble ("fmod ad-sd", exprid, fmod(earrd1,esd2), fmod(arrd1,sd2));
  checkArrDouble ("fmod sd-ad", exprid, fmod(esd1,earrd2), fmod(sd1,arrd2));
  // Check min of max of 2 values.
  checkScaInt ("min si-si", exprid, min(esi2,esi1), std::min(si2,si1));
  checkScaDouble ("min si-sd", exprid, min(esi2,esd1), std::min(sid2,sd1));
  checkScaDouble ("min sd-si", exprid, min(esd2,esi1), std::min(sd2,sid1));
  checkScaDouble ("min sd-sd", exprid, min(esd2,esd1), std::min(sd2,sd1));
  checkArrDouble ("min ad-ad", exprid, min(earrd2,earrd1), min(arrd2,arrd1));
  checkArrDouble ("min sd-ad", exprid, min(esd2,earrd1), min(arrd1,sd2));
  checkArrDouble ("min ad-sd", exprid, min(earrd1,esd2), min(arrd1,sd2));
  checkArrInt ("min ai-si", exprid, min(earri2,esi1), min(arri2,si1));
  checkArrInt ("min ai-ai", exprid, min(earri2,earri2), min(arri2,arri2));
  checkArrDouble ("min ai-sd", exprid, min(earri2,esd1), min(arrid2,sd1));
  checkArrDouble ("min sd-ai", exprid, min(esd2,earri2), min(sd2,arrid2));
  checkArrDouble ("min ad-ai", exprid, min(earrd2,earri1), min(arrd2,arrid1));
  checkScaDComplex ("min sz-sz", exprid, min(esz2,esz1), min(sz2,sz1));
  checkArrDComplex ("min az-az", exprid, min(earrz2,earrz1),
		    ::min(arrz2,arrz1));
  checkArrDComplex ("min sz-az", exprid, min(esz2,earrz1), min(arrz1,sz2));
  checkArrDComplex ("min az-sz", exprid, min(earrz1,esz2),
		    ::min(arrz1,sz2arr));
  checkScaDComplex ("min sd-sz", exprid, min(esd2,esz1), min(sdz2,sz1));
  checkArrDComplex ("min ad-az", exprid, min(earrd2,earrz1),
		    ::min(arrdz2,arrz1));
  checkArrDComplex ("min sd-az", exprid, min(esd2,earrz1), min(arrz1,sdz2));
  checkArrDComplex ("min ad-sz", exprid, min(earrd1,esz2),
		    ::min(arrdz1,sz2arr));
  checkScaDComplex ("min sz-sd", exprid, min(esz2,esd1), min(sz2,sdz1));
  checkArrDComplex ("min az-ad", exprid, min(earrz2,earrd1),
		    ::min(arrz2,arrdz1));
  checkArrDComplex ("min sz-ad", exprid, min(esz2,earrd1), min(arrdz1,sz2));
  checkArrDComplex ("min az-sd", exprid, min(earrz1,esd2), min(arrz1,sdz2));
  checkScaInt ("max si-si", exprid, max(esi2,esi1), std::max(si2,si1));
  checkScaDouble ("max si-sd", exprid, max(esi2,esd1), std::max(sid2,sd1));
  checkScaDouble ("max sd-si", exprid, max(esd2,esi1), std::max(sd2,sid1));
  checkScaDouble ("max sd-sd", exprid, max(esd2,esd1), std::max(sd2,sd1));
  checkArrDouble ("max ad-ad", exprid, max(earrd2,earrd1), max(arrd2,arrd1));
  checkArrDouble ("max sd-ad", exprid, max(esd2,earrd1), max(arrd1,sd2));
  checkArrDouble ("max ad-sd", exprid, max(earrd1,esd2), max(arrd1,sd2));
  checkArrInt ("max ai-si", exprid, max(earri2,esi1), max(arri2,si1));
  checkArrInt ("max ai-ai", exprid, max(earri2,earri2), max(arri2,arri2));
  checkArrDouble ("max ai-sd", exprid, max(earri2,esd1), max(arrid2,sd1));
  checkArrDouble ("max sd-ai", exprid, max(esd2,earri2), max(sd2,arrid2));
  checkArrDouble ("max ad-ai", exprid, max(earrd2,earri1), max(arrd2,arrid1));
  checkScaDComplex ("max sz-sz", exprid, max(esz2,esz1), max(sz2,sz1));
  checkArrDComplex ("max az-az", exprid, max(earrz2,earrz1),
		    ::max(arrz2,arrz1));
  checkArrDComplex ("max sz-az", exprid, max(esz2,earrz1), max(arrz1,sz2));
  checkArrDComplex ("max az-sz", exprid, max(earrz1,esz2),
		    ::max(arrz1,sz2arr));
  checkScaDComplex ("max sd-sz", exprid, max(esd2,esz1), max(sdz2,sz1));
  checkArrDComplex ("max ad-az", exprid, max(earrd2,earrz1), ::max(arrdz2,arrz1));
  checkArrDComplex ("max sd-az", exprid, max(esd2,earrz1), max(arrz1,sdz2));
  checkArrDComplex ("max ad-sz", exprid, max(earrd1,esz2),
		    ::max(arrdz1,sz2arr));
  checkScaDComplex ("max sz-sd", exprid, max(esz2,esd1), max(sz2,sdz1));
  checkArrDComplex ("max az-ad", exprid, max(earrz2,earrd1),
		    ::max(arrz2,arrdz1));
  checkArrDComplex ("max sz-ad", exprid, max(esz2,earrd1), max(arrdz1,sz2));
  checkArrDComplex ("max az-sd", exprid, max(earrz1,esd2), max(arrz1,sdz2));

  // Test the iif functions in various ways.
  checkScaDouble ("iif sb-sd-sd", exprid, iif(True,esd1,esd2), sd1);
  checkScaDouble ("iif sb-sd-si", exprid, iif(False,esd1,esi2), sid2);
  checkScaDouble ("iif sb-sd-si", exprid, iif(False,esd1,esi2), sid2);
  checkArrDouble ("iif sb-ad-si", exprid, iif(False,earrd1,0), arrdzero);
  checkArrInt ("iif ab-si-si", exprid, iif(earrb1,0,1), arrbi);
  checkArrDouble ("iif ab-si-sd", exprid, iif(earrb1,1,0.), arrbd);

  // Check the array->scalar functions (they also operate on scalars).
  checkScaInt ("sum(si)", exprid, sum(esi1), si1);
  checkScaInt ("product(si)", exprid, product(esi1), si1);
  checkScaInt ("sumSquare(si)", exprid, sumSquare(esi1), si1*si1);
  checkScaInt ("min(si)", exprid, min(esi1), si1);
  checkScaInt ("max(si)", exprid, max(esi1), si1);
  checkScaDouble ("mean(si)", exprid, mean(esi1), si1);
  checkScaDouble ("variance(si)", exprid, variance(esi1), 0);
  checkScaDouble ("stddev(si)", exprid, stddev(esi1), 0);
  checkScaDouble ("avdev(si)", exprid, avdev(esi1), 0);
  checkScaDouble ("rms(si)", exprid, rms(esi1), si1);
  checkScaDouble ("median(si)", exprid, median(esi1), si1);
  checkScaDouble ("fractile(si, 0.3)", exprid, fractile(esi1, 0.3), si1);
  checkScaDouble ("sum(sd)", exprid, sum(esd1), sd1);
  checkScaDouble ("product(sd)", exprid, product(esd1), sd1);
  checkScaDouble ("sumSquare(sd)", exprid, sumSquare(esd1), sd1*sd1);
  checkScaDouble ("min(sd)", exprid, min(esd1), sd1);
  checkScaDouble ("max(sd)", exprid, max(esd1), sd1);
  checkScaDouble ("mean(sd)", exprid, mean(esd1), sd1);
  checkScaDouble ("variance(sd)", exprid, variance(esd1), 0);
  checkScaDouble ("stddev(sd)", exprid, stddev(esd1), 0);
  checkScaDouble ("avdev(sd)", exprid, avdev(esd1), 0);
  checkScaDouble ("rms(sd)", exprid, rms(esd1), sd1);
  checkScaDouble ("median(sd)", exprid, median(esd1), sd1);
  checkScaDouble ("fractile(sd, 0.3)", exprid, fractile(esd1, 0.3), sd1);
  checkScaDComplex ("sum(sz)", exprid, sum(esz1), sz1);
  checkScaDComplex ("product(sz)", exprid, product(esz1), sz1);
  checkScaDComplex ("sumSquare(sz)", exprid, sumSquare(esz1), sz1*sz1);
  checkScaInt ("sum(arri)", exprid, sum(earri1), 170);
  checkScaInt ("product(arri)", exprid, product(earri1), 0);
  checkScaInt ("sumSquare(arri)", exprid, sumSquare(earri1), 2110);
  checkScaInt ("min(arri)", exprid, min(earri1), -1);
  checkScaInt ("max(arri)", exprid, max(earri1), 18);
  checkScaDouble ("mean(arri)", exprid, mean(earri1), 170./20);
  checkScaDouble ("variance(arri)", exprid, variance(earri1), 665./19.);
  checkScaDouble ("stddev(arri)", exprid, stddev(earri1), sqrt(665./19.));
  checkScaDouble ("avdev(arri)", exprid, avdev(earri1), 100./20.);
  checkScaDouble ("rms(arri)", exprid, rms(earri1), sqrt(2110./20.));
  checkScaDouble ("median(arri)", exprid, median(earri1), 8.5);
  checkScaDouble ("fractile(arri, 0.3)", exprid, fractile(earri1, 0.3), 4.);
  checkScaDouble ("sum(arrd)", exprid, sum(arrdsign), -4.);
  checkScaDouble ("product(arrd)", exprid, product(arrdsign), 1.);
  checkScaDouble ("sumSquare(arrd)", exprid, sumSquare(arrdsign), 20.);
  checkScaDouble ("min(arrd)", exprid, min(arrdsign), -1.);
  checkScaDouble ("max(arrd)", exprid, max(arrdsign), 1.);
  checkScaDouble ("mean(arrd)", exprid, mean(arrdsign), -4./20);
  checkScaDouble ("variance(arrd)", exprid, variance(arrdsign), 19.2/19);
  checkScaDouble ("stddev(arrd)", exprid, stddev(arrdsign), sqrt(19.2/19));
  checkScaDouble ("avdev(arrd)", exprid, avdev(arrdsign), 19.2/20.);
  checkScaDouble ("rms(arrd)", exprid, rms(arrdsign), 1.);
  checkScaDouble ("median(arrd)", exprid, median(arrdsign), -1.);
  checkScaDouble ("fractile(arrd, 0.3)", exprid, fractile(arrdsign, 0.3), -1.);
  checkScaDComplex ("sum(arrz)", exprid, sum(earrz1), DComplex(-640,-960));
  checkScaDComplex ("product(arrz)", exprid, product(earrz1),
                    Complex(1.55851e42, 1.62512e42));
  checkScaDComplex ("sumSquare(arrz)", exprid, sumSquare(earrz1),
                    Complex(-358100,859440));

  // Check the functions operating on Bool arrays (and scalars).
  checkScaBool ("any(sb)", exprid, any(esb1), sb1);
  checkScaBool ("all(sb)", exprid, all(esb1), sb1);
  checkScaInt ("ntrue(sb)", exprid, ntrue(esb1), 1);
  checkScaInt ("nfalse(sb)", exprid, nfalse(esb1), 0);
  checkScaBool ("any(ab)", exprid, any(earrb1), True);
  checkScaBool ("any(ab)", exprid, any(earrb1==earrb1), True);
  checkScaBool ("any(ab)", exprid, any(earrb1!=earrb1), False);
  checkScaBool ("all(ab)", exprid, all(earrb1), False);
  checkScaBool ("all(ab)", exprid, all(earrb1==earrb1), True);
  checkScaBool ("all(ab)", exprid, all(earrb1!=earrb1), False);
  checkScaInt ("ntrue(ab)", exprid, ntrue(earrb1), 19);
  checkScaInt ("nfalse(ab)", exprid, nfalse(earrb1), 1);
  checkScaBool ("isnan(si)", exprid, isNaN(esi1), False);
  checkScaBool ("isnan(sd)", exprid, isNaN(esd1), False);
  checkScaBool ("isnan(sz)", exprid, isNaN(esz1), False);
  checkScaBool ("isnan(ai)", exprid, any(isNaN(earri1)), False);
  checkScaBool ("isnan(ad)", exprid, any(isNaN(earrd1)), False);
  checkScaBool ("isnan(az)", exprid, any(isNaN(earrz1)), False);

  // Check array functions.
  checkScaInt ("array(ab2)", exprid, ntrue(array(esb1,shp)), 20);
  checkScaInt ("array(ab1)", exprid, ntrue(array(esb1,shp) == earrb1), 19);
  checkArrDouble ("array(ad)", exprid, array(earrd1,shp), arrd1);
  checkArrInt ("array(ai)", exprid, array(earri1,shp), arri1);
  checkArrDComplex ("array(az)", exprid, array(earrz1,shp), arrz1);
  checkArrString ("array(as)", exprid, array(earrs1,shp), mats1);
  checkScaInt ("ndim(si)", exprid, ndim(esi1), 0);
  checkScaInt ("ndim(ad)", exprid, ndim(earrd1), shp.size());
  checkArrInt ("shape(az)", exprid, shape(earrz1), shpVec);

  // Check string and pattern functions.
  checkScaInt ("strlength(ss)", exprid, strlength(ess1), ss1.size());
  checkScaString ("upcase(ss)", exprid, upcase(ess1), "SS1");
  checkScaString ("downcase(ss)", exprid, downcase(upcase(ess1)), "ss1");
  checkScaString ("trim(ss)", exprid, trim(TableExprNode("  a b  ")), "a b");
  checkScaString ("trim(ss)", exprid, trim(TableExprNode("")), "");
  checkScaString ("trim(ss)", exprid, trim(TableExprNode("a b")), "a b");
  checkScaString ("ltrim(ss)", exprid, rtrim(TableExprNode("  a b  ")), "  a b");
  checkScaString ("rtrim(ss)", exprid, ltrim(TableExprNode("  a b  ")), "a b  ");
  checkScaString ("substr(ss,2)", exprid, substr(TableExprNode("abcdef"),2), "cdef");
  checkScaString ("substr(ss,2,10)", exprid, substr(TableExprNode("abcdef"),2,10), "cdef");
  checkScaString ("substr(ss,3,2)", exprid, substr(TableExprNode("abcdef"),3,2), "de");
  checkArrString ("substr(trim(as),-1,2)", exprid, substr(trim(earrs1),-1,2), Vector<String>(arrs1.size(), "a1"));
  checkArrString ("substr(as,-1,-1)", exprid, substr(earrs1,-1,-1), Vector<String>(arrs1.size(), ""));
  checkScaString ("replace(ss,ss1)", exprid, replace(TableExprNode("abcdef"),"ab"), "cdef");
  checkScaString ("replace(ss,ss2)", exprid, replace(TableExprNode("abcdefab"),"ab"), "cdef");
  checkScaString ("replace(ss,ss2,ss)", exprid, replace(TableExprNode("abcdefab"),"ab", "xyz"), "xyzcdefxyz");
  checkScaString ("replace(ss,rg2)", exprid, replace(TableExprNode("abcdefab"),Regex("a.")), "cdef");
  checkScaString ("replace(ss,rg2,ss)", exprid, replace(TableExprNode("abcdefab"),Regex("a."), "xaz"), "xazcdefxaz");
  checkScaString ("replace(ss,rg1,ss)", exprid, replace(TableExprNode("abcdefab"),Regex("a.$"), "xaz"), "abcdefxaz");
  checkArrString ("replace(as,Regex(.*)", exprid, replace(earrs1,Regex(".*")), Vector<String>(arrs1.size(), ""));
  checkScaBool ("ss==regex", exprid, ess1==regex(TableExprNode("s.*")), True);
  checkScaBool ("ss==regex", exprid, ess1==regex(TableExprNode("as.*")), False);
  checkScaBool ("ss==patt", exprid, ess1==pattern(TableExprNode("s*")), True);
  checkScaBool ("ss==patt", exprid, ess1==pattern(TableExprNode("as*")), False);
  checkScaBool ("ss==sqlpatt", exprid, ess1==sqlpattern(TableExprNode("s%")), True);
  checkScaBool ("ss==sqlpatt", exprid, ess1==sqlpattern(TableExprNode("as%")), False);

  // Check date functions.
  checkScaDouble ("time", exprid, time("5Apr09/12:"), C::pi);
  checkScaDate ("datetime", exprid, datetime("5Apr09/12:"), MVTime(54926.5));
  checkScaDouble ("mjd", exprid, mjd(datetime("5Apr09/12:")), 54926.5);
  checkScaInt ("day", exprid, day(datetime("5Apr09/12:")), 5);
  checkScaInt ("month", exprid, month(datetime("5Apr09/12:")), 4);
  checkScaInt ("year", exprid, year(datetime("5Apr09/12:")), 2009);
  checkScaInt ("weekday", exprid, weekday(datetime("5Apr09/12:")), 7);
  checkScaInt ("weekday", exprid, weekday(datetime("6Apr09/12:")), 1);
  checkScaInt ("week", exprid, week(datetime("5Apr09/12:")), 14);
  checkScaInt ("week", exprid, week(datetime("6Apr09/12:")), 15);
  checkScaInt ("week", exprid, week(datetime("1Jan09/12:")), 1);
  checkScaString ("cdow", exprid, cdow(datetime("1Jan09/12:")), "Thu");
  checkScaString ("cmonth", exprid, cmonth(datetime("1Jan09/12:")), "Jan");

  // Check various kind of failures.
  checkFailure ("& si-sd", esi1&esd1);
  checkFailure ("| ad-si", earrd1|esi1);
  checkFailure ("sin ss", sin(ess1));
  checkFailure ("sin ss", sin(earrb1));
  checkFailure ("integer sz", integer(esz1));
  checkFailure ("asin sz", asin(esz1));
  checkFailure ("tan sz", tan(esz1));
  checkFailure ("ceil sz", ceil(esz1));
  checkFailure ("sign sz", sign(esz1));
  checkFailure ("round sz", round(esz1));
  checkFailure ("complex ab", formComplex(ess1,esd1));
  checkFailure ("complex ab", formComplex(ess1,ess1));
  checkFailure ("complex sz-sz", formComplex(esz1,esz2));
  checkFailure ("near2 ss-ss", near(ess1,ess1));
  checkFailure ("near2 ai-ss", near(earri1,ess1));
  checkFailure ("nearabs2 ss-ss", nearAbs(ess1,ess1));
  checkFailure ("nearabs2 as-si", nearAbs(earrs1,esi1));
  checkFailure ("near3 ss-ss", near(ess1,ess1, 1e-10));
  checkFailure ("near3 ai-ss", near(earri1,ess1, 1e-10));
  checkFailure ("nearabs2 ss-ss", nearAbs(ess1,ess1));
  checkFailure ("nearabs2 as-si", nearAbs(earrs1,esi1));
  checkFailure ("fmod sz-sd", fmod(esz1,esd1));
  checkFailure ("min sz", min(esz1));
}

void doShow()
{
  // Make some expressions where constants should have been pre-evaluated.
  TableExprNode expr1(TableExprNode(1)*2+3);
  cout << "1*2+3 = " << expr1.getInt(0) << "   ";
  expr1.show (cout);
  TableExprNode expr2(ndim(array(3, IPosition(1,5))));
  cout << "ndim(array(3,[5])) = " << expr2.getInt(0) << "   ";
  expr2.show (cout);
}

int main()
{
  try {
    doIt();
    doShow();
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  } catch (...) {
    cout << "Unexpected unknown exception" << endl;
    return 1;
  }
  if (foundError) {
    cout << "Some unexpected results were found" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
