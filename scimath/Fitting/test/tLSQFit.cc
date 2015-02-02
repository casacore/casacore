//# tLSQFit.cc -- test LSQFit
//# Copyright (C) 1999-2002,2004-2006,2008
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

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Fitting/LSQFit.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicMath/Random.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
Double Y(const Double x, const Double y=3e-15) {
  return (abs(x) < y) ? 0 : x;
}

Float Y(const Float x, const Double y=3e-15) {
  return (abs(x) < y) ? 0 : x;
}

DComplex Y(const DComplex x, const Double y=4e-15) {
  return DComplex(Y(real(x), y), Y(imag(x), y));
}

Complex Y(const Complex x, const Double y=4e-15) {
  return Complex(Y(real(x), y), Y(imag(x), y));
}

void showdt(const LSQFit &lsq) {
  uInt nun, np, ncon, ner, rank;
  Double *norm, *known, *constr, *err, *sEq, *sol;
  uInt *piv;
  Double prec, nonlin;
  lsq.debugIt(nun, np, ncon, ner, rank,
	      norm, known, constr, err, piv, sEq, sol,
	      prec, nonlin);
  cout << "nun, np, ncon, ner, rank: " << nun << ", " << np << ", " <<
    ncon << ", " << ner << ", " << rank << endl;
  cout << "collinearity, factor-1: " << prec << ", " << nonlin << endl;

  cout << "Norm";
  if (norm) {
    Int i00=0;
    for (uInt i=0; i<nun; ++i) {
      if (i==0) cout << "-";
      else cout << "    -";
      cout << i;
      for (uInt i0=i; i0<nun; ++i0) {
	cout << ": " << Y(norm[i00]);
	++i00;
      }
      cout << endl;
    }
  } else cout << "-0: --";
  cout << endl;

  cout << "Known";
  if (known) {
    for (uInt i2=0; i2<np; i2++) {
      cout << ": " << Y(known[i2]);
    }
  } else cout << ": --";
  cout << endl;

  cout << "Constraint";
  if (constr) {
    Int i00=0;
    for (uInt i=0; i<ncon; ++i) {
      if (i==0) cout << "-";
      else cout << "          -";
      cout << i;
      for (uInt i0=0; i0<nun; ++i0) {
	cout << ": " << Y(constr[i00]);
	++i00;
      }
      cout << endl;
    }
  } else cout << "-0: --";
  cout << endl;

  cout << "Error";
  if (err) {
    for (uInt i1=0; i1<ner; ++i1) {
      cout << ": " << Y(err[i1]);
    }
  } else cout << ": --";
  cout << endl;

  cout << "Pivot";
  if (piv) {
    for (uInt i3=0; i3<np; ++i3) {
      cout << ": " << piv[i3];
    }
  } else cout << ": --";
  cout << endl;

  cout << "Invert";
  if (sEq) {
    Int i00=0;
    for (uInt i=0; i<np; ++i) {
      if (i==0) cout << "-";
      else cout << "      -";
      cout << i;
      for (uInt i0=i; i0<np; ++i0) {
	cout << ": " << Y(sEq[i00]);
	++i00;
      }
      cout << endl;
    }
  } else cout << "-0: --";
  cout << endl;

  cout << "Sol";
  if (sol) {
    for (uInt i2=0; i2<np; i2++) {
      cout << ": " << Y(sol[i2]);
    }
  } else cout << ": --";
  cout << endl;

  cout << "---------------------------------------------------" << endl;
}

int main() {

  const uInt N=3;		// # unknowns
  const uInt N1=14;		// # unknowns
  const uInt M=6;		// # knowns
  // Data to be used
  Complex csol[2*N1];
  Double mu, me, sol[4*N1];
  
  Complex cce[M][N] = {
    {Complex(1,0),Complex(1,0),Complex(1,0)},
    {Complex(1,0),Complex(0,-1),Complex(2,0)},
    {Complex(1,0),Complex(-2,0),Complex(0,2)},
    {Complex(1,0),Complex(1,0),Complex(1,0)},
    {Complex(1,0),Complex(0,-1),Complex(2,0)},
    {Complex(1,0),Complex(-2,0),Complex(0,2)} };
  Complex cob[M] = {
    Complex(6,4),Complex(3,8),Complex(-15,9),
    Complex(6,4),Complex(3,8),Complex(-15,9)};
  DComplex dcce[M][N] = {
    {DComplex(1,0),DComplex(1,0),DComplex(1,0)},
    {DComplex(1,0),DComplex(0,-1),DComplex(2,0)},
    {DComplex(1,0),DComplex(-2,0),DComplex(0,2)},
    {DComplex(1,0),DComplex(1,0),DComplex(1,0)},
    {DComplex(1,0),DComplex(0,-1),DComplex(2,0)},
    {DComplex(1,0),DComplex(-2,0),DComplex(0,2)} };
  DComplex dcob[M] = {
    DComplex(6,4),DComplex(3,8),DComplex(-15,9),
    DComplex(6,4),DComplex(3,8),DComplex(-15,9)};
  Complex * cceit;
  DComplex *dcceit;
  Float wt[M] = {
    1,5,2,7,3,4};
  Double ceq[2*N][2*N];
  Complex cceq[N][N];
  DComplex dcceq[N][N];
  Double *ceqit;
  Complex *cceqit;
  DComplex *dcceqit;

  Double val12[512];
  Float val12f[512];
  for (uInt j=0; j<512; j++) val12f[j] = val12[j] = 1+2*j;
  Double val1a[6], sol1[6], sd1, mu1, err1[6];
  Float  val1fa[6], sol1f[6], err1f[6], sdf, muf;
  Double *val1 = val1a;
  Float *val1f = val1fa;
  Double cv1[6][6];
  Float cv1f[6][6];
  uInt nr1;

  try {
    cout << "Test LSQFit" << endl;
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- ctor --------" << endl;
    LSQFit lsq5(6);
    for (Int j0=0; j0<511; j0++) {
      val1[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1[j1] = val1[j1-1]*j0;
      lsq5.makeNorm(val1, 1.0, val12[j0]);
    }
    val1[0] = 1;
    for (uInt j1=1; j1<6; j1++) val1[j1] = val1[j1-1]*511;
    lsq5.makeNorm(val1, 1.0, val12[511]);
    cout << "Invert = " << lsq5.invert(nr1);
    cout << ", rank=" << nr1 << endl;
    lsq5.solve(sol1);
    sd1 = lsq5.getSD();
    mu1 = lsq5.getWeightedSD();
    for (uInt i=0; i<6; i++) { 
      cout << "Sol" << i << ": " <<
	Y(sol1[i], 1e-12) << ", " << sd1 << ", " << mu1 << endl;
    }
    cout << "Chi2: " << lsq5.getChi() << endl;
    lsq5.getErrors(err1);
    cout << "Errors: ";
    for (uInt i=0; i<6; i++) {
      if (i != 0) cout << ", ";
      cout << err1[i];
    }
    cout << endl;
    lsq5.getCovariance(&cv1[0][0]);
    for (uInt i5=0; i5<6; i5++) {
      cout << "Cov(" << i5 << ")";
      for (uInt i6=0; i6<6; i6++) {
	cout << ": " << Y(cv1[i5][i6], 1e-12);
      }
      cout << endl;
    }
    cout << "Float: " << endl;
    lsq5.solve(sol1f);
    sdf = lsq5.getSD();
    muf = lsq5.getWeightedSD();
    for (uInt i=0; i<6; i++) { 
      cout << "Sol" << i << ": " <<
	Y(sol1f[i], 1e-12) << ", " <<
	Y(sdf, 0.0006) << ", " << Y(muf, 0.0006) << endl;
    }
    lsq5.getErrors(err1f);
    cout << "Errors: ";
    for (uInt i=0; i<6; i++) {
      if (i != 0) cout << ", ";
      cout << Y(err1f[i], 0.00015);
    }
    cout << endl;
    lsq5.getCovariance(&cv1f[0][0]);
    for (uInt i5=0; i5<6; i5++) {
      cout << "Cov(" << i5 << ")";
      for (uInt i6=0; i6<6; i6++) {
	cout << ": " << Y(cv1f[i5][i6], 1e-12);
      }
      cout << endl;
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float -------" << endl;
    {
      LSQFit lsq5(6);
      for (Int j0=0; j0<511; j0++) {
	val1f[0] = 1;
	for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*j0;
	lsq5.makeNorm(val1f, 1.0f, val12f[j0]);
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      cout << "Invert = " << lsq5.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float ---  record ----" << endl;
    {
      LSQFit lsq5(6);
      for (Int j0=0; j0<511; j0++) {
	val1f[0] = 1;
	for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*j0;
	lsq5.makeNorm(val1f, 1.0f, val12f[j0]);
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      Record lrec1;
      Record lrec2;
      String error;
      cout << "Record: " << lsq5.toRecord(error, lrec1);
      cout << ", Error: " << error << endl;
      cout << "Invert = " << lsq5.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq5.solve(sol1);
      cout << "Record: " << lsq5.toRecord(error, lrec2);
      cout << ", Error: " << error << endl;
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
      LSQFit lsq6;
      cout << "From record: " << lsq6.fromRecord(error, lrec1);
      cout << ", Error: " << error << endl;
      cout << "Invert = " << lsq6.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq6.solve(sol1);
      sd1 = lsq6.getSD();
      mu1 = lsq6.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
      cout << "From record: " << lsq6.fromRecord(error, lrec1);
      cout << ", Error: " << error << endl;
      sd1 = lsq6.getSD();
      mu1 = lsq6.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float ---  aipsio ----" << endl;
    {
      LSQFit lsq5(6);
      for (Int j0=0; j0<511; j0++) {
	val1f[0] = 1;
	for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*j0;
	lsq5.makeNorm(val1f, 1.0f, val12f[j0]);
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      MemoryIO memio;
      AipsIO aio(&memio);
      lsq5.toAipsIO (aio);
      LSQFit lsq6;
      aio.setpos (0);
      lsq6.fromAipsIO (aio);
      cout << "Invert = " << lsq6.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq6.solve(sol1);
      sd1 = lsq6.getSD();
      mu1 = lsq6.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
      aio.setpos (0);
      lsq6.fromAipsIO (aio);
      cout << "Invert = " << lsq6.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq6.solve(sol1);
      sd1 = lsq6.getSD();
      mu1 = lsq6.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float --- merged ---" << endl;
    {
      LSQFit lsq5(6);
      LSQFit lsq5a(6);
      for (Int j0=0; j0<511; j0++) {
	val1f[0] = 1;
	for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*j0;
	if (j0<300) lsq5.makeNorm(val1f, 1.0f, val12f[j0]);
	else lsq5a.makeNorm(val1f, 1.0f, val12f[j0]);
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      cout << "Merge = " << lsq5.merge(lsq5a);
      cout << ", Invert = " << lsq5.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float --- merged --- indexed ---" << endl;
    {
      LSQFit lsq5(6);
      LSQFit lsq5a(6);
      uInt ixa[6]    = {0,4,3,2,5,1};
      uInt ixreva[6] = {0,5,3,2,1,4};
      uInt *ix =ixa;
      uInt *ixrev = ixreva;
      for (Int j0=0; j0<511; j0++) {
	if (j0<300) {
	  val1f[0] = 1;
	  for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*j0;
	  lsq5.makeNorm(val1f, 1.0f, val12f[j0]);
	} else {
	  val1f[ixrev[0]] = 1;
	  for (uInt j1=1; j1<6; j1++) val1f[ixrev[j1]] = val1f[ixrev[j1-1]]*j0;
	  lsq5a.makeNorm(val1f, 1.0f, val12f[j0]);
	}
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      cout << "Merge = " << lsq5.merge(lsq5a, 6, ix);
      cout << ", Invert = " << lsq5.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float --- merged --- indexed[2] -" << endl;
    {
      LSQFit lsq5(6);
      LSQFit lsq5a(6);
      uInt ixa[6]    = {0,4,3,2,5,1};
      uInt ixreva[6] = {0,5,3,2,1,4};
      uInt *ixrev = ixreva;
      std::vector<uInt> ixv;
      std::vector<uInt> ixrevv;
      for (uInt i=0; i<6; ++i) {
	ixv.push_back(ixa[i]);
	ixrevv.push_back(ixreva[i]);
      }
      for (Int j0=0; j0<511; j0++) {
	if (j0<300) {
	  val1f[0] = 1;
	  for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*j0;
	  lsq5.makeNorm(val1f, 1.0f, val12f[j0]);
	} else {
	  val1f[ixrev[0]] = 1;
	  for (uInt j1=1; j1<6; j1++) val1f[ixrev[j1]] = val1f[ixrev[j1-1]]*j0;
	  lsq5a.makeNorm(val1f, 1.0f, val12f[j0]);
	}
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      cout << "Merge = " << lsq5.merge(lsq5a, 6, ixv);
      cout << ", Invert = " << lsq5.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float ------- indexed ---" << endl;
    {
      LSQFit lsq5(6);
      uInt ixa[6]    = {0,4,3,2,5,1};
      uInt ixreva[6] = {0,5,3,2,1,4};
      uInt *ix =ixa;
      uInt *ixrev = ixreva;
      for (Int j0=0; j0<511; j0++) {
	val1f[ixrev[0]] = 1;
	for (uInt j1=1; j1<6; j1++) val1f[ixrev[j1]] = val1f[ixrev[j1-1]]*j0;
	lsq5.makeNorm(6, ix, val1f, 1.0f, val12f[j0]);
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      cout << "Invert = " << lsq5.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float ------- paired ---" << endl;
    {
      LSQFit lsq5(6);
      uInt ixreva[6] = {0,5,3,2,1,4};
      uInt *ixrev = ixreva;
      std::vector<std::pair<uInt, Float> > valpf(6);
      for (Int j0=0; j0<511; ++j0) {
	val1f[ixrev[0]] = 1;
	for (uInt j1=1; j1<6; ++j1) val1f[ixrev[j1]] = val1f[ixrev[j1-1]]*j0;
	for (uInt j1=0; j1<6; ++j1)
	  valpf[j1] = std::make_pair(j1, val1f[ixrev[j1]]);
	lsq5.makeNorm(valpf, 1.0f, val12f[j0]);
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      cout << "Invert = " << lsq5.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float ---- sorted index ---" << endl;
    {
      LSQFit lsq5(6);
      uInt ixa[6]    = {0,4,3,2,5,1};
      uInt ixreva[6] = {0,5,3,2,1,4};
      uInt *ixrev = ixreva;
      uInt *ix = ixa;
      Float valpf[6];
      Float *valsf = valpf;
      for (Int j0=0; j0<511; ++j0) {
	val1f[ixrev[0]] = 1;
	for (uInt j1=1; j1<6; ++j1) val1f[ixrev[j1]] = val1f[ixrev[j1-1]]*j0;
	for (uInt j1=0; j1<6; ++j1) {
	  ix[j1] = j1;
	  valpf[j1] = val1f[ixrev[j1]];
	}
	lsq5.makeNormSorted(6, ix, valsf, valsf, 1.0f,
			    val12f[j0], val12f[j0]);
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      cout << "Invert = " << lsq5.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float ---- unsorted index-2- ---" << endl;
    {
      LSQFit lsq5(6);
      uInt ixa[6]    = {0,4,3,2,5,1};
      uInt ixreva[6] = {0,5,3,2,1,4};
      uInt *ixrev = ixreva;
      uInt *ix = ixa;
      for (Int j0=0; j0<511; ++j0) {
	val1f[ixrev[0]] = 1;
	for (uInt j1=1; j1<6; ++j1) val1f[ixrev[j1]] = val1f[ixrev[j1-1]]*j0;
	lsq5.makeNorm(6, ix, val1f, val1f, 1.0f,
		      val12f[j0], val12f[j0]);
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(val1f, 1.0f, val12f[511]);
      cout << "Invert = " << lsq5.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i],1e-12) << ", " << Y(sd1, 1e-5) << ", " <<
	  Y(mu1, 1e-5) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- set --------" << endl;
    {
      LSQFit lsq5;
      lsq5.set(6,LSQReal());
      lsq5.set(1e-8);
      for (Int j0=0; j0<512; j0++) {
	val1[0] = 1;
	for (uInt j1=1; j1<6; j1++) val1[j1] = val1[j1-1]*j0;
	lsq5.makeNorm(val1, 1.0, val12[j0]);
      }
      cout << "Invert = " << lsq5.invert(nr1);
      cout << ", rank=" << nr1 << endl;
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i], 1e-12) << ", " << sd1 << ", " << mu1 << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns ---  =  --------" << endl;
    {
      LSQFit lsq6;
      lsq6 = lsq5;
      lsq6.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i], 1e-12) << ", " <<
	  Y(sd1, 0.0006) << ", " << Y(mu1, 0.0006) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;
    
    cout << "Real -- 6 unknowns --- copy -------" << endl;
    {
      LSQFit lsq6(lsq5);
      lsq6.solve(sol1);
      sd1 = lsq6.getSD();
      mu1 = lsq6.getWeightedSD();
      for (uInt i=0; i<6; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i], 1e-12) << ", " <<
	  Y(sd1, 0.0006) << ", " << Y(mu1, 0.0006) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 3 angles -------" << endl;
    {
      LSQFit lsq5(3);
      Float *val = new Float[3];
      val[0] = 1; val[1] = 0; val[2] = 0;
      lsq5.makeNorm(val, 1.0f, -90.0f);
      val[0] = 1; val[1] = 1; val[2] = 0;
      lsq5.makeNorm(val, 1.0f, -45.0f);
      val[0] = 1; val[1] = 1; val[2] = 1;
      lsq5.makeNorm(val, 1.0f, 1.0f);
      lsq5.invert(nr1);
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<3; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i], 1e-12) << ", " <<
	  Y(sd1, 8e-7) << ", " << Y(mu1, 8e-7) << endl;
      }
      delete [] val;
    }
    cout << "---------------------------------------------------" << endl;
    
    cout << "Real -- 3 angles - constraint 180" << endl;
    {
      LSQFit lsq5(3, 1);
      Float *val = new Float[3];
      val[0] = 1; val[1] = 0; val[2] = 0;
      lsq5.makeNorm(val, 1.0f, 90.0f);
      val[0] = 0; val[1] = 1; val[2] = 0;
      lsq5.makeNorm(val, 1.0f, 45.0f);
      val[0] = 0; val[1] = 0; val[2] = 1;
      lsq5.makeNorm(val, 1.0f, 46.0f);
      val[0] = 1; val[1] = 1; val[2] = 1;
      lsq5.setConstraint(0, val, 180.0f);
      lsq5.invert(nr1);
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<3; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i], 1e-12) << ", " << sd1 << ", " << mu1 << endl;
      }
      delete [] val;
    }
    
    cout << "---------------------------------------------------" << endl;
    
    cout << "Real -- 3 angles - add constraint 180" << endl;
    {
      LSQFit lsq5(3);
      Float *val = new Float[3];
      val[0] = 1; val[1] = 0; val[2] = 0;
      lsq5.makeNorm(val, 1.0f, 90.0f);
      val[0] = 0; val[1] = 1; val[2] = 0;
      lsq5.makeNorm(val, 1.0f, 45.0f);
      val[0] = 0; val[1] = 0; val[2] = 1;
      lsq5.makeNorm(val, 1.0f, 46.0f);
      val[0] = 1; val[1] = 1; val[2] = 1;
      lsq5.addConstraint(val, 180.0f);
      lsq5.invert(nr1);
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<3; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i], 1e-12) << ", " << sd1 << ", " << mu1 << endl;
      }
      delete [] val;
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Complex-----------------------" << endl;
    LSQFit lsqc1(N, LSQComplex());
    for (uInt i=0; i<M; i++) {
      cout << "(" << i << "): " << 
	", wt: " << wt[i] << ", ob: " << cob[i] << endl;
      cceit = cce[i];
      lsqc1.makeNorm(cceit, wt[i], cob[i], LSQFit::COMPLEX);
    }
    showdt(lsqc1);
    uInt nr;
    lsqc1.invert(nr);
    showdt(lsqc1);
    lsqc1.solve(sol);
    mu = lsq5.getSD();
    me = lsq5.getWeightedSD();
    cout << "Sol";
    for (uInt i4=0; i4<2*N; i4++) {
      cout << ": " << sol[i4];
    }
    cout << endl << "mu: " << mu << ", me: " << me << endl;
    {
      Double cv[2*N][2*N];
      lsqc1.getCovariance(&cv[0][0]);
      for (uInt i5=0; i5<2*N; i5++) {
	cout << "Cov(" << i5 << ")";
	for (uInt i6=0; i6<2*N; i6++) {
	  cout << ": " << Y(cv[i5][i6]);
	}
	cout << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;
    
    cout << "Complex------  other calls ----------" << endl;
    {
      LSQFit lsqc1(N, LSQComplex());
      for (uInt i=0; i<M; i++) {
	cceit = cce[i];
	dcceit = dcce[i];
	lsqc1.makeNorm(cceit, wt[i]/2, cob[i], LSQFit::COMPLEX);
	lsqc1.makeNorm(dcceit, wt[i]/2.0, dcob[i], LSQFit::COMPLEX);
      }
      uInt nr;
      lsqc1.invert(nr);
      lsqc1.solve(sol);
      mu = lsq5.getSD();
      me = lsq5.getWeightedSD();
      cout << "Sol";
      for (uInt i4=0; i4<2*N; i4++) {
	cout << ": " << sol[i4];
      }
      cout << endl << "mu: " << mu << ", me: " << me << endl;
      lsqc1.solve(csol);
      muf = lsqc1.getSD();
      sdf = lsqc1.getWeightedSD();
      cout << "Sol";
      for (uInt i4=0; i4<N; i4++) {
	cout << ": " << csol[i4];
      }
      cout << endl << "mu: " << muf << ", me: " << sdf << endl;
      DComplex dcsol[N1];
      lsqc1.solve(dcsol);
      mu = lsqc1.getSD();
      me = lsqc1.getWeightedSD();
      cout << "Sol";
      for (uInt i4=0; i4<N; i4++) {
	cout << ": " << dcsol[i4];
      }
      cout << endl << "mu: " << mu << ", me: " << me << endl;
      {
	Double cv[2*N][2*N];
	Complex ccv[N][N];
	DComplex dccv[N][N];
	lsqc1.getCovariance(&cv[0][0]);
	for (uInt i5=0; i5<2*N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<2*N; i6++) {
	    cout << ": " << Y(cv[i5][i6]);
	  }
	  cout << endl;
	}
	lsqc1.getCovariance(&ccv[0][0]);
	for (uInt i5=0; i5<N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<N; i6++) {
	    cout << ": " << Y(ccv[i5][i6]);
	  }
	  cout << endl;
	}
	lsqc1.getCovariance(&dccv[0][0]);
	for (uInt i5=0; i5<N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<N; i6++) {
	    cout << ": " << Y(dccv[i5][i6]);
	  }
	  cout << endl;
	  }
      }
    }
    cout << "---------------------------------------------------" << endl;
    
    cout << "Complex-Rank------------------" << endl;
    {
      for (uInt i=N-1; i<M; i+=N) {
	cob[i] = cob[i-1];
	for (uInt i1=0; i1<N; i1++) {
	  cce[i][i1] = cce[i-1][i1];
	}
      }
    }
    uInt i2;
    {
      LSQFit lsqc1(N, LSQComplex());
      for (uInt i=0; i<M; i++) {
	cout << "(" << i << "): " <<
	  ", wt: " << wt[i] << ", ob: " << cob[i] << endl;
	cceit = cce[i];
	lsqc1.makeNorm(cceit, wt[i], cob[i], LSQFit::COMPLEX);
      }
      showdt(lsqc1);
      uInt nr;
      lsqc1.invert(nr, True);
      showdt(lsqc1);
      lsqc1.solve(sol);
      mu = lsqc1.getSD();
      me = lsqc1.getWeightedSD();
      cout << "Sol";
      for (uInt i4=0; i4<2*N; i4++) {
	cout << ": " << sol[i4];
      }
      cout << endl << "mu: " << mu << ", me: " << me << endl;
      {
	for (uInt i=0; i<M; i++) {
	  DComplex cd1(0,0);
	  for (uInt i1=0; i1<N; i1++) {
	    cd1 += DComplex(sol[2*i1], sol[2*i1+1])*DComplex(cce[i][i1]);
	  }
	  cout << "LIN= " << cob[i] << "	LCHECK= " << cd1 << endl;
	}
      }
      i2 = lsqc1.getDeficiency();
      for (uInt i=0; i<i2; ++i) {
	ceqit = ceq[i];
	lsqc1.getConstraint(i, ceqit);
      }
      for (uInt i=0; i<i2/2; ++i) {
	cceqit = cceq[i];
	lsqc1.getConstraint(i, cceqit);
      }
      for (uInt i=0; i<i2/2; ++i) {
	dcceqit = dcceq[i];
	lsqc1.getConstraint(i, dcceqit);
      }
      for (uInt i3=0; (Int)i3<(Int)i2; i3++) {
	cout << "Constraint(" << i3 << ")";
	for (uInt i4=0; i4<2*N; i4++) {
	  cout << ": " << ceq[i3][i4];
	}
	cout << endl;
      }
      for (uInt i3=0; (Int)i3<(Int)i2/2; i3++) {
	cout << "Constraint(" << i3 << ")";
	for (uInt i4=0; i4<N; i4++) {
	  cout << ": " << cceq[i3][i4];
	}
	cout << endl;
      }
      {
	DComplex cd1(0,0);
	for (uInt i3=0; i3<i2; i3++) {
	  for (uInt i=0; i<N; i++) {
	    cd1 += sol[2*i]*ceq[i3][2*i] + sol[2*i+1]*ceq[i3][2*i+1];
	  }
	  cout << "Gives: " << Y(cd1) <<endl;
	}
      }
      {
	Double cv[2*N][2*N];
	lsqc1.getCovariance(&cv[0][0]);
	for (uInt i5=0; i5<2*N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<2*N; i6++) {
	    cout << ": " << Y(cv[i5][i6]);
	  }
	  cout << endl;
	}
      }
      cout << "---------------------------------------------------" << endl;
    }
    
    cout << "Complex+Constraint------------" << endl;
    {
      LSQFit lsqc1(N, LSQComplex(), i2/2);
      for (uInt i=0; i<M; i++) {
	cout << "(" << i << "): " <<
	  ", wt: " << wt[i] << ", ob: " << cob[i] << endl;
	cceit = cce[i];
	lsqc1.makeNorm(cceit, wt[i], cob[i], LSQFit::COMPLEX);
      }
      showdt(lsqc1);
      for (uInt i=0; i<i2; ++i) for (uInt j=0; j<N; ++j) {
	cout <<": "<< ceq[i][j] << endl;
      }
      for (uInt i=0; i<i2; ++i) {
	ceqit = ceq[i];
	lsqc1.setConstraint(i, ceqit, 0.0);
      }
      showdt(lsqc1);
      uInt nr;
      lsqc1.invert(nr, True);
      showdt(lsqc1);
      lsqc1.solve(sol);
      mu = lsqc1.getSD();
      me = lsqc1.getWeightedSD();
      cout << "Sol";
      for (uInt i4=0; i4<2*N; i4++) {
	cout << ": " << sol[i4];
      }
      cout << endl << "mu: " << mu << ", me: " << me << endl;
      {
	Double cv[2*N][2*N];
	lsqc1.getCovariance(&cv[0][0]);
	for (uInt i5=0; i5<2*N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<2*N; i6++) {
	    cout << ": " << Y(cv[i5][i6]);
	  }
	  cout << endl;
	}
      }
    }
    cout << "---------------------------------------------------" << endl;
    
    cout << "Complex+Complex Constraint------------" << endl;
    {
      LSQFit lsqc1(N, LSQComplex(), i2/2);
      for (uInt i=0; i<M; i++) {
	cceit = cce[i];
	lsqc1.makeNorm(cceit, wt[i], cob[i], LSQFit::COMPLEX);
      }
      for (uInt i=0; i<i2/2; ++i) {
	cceqit = cceq[i];
	lsqc1.setConstraint(i, cceqit, Complex(0,0));
      }
      showdt(lsqc1);
      uInt nr;
      lsqc1.invert(nr, True);
      showdt(lsqc1);
      lsqc1.solve(sol);
      mu = lsqc1.getSD();
      me = lsqc1.getWeightedSD();
      cout << "Sol";
      for (uInt i4=0; i4<2*N; i4++) {
	cout << ": " << sol[i4];
      }
      cout << endl << "mu: " << mu << ", me: " << me << endl;
      {
	Double cv[2*N][2*N];
	lsqc1.getCovariance(&cv[0][0]);
	for (uInt i5=0; i5<2*N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<2*N; i6++) {
	    cout << ": " << Y(cv[i5][i6]);
	  }
	  cout << endl;
	}
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Complex+DComplex Constraint------------" << endl;
    {
      LSQFit lsqc1(N, LSQComplex(), i2/2);
      for (uInt i=0; i<M; i++) {
	cceit = cce[i];
	lsqc1.makeNorm(cceit, wt[i], cob[i], LSQFit::COMPLEX);
      }
      for (uInt i=0; i<i2/2; ++i) {
	dcceqit = dcceq[i];
	lsqc1.setConstraint(i, dcceqit, DComplex(0,0));
      }
      showdt(lsqc1);
      uInt nr;
      lsqc1.invert(nr, True);
      showdt(lsqc1);
      lsqc1.solve(sol);
      mu = lsqc1.getSD();
      me = lsqc1.getWeightedSD();
      cout << "Sol";
      for (uInt i4=0; i4<2*N; i4++) {
	cout << ": " << sol[i4];
      }
      cout << endl << "mu: " << mu << ", me: " << me << endl;
      {
	Double cv[2*N][2*N];
	lsqc1.getCovariance(&cv[0][0]);
	for (uInt i5=0; i5<2*N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<2*N; i6++) {
	    cout << ": " << Y(cv[i5][i6]);
	  }
	  cout << endl;
	}
      }
    }
    cout << "---------------------------------------------------" << endl;
    
    cout << "DComplex Non-linear------------" << endl;
    {
      LSQFit lnl(3, LSQComplex());
      const uInt n=100;
      Double x[n];
      Double y[n];
      for (uInt i=0; i<n; i++) {
	x[i] = i*0.5;
	y[i] = 20*exp(-(((x[i]-25)/4)*((x[i]-25)/4)));
      }
      DComplex sol[3] = {DComplex(10), DComplex(20), DComplex(2)};
      DComplex una[3];
      DComplex *un = una;
      DComplex kn[1];
      const Int Niter = 30;
      lnl.setMaxIter(Niter);
      uInt nr;
      Timer tim1;
      tim1.mark();
      while (!lnl.isReady()) {
	for (uInt i=0; i<n; i++) {
	  DComplex A = sol[0]*exp(-(((x[i]-sol[1])/sol[2])*
				  ((x[i]-sol[1])/sol[2])));
	  DComplex b = 2.0*(x[i]-sol[1])/sol[2];
	  un[0] = A/sol[0];
	  un[1] = b*A/sol[2];
	  un[2] = b*b*A/2.0/sol[2];
	  kn[0] = y[i]-A;
	  lnl.makeNorm(un, 1.0, kn[0], LSQFit::COMPLEX);
	}
	if (!lnl.solveLoop(nr, sol)) {
	  cout << "Error in loop: " << nr << endl;
	  break;
	}
      }
      mu = lnl.getSD();
      me = lnl.getWeightedSD();
      cout << "Iterations: " << lnl.nIterations() << endl;
      cout << "Ready:      " << lnl.readyText() << endl;
      cout << "Sol:        " << sol[0] << ", " << sol[1] << ", " << sol[2] << 
	endl;
      cout << "me:         " << mu << ", " << me << endl;
      cerr << "User time:  " << tim1.user() << endl;
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Complex Non-linear------------" << endl;
    {
      LSQFit lnl(3, LSQComplex());
      const uInt n=100;
      Float x[n];
      Float y[n];
      for (uInt i=0; i<n; i++) {
	x[i] = i*0.5;
	y[i] = 20*exp(-(((x[i]-25)/4)*((x[i]-25)/4)));
      }
      Complex sol[3] = {Complex(10), Complex(20), Complex(2)};
      Complex una[3];
      Complex *un = una;
      Complex kn[1];
      Float mu, me;
      const Int Niter = 30;
      lnl.setMaxIter(Niter);
      uInt nr;
      Timer tim1;
      tim1.mark();
      while (!lnl.isReady()) {
	for (uInt i=0; i<n; i++) {
	  Complex A = sol[0]*exp(-(((x[i]-sol[1])/sol[2])*
				  ((x[i]-sol[1])/sol[2])));
	  Complex b = 2.0f*(x[i]-sol[1])/sol[2];
	  un[0] = A/sol[0];
	  un[1] = b*A/sol[2];
	  un[2] = b*b*A/2.0f/sol[2];
	  kn[0] = y[i]-A;
	  lnl.makeNorm(un, 1.0f, kn[0], LSQFit::COMPLEX);
	}
	if (!lnl.solveLoop(nr, sol)) {
	  cout << "Error in loop: " << nr << endl;
	  break;
	}
      }
      mu = lnl.getSD();
      me = lnl.getWeightedSD();
      cout << "Iterations: " << lnl.nIterations() << endl;
      cout << "Ready:      " << lnl.readyText() << endl;
      cout << "Sol:       " << sol[0] << ", " << sol[1] << ", " << sol[2] << 
	endl;
      if (mu == me && mu < 1e-7) {
	cout << "me:        " << 0.0 << ", " << 0.0 << endl;
      } else {
	cout << "me:        " << mu << ", " << me << endl;
      }
      cerr << "User time: " << tim1.user() << endl;
    }
    cout << "---------------------------------------------------" << endl;
    
    cout << "Complex -- all types ----------" << endl;
    cout << "Complex -- COMPLEX ------------" << endl;
    {
      // Condition equations for x+y=2,3i; x-y=4,1i;
      DComplex ce[2][3] = {{DComplex(1,0), DComplex(1,0), DComplex(0,0)},
			   {DComplex(1,0), DComplex(-1,0), DComplex(0,0)}};
      DComplex cer[2][3]= {{DComplex(1,0), DComplex(1,0), DComplex(0,0)},
			   {DComplex(-1,0), DComplex(1,0), DComplex(0,0)}};
      DComplex *ceit;
      DComplex *cerit;
      uInt cindexa[2] = {1,0};
      uInt *cindex = cindexa;
      DComplex m[2] = {DComplex(2,3), DComplex(4,1)};
      // Solution and error area
      DComplex sol[3];
      Double sd, mu;
      uInt rank;
      Bool ok;
      // LSQFit area
      LSQFit fit(2, LSQComplex());
      // Make normal equation
      for (uInt i=0; i<2; i++) {
	ceit = ce[i];
	fit.makeNorm(ceit, 1.0, m[i], LSQFit::COMPLEX);
      }
      // Invert and show
      ok = fit.invert(rank);
      cout << "ok? " << ok << "; rank: " << rank << endl;
      // Solve and show
      if (ok) {
	fit.solve(sol);
	sd = fit.getSD();
	mu = fit.getWeightedSD();
	for (uInt i=0; i<2; i++) cout << "Sol" << i << ": " << sol[i] << endl;
	cout << "sd: "<< sd << "; mu: " << mu << endl;
      }
      cout << "Complex -- COMPLEX ------------ indexed ---" << endl;
      fit.set(2, LSQComplex());
      // Make normal equation
      for (uInt i=0; i<2; i++) {
	cerit = cer[i];
	fit.makeNorm(2, cindex, cerit, 1.0, m[i], LSQFit::COMPLEX);
      }
      // Invert and show
      ok = fit.invert(rank);
      cout << "ok? " << ok << "; rank: " << rank << endl;
      // Solve and show
      if (ok) {
	fit.solve(sol);
	sd = fit.getSD();
	mu = fit.getWeightedSD();
	for (uInt i=0; i<2; i++) cout << "Sol" << i << ": " << sol[i] << endl;
	cout << "sd: "<< sd << "; mu: " << mu << endl;
      }
      cout << "Complex -- ASREAL -------------" << endl;
      // Retry with ASREAL type
      fit.set(2, LSQComplex()); 
      for (uInt i=0; i<2; i++) {
	ceit = ce[i];
	fit.makeNorm(ceit, 1.0, m[i], LSQFit::ASREAL);
      }
      ok = fit.invert(rank);
      cout << "ok? " << ok << "; rank: " << rank << endl;
      if (ok) {
	fit.solve(sol);
	sd = fit.getSD();
	mu = fit.getWeightedSD();
	for (uInt i=0; i<2; i++) cout << "Sol" << i << ": " << sol[i] << endl;
	cout << "sd: "<< sd << "; mu: " << mu << endl; 
      }
      cout << "Complex -- ASREAL ------------- indexed ---" << endl;
      fit.set(2, LSQComplex()); 
      for (uInt i=0; i<2; i++) {
	cerit = cer[i];
	fit.makeNorm(2, cindex, cerit, 1.0, m[i], LSQFit::ASREAL);
      }
      ok = fit.invert(rank);
      cout << "ok? " << ok << "; rank: " << rank << endl;
      if (ok) {
	fit.solve(sol);
	sd = fit.getSD();
	mu = fit.getWeightedSD();
	for (uInt i=0; i<2; i++) cout << "Sol" << i << ": " << sol[i] << endl;
	cout << "sd: "<< sd << "; mu: " << mu << endl; 
      }
      cout << "Complex -- SEPARABLE ----------" << endl;
      // Retry with SEPARABLE type: note # of unknowns!
      fit.set(1, LSQComplex());
      m[0] = DComplex(2,3); m[1] = DComplex(2,-3);
      for (uInt i=0; i<2; i++) {
	ceit = ce[i];
	fit.makeNorm(ceit, 1.0, m[i], LSQFit::SEPARABLE);
      }
      ok = fit.invert(rank);
      cout << "ok? " << ok << "; rank: " << rank << endl;
      if (ok) {
	fit.solve(sol);
	sd = fit.getSD();
	mu = fit.getWeightedSD();
	for (uInt i=0; i<1; i++) cout << "Sol" << i << ": " << sol[i] << endl;
	if (sd == mu && mu < 1e-7) {
	  cout << "sd: " << 0.0 << "; mu: " << 0.0 << endl;
	} else {
	  cout << "sd: " << sd  << "; mu: " << mu  << endl; 
	}
      }
      cout << "Complex -- SEPARABLE ---------- indexed ---" << endl;
      // Retry with SEPARABLE type: note # of unknowns!
      fit.set(1, LSQComplex());
      for (uInt i=0; i<2; i++) {
	cerit = cer[i];
	fit.makeNorm(2, cindex, cerit, 1.0, m[i], LSQFit::SEPARABLE);
      }
      ok = fit.invert(rank);
      cout << "ok? " << ok << "; rank: " << rank << endl;
      if (ok) {
	fit.solve(sol);
	sd = fit.getSD();
	mu = fit.getWeightedSD();
	for (uInt i=0; i<1; i++) cout << "Sol" << i << ": " << sol[i] << endl;
	if (sd == mu && mu < 1e-7) {
	  cout << "sd: " << 0.0 << "; mu: " << 0.0 << endl;
	} else {
	  cout << "sd: " << sd  << "; mu: " << mu  << endl; 
	}
      }
      cout << "Complex -- CONJUGATE ----------" << endl;
      // Retry with CONJUGATE type: note # of unknowns!
      fit.set(1, LSQComplex());
      m[0] = DComplex(2,0); m[1] = DComplex(0,1);
      for (uInt i=0; i<2; i++) {
	ceit = ce[i];
	fit.makeNorm(ceit, 1.0, m[i], LSQFit::CONJUGATE);
      }
      ok = fit.invert(rank, True);
      cout << "ok? " << ok << "; rank: " << rank << endl;
      if (ok) {
	fit.solve(sol);
	sd = fit.getSD();
	mu = fit.getWeightedSD();
	for (uInt i=0; i<1; i++) cout << "Sol" << i << ": " << sol[i] << endl;
	cout << "sd: "<< sd << "; mu: " << mu << endl; 
      }
      cout << "Complex -- CONJUGATE ---------- indexed ---" << endl;
      // Retry with CONJUGATE type: note # of unknowns!
      fit.set(1, LSQComplex());
      m[0] = DComplex(2,0); m[1] = DComplex(0,1);
      for (uInt i=0; i<2; i++) {
	cerit = cer[i];
	fit.makeNorm(2, cindex, cerit, 1.0, m[i], LSQFit::CONJUGATE);
      }
      ok = fit.invert(rank, True);
      cout << "ok? " << ok << "; rank: " << rank << endl;
      if (ok) {
	fit.solve(sol);
	sd = fit.getSD();
	mu = fit.getWeightedSD();
	for (uInt i=0; i<1; i++) cout << "Sol" << i << ": " << sol[i] << endl;
	cout << "sd: "<< sd << "; mu: " << mu << endl; 
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Non-linear------------" << endl;
    {
      LSQFit lnl(3);
      const uInt n=100;
      Double x[n];
      Double y[n];
      for (uInt i=0; i<n; i++) {
	x[i] = i*0.5;
	y[i] = 20*exp(-(((x[i]-25)/4)*((x[i]-25)/4)));
      }
      Double sol[3] = {10, 20, 2};
      Double una[3];
      Double* un = una;
      
      Double kn[1];
      const Int Niter = 30;
      lnl.setMaxIter(Niter);
      uInt nr;
      Timer tim1;
      tim1.mark();
      while (!lnl.isReady()) {
	for (uInt i=0; i<n; i++) {
	  Double A = sol[0]*exp(-(((x[i]-sol[1])/sol[2])*
				  ((x[i]-sol[1])/sol[2])));
	  Double b = 2*(x[i]-sol[1])/sol[2];
	  un[0] = A/sol[0];
	  un[1] = b*A/sol[2];
	  un[2] = b*b*A/2/sol[2];
	  kn[0] = y[i]-A;
	  lnl.makeNorm(un, 1.0, kn[0]);
	}
	if (!lnl.solveLoop(nr, sol)) {
	  cout << "Error in loop: " << nr << endl;
	  break;
	}
      }
      mu = lnl.getSD();
      me = lnl.getWeightedSD();
      cout << "Iterations: " << lnl.nIterations() << endl;
      cout << "Ready:      " << lnl.readyText() << endl;
      cout << "Sol:       " << sol[0] << ", " << sol[1] << ", " << sol[2] << 
	endl;
      if (mu == me && mu < 1e-15) {
	mu = 0;
	me = 0;
      }
      cout << "me:        " << mu << ", " << me << endl;
      cerr << "User time: " << tim1.user() << endl;
    }
    {
      cout << "Non-linear with 1.0 (5% of max) noise ------------" << endl;
      LSQFit lnl(3);
      const uInt n=100;
      Double x[n];
      Double y[n];
      for (uInt i=0; i<n; i++) {
	x[i] = i*0.5;
	y[i] = 20*exp(-(((x[i]-25)/4)*((x[i]-25)/4)));
      }
      MLCG genit;
      Normal noise(&genit, 0.0, 1.0);
      for (uInt i=0; i<n; i++) {
	y[i] += noise();
      }
      Double sol[3] = {10, 20, 2};
      Double *un = new Double[3];
      
      Double kn[1];
      const Int Niter = 30;
      lnl.setMaxIter(2*Niter);
      uInt nr;
      Timer tim1;
      tim1.mark();
      while (!lnl.isReady()) {
	for (uInt i=0; i<n; i++) {
	  Double A = sol[0]*exp(-(((x[i]-sol[1])/sol[2])*
				  ((x[i]-sol[1])/sol[2])));
	  Double b = 2*(x[i]-sol[1])/sol[2];
	  un[0] = A/sol[0];
	  un[1] = b*A/sol[2];
	  un[2] = b*b*A/2/sol[2];
	  kn[0] = y[i]-A;
	  lnl.makeNorm(un, 1.0, kn[0]);
	}
	if (!lnl.solveLoop(nr, sol)) {
	  cout << "Error in loop: " << nr << endl;
	  break;
	}
      }
      mu = lnl.getSD();
      me = lnl.getWeightedSD();
      cout << "Iterations: " << lnl.nIterations() << endl;
      cout << "Ready:      " << lnl.readyText() << endl;
      cout << "Sol:       " << sol[0] << ", " << sol[1] << ", " << sol[2] << 
	endl;
      cout << "me:        " << mu << ", " << me << endl;
      cerr << "User time: " << tim1.user() << endl;
      lnl.reset();
      for (uInt i=0; i<n; i++) {
	Double A = sol[0]*exp(-(((x[i]-sol[1])/sol[2])*
				((x[i]-sol[1])/sol[2])));
	Double b = 2*(x[i]-sol[1])/sol[2];
	un[0] = A/sol[0];
	un[1] = b*A/sol[2];
	un[2] = b*b*A/2/sol[2];
	kn[0] = y[i]-A;
	lnl.makeNorm(un, 1.0, kn[0]);
      }
      lnl.invert(nr);
      Double sold[3];
      Double covd[9];
      lnl.solve(sold);
      mu = lnl.getSD();
      me = lnl.getWeightedSD();
      lnl.getCovariance(covd);
      cout << "Sol:       " << sold[0] << ", " << sold[1] << ", " <<
	sold[2] << endl;
      cout << "me:        " << mu << ", " << me << endl;
      for (uInt i=0; i<9; i += 3) {
	cout << "Covar: " << Y(covd[i+0], 1e-16) << ", " <<
	  Y(covd[i+1], 1e-16) << ", " << Y(covd[i+2], 1e-16) << endl;
      }
      cerr << "User time: " << tim1.user() << endl;
      delete [] un;
    }

    cout << "Non-linear---- Float --------" << endl;
    {
      LSQFit lnl(3);
      const uInt n=100;
      Double x[n];
      Double y[n];
      for (uInt i=0; i<n; i++) {
	x[i] = i*0.5;
	y[i] = 20*exp(-(((x[i]-25)/4)*((x[i]-25)/4)));
      }
      Float sol[3] = {10, 20, 2};
      Float muf, mef;
      mef = lnl.getWeightedSD();
      Double una[3];
      Double *un = una;
      Double kn[1];
      const Int Niter = 30;
      lnl.setMaxIter(Niter);
      uInt nr;
      Timer tim1;
      tim1.mark();
      while (!lnl.isReady()) {
	for (uInt i=0; i<n; i++) {
	  Double A = sol[0]*exp(-(((x[i]-sol[1])/sol[2])*
				  ((x[i]-sol[1])/sol[2])));
	  Double b = 2*(x[i]-sol[1])/sol[2];
	  un[0] = A/sol[0];
	  un[1] = b*A/sol[2];
	  un[2] = b*b*A/2/sol[2];
	  kn[0] = y[i]-A;
	  lnl.makeNorm(un, 1.0, kn[0]);
	}
	if (!lnl.solveLoop(nr, sol)) {
	  cout << "Error in loop: " << nr << endl;
	  break;
	}
      }
      muf = lnl.getSD();
      cout << "Iterations: " << lnl.nIterations() << endl;
      cout << "Ready:      " << lnl.readyText() << endl;
      cout << "Sol:       " << sol[0] << ", " << sol[1] << ", " << sol[2] << 
	endl;
      if (muf == mef && muf < 1e-15) {
	muf = 0;
	mef = 0;
      }
      cout << "me:        " << muf << ", " << mef << endl;
      cerr << "User time: " << tim1.user() << endl;
    }

    cout << "---------------------------------------------------" << endl;
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  }
  return 0;
}
