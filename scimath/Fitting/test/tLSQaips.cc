//# tLSQaips.cc -- test LSQFit for Casacore Vectors
//# Copyright (C) 1999-2002,2004,2006
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
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/VectorSTLIterator.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Fitting/LSQaips.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicMath/Random.h>
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

void showdt(const LSQaips &lsq) {
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
  Vector<Complex> csol(2*N1);
  Double mu, me;
  Vector<Double> sol(4*N1);
  
  Complex vcce[M][N] = {
    {Complex(1,0),Complex(1,0),Complex(1,0)},
    {Complex(1,0),Complex(0,-1),Complex(2,0)},
    {Complex(1,0),Complex(-2,0),Complex(0,2)},
    {Complex(1,0),Complex(1,0),Complex(1,0)},
    {Complex(1,0),Complex(0,-1),Complex(2,0)},
    {Complex(1,0),Complex(-2,0),Complex(0,2)} };
  Complex cob[M] = {
    Complex(6,4),Complex(3,8),Complex(-15,9),
    Complex(6,4),Complex(3,8),Complex(-15,9)};
  DComplex vdcce[M][N] = {
    {DComplex(1,0),DComplex(1,0),DComplex(1,0)},
    {DComplex(1,0),DComplex(0,-1),DComplex(2,0)},
    {DComplex(1,0),DComplex(-2,0),DComplex(0,2)},
    {DComplex(1,0),DComplex(1,0),DComplex(1,0)},
    {DComplex(1,0),DComplex(0,-1),DComplex(2,0)},
    {DComplex(1,0),DComplex(-2,0),DComplex(0,2)} };
  Vector<Complex> cce(N);
  Vector<DComplex> dcce(N);
  VectorSTLIterator<std::complex<Float> > cceit(cce);
  VectorSTLIterator<std::complex<Double> > dcceit(dcce);
  DComplex dcob[M] = {
    DComplex(6,4),DComplex(3,8),DComplex(-15,9),
    DComplex(6,4),DComplex(3,8),DComplex(-15,9)};
  Float wt[M] = {
    1,5,2,7,3,4};
  Double vceq[2*N][2*N];
  Complex vcceq[N][N];
  DComplex vdcceq[N][N];
  Vector<Double> ceq(2*N);
  Vector<Complex> cceq(N);
  Vector<DComplex> dcceq(N);
  VectorSTLIterator<Double> ceqit(ceq);
  VectorSTLIterator<std::complex<Float> > cceqit(cceq);
  VectorSTLIterator<std::complex<Double> > dcceqit(dcceq);

  Double val12[512];
  Float val12f[512];
  for (uInt j=0; j<512; j++) val12f[j] = val12[j] = 1+2*j;
  Vector<Double> sol1(6);
  Double sd1, mu1;
  Vector<Double> err1(6);
  Vector<Float> sol1f(6);
  Float  sdf, muf;
  Vector<Float> err1f(6);
  Vector<Double> val1(6);
  VectorSTLIterator<Double> valit(val1);
  Vector<Float>  val1f(6);
  VectorSTLIterator<Float> valfit(val1f);
  Matrix<Double> cv1(6,6);
  Matrix<Float> cv1f(6,6);
  uInt nr1;

  try {
    cout << "Test LSQaips" << endl;
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- ctor --------" << endl;
    LSQaips lsq5(6);
    for (Int j0=0; j0<511; j0++) {
      val1[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1[j1] = val1[j1-1]*j0;
      lsq5.makeNorm(valit, 1.0, val12[j0]);
    }
    val1[0] = 1;
    for (uInt j1=1; j1<6; j1++) val1[j1] = val1[j1-1]*511;
    lsq5.makeNorm(valit, 1.0, val12[511]);
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
    lsq5.getCovariance(cv1);
    for (uInt i5=0; i5<6; i5++) {
      cout << "Cov(" << i5 << ")";
      for (uInt i6=0; i6<6; i6++) {
	cout << ": " << Y(cv1(i5,i6), 1e-12);
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
    lsq5.getCovariance(cv1f);
    for (uInt i5=0; i5<6; i5++) {
      cout << "Cov(" << i5 << ")";
      for (uInt i6=0; i6<6; i6++) {
	cout << ": " << Y(cv1f(i5,i6), 1e-12);
      }
      cout << endl;
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Real -- 6 unknowns --- float -------" << endl;
    {
      LSQaips lsq5(6);
      for (Int j0=0; j0<511; j0++) {
	val1f[0] = 1;
	for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*j0;
	lsq5.makeNorm(valfit, 1.0f, val12f[j0]);
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(valfit, 1.0f, val12f[511]);
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
    
    cout << "Real -- 6 unknowns --- float ------- indexed ---" << endl;
    {
      LSQaips lsq5(6);
      uInt cix[6]    = {0,4,3,2,5,1};
      uInt cixrev[6] = {0,5,3,2,1,4};
      Vector<uInt> ix(6);
      Vector<uInt> ixrev(6);
      VectorSTLIterator<uInt> ixit(ix);
      for (uInt i=0; i<6; ++i) {
	ix[i]    = cix[i];
	ixrev[i] = cixrev[i];
      }
      for (Int j0=0; j0<511; j0++) {
	val1f[ixrev[0]] = 1;
	for (uInt j1=1; j1<6; j1++) val1f[ixrev[j1]] = val1f[ixrev[j1-1]]*j0;
	lsq5.makeNorm(6, ixit, valfit, 1.0f, val12f[j0]);
      }
      val1f[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1f[j1] = val1f[j1-1]*511;
      lsq5.makeNorm(valfit, 1.0f, val12f[511]);
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
      LSQaips lsq5;
      lsq5.set(6,LSQReal());
      lsq5.set(1e-8);
      for (Int j0=0; j0<512; j0++) {
	val1[0] = 1;
	for (uInt j1=1; j1<6; j1++) val1[j1] = val1[j1-1]*j0;
	lsq5.makeNorm(valit, 1.0, val12[j0]);
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
      LSQaips lsq6;
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
      LSQaips lsq6(lsq5);
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
      LSQaips lsq5(3);
      Vector<Float> val(3);
      VectorSTLIterator<Float> valot(val);
      val[0] = 1; val[1] = 0; val[2] = 0;
      lsq5.makeNorm(valot, 1.0f, -90.0f);
      val[0] = 1; val[1] = 1; val[2] = 0;
      lsq5.makeNorm(valot, 1.0f, -45.0f);
      val[0] = 1; val[1] = 1; val[2] = 1;
      lsq5.makeNorm(valot, 1.0f, 1.0f);
      lsq5.invert(nr1);
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<3; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i], 1e-12) << ", " <<
	  Y(sd1, 8e-7) << ", " << Y(mu1, 8e-7) << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;
    
    cout << "Real -- 3 angles - constraint 180" << endl;
    {
        LSQaips lsq5(3, 1);
      Vector<Float> val(3);
      VectorSTLIterator<Float> valot(val);
      val[0] = 1; val[1] = 0; val[2] = 0;
      lsq5.makeNorm(valot, 1.0f, 90.0f);
      val[0] = 0; val[1] = 1; val[2] = 0;
      lsq5.makeNorm(valot, 1.0f, 45.0f);
      val[0] = 0; val[1] = 0; val[2] = 1;
      lsq5.makeNorm(valot, 1.0f, 46.0f);
      val[0] = 1; val[1] = 1; val[2] = 1;
      lsq5.setConstraint(0, valot, 180.0f);
      lsq5.invert(nr1);
      lsq5.solve(sol1);
      sd1 = lsq5.getSD();
      mu1 = lsq5.getWeightedSD();
      for (uInt i=0; i<3; i++) { 
	cout << "Sol" << i << ": " <<
	  Y(sol1[i], 1e-12) << ", " << sd1 << ", " << mu1 << endl;
      }
    }
    
    cout << "---------------------------------------------------" << endl;
    cout << "Complex-----------------------" << endl;
    LSQaips lsqc1(N, LSQComplex());
    for (uInt i=0; i<M; i++) {
      cout << "(" << i << "): " << 
	", wt: " << wt[i] << ", ob: " << cob[i] << endl;
      for (uInt j=0; j<N; ++j) cce[j]  = vcce[i][j];
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
      Matrix<Double> cv(2*N,2*N);
      lsqc1.getCovariance(cv);
      for (uInt i5=0; i5<2*N; i5++) {
	cout << "Cov(" << i5 << ")";
	for (uInt i6=0; i6<2*N; i6++) {
	  cout << ": " << Y(cv(i5,i6));
	}
	cout << endl;
      }
    }
    cout << "---------------------------------------------------" << endl;

    
    cout << "Complex------  other calls ----------" << endl;
    {
      LSQaips lsqc1(N, LSQComplex());
      for (uInt i=0; i<M; i++) {
	for (uInt j=0; j<N; ++j) cce[j]  = vcce[i][j];
	lsqc1.makeNorm(cceit, wt[i]/2, cob[i], LSQFit::COMPLEX);
	for (uInt j=0; j<N; ++j) dcce[j]  = vdcce[i][j];
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
      Vector<DComplex> dcsol(N1);
      lsqc1.solve(dcsol);
      mu = lsqc1.getSD();
      me = lsqc1.getWeightedSD();
      cout << "Sol";
      for (uInt i4=0; i4<N; i4++) {
	cout << ": " << dcsol[i4];
      }
      cout << endl << "mu: " << mu << ", me: " << me << endl;
      {
	Matrix<Double> cv(2*N,2*N);
	Matrix<Complex> ccv(N,N);
	Matrix<DComplex> dccv(N,N);
	lsqc1.getCovariance(cv);
	for (uInt i5=0; i5<2*N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<2*N; i6++) {
	    cout << ": " << Y(cv(i6,i5));
	  }
	  cout << endl;
	}
	lsqc1.getCovariance(ccv);
	for (uInt i5=0; i5<N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<N; i6++) {
	    cout << ": " << Y(ccv(i6,i5));
	  }
	  cout << endl;
	}
	lsqc1.getCovariance(dccv);
	for (uInt i5=0; i5<N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<N; i6++) {
	    cout << ": " << Y(dccv(i6,i5));
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
	  vcce[i][i1] = vcce[i-1][i1];
	}
      }
    }
    uInt i2;
    {
      LSQaips lsqc1(N, LSQComplex());
      for (uInt i=0; i<M; i++) {
	cout << "(" << i << "): " <<
	  ", wt: " << wt[i] << ", ob: " << cob[i] << endl;
	for (uInt j=0; j<N; ++j) cce[j]  = vcce[i][j];
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
	    cd1 += DComplex(sol[2*i1], sol[2*i1+1])*DComplex(vcce[i][i1]);
	  }
	  cout << "LIN= " << cob[i] << "	LCHECK= " << cd1 << endl;
	}
      }
      i2 = lsqc1.getDeficiency();
      for (uInt i=0; i<i2; ++i) {
	lsqc1.getConstraint(i, ceqit);
	for (uInt j=0; j<2*N; ++j) vceq[i][j] = ceq[j];
      }
      for (uInt i=0; i<i2/2; ++i) {
	lsqc1.getConstraint(i, cceqit);
	for (uInt j=0; j<N; ++j) vcceq[i][j] = cceq[j];
      }
      for (uInt i=0; i<i2/2; ++i) {
	lsqc1.getConstraint(i, dcceqit);
	for (uInt j=0; j<N; ++j) vdcceq[i][j] = dcceq[j];
      }
      for (uInt i3=0; (Int)i3<(Int)i2; i3++) {
	cout << "Constraint(" << i3 << ")";
	for (uInt i4=0; i4<2*N; i4++) {
	  cout << ": " << vceq[i3][i4];
	}
	cout << endl;
      }
      for (uInt i3=0; (Int)i3<(Int)i2/2; i3++) {
	cout << "Constraint(" << i3 << ")";
	for (uInt i4=0; i4<N; i4++) {
	  cout << ": " << vcceq[i3][i4];
	}
	cout << endl;
      }
      {
	DComplex cd1(0,0);
	for (uInt i3=0; i3<i2; i3++) {
	  for (uInt i=0; i<N; i++) {
	    cd1 += sol[2*i]*vceq[i3][2*i] + sol[2*i+1]*vceq[i3][2*i+1];
	  }
	  cout << "Gives: " << Y(cd1) <<endl;
	}
      }
      {
	Matrix<Double> cv(2*N,2*N);
	lsqc1.getCovariance(cv);
	for (uInt i5=0; i5<2*N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<2*N; i6++) {
	    cout << ": " << Y(cv(i5,i6));
	  }
	  cout << endl;
	}
      }
      cout << "---------------------------------------------------" << endl;
    }
    
    cout << "Complex+Constraint------------" << endl;
    {
      LSQaips lsqc1(N, LSQComplex(), i2/2);
      for (uInt i=0; i<M; i++) {
	cout << "(" << i << "): " <<
	  ", wt: " << wt[i] << ", ob: " << cob[i] << endl;
	for (uInt j=0; j<N; ++j) cce[j]  = vcce[i][j];
	lsqc1.makeNorm(cceit, wt[i], cob[i], LSQFit::COMPLEX);
      }
      showdt(lsqc1);
      for (uInt i=0; i<i2; ++i) for (uInt j=0; j<N; ++j) {
	cout <<": "<< vceq[i][j] <<endl;
      }
      for (uInt i=0; i<i2; ++i) {
	for (uInt j=0; j<2*N; ++j) ceq[j] = vceq[i][j];
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
	Matrix<Double> cv(2*N,2*N);
	lsqc1.getCovariance(cv);
	for (uInt i5=0; i5<2*N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<2*N; i6++) {
	    cout << ": " << Y(cv(i5,i6));
	  }
	  cout << endl;
	}
      }
    }
    cout << "---------------------------------------------------" << endl;
    
    cout << "Complex+Complex Constraint------------" << endl;
    {
      LSQaips lsqc1(N, LSQComplex(), i2/2);
      for (uInt i=0; i<M; i++) {
	for (uInt j=0; j<N; ++j) cce[j]  = vcce[i][j];
	lsqc1.makeNorm(cceit, wt[i], cob[i], LSQFit::COMPLEX);
      }
      for (uInt i=0; i<i2/2; ++i) {
	for (uInt j=0; j<N; ++j) cceq[j] = vcceq[i][j];
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
	Matrix<Double> cv(2*N,2*N);
	lsqc1.getCovariance(cv);
	for (uInt i5=0; i5<2*N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<2*N; i6++) {
	    cout << ": " << Y(cv(i5,i6));
	  }
	  cout << endl;
	}
      }
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Complex+DComplex Constraint------------" << endl;
    {
      LSQaips lsqc1(N, LSQComplex(), i2/2);
      for (uInt i=0; i<M; i++) {
	for (uInt j=0; j<N; ++j) cce[j]  = vcce[i][j];
	lsqc1.makeNorm(cceit, wt[i], cob[i], LSQFit::COMPLEX);
      }
      for (uInt i=0; i<i2/2; ++i) {
	for (uInt j=0; j<N; ++j) dcceq[j]  = vdcceq[i][j];
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
	Matrix<Double> cv(2*N,2*N);
	lsqc1.getCovariance(cv);
	for (uInt i5=0; i5<2*N; i5++) {
	  cout << "Cov(" << i5 << ")";
	  for (uInt i6=0; i6<2*N; i6++) {
	    cout << ": " << Y(cv(i5,i6));
	  }
	  cout << endl;
	}
      }
    }
    cout << "---------------------------------------------------" << endl;
    
    cout << "DComplex Non-linear------------" << endl;
    {
      LSQaips lnl(3, LSQComplex());
      const uInt n=100;
      Double x[n];
      Double y[n];
      for (uInt i=0; i<n; i++) {
	x[i] = i*0.5;
	y[i] = 20*exp(-(((x[i]-25)/4)*((x[i]-25)/4)));
      }
      DComplex sol[3] = {DComplex(10), DComplex(20), DComplex(2)};
      Vector<DComplex> un(3);
      VectorSTLIterator<DComplex> unitit(un);
      Vector<DComplex> vsol(3);
      for (uInt i=0; i<3; ++i) vsol[i] = sol[i];
      DComplex kn[1];
      const Int Niter = 30;
      lnl.setMaxIter(Niter);
      uInt nr;
      Timer tim1;
      tim1.mark();
      while (!lnl.isReady()) {
	for (uInt i=0; i<n; i++) {
	  DComplex A = vsol[0]*exp(-(((x[i]-vsol[1])/vsol[2])*
				      ((x[i]-vsol[1])/vsol[2])));
	  DComplex b = 2.0*(x[i]-vsol[1])/vsol[2];
	  un[0] = A/vsol[0];
	  un[1] = b*A/vsol[2];
	  un[2] = b*b*A/2.0/vsol[2];
	  kn[0] = y[i]-A;
	  lnl.makeNorm(unitit, 1.0, kn[0], LSQFit::COMPLEX);
	}
	if (!lnl.solveLoop(nr, vsol)) {
	  cout << "Error in loop: " << nr << endl;
	  break;
	}
      }
      mu = lnl.getSD();
      me = lnl.getWeightedSD();
      cout << "Iterations: " << lnl.nIterations() << endl;
      cout << "Ready:      " << lnl.readyText() << endl;
      cout << "Sol:       " << vsol[0] << ", " << vsol[1] << ", " << vsol[2] << 
	endl;
      cout << "me:        " << mu << ", " << me << endl;
      cerr << "User time: " << tim1.user() << endl;
    }
    cout << "---------------------------------------------------" << endl;

    cout << "Complex Non-linear------------" << endl;
    {
      LSQaips lnl(3, LSQComplex());
      const uInt n=100;
      Float x[n];
      Float y[n];
      for (uInt i=0; i<n; i++) {
	x[i] = i*0.5;
	y[i] = 20*exp(-(((x[i]-25)/4)*((x[i]-25)/4)));
      }
      Complex sol[3] = {Complex(10), Complex(20), Complex(2)};
      Vector<Complex> un(3);
      VectorSTLIterator<Complex> unitit(un);
      Vector<Complex> vsol(3);
      VectorSTLIterator<Complex> solit(vsol);
      std::copy(sol, sol+3, solit);
      Complex kn[1];
      Float mu, me;
      const Int Niter = 30;
      lnl.setMaxIter(Niter);
      uInt nr;
      Timer tim1;
      tim1.mark();
      while (!lnl.isReady()) {
	for (uInt i=0; i<n; i++) {
	  Complex A = vsol[0]*exp(-(((x[i]-vsol[1])/vsol[2])*
				  ((x[i]-vsol[1])/vsol[2])));
	  Complex b = 2.0f*(x[i]-vsol[1])/vsol[2];
	  un[0] = A/vsol[0];
	  un[1] = b*A/vsol[2];
	  un[2] = b*b*A/2.0f/vsol[2];
	  kn[0] = y[i]-A;
	  lnl.makeNorm(unitit, 1.0f, kn[0], LSQFit::COMPLEX);
	}
	if (!lnl.solveLoop(nr, vsol)) {
	  cout << "Error in loop: " << nr << endl;
	  break;
	}
      }
      mu = lnl.getSD();
      me = lnl.getWeightedSD();
      cout << "Iterations: " << lnl.nIterations() << endl;
      cout << "Ready:      " << lnl.readyText() << endl;
      cout << "Sol:       " << vsol[0] << ", " << vsol[1] << ", " << vsol[2] << 
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
      DComplex vce[2][3] = {{DComplex(1,0), DComplex(1,0), DComplex(0,0)},
			    {DComplex(1,0), DComplex(-1,0), DComplex(0,0)}};
      DComplex vcer[2][3]= {{DComplex(1,0), DComplex(1,0), DComplex(0,0)},
			    {DComplex(-1,0), DComplex(1,0), DComplex(0,0)}};
      Vector<DComplex> ce(3);
      Vector<DComplex> cer(3);
      VectorSTLIterator<DComplex> ceit(ce);
      VectorSTLIterator<DComplex> cerit(cer);
      uInt vcindex[2] = {1,0};
      Vector<uInt> cindex(2);
      VectorSTLIterator<uInt> cindexit(cindex); 
      for (uInt j=0; j<2; ++j) cindex[j] =vcindex[j];
      DComplex m[2] = {DComplex(2,3), DComplex(4,1)};
      // Solution and error area
      Vector<DComplex> sol(3);
      Double sd, mu;
      uInt rank;
      Bool ok;
      // LSQFit area
      LSQaips fit(2, LSQComplex());
      // Make normal equation
      for (uInt i=0; i<2; i++) {
	for (uInt j=0; j<3; ++j) ce[j] =vce[i][j];
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
	for (uInt j=0; j<3; ++j) cer[j] = vcer[i][j];
	fit.makeNorm(2, cindexit, cerit, 1.0, m[i], LSQFit::COMPLEX);
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
      for (uInt i=0; i<2; i++) 	{
	for (uInt j=0; j<3; ++j) ce[j] = vce[i][j];
	fit.makeNorm(ceit, 1.0, m[i],  LSQFit::ASREAL);
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
	for (uInt j=0; j<3; ++j) cer[j] = vcer[i][j];
	fit.makeNorm(2, cindexit, cerit, 1.0, m[i], LSQFit::ASREAL);
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
      for (uInt i=0; i<2; i++) 	{
	for (uInt j=0; j<3; ++j) ce[j] = vce[i][j];
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
      for (uInt i=0; i<2; i++) 	{
	for (uInt j=0; j<3; ++j) cer[j] = vcer[i][j];
	fit.makeNorm(2, cindexit, cerit, 1.0, m[i], LSQFit::SEPARABLE);
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
	for (uInt j=0; j<3; ++j) ce[j] = vce[i][j];
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
      for (uInt i=0; i<2; i++) 	{
	for (uInt j=0; j<3; ++j) cer[j] = vcer[i][j];
	fit.makeNorm(2, cindexit, cerit, 1.0, m[i], LSQFit::CONJUGATE);
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
      LSQaips lnl(3);
      const uInt n=100;
      Double x[n];
      Double y[n];
      for (uInt i=0; i<n; i++) {
	x[i] = i*0.5;
	y[i] = 20*exp(-(((x[i]-25)/4)*((x[i]-25)/4)));
      }
      Double sol[3] = {10, 20, 2};
      Vector<Double> vsol(3);
      VectorSTLIterator<Double> solit(vsol);
      std::copy(sol, sol+3, solit);
      Vector<Double> un(3);
      VectorSTLIterator<Double> unit(un);
      Double kn[1];
      const Int Niter = 30;
      lnl.setMaxIter(Niter);
      uInt nr;
      Timer tim1;
      tim1.mark();
      while (!lnl.isReady()) {
	for (uInt i=0; i<n; i++) {
	  Double A = vsol[0]*exp(-(((x[i]-vsol[1])/vsol[2])*
				  ((x[i]-vsol[1])/vsol[2])));
	  Double b = 2*(x[i]-vsol[1])/vsol[2];
	  un[0] = A/vsol[0];
	  un[1] = b*A/vsol[2];
	  un[2] = b*b*A/2/vsol[2];
	  kn[0] = y[i]-A;
	  lnl.makeNorm(unit, 1.0, kn[0]);
	}
	if (!lnl.solveLoop(nr, vsol)) {
	  cout << "Error in loop: " << nr << endl;
	  break;
	}
      }
      mu = lnl.getSD();
      me = lnl.getWeightedSD();
      cout << "Iterations: " << lnl.nIterations() << endl;
      cout << "Ready:      " << lnl.readyText() << endl;
      cout << "Sol:       " << vsol[0] << ", " << vsol[1] << ", " << vsol[2] << 
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
      LSQaips lnl(3);
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
      Vector<Double> vsol(3);
      VectorSTLIterator<Double> solit(vsol);
      std::copy(sol, sol+3, solit);
      Vector<Double> un(3);
      VectorSTLIterator<Double> unit(un);
      Double kn[1];
      const Int Niter = 30;
      lnl.setMaxIter(Niter);
      uInt nr;
      Timer tim1;
      tim1.mark();
      while (!lnl.isReady()) {
	for (uInt i=0; i<n; i++) {
	  Double A = vsol[0]*exp(-(((x[i]-vsol[1])/vsol[2])*
				  ((x[i]-vsol[1])/vsol[2])));
	  Double b = 2*(x[i]-vsol[1])/vsol[2];
	  un[0] = A/vsol[0];
	  un[1] = b*A/vsol[2];
	  un[2] = b*b*A/2/vsol[2];
	  kn[0] = y[i]-A;
	  lnl.makeNorm(unit, 1.0, kn[0]);
	}
	if (!lnl.solveLoop(nr, vsol)) {
	  cout << "Error in loop: " << nr << endl;
	  break;
	}
      }
      mu = lnl.getSD();
      me = lnl.getWeightedSD();
      cout << "Iterations: " << lnl.nIterations() << endl;
      cout << "Ready:      " << lnl.readyText() << endl;
      cout << "Sol:       " << vsol[0] << ", " << vsol[1] << ", " << vsol[2] << 
	endl;
      cout << "me:        " << mu << ", " << me << endl;
      cerr << "User time: " << tim1.user() << endl;
      lnl.reset();
      for (uInt i=0; i<n; i++) {
	Double A = vsol[0]*exp(-(((x[i]-vsol[1])/vsol[2])*
				  ((x[i]-vsol[1])/vsol[2])));
	Double b = 2*(x[i]-vsol[1])/vsol[2];
	un[0] = A/vsol[0];
	un[1] = b*A/vsol[2];
	un[2] = b*b*A/2/vsol[2];
	kn[0] = y[i]-A;
	lnl.makeNorm(unit, 1.0, kn[0]);
      }
      lnl.invert(nr);
      Vector<Double> sold(3);
      VectorSTLIterator<Double> soldit(sold);
      Double covd[9];
      lnl.solve(soldit);
      mu = lnl.getSD();
      me = lnl.getWeightedSD();
      lnl.getCovariance(covd);
      cout << "Sol:       " << soldit[0] << ", " << soldit[1] << ", " <<
	soldit[2] << endl;
      cout << "me:        " << mu << ", " << me << endl;
      for (uInt i=0; i<9; i += 3) {
	cout << "Covar: " << Y(covd[i+0], 1e-16) << ", " <<
	  Y(covd[i+1], 1e-16) << ", " << Y(covd[i+2], 1e-16) << endl;
      }
      cerr << "User time: " << tim1.user() << endl;
    }
    
    cout << "Non-linear---- Float --------" << endl;
    {
      LSQaips lnl(3);
      const uInt n=100;
      Double x[n];
      Double y[n];
      for (uInt i=0; i<n; i++) {
	x[i] = i*0.5;
	y[i] = 20*exp(-(((x[i]-25)/4)*((x[i]-25)/4)));
      }
      Float sol[3] = {10, 20, 2};
      Vector<Float> vsol(3);
      VectorSTLIterator<Float> solit(vsol);
      std::copy(sol, sol+3, solit);
      Float muf, mef;
      Vector<Double> un(3);
      VectorSTLIterator<Double> unit(un);
      Double kn[1];
      const Int Niter = 30;
      lnl.setMaxIter(Niter);
      uInt nr;
      Timer tim1;
      tim1.mark();
      while (!lnl.isReady()) {
	for (uInt i=0; i<n; i++) {
	  Double A = vsol[0]*exp(-(((x[i]-vsol[1])/vsol[2])*
				    ((x[i]-vsol[1])/vsol[2])));
	  Double b = 2*(x[i]-vsol[1])/vsol[2];
	  un[0] = A/vsol[0];
	  un[1] = b*A/vsol[2];
	  un[2] = b*b*A/2/vsol[2];
	  kn[0] = y[i]-A;
	  lnl.makeNorm(unit, 1.0, kn[0]);
	}
	if (!lnl.solveLoop(nr, vsol)) {
	  cout << "Error in loop: " << nr << endl;
	  break;
	}
      }
      muf = lnl.getSD();
      mef = lnl.getWeightedSD();
    cout << "Iterations: " << lnl.nIterations() << endl;
    cout << "Ready:      " << lnl.readyText() << endl;
    cout << "Sol:       " << vsol[0] << ", " << vsol[1] << ", " << vsol[2] << 
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
