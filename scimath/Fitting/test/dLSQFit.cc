//# dLSQBase.cc -- LSQ demonstration
//# Copyright (C) 1999,2000,2001,2004
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
#include <casacore/scimath/Fitting/LSQFit.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>

void showdt(const LSQFit &lsq, Int n) {
  uInt nun, np, ncon, ner, rk, *piv;
  Double *norm, *known, *constr, *err, *sEq, *sol, prec, nonlin;
  lsq.debugIt(nun, np, ncon, ner, rk, norm, known, constr, err,
	      piv, sEq, sol, prec, nonlin);
  if (norm) {
    for (Int i=0; i<(n*(n+1))/2; i++) {
      std::cout << "  Norm" << i << ": " << norm[i];
    }
  }
  std::cout << std::endl;
  if (err) {
    for (Int i1=0; i1<4; i1++) {
      std::cout << "  Erra" << i1 << ": " << err[i1];
    }
  }
  std::cout << std::endl;
  if (known) {
    for (Int i2=0; i2<n; i2++) {
      std::cout << "  Know" << i2 << ": " << known[i2];
    }
  }
  std::cout << std::endl;
  if (piv) {
    for (Int i3=0; i3<n; i3++) {
      std::cout << "  Pivo" << i3 << ": " << piv[i3];
    }
  }
  std::cout << std::endl;
  std::cout << "  Rank: " << rk << std::endl;
  std::cout << "---------------------------------------------------" << std::endl;
}

int main() {
  std::cout << "Test LSQ with 1 unknown" << std::endl;
  std::cout << "---------------------------------------------------" << std::endl;
  LSQFit lsq1(1);
  uInt nr1;
  Double val1[] = {2, 1, 6};
  Double sol1[1];
  showdt(lsq1,1);
  lsq1.makeNorm(&val1[0], val1[1], val1[2]);
  showdt(lsq1,1);
  std::cout << "Invert = " << lsq1.invert(nr1);
  std::cout << ", rank=" << nr1 << std::endl;
  showdt(lsq1,1);
  lsq1.solve(sol1);
  std::cout << "Sol: " << sol1[0] << ", " << lsq1.getSD() << ", " <<
    lsq1.getWeightedSD() << std::endl;
  std::cout << "Expected: " << "3, 0, 0" << std::endl;
  std::cout << "---------------------------------------------------" << std::endl;
  LSQFit lsq3(1);
  {
    uInt nr1;
    Double val12[] = {3, 1, 12};
    lsq3.makeNorm(&val1[0], val1[1], val1[2]);
    lsq3.makeNorm(&val12[0], val12[1], val12[2]);
    showdt(lsq3,1);
    std::cout << "Invert = " << lsq3.invert(nr1);
    std::cout << ", rank=" << nr1 << std::endl;
    showdt(lsq3,1);
    lsq3.solve(sol1);
    std::cout << "Sol: " << sol1[0] << ", " << lsq3.getSD() << ", " <<
      lsq3.getWeightedSD() << std::endl;
    std::cout << "Expected: " << "3.69, 1.66, 1.66" << std::endl;
    std::cout << "---------------------------------------------------" << std::endl;
  }
  std::cout << "Test LSQFit with 2 unknowns" << std::endl;
  std::cout << "---------------------------------------------------" << std::endl;
  LSQFit lsq2(2);
  {
    uInt nr1;
    Double val1[] = {1, 1, 1, 5};
    Double val12[] = {1, -1, 1, -1};
    Double sol1[2];
    showdt(lsq2,2);
    lsq2.makeNorm(&val1[0], val1[2], val1[3]);
    lsq2.makeNorm(&val12[0], val12[2], val12[3]);
    showdt(lsq2,2);
    std::cout << "Invert = " << lsq2.invert(nr1);
    std::cout << ", rank=" << nr1 << std::endl;
    showdt(lsq2,2);
    lsq2.solve(sol1);
    std::cout << "Sol: " << sol1[0] << ", " << lsq2.getSD() << ", " <<
      lsq2.getWeightedSD() << std::endl;
    std::cout << "Sol: " << sol1[1] << ", " << lsq2.getSD() << ", " <<
      lsq2.getWeightedSD() << std::endl;
    std::cout << "Expected: " << "2, 0, 0" << std::endl;
    std::cout << "Expected: " << "3, 0, 0" << std::endl;
    std::cout << "---------------------------------------------------" << std::endl;
  }
  std::cout << "Test LSQFit with 6 unknowns" << std::endl;
  std::cout << "---------------------------------------------------" << std::endl;
  LSQFit lsq5(6);
  {
    uInt nr1;
    Double val1[6];
    Double val12[512];
    for (Int j=0; j<512; j++) val12[j] = 1+2*j;
    Double sol1[6];
    /// showdt(lsq5,6);
    for (Int j0=0; j0<512; j0++) {
      val1[0] = 1;
      for (uInt j1=1; j1<6; j1++) val1[j1] = val1[j1-1]*j0;
      lsq5.makeNorm(&val1[0], 1.0, val12[j0]);
    }
    /// showdt(lsq5,6);
    std::cout << "Invert = " << lsq5.invert(nr1);
    std::cout << ", rank=" << nr1 << std::endl;
    ///     showdt(lsq5,6);
    lsq5.solve(sol1);
    for (uInt i=0; i<6; i++) { 
      std::cout << "Sol" << i << ": " <<
	sol1[i] << ", " << lsq5.getSD() << ", " << lsq5.getWeightedSD() << std::endl;
    }
    /// std::cout << "Expected: " << "2, 0, 0" << std::endl;
    std::cout << "---------------------------------------------------" << std::endl;
  }
  return 0;
}
