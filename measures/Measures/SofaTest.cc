//# SofaTest.cc: Wrapping of IAU SOFA Fortran routines and test class
//# Copyright (C) 2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Include files
#include <casacore/measures/Measures/SofaTest.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/iostream.h>
#include <limits.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
// Constructors 
SofaTest::SofaTest() :
  n_p(0), sum_p(0), sq_p(0), max_p(-1e30), min_p(1e30),
  hstep_p(0), hsize_p(HISTO_WIDTH), histo_p(0) {
  hwidth_p = 2*hsize_p;
  histo_p = new Int[hwidth_p];
  clear();
}

SofaTest::SofaTest(const SofaTest &other) :
  n_p(0), sum_p(0), sq_p(0), max_p(-1e30), min_p(1e30),
  hstep_p(0), hsize_p(HISTO_WIDTH), histo_p(0) {
  copy(other);
}

SofaTest::~SofaTest() {
  delete [] histo_p; histo_p = 0;
}

// Operators
SofaTest &SofaTest::operator=(const SofaTest &other) {
  if (this != &other) copy(other);
  return *this;
}

// Methods
void SofaTest::clear() {
  n_p = 0;
  sum_p = 0;
  sq_p = 0;
  max_p = -1e30;
  min_p = 1e30;
  hstep_p = 0;
  hwidth_p = 2*hsize_p;
  for (uInt i=0; i<hwidth_p; i++) histo_p[i] = 0;
}

void SofaTest::put(const Double in) {
  n_p++;
  sum_p += in;
  sq_p += in*in;
  max_p = max(in, max_p);
  min_p = min(in, min_p);
  if (hstep_p <= 0.0) hstep_p = 0.001/hsize_p/2.0;
  while (abs(in/hstep_p)>hsize_p) {
    hstep_p *= 2.0;
    for (uInt i=0; i<hsize_p/2; i++) {
      histo_p[hsize_p+i] = histo_p[hsize_p+2*i] + histo_p[hsize_p+2*i+1];
      histo_p[hsize_p-i-1] = histo_p[hsize_p-2*i-1] += histo_p[hsize_p-2*i-2];
    }
    for (uInt i=0; i<hsize_p/2; i++) {
      histo_p[hsize_p+hsize_p/2+i] = 0;
      histo_p[hsize_p-hsize_p/2-i-1] = 0;
    }
  }
  Int n=Int(floor(in/hstep_p)+hsize_p);
  if (n>=0 && n<Int(hwidth_p)) histo_p[n]++;
}

void SofaTest::show(ostream &os) {
  if (n_p == 0) {
    os << "No data present" << endl;
  } else {
    os << n_p << " points were accumulated" << endl;
    os << "with max = " << max_p << ", and min = " << min_p << endl;
    os << "and an average of " << sum_p/n_p <<
      " and a standard deviation of ";
    if (n_p == 1) os << 0.0;
    else os << sqrt(sq_p/(n_p-1));
    os << endl;
  }
}

void SofaTest::showHisto(ostream &os) {
  Int cnt[41];
  for (uInt i=0; i<41; i++) cnt[i]=0;
  Int n=Int(ceil(Double(hwidth_p)/40.));
  Int k=0;
  for (Int i=-20; i<20; i++) {
    for (Int j=hsize_p +i*n; j<Int(hsize_p +(i+1)*n); j++) {
      if (j>=0 && j<Int(hwidth_p)) cnt[k] += histo_p[j];
    }
    k++;
  }
  Double step = n*hstep_p;
  k=0;
  for (uInt i=0; i<41; i++) k = (cnt[i]>k) ? cnt[i] : k;
  n = Int(ceil(Double(k)/60.));
  if (n==0) n=1;
  os << endl << n << " counts per step; " << step << " value." << endl; 
  for (uInt i=0; i<41; i++) {
    if (i==19) os << " _";
    else os << " |";
    if (cnt[i] != 0) {
      for (Int j=0; j<cnt[i]/n; j++) os << "-";
    }
    os << "*" << endl;
  }
}

void SofaTest::copy(const SofaTest &other) {
  n_p = other.n_p;
  sum_p = other.sum_p;
  sq_p = other.sq_p;
  max_p = other.max_p;
  min_p = other.min_p;
  hstep_p = other.hstep_p;
  hsize_p = other.hsize_p;
  hwidth_p = 2*hsize_p;
  delete [] histo_p; histo_p = 0;
  histo_p = new Int[hwidth_p];
  for (uInt i=0; i<hwidth_p; i++) histo_p[i] = other.histo_p[i];
}

} //# NAMESPACE CASACORE - END

