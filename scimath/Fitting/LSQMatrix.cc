//# LSQMatrix.cc: Support class for the LSQ package
//# Copyright (C) 2004
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

//# Includes
#include <casacore/scimath/Fitting/LSQMatrix.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
LSQMatrix::LSQMatrix() 
  : n_p(0),
    len_p(0), nm1_p(0), n2m1_p(0), n2p1_p(0),
    trian_p(0) {}

LSQMatrix::LSQMatrix(uInt n)
  : n_p(n),
    len_p(0), nm1_p(0), n2m1_p(0), n2p1_p(0),
    trian_p(0) {
  init();
  clear();
}

LSQMatrix::LSQMatrix(uInt n, Bool)
  : n_p(2*n),
    len_p(0), nm1_p(0), n2m1_p(0), n2p1_p(0),
    trian_p(0) {
  init();
  clear();
}

LSQMatrix::LSQMatrix(const LSQMatrix &other) 
  : RecordTransformable(),
    n_p(other.n_p),
    len_p(0), nm1_p(0), n2m1_p(0), n2p1_p(0),
    trian_p(0) {
  init();
  copy(other);
}

LSQMatrix &LSQMatrix::operator=(const LSQMatrix &other) {
  if (this != &other) {
    deinit();
    n_p    = other.n_p; 
    init();
    copy(other);
  }
  return *this;
}

//# Destructor
LSQMatrix::~LSQMatrix() {
  deinit();
}

void LSQMatrix::init() {
  if (n_p) {
    len_p    = (n_p*(n_p+1))/2;
    nm1_p    = n_p-1;
    n2m1_p   = 2*n_p-1;
    n2p1_p   = 2*n_p+1;
    trian_p  = new Double[len_p];
  } else {
    len_p    = 0;
    nm1_p    = 0;
    n2m1_p   = 0;
    n2p1_p   = 0;
    trian_p  = 0;
  }
}

void LSQMatrix::clear() {
  std::fill_n(trian_p, len_p, 0.0);
}

void LSQMatrix::deinit() {
  delete [] trian_p;	trian_p=0;
}

void LSQMatrix::set(uInt n) {
  deinit();
  n_p = n;
  init();
  clear();
}

void LSQMatrix::set(uInt n, Bool) {
  deinit();
  n_p = 2*n;
  init();
  clear();
}

void LSQMatrix::copy(const LSQMatrix &other) {
  if (!trian_p && len_p) trian_p = new Double[len_p];
  std::copy(other.trian_p, other.trian_p+len_p, trian_p);
}

//# Member functions

void LSQMatrix::doDiagonal(uInt n) {
  for (uInt i=0; i<n; ++i) {
    Double *j = diag(i);
    if (*j == 0.0) *j = 1.0;
  }
}
  
void LSQMatrix::mulDiagonal(uInt n, Double fac) {
  fac += 1.0;
  for (uInt i=0; i<n; ++i) *diag(i) *= fac;
}

  void LSQMatrix::addDiagonal(uInt n, Double fac) {
    for (uInt i=0; i<n; ++i) *diag(i) += fac;
  }

  Double LSQMatrix::maxDiagonal(uInt n) {
    Double x=0;
    for (uInt i=0; i<n; ++i) x = std::max(x, std::abs(*diag(i)));
    return x;
  }

} //# NAMESPACE CASACORE - END

