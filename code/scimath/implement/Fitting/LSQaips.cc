//# LSQaips.cc: Interface for aips++ Vectors in least squares fitting
//# Copyright (C) 1998,1999,2000,2001,2004
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
#include <aips/Fitting/LSQaips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/IPosition.h>

//# Constructors
LSQaips::LSQaips(uInt nUnknowns, uInt nConstraints)
  : LSQFit(nUnknowns, nConstraints) {;}

LSQaips::LSQaips(uInt nUnknowns, const LSQReal &, uInt nConstraints)
  : LSQFit(nUnknowns, LSQReal(), nConstraints) {;}

LSQaips::LSQaips(uInt nUnknowns, const LSQComplex &, uInt nConstraints)
  : LSQFit(nUnknowns, LSQComplex(), nConstraints) {;}

LSQaips::LSQaips()
  : LSQFit() {;}

LSQaips::LSQaips(const LSQaips &other) 
  : LSQFit(other) {;}

LSQaips &LSQaips::operator=(const LSQaips &other) {
  if (this != &other) LSQFit::operator=(other);
  return *this;
}

//# Destructor
LSQaips::~LSQaips() {;}

//# Member functions

template <class U>
Bool LSQaips::getCovariance(Array<U> &covar) {
  if (!invertRect()) return False;
  IPosition iw(2, n_p, n_p);
  if (!(covar.shape().conform(iw) && covar.shape() == iw)) {
    covar.resize();
    covar.resize(iw);
  };
  return LSQFit::getCovariance(covar.data());
}

// Template definitions

template Bool LSQaips::getCovariance<Double>(Array<Double> &);
template Bool LSQaips::getCovariance<Float>(Array<Float> &);
template Bool LSQaips::getCovariance<DComplex>(Array<DComplex> &);
template Bool LSQaips::getCovariance<std::complex<Float> >
(Array<std::complex<Float> > &);

