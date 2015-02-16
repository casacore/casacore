//# LSQaips.cc: Interface for Casacore Vectors in least squares fitting
//# Copyright (C) 1998,1999,2000,2001,2004,2006
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

#ifndef SCIMATH_LSQAIPS_TCC
#define SCIMATH_LSQAIPS_TCC

//# Includes
#include <casacore/scimath/Fitting/LSQaips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Destructor

//# Member functions

template <class U>
Bool LSQaips::getCovariance(Array<U> &covar) {
  if (!invertRect()) return False;
  covar.resize();
  uInt n = nUnknowns()/LSQTraits<U>::size;
  covar.resize(IPosition(2, n, n));
  return LSQFit::getCovariance(covar.data());
}

template <class U>
Bool LSQaips::solveLoop(Double &fit, uInt &nRank,
			Vector<U> &sol, Bool doSVD) {
  VectorSTLIterator<U> solit(sol);
  return LSQFit::solveLoop(fit, nRank, solit, doSVD);
}

template <class U>
Bool LSQaips::solveLoop(uInt &nRank,
			Vector<U> &sol, Bool doSVD) {
  VectorSTLIterator<U> solit(sol);
  return LSQFit::solveLoop(nRank, solit, doSVD);
}




} //# NAMESPACE CASACORE - END


#endif
