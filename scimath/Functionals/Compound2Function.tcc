//# Compound2Function.cc:  Compound of functions AutoDiff specialization
//# Copyright (C) 2001,2002,2004
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

#ifndef SCIMATH_COMPOUND2FUNCTION_TCC
#define SCIMATH_COMPOUND2FUNCTION_TCC

//# Includes
#include <casacore/scimath/Functionals/CompoundFunction.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Operators
template <class T>
AutoDiff<T> CompoundFunction<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg x) const {
  if (this->parset_p) fromParam_p();
  AutoDiff<T> tmp(T(0), this->nparameters());
  tmp.value() = 0;
  for (uInt j=0; j<tmp.nDerivatives(); j++) tmp.deriv(j) = 0.0;
  // function value
  for (uInt i = 0; i< this->nFunctions(); ++i) {
    AutoDiff<T> t = this->function(i)(x);
    tmp.value() += t.value();
    for (uInt j=0; j<t.nDerivatives(); ++j) {
      tmp.deriv(this->paroff_p[i]+j) += t.deriv(j);
    }
  }
  return tmp;
}

//# Member functions
template <class T>
uInt CompoundFunction<AutoDiff<T> >::
addFunction(const Function<AutoDiff<T> > &newFunction) {
  uInt nf = CompoundParam<AutoDiff<T> >::addFunction(newFunction);
  toParam_p();
  return nf;
}

template <class T>
void CompoundFunction<AutoDiff<T> >::fromParam_p() const {
  if (this->parset_p) {
    for (uInt i=0; i<this->nparameters(); ++i) {
      uInt k = this->functionPtr_p[this->funpar_p[i]]->nparameters();
      uInt l = (*this->functionPtr_p[this->funpar_p[i]])[this->locpar_p[i]].nDerivatives();
      // Set correct number of derivatives in sub-functions
      if (this->param_p[i].nDerivatives() < this->paroff_p[this->funpar_p[i]] + k) {
	if (l != 0) (*this->functionPtr_p[this->funpar_p[i]])[this->locpar_p[i]] = AutoDiff<T>();
	l = 0;
      } else if (k != l) {
	(*this->functionPtr_p[this->funpar_p[i]])[this->locpar_p[i]] = AutoDiff<T>(T(0), k);
	l = k;
      }
      // Set the parameter data
      for (uInt j=0; j<l; ++j) {
	(*this->functionPtr_p[this->funpar_p[i]])[this->locpar_p[i]].deriv(j) =
	  this->param_p[i].deriv(j+this->paroff_p[this->funpar_p[i]]);
      }
      (*this->functionPtr_p[this->funpar_p[i]])[this->locpar_p[i]].value() = this->param_p[i].value();
      this->functionPtr_p[this->funpar_p[i]]->mask(this->locpar_p[i]) = this->param_p.mask(i);
    }
    this->parset_p = False;
  }
}

template <class T>
void CompoundFunction<AutoDiff<T> >::toParam_p() {
  for (uInt i=0; i<this->nparameters(); ++i) {
  // Set derivatives
    if (this->nparameters() != this->param_p[i].nDerivatives()) {
      this->param_p[i] = AutoDiff<T>(this->param_p[i].value(), this->nparameters());
    }
    uInt k = this->functionPtr_p[this->funpar_p[i]]->nparameters();
    uInt l = (*this->functionPtr_p[this->funpar_p[i]])[this->locpar_p[i]].nDerivatives();
    // Set correct number of derivatives in sub-functions
      if (this->param_p[i].nDerivatives() < this->paroff_p[this->funpar_p[i]] + k) {
	if (l != 0) (*this->functionPtr_p[this->funpar_p[i]])[this->locpar_p[i]] = AutoDiff<T>();
	l = 0;
      } else if (k != l) {
	(*this->functionPtr_p[this->funpar_p[i]])[this->locpar_p[i]] = AutoDiff<T>(T(0), k);
	l = k;
      }
    // Set the parameter data
    for (uInt j=0; j<l; ++j) {
      this->param_p[i].deriv(j+this->paroff_p[this->funpar_p[i]]) =
	(*this->functionPtr_p[this->funpar_p[i]])[this->locpar_p[i]].deriv(j);
    }
    this->param_p[i].value() = (*this->functionPtr_p[this->funpar_p[i]])[this->locpar_p[i]].value();
    this->param_p.mask(i) = this->functionPtr_p[this->funpar_p[i]]->mask(this->locpar_p[i]);
  }
}

} //# NAMESPACE CASACORE - END


#endif
