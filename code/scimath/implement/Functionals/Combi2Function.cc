//# NQCombi2Function.cc:  Combination of Functions AutoDiff specialization
//# Copyright (C) 2001
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
#include <aips/Functionals/NQCombiFunction.h>

//# Constructors

//# Operators
template<class T>
AutoDiff<T> NQCombiFunction<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg x) const {
  AutoDiff<T> tmp(0);
  for (uInt i=0; i<nparameters(); ++i) {
    if (param_p[i].nDerivatives() > 0) {
      tmp = param_p[i];
      break;
    };
  };
  for (uInt j=0; j<tmp.nDerivatives(); j++) tmp.deriv(j) = 0.0;
  // function value
  for (uInt i = 0; i< nparameters(); ++i) {
    T v = (function(i))(x).value();
    tmp.value() += param_p[i].value()*v;
    // get derivatives (assuming either all or none)
    if (tmp.nDerivatives()>0 && param_p.mask(i)) tmp.deriv(i) = v;
  };
  return tmp;
}

//# Member functions
template<class T>
Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
*NQCombiFunction<AutoDiff<T> >::cloneAD() const {
  Function<typename FunctionTraits<AutoDiff<T> >::DiffType> *t =
    new NQCombiFunction<typename FunctionTraits<AutoDiff<T> >::DiffType>();
  for (uInt i=0; i<nFunctions(); ++i) {
    dynamic_cast<NQCombiFunction<typename FunctionTraits<AutoDiff<T> >::
      DiffType> *>(t)->addFunction(*(function(i).cloneAD()));
  };
  return t;
}

template<class T>
Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
*NQCombiFunction<AutoDiff<T> >::cloneBase() const {
  Function<typename FunctionTraits<AutoDiff<T> >::BaseType> *t =
    new NQCombiFunction<typename FunctionTraits<AutoDiff<T> >::BaseType>();
  for (uInt i=0; i<nFunctions(); ++i) {
    dynamic_cast<NQCombiFunction<typename FunctionTraits<AutoDiff<T> >::
      BaseType> *>(t)->addFunction(*(function(i).cloneBase()));
  };
  return t;
}
