//# Sinusoid1D2.cc: specialized Sinusoid1D class for AutoDiff
//# Copyright (C) 1997,1998,2001,2002
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
#include <scimath/Functionals/Sinusoid1D.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>

//# Constructors

//# Operators
template<class T>
AutoDiff<T> Sinusoid1D<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg x) const {
  AutoDiff<T> tmp;
  if (param_p[AMPLITUDE].nDerivatives() > 0) tmp = param_p[AMPLITUDE];
  else if (param_p[PERIOD].nDerivatives() > 0) tmp = param_p[PERIOD];
  else if (param_p[X0].nDerivatives() > 0) tmp = param_p[X0];
  typename AutoDiff<T>::value_type arg =
    static_cast<typename AutoDiff<T>::value_type>(C::_2pi) *
    (x[0] - param_p[X0].value())/param_p[PERIOD].value();
  typename AutoDiff<T>::value_type cosarg = cos(arg);
  typename AutoDiff<T>::value_type sinarg = sin(arg);
  // Function value
  tmp.value() = param_p[AMPLITUDE].value() * cosarg;
  // get derivatives (assuming either all or none)
  if (tmp.nDerivatives()>0) {
    for (uInt j = 0; j < tmp.nDerivatives(); j++) tmp.deriv(j) = 0.0;
    // derivative wrt amplitude
    typename AutoDiff<T>::value_type dev = cosarg;
    if (param_p.mask(AMPLITUDE)) tmp.deriv(AMPLITUDE) = dev;
    // derivative wrt period
    dev = param_p[AMPLITUDE].value() * arg * sinarg / param_p[PERIOD].value();
    if (param_p.mask(PERIOD)) tmp.deriv(PERIOD) = dev;
    // derivative wrt x0
    dev = param_p[AMPLITUDE].value() *
      static_cast<typename AutoDiff<T>::value_type>(C::_2pi) *
      sinarg / param_p[PERIOD].value();
    if (param_p.mask(X0)) tmp.deriv(X0) = dev;
  };
  return tmp;
}

//# Member functions
