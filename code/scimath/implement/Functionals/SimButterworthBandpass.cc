//# NQSimButterworthBandpass.cc: Defines a Butterworth function
//# Copyright (C) 2000,2001
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
#include <aips/Functionals/NQSimButterworthBandpass.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>

//# Constructors
template <class T>
NQSimButterworthBandpass<T>::NQSimButterworthBandpass() :
  NQFunction1D<T>(4), nl_p(0), nh_p(0) {
  param_p[MINCUTOFF] = T(-1);
  param_p[MAXCUTOFF] = T(1);
  param_p[PEAK] = T(1);
}

template <class T>
NQSimButterworthBandpass<T>::NQSimButterworthBandpass(const uInt minord, 
						      const uInt maxord, 
						      const T &mincut, 
						      const T &maxcut, 
						      const T &center, 
						      const T &peak) :
  NQFunction1D<T>(4), nl_p(minord), nh_p(maxord) {
  param_p[MINCUTOFF] = mincut;
  param_p[MAXCUTOFF] = maxcut;
  param_p[CENTER] = center;
  param_p[PEAK] = peak;
}

template <class T>
NQSimButterworthBandpass<T>::
NQSimButterworthBandpass(const NQSimButterworthBandpass<T> &other) : 
  NQFunction1D<T>(other), nl_p(other.nl_p), nh_p(other.nh_p) {} 

template <class T>
NQSimButterworthBandpass<T>::~NQSimButterworthBandpass() {}

//# Operators
template<class T>
NQSimButterworthBandpass<T> &
NQSimButterworthBandpass<T>::
operator=(const NQSimButterworthBandpass<T> &other) {
  if (this != &other) {
    NQFunction1D<T>::operator=(other);
    nl_p = other.nl_p;
    nh_p = other.nh_p;
  };
  return *this;
}

template <class T>
T NQSimButterworthBandpass<T>::
eval(typename NQFunction1D<T>::FunctionArg x) const {
  // this does not reflect the true responses of Butterworth filters
  // calculate the low-pass portion
  T hp = 1;
  if (nh_p == 0) hp = T(1);
  else if (x[0] > param_p[CENTER]) {
    hp = T(1) / sqrt(T(1) + pow((x[0] - param_p[CENTER])/
				(param_p[MAXCUTOFF] - param_p[CENTER]),
				T(2*nh_p)));
  };
  // calculate the high-pass portion
  if (nl_p == 0) ;
  else if (x[0] < param_p[CENTER]) { 
    hp *= T(1) / sqrt(T(1) + pow((param_p[CENTER] - x[0])/
				(param_p[MINCUTOFF] - param_p[CENTER]),
				T(2*nl_p)));
  };  
  return param_p[PEAK]*hp;
}

