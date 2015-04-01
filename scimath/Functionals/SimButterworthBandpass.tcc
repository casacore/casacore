//# SimButterworthBandpass.cc: Defines a Butterworth function
//# Copyright (C) 2000,2001,2002,2003
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

#ifndef SCIMATH_SIMBUTTERWORTHBANDPASS_TCC
#define SCIMATH_SIMBUTTERWORTHBANDPASS_TCC

//# Includes
#include <casacore/scimath/Functionals/SimButterworthBandpass.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Containers/RecordInterface.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
template <class T>
SimButterworthBandpass<T>::SimButterworthBandpass() :
  Function1D<T>(4), nl_p(0), nh_p(0) {
  param_p[MINCUTOFF] = T(-1);
  param_p[MAXCUTOFF] = T(1);
  param_p[PEAK] = T(1);
}

template <class T>
SimButterworthBandpass<T>::SimButterworthBandpass(const uInt minord, 
						      const uInt maxord, 
						      const T &mincut, 
						      const T &maxcut, 
						      const T &center, 
						      const T &peak) :
  Function1D<T>(4), nl_p(minord), nh_p(maxord) {
  param_p[MINCUTOFF] = mincut;
  param_p[MAXCUTOFF] = maxcut;
  param_p[CENTER] = center;
  param_p[PEAK] = peak;
}

template <class T>
SimButterworthBandpass<T>::
SimButterworthBandpass(const RecordInterface& gr, T mincut, T maxcut, 
		       T center, T peak) :
  Function1D<T>(4), nl_p(0), nh_p(0) 
{
    setMode(gr);
    param_p[MINCUTOFF] = mincut;
    param_p[MAXCUTOFF] = maxcut;
    param_p[CENTER] = center;
    param_p[PEAK] = peak;
}


template <class T>
SimButterworthBandpass<T>::
SimButterworthBandpass(const SimButterworthBandpass<T> &other) : 
  Function1D<T>(other), nl_p(other.nl_p), nh_p(other.nh_p) {} 

template <class T>
SimButterworthBandpass<T>::~SimButterworthBandpass() {}

//# Operators
template<class T>
SimButterworthBandpass<T> &
SimButterworthBandpass<T>::
operator=(const SimButterworthBandpass<T> &other) {
  if (this != &other) {
    Function1D<T>::operator=(other);
    nl_p = other.nl_p;
    nh_p = other.nh_p;
  }
  return *this;
}

template <class T>
T SimButterworthBandpass<T>::
eval(const typename  FunctionTraits<T>::ArgType *x) const {
  // this does not reflect the true responses of Butterworth filters
  // calculate the low-pass portion
  T hp = T(1);
  if (x[0] > param_p[CENTER]) {
    hp = T(1) / sqrt(T(1) + pow((x[0] - param_p[CENTER])/
				(param_p[MAXCUTOFF] - param_p[CENTER]),
				T(2*nh_p)));
  }
  // calculate the high-pass portion
  if (x[0] < param_p[CENTER]) { 
    hp *= T(1) / sqrt(T(1) + pow((param_p[CENTER] - x[0])/
				(param_p[MINCUTOFF] - param_p[CENTER]),
				T(2*nl_p)));
  }  
  return param_p[PEAK]*hp;
}

template <class T>
Bool SimButterworthBandpass<T>::hasMode() const { return True; }

template <class T>
void SimButterworthBandpass<T>::setMode(const RecordInterface& in) {
    uInt order=0;

    // min order
    if (in.isDefined(String("minOrder"))) {
	RecordFieldId fld("minOrder");
	if (in.type(in.idToNumber(fld)) == TpInt) {
	    Int tmp;
	    in.get(fld, tmp);
	    order = static_cast<uInt>(abs(tmp));
	}
	else if (in.type(in.idToNumber(fld)) == TpUInt) {
	    in.get(fld, order);
	}
	setMinOrder(order);
    }

    // max order
    if (in.isDefined(String("maxOrder"))) {
	RecordFieldId fld("maxOrder");
	if (in.type(in.idToNumber(fld)) == TpInt) {
	    Int tmp;
	    in.get(fld, tmp);
	    order = static_cast<uInt>(abs(tmp));
	}
	else if (in.type(in.idToNumber(fld)) == TpUInt) {
	    in.get(fld, order);
	}
	setMaxOrder(order);
    }
}

template <class T>
void SimButterworthBandpass<T>::getMode(RecordInterface& out) const {
    out.define(RecordFieldId("minOrder"), getMinOrder());
    out.define(RecordFieldId("maxOrder"), getMaxOrder());
}


} //# NAMESPACE CASACORE - END


#endif
