//# HyperPlane.cc: Defines HyperPlane 
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

#ifndef SCIMATH_CONSTANTND_TCC
#define SCIMATH_CONSTANTND_TCC

//# Includes
#include <casacore/scimath/Functionals/ConstantND.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Operators
template<class T>
T ConstantND<T>::eval(typename Function<T>::FunctionArg) const {
	return param_p[0];
}

template<class T>
AutoDiff<T> ConstantND<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg) const {
  AutoDiff<T> tmp = param_p[0];
  return tmp;
}


//# Member functions

} //# NAMESPACE CASACORE - END


#endif
