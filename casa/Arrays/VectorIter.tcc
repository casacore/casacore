//# VectorIter.cc: Iterate a vector cursor through another array
//# Copyright (C) 1993,1994,1995
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

#ifndef CASA_VECTORITER_TCC
#define CASA_VECTORITER_TCC

#include<casacore/casa/Arrays/VectorIter.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> VectorIterator<T>::VectorIterator(Array<T> &a, uInt axis)
  : ArrayIterator<T>(a, IPosition(1,axis), True)
{
    // We need to ensure that ap points at a vector
    Vector<T> *vp = new Vector<T>(*this->ap_p); // reference
    delete this->ap_p;
    this->ap_p = vp;
}

} //# NAMESPACE CASACORE - END

#endif
