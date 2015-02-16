//# MatrixIter.cc: Iterate a matrix cursor through another array
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

#ifndef CASA_MATRIXITER_TCC
#define CASA_MATRIXITER_TCC

#include<casacore/casa/Arrays/MatrixIter.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> MatrixIterator<T>::MatrixIterator(Array<T> &a)
: ArrayIterator<T>(a, 2)
{
    // We need to ensure that ap points at a Matrix
    Matrix<T> *mp = new Matrix<T>(*this->ap_p); // reference
    delete this->ap_p;
    this->ap_p = mp;
}

template<class T> MatrixIterator<T>::MatrixIterator(Array<T> &a,
						    uInt cursorAxis1,
						    uInt cursorAxis2)
: ArrayIterator<T>(a, IPosition(1,cursorAxis1, cursorAxis2), True)
{
    // We need to ensure that ap points at a Matrix
    Matrix<T> *mp = new Matrix<T>(*this->ap_p); // reference
    delete this->ap_p;
    this->ap_p = mp;
}

} //# NAMESPACE CASACORE - END

#endif
