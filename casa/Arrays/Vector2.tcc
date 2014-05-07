//# Vector2.cc: Definitions for STL vector related methods
//# Copyright (C) 2002
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

#include <casa/Arrays/Vector.h>
#include <casa/stdvector.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template<class T>
template<class U>
Vector<T>::Vector(const vector<T, U> &other)
  : Array<T>(IPosition(1, other.size())) {
  uInt i=0;
  for (typename vector<T, U>::const_iterator pos=other.begin();
       pos != other.end(); pos++) (*this)[i++] = *pos;
}

template<class T>
template<class U>
void Array<T>::tovector(vector<T, U> &out) const {
  Bool deleteIt;
  const T *stor = this->getStorage(deleteIt);
  out.assign(stor, stor+nelements());
  this->freeStorage(stor, deleteIt);
}  

} //# NAMESPACE CASA - END

