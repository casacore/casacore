//# COWPtr.cc: this defines the Copy-On-Write-Pointer class.
//# Copyright (C) 1996
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
//#
//# $Id$

#ifndef CASA_COWPTR_TCC
#define CASA_COWPTR_TCC

#include <casacore/casa/Utilities/COWPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> COWPtr<T>::COWPtr(T *obj, Bool deleteIt, Bool readOnly)
: obj_p(obj, deleteIt), const_p(readOnly)
{
  // does nothing
}

template <class T> void COWPtr<T>::set(T *obj, Bool deleteIt, Bool readOnly)
{
  obj_p = CountedPtr<T>(obj, deleteIt);
  const_p = readOnly;
}

// make this a copy if more than one exists. 
template <class T> Bool COWPtr<T>::makeUnique()
{
  Bool val = False;
  if (const_p || obj_p.nrefs() > 1){
    T *tmp = new T;
    *tmp = *obj_p;
    // CountedPtr assignment operator takes care of deletion of old ptr.
    obj_p = CountedPtr<T>(tmp, True);
    const_p = False;
    val = True;
  } 
  return val;
}





} //# NAMESPACE CASACORE - END


#endif
