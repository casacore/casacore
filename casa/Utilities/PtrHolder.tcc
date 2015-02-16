//# PtrHolder.cc: Hold pointers to be deleted when exceptions are thrown
//# Copyright (C) 1994,1995,2000
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

#ifndef CASA_PTRHOLDER_TCC
#define CASA_PTRHOLDER_TCC
//#

#include <casacore/casa/Utilities/PtrHolder.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> PtrHolder<T>::PtrHolder()
: ptr_p     (0),
  isCarray_p(False)
{}

template<class T> PtrHolder<T>::PtrHolder (T *pointer, Bool isCarray)
: ptr_p     (pointer),
  isCarray_p(isCarray)
{}

template<class T> void PtrHolder<T>::set (T *pointer, Bool isCarray, 
					  Bool deleteCurrentPtr)
{
    if (deleteCurrentPtr) {
	delete_pointer_if_necessary();
    }
    ptr_p = pointer;
    isCarray_p = isCarray;
}

template<class T> void PtrHolder<T>::clear (Bool deleteCurrentPtr)
{
    if (deleteCurrentPtr) {
	delete_pointer_if_necessary();
    }
    ptr_p = 0;
}

template<class T> void PtrHolder<T>::delete_pointer_if_necessary()
{
    if (ptr_p) {
	if (isCarray_p) {
	    delete [] ptr_p;
	} else {
	    delete ptr_p;
	}
	ptr_p = 0;
    }
}

template<class T> PtrHolder<T>::~PtrHolder()
{
    delete_pointer_if_necessary();
}

} //# NAMESPACE CASACORE - END


#endif
