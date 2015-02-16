//# LELInterface.cc:  this defines LELInterface.cc
//# Copyright (C) 1997,1998,1999,2000,2003
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

#ifndef LATTICES_LELINTERFACE_TCC
#define LATTICES_LELINTERFACE_TCC

#include <casacore/lattices/LEL/LELInterface.h>
#include <casacore/lattices/LEL/LELUnary.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
LELInterface<T>::~LELInterface()
{}

template<class T>
void LELInterface<T>::setAttr (const LELAttribute& attr)
{
    attr_p = attr;
}

template<class T>
void LELInterface<T>::evalRef (LELArrayRef<T>& result,
			       const Slicer& section) const
{
    // For one reason or another gcc requires an explicit cast
    // for LELInterface<Bool>.
    eval ((LELArray<T>&)result, section);
}

template<class T>
LELArray<T> LELInterface<T>::getArray() const
{
  const IPosition& shp = shape();
  if (shp.nelements() == 0) {
    throw AipsError ("LELInterface::getArray: shape is unknown");
  }
  LELArray<T> result(shp);
  Slicer slc(IPosition(shp.nelements(),0), shp);
  eval (result, slc);
  return result;
}

template<class T>
Bool LELInterface<T>::replaceScalarExpr (CountedPtr<LELInterface<T> >& expr)
{
// Recursively prepare (optimize) a scalar subexpression
    Bool isInvalidScalar = expr->prepareScalarExpr();
// If the value is a valid scalar expression, replace it by its result
// (which can be an invalid scalar in itself).
    if (!isInvalidScalar  &&  expr->isScalar()) {
        LELScalar<T> tmp = expr->getScalar();
	if (tmp.mask()) {
	    expr = new LELUnaryConst<T> (tmp.value());
	} else {
	    isInvalidScalar = True;
	}
    }
// If the value is an invalid scalar expression, replace by scalar
// with false mask.
    if (isInvalidScalar) {
	expr = new LELUnaryConst<T>();
    }
    return isInvalidScalar;
}


template<class T>
Bool LELInterface<T>::lock (FileLocker::LockType, uInt)
{
    return True;
}
template<class T>
void LELInterface<T>::unlock()
{}
template<class T>
Bool LELInterface<T>::hasLock (FileLocker::LockType) const
{
    return True;
}
template<class T>
void LELInterface<T>::resync()
{}


} //# NAMESPACE CASACORE - END


#endif
