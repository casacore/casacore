//# LELConvert.cc:  LELConvert.cc
//# Copyright (C) 1997,1998,1999,2000,2001
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

#ifndef LATTICES_LELCONVERT_TCC
#define LATTICES_LELCONVERT_TCC


#include <casacore/lattices/LEL/LELConvert.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/LEL/LELScalar.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/ConvertScalar.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T, class F>
LELConvert<T,F>::LELConvert(const CountedPtr<LELInterface<F> >& expr)
: pExpr_p (expr)
//
// F is the type we are converting from
// T is the type we are converting to.
//
{
   this->setAttr (expr->getAttribute());

#if defined(AIPS_TRACE)
   cout << "LELConvert:: constructor" << endl;
#endif
}


template <class T, class F>
LELConvert<T,F>::~LELConvert()
{
#if defined(AIPS_TRACE)
   cout << "LELConvert:: destructor" << endl;
#endif
}


template <class T, class F>
void LELConvert<T,F>::eval (LELArray<T>& result,
			    const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELConvert::eval" << endl;
#endif

   LELArrayRef<F> temp(result.shape());
   pExpr_p->evalRef (temp, section);
   result.setMask (temp);
   convertArray (result.value(), temp.value());
}


template <class T, class F>
LELScalar<T> LELConvert<T,F>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELConvert::getScalar" << endl;
#endif

   T tmp;
   convertScalar (tmp, pExpr_p->getScalar().value());
   return tmp;
}


template <class T, class F>
Bool LELConvert<T,F>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELConvert::prepare" << endl;
#endif

   return LELInterface<F>::replaceScalarExpr (pExpr_p);
}


template <class T, class F>
String LELConvert<T,F>::className() const
{
   return "LELConvert";
}


template <class T, class F>
Bool LELConvert<T,F>::lock (FileLocker::LockType type, uInt nattempts)
{
  return pExpr_p->lock (type, nattempts);
}
template <class T, class F>
void LELConvert<T,F>::unlock()
{
    pExpr_p->unlock();
}
template <class T, class F>
Bool LELConvert<T,F>::hasLock (FileLocker::LockType type) const
{
    return pExpr_p->hasLock (type);
}
template <class T, class F>
void LELConvert<T,F>::resync()
{
    pExpr_p->resync();
}

} //# NAMESPACE CASACORE - END


#endif
