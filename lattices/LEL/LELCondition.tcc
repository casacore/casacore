//# LELCondition.cc:  Class to make a mask from a condition 
//# Copyright (C) 1999,2000
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

#ifndef LATTICES_LELCONDITION_TCC
#define LATTICES_LELCONDITION_TCC


#include <casacore/lattices/LEL/LELCondition.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/LEL/LELScalar.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
LELCondition<T>::LELCondition (const CountedPtr<LELInterface<T> >& expr,
	                       const CountedPtr<LELInterface<Bool> >& cond)
{
#if defined(AIPS_TRACE)
   cout << "LELCondition:: constructor" << endl;
#endif
   // The operands have to be lattices.
   if (expr->isScalar()  ||  cond->isScalar()) {
      throw (AipsError ("LELCondition: when using the [] operator, its "
                        "operands cannot be scalars"));
   }
   // Form the attributes (which also checks if both operands conform).
   LELAttribute attr (expr->getAttribute(), cond->getAttribute());
   // The result is always masked, since the condition forms a mask.
   setAttr (LELAttribute (True, attr.shape(), attr.tileShape(),
	    attr.coordinates()));
   // Fill these variables here, so an exception in setAttr does
   // not leave them undestructed.
   pExpr_p = expr;
   pCond_p = cond;
}

template <class T>
LELCondition<T>::~LELCondition()
{
#if defined(AIPS_TRACE)
   cout << "LELCondition:: destructor" << endl;
#endif
}


template <class T>
void LELCondition<T>::eval (LELArray<T>& result,
			    const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELCondition::eval" << endl;
#endif

   LELArrayRef<Bool> condval(result.shape());
   pExpr_p->eval (result, section);
   pCond_p->evalRef (condval, section);
   result.combineMask (condval);
   result.combineMask (condval.value());
}


template <class T>
LELScalar<T> LELCondition<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELCondition::getScalar" << endl;
#endif

// This should never be called, because the operands are no scalars.
   return pExpr_p->getScalar().value();
}


template <class T>
Bool LELCondition<T>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELCondition::prepare" << endl;
#endif

   if (LELInterface<T>::replaceScalarExpr (pExpr_p)) {
      return True;
   }
   if (LELInterface<Bool>::replaceScalarExpr (pCond_p)) {
      return True;
   }
   return False;
}


template <class T>
String LELCondition<T>::className() const
{
   return "LELCondition";
}


template<class T>
Bool LELCondition<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  if (! pExpr_p->lock (type, nattempts)) {
    return False;
  }
  return pCond_p->lock (type, nattempts);
}
template<class T>
void LELCondition<T>::unlock()
{
    pExpr_p->unlock();
    pCond_p->unlock();
}
template<class T>
Bool LELCondition<T>::hasLock (FileLocker::LockType type) const
{
    return pExpr_p->hasLock (type)  &&   pCond_p->hasLock (type);
}
template<class T>
void LELCondition<T>::resync()
{
    pExpr_p->resync();
    pCond_p->resync();
}

} //# NAMESPACE CASACORE - END


#endif
