//# LELCondition.cc:  Class to make a mask from a condition 
//# Copyright (C) 1999
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


#include <trial/Lattices/LELCondition.h>
#include <trial/Lattices/LELArray.h>
#include <trial/Lattices/LELScalar.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Arrays/Array.h>
#include <aips/Exceptions/Error.h>

typedef CountedPtr<LELInterface<Bool> > lelcondition_gppbug1;


template <class T>
LELCondition<T>::LELCondition (const CountedPtr<LELInterface<T> >& expr,
	                       const CountedPtr<LELInterface<Bool> >& cond)
: pExpr_p (expr),
  pCond_p (cond)
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

   LELArray<Bool> condval(result.shape());
   pExpr_p->eval (result, section);
   pCond_p->eval (condval, section);
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
