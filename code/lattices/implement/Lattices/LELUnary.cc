//# LELUnary.cc:  this defines templated classes in LELUnary.h
//# Copyright (C) 1997
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

#include <trial/Lattices/LELUnary.h>
#include <trial/Lattices/PixelRegion.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h> 



template <class T>
LELUnaryConst<T>::LELUnaryConst(const T val)
: val_p(val)
{
   setAttr (LELAttribute(True, IPosition()));

#if defined(AIPS_TRACE)
   cout << "LELUnaryConst:: constructor" << endl;
#endif
}

template <class T>
LELUnaryConst<T>::~LELUnaryConst()
{
#if defined(AIPS_TRACE)
   cout << "LELUnaryConst:: destructor " << endl;
#endif
}

template <class T>
void LELUnaryConst<T>::eval(Array<T>&,
			    const PixelRegion&) const
{
   throw (AipsError ("LELUnaryConst::eval cannot be used"));
}

template <class T>
T LELUnaryConst<T>::getScalar() const
{
   return val_p;
}

template <class T>
void LELUnaryConst<T>::prepare()
{}

template <class T>
String LELUnaryConst<T>::className() const
{
   return String("LELUnaryConst");
}



template <class T>
LELUnary<T>::LELUnary(const LELUnaryEnums::Operation op,
		      const CountedPtr<LELInterface<T> >& pExpr)
: op_p(op), pExpr_p(pExpr)
{
   setAttr(pExpr->getAttribute());

#if defined(AIPS_TRACE)
   cout << "LELUnary:: constructor" << endl;
#endif
}

template <class T>
LELUnary<T>::~LELUnary()
{
#if defined(AIPS_TRACE)
   cout << "LELUnary:: destructor " << endl;
#endif
}


template <class T>
void LELUnary<T>::eval(Array<T>& result,
		       const PixelRegion& region) const
{
#if defined(AIPS_TRACE)
   cout << "LELUnary:: eval " << endl;
#endif

// Get the value and apply the unary operation
   pExpr_p->eval(result, region);
   switch(op_p) {
   case LELUnaryEnums::MINUS :
   {
      Array<T> tmp(-result);
      result.reference(tmp);
      break;
   }
   default:
      throw(AipsError("LELUnary: unknown operation"));
   }
}

template <class T>
T LELUnary<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELUnary::getScalar" << endl;
#endif

   switch(op_p) {
   case LELUnaryEnums::MINUS :
      return -(pExpr_p->getScalar());
   default:
      throw(AipsError("LELUnary: unknown operation"));
   }
   return pExpr_p->getScalar();      // Make compiler happy
}

template <class T>
void LELUnary<T>::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELUnary::prepare" << endl;
#endif

   LELInterface<T>::replaceScalarExpr (pExpr_p);
}

template <class T>
String LELUnary<T>::className() const
{
   return String("LELUnary");
}
