//# LELFunction.cc:  this defines templated classes in LELFunction.h
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

#include <trial/Lattices/LELFunction.h>
#include <trial/Lattices/LELFunctionEnums.h>
#include <trial/Lattices/PixelRegion.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LatticeIterator.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h> 


// LELFunction1D
template <class T>
LELFunction1D<T>::LELFunction1D(const LELFunctionEnums::Function function,
				const CountedPtr<LELInterface<T> >& exp)
: function_p(function), pExpr_p(exp)
{
   switch (function_p) {
   case LELFunctionEnums::MIN1D :
   case LELFunctionEnums::MAX1D :
   case LELFunctionEnums::MEAN1D :
   case LELFunctionEnums::SUM :
      setAttr(LELAttribute (True, IPosition()));    // these result in a scalar
      break;
   default:
      setAttr(exp->getAttribute());
   }

#if defined(AIPS_TRACE)
   cout << "LELFunction1D: constructor" << endl;
#endif
}

template <class T>
LELFunction1D<T>::~LELFunction1D()
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D: destructor" << endl;
#endif
}


template <class T>
void LELFunction1D<T>::eval(Array<T>& result,
			    const PixelRegion& region) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D:: eval" << endl;
#endif

// Evaluate the expression
   pExpr_p->eval(result, region);

// Apply the 1D function
   switch(function_p) {
   case LELFunctionEnums::SIN :
   {
      Array<T> tmp(sin(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::SINH :
   {
      Array<T> tmp(sinh(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::COS :
   {
      Array<T> tmp(cos(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::COSH :
   {
      Array<T> tmp(cosh(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::EXP :
   {
      Array<T> tmp(exp(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::LOG :
   {
      Array<T> tmp(log(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::LOG10 :
   {
      Array<T> tmp(log10(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::SQRT :
   {
      Array<T> tmp(sqrt(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::ABS :
   {
      Array<T> tmp(abs(result));
      result.reference(tmp);
      break;
   }
   default:
      throw(AipsError("LELFunction1D: unknown function"));
   }

}

template <class T>
T LELFunction1D<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D:: getScalar" << endl;
#endif

// Apply the 1D function

   switch(function_p) {
   case LELFunctionEnums::SIN :
      return sin(pExpr_p->getScalar());
   case LELFunctionEnums::SINH :
      return sinh(pExpr_p->getScalar());
   case LELFunctionEnums::COS :
      return cos(pExpr_p->getScalar());
   case LELFunctionEnums::COSH :
      return cosh(pExpr_p->getScalar());
   case LELFunctionEnums::EXP :
      return exp(pExpr_p->getScalar());
   case LELFunctionEnums::LOG :
      return log(pExpr_p->getScalar());
   case LELFunctionEnums::LOG10 :
      return log10(pExpr_p->getScalar());
   case LELFunctionEnums::SQRT :
      return sqrt(pExpr_p->getScalar());
   case LELFunctionEnums::ABS :
      return abs(pExpr_p->getScalar());
   case LELFunctionEnums::MIN1D :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      Bool firstTime = True;
      T minVal;
      LatticeExpr<T> latExpr(pExpr_p, 0);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      while (! iter.atEnd()) {
	 T minv = min(iter.cursor());
	 if (firstTime  ||  minv < minVal) {
	    firstTime = False;
	    minVal = minv;
	 }
	 iter++;
      }
      return minVal;
   }
   case LELFunctionEnums::MAX1D :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      Bool firstTime = True;
      T maxVal;
      LatticeExpr<T> latExpr(pExpr_p, 0);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      while (! iter.atEnd()) {
	 T maxv = max(iter.cursor());
	 if (firstTime  ||  maxv > maxVal) {
	    firstTime = False;
	    maxVal = maxv;
	 }
	 iter++;
      }
      return maxVal;
   }
   case LELFunctionEnums::MEAN1D :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      T sumVal = 0;
      LatticeExpr<T> latExpr(pExpr_p, 0);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      while (! iter.atEnd()) {
	 sumVal += sum(iter.cursor());
	 iter++;
      }
      return sumVal / pExpr_p->shape().product();
   }
   case LELFunctionEnums::SUM :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      T sumVal = 0;
      LatticeExpr<T> latExpr(pExpr_p, 0);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      while (! iter.atEnd()) {
	 sumVal += sum(iter.cursor());
	 iter++;
      }
      return sumVal;
   }
   default:
      throw(AipsError("LELFunction1D: unknown function"));
   }
   return pExpr_p->getScalar();         // to make compiler happy
}

template <class T>
void LELFunction1D<T>::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D::prepare" << endl;
#endif

   LELInterface<T>::replaceScalarExpr (pExpr_p);
}

template <class T>
String LELFunction1D<T>::className() const
{
   return String("LELFunction1D");
}



// LELFunctionReal1D
template <class T>
LELFunctionReal1D<T>::LELFunctionReal1D
                               (const LELFunctionEnums::Function function,
				const CountedPtr<LELInterface<T> >& exp)
: function_p(function), pExpr_p(exp)
{
   setAttr(exp->getAttribute());

#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D: constructor" << endl;
#endif
}

template <class T>
LELFunctionReal1D<T>::~LELFunctionReal1D()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D: destructor" << endl;
#endif
}


template <class T>
void LELFunctionReal1D<T>::eval(Array<T>& result,
			    const PixelRegion& region) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D:: eval" << endl;
#endif

// Evaluate the expression
   pExpr_p->eval(result, region);

// Apply the Real1D function
   switch(function_p) {
   case LELFunctionEnums::ASIN :
   {
      Array<T> tmp(asin(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::ACOS :
   {
      Array<T> tmp(acos(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::TAN :
   {
      Array<T> tmp(tan(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::TANH :
   {
      Array<T> tmp(tanh(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::ATAN :
   {
      Array<T> tmp(atan(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::CEIL :
   {
      Array<T> tmp(ceil(result));
      result.reference(tmp);
      break;
   }
   case LELFunctionEnums::FLOOR :
   {
      Array<T> tmp(floor(result));
      result.reference(tmp);
      break;
   }
   default:
      throw(AipsError("LELFunctionReal1D: unknown function"));
   }

}

template <class T>
T LELFunctionReal1D<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D:: getScalar" << endl;
#endif

// Apply the Real1D function

   switch(function_p) {
   case LELFunctionEnums::ASIN :
      return asin(pExpr_p->getScalar());
   case LELFunctionEnums::ACOS :
      return acos(pExpr_p->getScalar());
   case LELFunctionEnums::TAN :
      return tan(pExpr_p->getScalar());
   case LELFunctionEnums::TANH :
      return tanh(pExpr_p->getScalar());
   case LELFunctionEnums::ATAN :
      return atan(pExpr_p->getScalar());
   case LELFunctionEnums::CEIL :
      return ceil(pExpr_p->getScalar());
   case LELFunctionEnums::FLOOR :
      return floor(pExpr_p->getScalar());
   default:
      throw(AipsError("LELFunctionReal1D: unknown function"));
   }
   return pExpr_p->getScalar();         // to make compiler happy
}

template <class T>
void LELFunctionReal1D<T>::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D::prepare" << endl;
#endif

    if (! pExpr_p.null()) {
       LELInterface<T>::replaceScalarExpr (pExpr_p);
    }
}

template <class T>
String LELFunctionReal1D<T>::className() const
{
   return String("LELFunctionReal1D");
}
