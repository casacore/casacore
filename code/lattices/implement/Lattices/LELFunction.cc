//# LELFunction.cc:  this defines templated classes in LELFunction.h
//# Copyright (C) 1997,1998,1999
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
#include <trial/Lattices/LELScalar.h>
#include <trial/Lattices/LELArray.h>
#include <aips/Lattices/Slicer.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LatticeIterator.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h> 
#include <aips/Mathematics/NumericTraits.h> 


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
      setAttr(LELAttribute());          // these result in a scalar
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
void LELFunction1D<T>::eval(LELArray<T>& result,
			    const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D:: eval" << endl;
#endif

// Evaluate the expression
   pExpr_p->eval(result, section);

// Apply the 1D function
   switch(function_p) {
   case LELFunctionEnums::SIN :
   {
      Array<T> tmp(sin(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::SINH :
   {
      Array<T> tmp(sinh(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::COS :
   {
      Array<T> tmp(cos(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::COSH :
   {
      Array<T> tmp(cosh(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::EXP :
   {
      Array<T> tmp(exp(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::LOG :
   {
      Array<T> tmp(log(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::LOG10 :
   {
      Array<T> tmp(log10(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::SQRT :
   {
      Array<T> tmp(sqrt(result.value()));
      result.value().reference(tmp);
      break;
   }
   default:
      throw(AipsError("LELFunction1D::eval - unknown function"));
   }

}

template <class T>
LELScalar<T> LELFunction1D<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D:: getScalar" << endl;
#endif

// Apply the 1D function

   switch(function_p) {
   case LELFunctionEnums::SIN :
      return sin(pExpr_p->getScalar().value());
   case LELFunctionEnums::SINH :
      return sinh(pExpr_p->getScalar().value());
   case LELFunctionEnums::COS :
      return cos(pExpr_p->getScalar().value());
   case LELFunctionEnums::COSH :
      return cosh(pExpr_p->getScalar().value());
   case LELFunctionEnums::EXP :
      return exp(pExpr_p->getScalar().value());
   case LELFunctionEnums::LOG :
      return log(pExpr_p->getScalar().value());
   case LELFunctionEnums::LOG10 :
      return log10(pExpr_p->getScalar().value());
   case LELFunctionEnums::SQRT :
      return sqrt(pExpr_p->getScalar().value());
   case LELFunctionEnums::MIN1D :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      Bool firstTime = True;
      T minVal = T();
      LatticeExpr<T> latExpr(pExpr_p, 0);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      if (! latExpr.isMasked()) {
	 while (! iter.atEnd()) {
	    T minv = min(iter.cursor());
	    if (firstTime  ||  minv < minVal) {
	       firstTime = False;
	       minVal = minv;
	    }
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<T> maskiter(latExpr, latExpr.niceCursorShape());
	 Bool delMask, delData;
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* maskPtr = mask.getStorage (delMask);
	    const T* dataPtr = array.getStorage (delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (maskPtr[i]) {
		  if (firstTime  ||  dataPtr[i] < minVal) {
		     firstTime = False;
		     minVal = dataPtr[i];
		  }
	       }
	    }
	    mask.freeStorage (maskPtr, delMask);
	    array.freeStorage (dataPtr, delData);
	    iter++;
	 }
      }
      if (firstTime) {
	  return LELScalar<T>();           // no element found
      }
      return minVal;
   }
   case LELFunctionEnums::MAX1D :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      Bool firstTime = True;
      T maxVal = T();
      LatticeExpr<T> latExpr(pExpr_p, 0);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      if (! latExpr.isMasked()) {
	 while (! iter.atEnd()) {
	    T maxv = max(iter.cursor());
	    if (firstTime  ||  maxv < maxVal) {
	       firstTime = False;
	       maxVal = maxv;
	    }
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<T> maskiter(latExpr, latExpr.niceCursorShape());
	 Bool delMask, delData;
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* maskPtr = mask.getStorage (delMask);
	    const T* dataPtr = array.getStorage (delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (maskPtr[i]) {
		  if (firstTime  ||  dataPtr[i] > maxVal) {
		     firstTime = False;
		     maxVal = dataPtr[i];
		  }
	       }
	    }
	    mask.freeStorage (maskPtr, delMask);
	    array.freeStorage (dataPtr, delData);
	    iter++;
	 }
      }
      if (firstTime) {
	  return LELScalar<T>();           // no element found
      }
      return maxVal;
   }
   case LELFunctionEnums::MEAN1D :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      NumericTraits<T>::PrecisionType sumVal = 0;
      uInt nrVal = 0;
      LatticeExpr<T> latExpr(pExpr_p, 0);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      Bool delData, delMask;

// Do the sum ourselves to avoid round off

      if (! latExpr.isMasked()) {
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    const T* dataPtr = array.getStorage(delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
		sumVal += dataPtr[i];
	    }
	    array.freeStorage(dataPtr, delData);
	    nrVal += n;
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<T> maskiter(latExpr, latExpr.niceCursorShape());
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* maskPtr = mask.getStorage (delMask);
	    const T* dataPtr = array.getStorage (delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (maskPtr[i]) {
		  sumVal += dataPtr[i];
		  nrVal++;
	       }
	    }
	    mask.freeStorage (maskPtr, delMask);
	    array.freeStorage (dataPtr, delData);
	    iter++;
	 }
      }
      if (nrVal == 0) {
	  return LELScalar<T>();           // no element found
      }
      return T(sumVal / Int(nrVal));
   }
   case LELFunctionEnums::SUM :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      NumericTraits<T>::PrecisionType sumVal = 0;
      LatticeExpr<T> latExpr(pExpr_p, 0);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      Bool delData, delMask;

// Do the sum ourselves to avoid round off

      if (! latExpr.isMasked()) {
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    const T* dataPtr = array.getStorage(delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
		sumVal += dataPtr[i];
	    }
	    array.freeStorage(dataPtr, delData);
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<T> maskiter(latExpr, latExpr.niceCursorShape());
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* maskPtr = mask.getStorage (delMask);
	    const T* dataPtr = array.getStorage (delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (maskPtr[i]) {
		  sumVal += dataPtr[i];
	       }
	    }
	    mask.freeStorage (maskPtr, delMask);
	    array.freeStorage (dataPtr, delData);
	    iter++;
	 }
      }
      return T(sumVal);
   }
   default:
      throw(AipsError("LELFunction1D::getScalar - unknown function"));
   }
   return pExpr_p->getScalar();         // to make compiler happy
}

template <class T>
Bool LELFunction1D<T>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D::prepare" << endl;
#endif

   return LELInterface<T>::replaceScalarExpr (pExpr_p);
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
void LELFunctionReal1D<T>::eval(LELArray<T>& result,
			    const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D:: eval" << endl;
#endif

// Evaluate the expression
   pExpr_p->eval(result, section);

// Apply the Real1D function
   switch(function_p) {
   case LELFunctionEnums::ASIN :
   {
      Array<T> tmp(asin(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::ACOS :
   {
      Array<T> tmp(acos(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::TAN :
   {
      Array<T> tmp(tan(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::TANH :
   {
      Array<T> tmp(tanh(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::ATAN :
   {
      Array<T> tmp(atan(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::CEIL :
   {
      Array<T> tmp(ceil(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::FLOOR :
   {
      Array<T> tmp(floor(result.value()));
      result.value().reference(tmp);
      break;
   }
   default:
      throw(AipsError("LELFunctionReal1D::eval - unknown function"));
   }

}

template <class T>
LELScalar<T> LELFunctionReal1D<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D:: getScalar" << endl;
#endif

// Apply the Real1D function

   switch(function_p) {
   case LELFunctionEnums::ASIN :
      return asin(pExpr_p->getScalar().value());
   case LELFunctionEnums::ACOS :
      return acos(pExpr_p->getScalar().value());
   case LELFunctionEnums::TAN :
      return tan(pExpr_p->getScalar().value());
   case LELFunctionEnums::TANH :
      return tanh(pExpr_p->getScalar().value());
   case LELFunctionEnums::ATAN :
      return atan(pExpr_p->getScalar().value());
   case LELFunctionEnums::CEIL :
      return ceil(pExpr_p->getScalar().value());
   case LELFunctionEnums::FLOOR :
      return floor(pExpr_p->getScalar().value());
   default:
      throw(AipsError("LELFunctionReal1D::getScalar - unknown function"));
   }
   return pExpr_p->getScalar();         // to make compiler happy
}

template <class T>
Bool LELFunctionReal1D<T>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D::prepare" << endl;
#endif

   if (! pExpr_p.null()) {
      return LELInterface<T>::replaceScalarExpr (pExpr_p);
   }
   return False;
}

template <class T>
String LELFunctionReal1D<T>::className() const
{
   return String("LELFunctionReal1D");
}




// LELFunctionND
template <class T>
LELFunctionND<T>::LELFunctionND(const LELFunctionEnums::Function function,
				const Block<LatticeExprNode>& exp)
: function_p(function), arg_p(exp)
{
   switch (function_p) {
   case LELFunctionEnums::IIF :
   {
      if (arg_p.nelements() != 3) {
         throw (AipsError ("LELFunctionFloat::constructor - "
			   "function IIF should have 3 arguments"));
      }
//# The 1st argument must be Bool, the 2nd and 3rd must be T.
//# The arguments do not need to be lattices.

      Block<Int> argType(3);
      argType[0] = TpBool;
      argType[1] = whatType((T*)0);
      argType[2] = whatType((T*)0);
      setAttr (LatticeExprNode::checkArg (arg_p, argType, False));
      break;
   }
   default:
      throw (AipsError ("LELFunctionND::constructor - unknown function"));
   }

#if defined(AIPS_TRACE)
   cout << "LELFunctionND: constructor" << endl;
#endif
}

template <class T>
LELFunctionND<T>::~LELFunctionND()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionND: destructor" << endl;
#endif
}


template <class T>
void LELFunctionND<T>::eval(LELArray<T>& result,
			    const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionND:: eval" << endl;
#endif

   switch (function_p) {
   case LELFunctionEnums::IIF :
   {
//# Evaluation is not difficult, but there are many scalar/lattice
//# combinations.
//# The optional masks make life even much more difficult.

// If the condition is a scalar, the result is simply the 1st or 2nd operand.
// If the operand taken is a scalar, its mask is certainly true.
// (otherwise prepareScalarExpr would have tackled it).

      if (arg_p[0].isScalar()) {
	 T tmp;
	 Bool tmpb;
	 arg_p[0].eval (tmpb);
	 if (tmpb) {
	    if (arg_p[1].isScalar()) {
	       arg_p[1].eval (tmp);
	       result.value() = tmp;
	    } else {
               LELArray<T>& resultRef(result);
	       arg_p[1].eval(resultRef, section);
	    }
	 } else {
	    if (arg_p[2].isScalar()) {
	       arg_p[2].eval (tmp);
	       result.value() = tmp;
	    } else {
               LELArray<T>& resultRef(result);
	       arg_p[2].eval(resultRef, section);
	    }
	 }
      } else {

// So the condition is an array.
// The result might get a mask. That is the case if one of the operands
// is an invalid scalar or an array with mask,

	 LELArray<Bool> tmpb(result.shape());
	 arg_p[0].eval (tmpb, section);
	 Bool deleteTmpb;
	 const Bool* tmpbData = tmpb.value().getStorage (deleteTmpb);
	 uInt n = tmpb.value().nelements();
	 Bool deleteRes, deleteMask;
	 T* resData = 0;
	 Bool* maskData = 0;
	 T tmp1, tmp2;
	 // The combination of left and right gets a mask if either
	 // of them has a mask.
	 Array<Bool> newMask;
	 Bool makeMask = ToBool (arg_p[1].isInvalidScalar()
                              || arg_p[1].isMasked()
			      || arg_p[2].isInvalidScalar()
			      || arg_p[2].isMasked());

// There are 4 different scalar/array combinations for 1st and 2nd operand.
// Each of them must handle the optional new mask.
// Because efficiency is needed, the test for a new mask is done
// outside the loop.

	 if (arg_p[1].isScalar()) {
	    arg_p[1].eval (tmp1);
	    Bool mask1 = ToBool (!arg_p[1].isInvalidScalar());
	    if (arg_p[2].isScalar()) {
		// Handle scalar,scalar case.
	       resData = result.value().getStorage (deleteRes);
	       arg_p[2].eval (tmp2);
	       Bool mask2 = ToBool (!arg_p[2].isInvalidScalar());
	       if (makeMask) {
		  newMask.resize (result.shape());
		  maskData = newMask.getStorage (deleteMask);
		  for (uInt i=0; i<n; i++) {
		     if (tmpbData[i]) {
		        resData[i] = tmp1;
			maskData[i] = mask1;
		     } else {
		        resData[i] = tmp2;
			maskData[i] = mask2;
		     }
		  }
	       } else {
		  for (uInt i=0; i<n; i++) {
		     if (tmpbData[i]) {
			resData[i] = tmp1;
		     } else {
			resData[i] = tmp2;
		     }
		  }
	       }
	    } else {
		// Handle scalar,array case.
               LELArray<T>& resultRef(result);
	       arg_p[2].eval(resultRef, section);
	       resData = result.value().getStorage (deleteRes);
	       if (makeMask) {
		  if (result.isMasked()) {
		     newMask.reference (result.mask());
		  } else {
		     newMask.resize (result.shape());
		     newMask = True;
		  }
		  maskData = newMask.getStorage (deleteMask);
		  for (uInt i=0; i<n; i++) {
		     if (tmpbData[i]) {
			resData[i] = tmp1;
			maskData[i] = mask1;
		     }
		  }
	       } else {
		  for (uInt i=0; i<n; i++) {
		     if (tmpbData[i]) {
			resData[i] = tmp1;
		     }
		  }
	       }
	    }

// The first operand is an array.

	 } else {
	    LELArray<T>& resultRef(result);
	    arg_p[1].eval(resultRef, section);
	    resData = result.value().getStorage (deleteRes);
	    if (makeMask) {
	       if (result.isMasked()) {
		  newMask.reference (result.mask());
	       } else {
		  newMask.resize (result.shape());
		  newMask = True;
	       }
	    }
	    if (arg_p[2].isScalar()) {
		// Handle array,scalar case.
	       arg_p[2].eval (tmp2);
	       if (makeMask) {
		  Bool mask2 = ToBool (!arg_p[2].isInvalidScalar());
		  maskData = newMask.getStorage (deleteMask);
		  for (uInt i=0; i<n; i++) {
		     if (! tmpbData[i]) {
		        resData[i] = tmp2;
			maskData[i] = mask2;
		     }
		  }
	       } else {
		  for (uInt i=0; i<n; i++) {
		     if (! tmpbData[i]) {
		        resData[i] = tmp2;
		     }
		  }
	       }
	    } else {
		// Handle array,array case.
	       LELArray<T> tmp(result.shape());
	       arg_p[2].eval (tmp, section);
	       Bool deleteTmp, deleteTmpMask;
	       const T* tmpData = tmp.value().getStorage (deleteTmp);
	       if (makeMask) {
		  maskData = newMask.getStorage (deleteMask);
		  if (tmp.isMasked()) {
		     const Bool* tmpMaskData = tmp.mask().getStorage
                                                               (deleteTmpMask);
		     for (uInt i=0; i<n; i++) {
			if (! tmpbData[i]) {
			   resData[i] = tmpData[i];
			   maskData[i] = tmpMaskData[i];
			}
		     }
		     tmp.mask().freeStorage (tmpMaskData, deleteTmpMask);
		  } else {
		     for (uInt i=0; i<n; i++) {
			if (! tmpbData[i]) {
			   resData[i] = tmpData[i];
			   maskData[i] = True;
			}
		     }
		  }
		  tmp.value().freeStorage (tmpData, deleteTmp);
	       } else {
		  for (uInt i=0; i<n; i++) {
		     if (! tmpbData[i]) {
			resData[i] = tmpData[i];
		     }
		  }
	       }
	    }
	 }
	 tmpb.value().freeStorage (tmpbData, deleteTmpb);
	 if (resData != 0) {
	    result.value().putStorage (resData, deleteRes);
	 }
	 if (maskData != 0) {
	    newMask.putStorage (maskData, deleteMask);
	 }
	 result.setMask (tmpb);
	 if (makeMask) {
	    result.combineMask (newMask);
	 }
      }
      break;
   }
   default:
      throw(AipsError("LELFunctionND::eval - unknown function"));
   }
}

template <class T>
LELScalar<T> LELFunctionND<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionND:: getScalar" << endl;
#endif

// Apply the ND function

   T tmp;
   switch(function_p) {
   case LELFunctionEnums::IIF :
   {
      Bool tmpb;
      arg_p[0].eval (tmpb);
      if (tmpb) {
	 arg_p[1].eval (tmp);
      } else {
	 arg_p[2].eval (tmp);
      }
      return tmp;
   }
   default:
      throw(AipsError("LELFunctionND::getScalar - unknown function"));
   }
   return tmp;                      // to make compiler happy
}

template <class T>
Bool LELFunctionND<T>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionND::prepare" << endl;
#endif

   uInt i;
   for (i=0; i<arg_p.nelements(); i++) {
       Bool invalid = arg_p[i].replaceScalarExpr();
       if (invalid  &&  function_p != LELFunctionEnums::IIF) {
	  return True;
       }
   }
   // IIF is invalid if:
   // - the condition is an invalid scalar.
   // - both operands are invalid scalars.
   // - condition is scalar and corresponding operand is invalid scalar.
   if (arg_p[0].isInvalidScalar()) {
      return True;
   }
   if (arg_p[1].isInvalidScalar() && arg_p[2].isInvalidScalar()) {
      return True;
   }
   if (arg_p[0].isScalar()) {
      i = (arg_p[0].getBool()  ?  1 : 2);
      if (arg_p[i].isInvalidScalar()) {
	 return True;
      }
   }  
   return False;
}

template <class T>
String LELFunctionND<T>::className() const
{
   return String("LELFunctionND");
}
