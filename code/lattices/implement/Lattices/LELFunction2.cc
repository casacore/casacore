//# LELFunction2.cc:  this defines non-templated classes in LELFunction.h
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
#include <trial/Lattices/LELArray.h>
#include <trial/Lattices/LELScalar.h>
#include <aips/Lattices/Slicer.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LatticeIterator.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h> 



// LELFunctionFloat
LELFunctionFloat::LELFunctionFloat(const LELFunctionEnums::Function function,
				   const Block<LatticeExprNode>& exp)
: function_p(function), arg_p(exp)
{
    switch (function_p) {
    case LELFunctionEnums::ABS :
    case LELFunctionEnums::ARG :
    case LELFunctionEnums::REAL :
    case LELFunctionEnums::IMAG :
    {
       if (arg_p.nelements() != 1) {
          throw (AipsError ("LELFunctionFloat::constructor - functions can only"
                            "have one argument"));
       }
       setAttr(arg_p[0].getAttribute());
       break;
    }
    case LELFunctionEnums::LENGTH :
    {
       if (arg_p.nelements() != 2) {
          throw (AipsError ("LELFunctionFloat::constructor - length function"
			    " should have 2 arguments"));
       }
       if (! (arg_p[1].isScalar()  &&
            (arg_p[1].dataType()==TpFloat || arg_p[1].dataType()==TpDouble))) {
          throw (AipsError ("LELFunctionFloat::constructor - 2nd argument of "
			    " length function should be a real scalar"));
       }
       setAttr (LELAttribute());                         // result is scalar
       break;
    }
    case LELFunctionEnums::ATAN2 :
    case LELFunctionEnums::POW :
    case LELFunctionEnums::FMOD :
    case LELFunctionEnums::MIN :
    case LELFunctionEnums::MAX :
    {
// Expect 2 Float arguments

	Block<Int> argType(2);
	argType[0] = TpFloat;
	argType[1] = TpFloat;
	setAttr (LatticeExprNode::checkArg (arg_p, argType, False));
	break;
    }
    default:
	throw (AipsError ("LELFunctionFloat::constructor - unknown Float function"));
    }

#if defined(AIPS_TRACE)
   cout << "LELFunctionFloat: constructor" << endl;
#endif
}

LELFunctionFloat::~LELFunctionFloat()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionFloat: destructor" << endl;
#endif
}


void LELFunctionFloat::eval(LELArray<Float>& result,
			    const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionFloat:: eval" << endl;
#endif

   if (arg_p.nelements() == 1) {
      switch (function_p) {
      case LELFunctionEnums::ABS :
      {
         if (arg_p[0].dataType() == TpFloat) {
            arg_p[0].eval(result, section);
            Array<Float> tmp(abs(result.value()));
            result.value().reference(tmp);
         } else {
            LELArray<Complex> tmpC(result.shape());
            arg_p[0].eval(tmpC, section);
	    result.setMask(tmpC);
   	    amplitude(result.value(), tmpC.value());
         }
         break;
      }
      case LELFunctionEnums::ARG :
      {
         LELArray<Complex> tmpC(result.shape());
         arg_p[0].eval(tmpC, section);
	 result.setMask(tmpC);
         phase(result.value(), tmpC.value());
         break;
      }
      case LELFunctionEnums::REAL :
      {
         if (arg_p[0].dataType() == TpFloat) {
            arg_p[0].eval(result, section);
         } else {
            LELArray<Complex> tmpC(result.shape());
            arg_p[0].eval(tmpC, section);
	    result.setMask(tmpC);
   	    real(result.value(), tmpC.value());
         }
         break;
      }
      case LELFunctionEnums::IMAG :
      {
         LELArray<Complex> tmpC(result.shape());
         arg_p[0].eval(tmpC, section);
	 result.setMask(tmpC);
         imag(result.value(), tmpC.value());
         break;
      }
      default:
         throw (AipsError ("LELFunctionFloat::eval - unknown Float function"));
      }
   } else {   
      if (arg_p[0].isScalar()) {
         Float scalarTemp;
         arg_p[0].eval(scalarTemp);
         arg_p[1].eval(result, section);
         switch (function_p) {
         case LELFunctionEnums::ATAN2 :
         {
	    Array<Float> templ (result.shape());
	    templ = scalarTemp;
   	    Array<Float> temp (atan2 (templ, result.value()));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::POW :
         {
	    Array<Float> templ (result.shape());
	    templ = scalarTemp;
   	    Array<Float> temp (pow (templ, result.value()));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::FMOD :
         {
	    Array<Float> templ (result.shape());
	    templ = scalarTemp;
   	    Array<Float> temp (fmod (templ, result.value()));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::MIN :
   	    min (result.value(), result.value(), scalarTemp);
   	    break;
         case LELFunctionEnums::MAX :
   	    max (result.value(), result.value(), scalarTemp);
   	    break;
         default:
            throw (AipsError ("LELFunctionFloat::eval - unknown Float function"));
         }

      } else if (arg_p[1].isScalar()) {
         Float scalarTemp;
         arg_p[1].eval(scalarTemp);
         arg_p[0].eval(result, section);
         switch (function_p) {
         case LELFunctionEnums::ATAN2 :
         {
	    Array<Float> tempr (result.shape());
	    tempr = scalarTemp;
   	    Array<Float> temp (atan2 (result.value(), tempr));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::POW :
         {
	    if (scalarTemp == 2) {
	       result.value() *= result.value();
	    } else {
	       Array<Float> temp (pow (result.value(), Double(scalarTemp)));
	       result.value().reference (temp);
	    }
	    break;
         }
         case LELFunctionEnums::FMOD :
         {
	    Array<Float> tempr (result.shape());
	    tempr = scalarTemp;
   	    Array<Float> temp (fmod (result.value(), tempr));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::MIN :
	    min (result.value(), result.value(), scalarTemp);
   	    break;
         case LELFunctionEnums::MAX :
	    max (result.value(), result.value(), scalarTemp);
	    break;
         default:
	    throw (AipsError ("LELFunctionFloat::eval - unknown Float function"));
         }

      } else {
         LELArray<Float> tempr(result.shape());
         arg_p[0].eval(result, section);
         arg_p[1].eval(tempr, section);
	 result.combineMask (tempr);
         switch(function_p) {
         case LELFunctionEnums::ATAN2 :
         {
   	    Array<Float> temp (atan2 (result.value(), tempr.value()));
   	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::POW :
         {
   	    Array<Float> temp (pow (result.value(), tempr.value()));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::FMOD :
         {
   	    Array<Float> temp (fmod (result.value(), tempr.value()));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::MIN :
	    min(result.value(), result.value(), tempr.value());
	    break;
         case LELFunctionEnums::MAX :
	    max(result.value(), result.value(), tempr.value());
	    break;
         default:
	    throw(AipsError("LELFunctionFloat::eval - unknown function"));
         }
      }
   }
}

LELScalar<Float> LELFunctionFloat::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionFloat:: getScalar" << endl;
#endif

   switch (function_p) {
   case LELFunctionEnums::LENGTH :       
   {
      Double axis;
      if (arg_p[1].dataType() == TpFloat) {
	 axis = arg_p[1].getFloat();
      } else {
	 axis = arg_p[1].getDouble();
      }
      axis += 0.5;                // for rounding
      if (axis < 1) {
	 throw (AipsError ("Axis argument in length function is < 1; "
			   "(note axis is 1-relative!)"));
      }
      if (arg_p[0].isScalar()) {
	 return 1;
      }
      const IPosition& shape = arg_p[0].shape();
      axis -= 1;
      if (axis >= shape.nelements()) {
	 return 1;
      }
      return shape(Int(axis));
   }
   case LELFunctionEnums::ABS :       
   {
       if (arg_p[0].dataType() == TpFloat) {
          return abs(arg_p[0].getFloat());
       } else {
          return Float(abs(arg_p[0].getComplex()));
       }
   }
   case LELFunctionEnums::ARG :
       return Float(arg(arg_p[0].getComplex()));
   case LELFunctionEnums::REAL :
       if (arg_p[0].dataType() == TpFloat) {
          return arg_p[0].getFloat();
       } else {
	  return Float(real(arg_p[0].getComplex()));
       }
   case LELFunctionEnums::IMAG :
       return Float(imag(arg_p[0].getComplex()));
   case LELFunctionEnums::ATAN2 :
       return atan2(arg_p[0].getFloat(), arg_p[1].getFloat());
   case LELFunctionEnums::POW :
       return pow(arg_p[0].getFloat(), arg_p[1].getFloat());
   case LELFunctionEnums::FMOD :
       return fmod(arg_p[0].getFloat(), arg_p[1].getFloat());
   case LELFunctionEnums::MIN :
       return min(arg_p[0].getFloat(), arg_p[1].getFloat());
   case LELFunctionEnums::MAX :
       return max(arg_p[0].getFloat(), arg_p[1].getFloat());
   default:
      throw(AipsError("LELFunctionFloat::getScalar - unknown function"));
   }
   return LELScalar<Float>();
}

Bool LELFunctionFloat::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionFloat::prepare" << endl;
#endif

   uInt i;
   for (i=0; i<arg_p.nelements(); i++) {
      if (arg_p[i].replaceScalarExpr()) {
	 return True;
      }
   }
   return False;
}

String LELFunctionFloat::className() const
{
   return String("LELFunctionFloat");
}



// LELFunctionDouble
LELFunctionDouble::LELFunctionDouble(const LELFunctionEnums::Function function,
  				     const Block<LatticeExprNode>& exp)
: function_p(function), arg_p(exp)
{
    switch (function_p) {
    case LELFunctionEnums::ABS :  
    case LELFunctionEnums::ARG :
    case LELFunctionEnums::REAL :
    case LELFunctionEnums::IMAG :
    case LELFunctionEnums::NELEM :

// Returns a real number

    {
       if (arg_p.nelements() != 1) {
          throw (AipsError ("LELFunctionDouble::constructor - functions can only"
                            "have one argument"));
       }
       if (function_p == LELFunctionEnums::NELEM) {
	   setAttr (LELAttribute());                         // result is scalar
       } else {
	   setAttr(arg_p[0].getAttribute());
       }
       break;
    }
    case LELFunctionEnums::NTRUE :
    case LELFunctionEnums::NFALSE :
    {
	Block<Int> argType(1);
	argType[0] = TpBool;
	LatticeExprNode::checkArg (arg_p, argType, True); // expect 1 Bool array
	setAttr (LELAttribute());                         // result is scalar
	break;
    }
    case LELFunctionEnums::ATAN2 :
    case LELFunctionEnums::POW :
    case LELFunctionEnums::FMOD :
    case LELFunctionEnums::MIN :
    case LELFunctionEnums::MAX :
    {
// Expect 2 Double arguments
	Block<Int> argType(2);
	argType[0] = TpDouble;
	argType[1] = TpDouble;
	setAttr (LatticeExprNode::checkArg (arg_p, argType, False));
	break;
    }
    default:
	throw (AipsError ("LELFunctionDouble::constructor - unknown Double function"));
    }

#if defined(AIPS_TRACE)
   cout << "LELFunctionDouble: constructor" << endl;
#endif
}

LELFunctionDouble::~LELFunctionDouble()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDouble: destructor" << endl;
#endif
}


void LELFunctionDouble::eval(LELArray<Double>& result,
			     const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDouble:: eval" << endl;
#endif

   if (arg_p.nelements() == 1) {
      switch (function_p) {
      case LELFunctionEnums::ABS :
      {
         if (arg_p[0].dataType() == TpDouble) {
            arg_p[0].eval(result, section);
            Array<Double> tmp(abs(result.value()));
            result.value().reference(tmp);
         } else {
            LELArray<DComplex> tmpC(result.shape());
            arg_p[0].eval(tmpC, section);
	    result.setMask(tmpC);
            amplitude(result.value(), tmpC.value());
         }
         break;
      }
      case LELFunctionEnums::ARG :
      {
         LELArray<DComplex> tmpC(result.shape());
         arg_p[0].eval(tmpC, section);
	 result.setMask(tmpC);
         phase(result.value(), tmpC.value());
         break;
      }
      case LELFunctionEnums::REAL :
      {
         if (arg_p[0].dataType() == TpDouble) {
            arg_p[0].eval(result, section);
         } else {
            LELArray<DComplex> tmpC(result.shape());
            arg_p[0].eval(tmpC, section);
	    result.setMask(tmpC);
   	    real(result.value(), tmpC.value());
         }
         break;
      }
      case LELFunctionEnums::IMAG :
      {
         LELArray<DComplex> tmpC(result.shape());
         arg_p[0].eval(tmpC, section);
	 result.setMask(tmpC);
         imag(result.value(), tmpC.value());
         break;
      }
      default:
         throw (AipsError ("LELFunctionDouble::eval - unknown Double function"));
      }
   } else {
      if (arg_p[0].isScalar()) {
         Double scalarTemp;
         arg_p[0].eval(scalarTemp);
         arg_p[1].eval(result, section);
         switch (function_p) {
         case LELFunctionEnums::ATAN2 :
         {
	    Array<Double> templ (result.shape());
	    templ = scalarTemp;
   	    Array<Double> temp (atan2 (templ, result.value()));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::POW :
         {
	    Array<Double> templ (result.shape());
	    templ = scalarTemp;
   	    Array<Double> temp (pow (templ, result.value()));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::FMOD :
         {
	    Array<Double> templ (result.shape());
	    templ = scalarTemp;
   	    Array<Double> temp (fmod (templ, result.value()));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::MIN :
   	    min (result.value(), result.value(), scalarTemp);
   	    break;
         case LELFunctionEnums::MAX :
	    max (result.value(), result.value(), scalarTemp);
	    break;
         default:
	    throw (AipsError ("LELFunctionDouble::eval - unknown Double function"));
         }

      } else if (arg_p[1].isScalar()) {
         Double scalarTemp;
         arg_p[1].eval(scalarTemp);
         arg_p[0].eval(result, section);
         switch (function_p) {
         case LELFunctionEnums::ATAN2 :
         {
	    Array<Double> tempr (result.shape());
	    tempr = scalarTemp;
   	    Array<Double> temp (atan2 (result.value(), tempr));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::POW :
         {
	    if (scalarTemp == 2) {
	       result.value() *= result.value();
	    } else {
	       Array<Double> temp (pow (result.value(), scalarTemp));
	       result.value().reference (temp);
	    }
            break;
         }
         case LELFunctionEnums::FMOD :
         {
	    Array<Double> tempr (result.shape());
	    tempr = scalarTemp;
   	    Array<Double> temp (fmod (result.value(), tempr));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::MIN :
            min (result.value(), result.value(), scalarTemp);
            break;
         case LELFunctionEnums::MAX :
	    max (result.value(), result.value(), scalarTemp);
	    break;
         default:
            throw (AipsError ("LELFunctionDouble::eval - unknown Double function"));
         }

      } else {
         LELArray<Double> tempr(result.shape());
         arg_p[0].eval(result, section);
         arg_p[1].eval(tempr, section);
	 result.combineMask (tempr);
         switch(function_p) {
         case LELFunctionEnums::ATAN2 :
         {
   	    Array<Double> temp (atan2 (result.value(), tempr.value()));
   	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::POW :
         {
   	    Array<Double> temp (pow (result.value(), tempr.value()));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::FMOD :
         {
   	    Array<Double> temp (fmod (result.value(), tempr.value()));
	    result.value().reference (temp);
	    break;
         }
         case LELFunctionEnums::MIN :
	    min(result.value(), result.value(), tempr.value());
	    break;
         case LELFunctionEnums::MAX :
	    max(result.value(), result.value(), tempr.value());
	    break;
         default:
            throw(AipsError("LELFunctionDouble::eval - unknown function"));
         }
      }
   }
}

LELScalar<Double> LELFunctionDouble::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDouble:: getScalar" << endl;
#endif

   switch (function_p) {
   case LELFunctionEnums::NTRUE :
   {
      uInt ntrue = 0;
      Bool deleteIt, deleteMask;
      LatticeExpr<Bool> latExpr(arg_p[0], 0);
      RO_LatticeIterator<Bool> iter(latExpr, latExpr.niceCursorShape());
      if (! arg_p[0].isMasked()) {
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    const Bool* data = array.getStorage (deleteIt);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (data[i]) {
		  ntrue++;
	       }
	    }
	    array.freeStorage (data, deleteIt);
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<Bool> maskiter(latExpr, latExpr.niceCursorShape());
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* data = array.getStorage (deleteIt);
	    const Bool* maskdata = mask.getStorage (deleteMask);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (data[i] && maskdata[i]) {
		  ntrue++;
	       }
	    }
	    array.freeStorage (data, deleteIt);
	    mask.freeStorage (maskdata, deleteMask);
	    iter++;
	 }
      }
      return ntrue;
   }
   case LELFunctionEnums::NFALSE :
   {
      uInt nfalse = 0;
      Bool deleteIt, deleteMask;
      LatticeExpr<Bool> latExpr(arg_p[0], 0);
      RO_LatticeIterator<Bool> iter(latExpr, latExpr.niceCursorShape());
      if (! arg_p[0].isMasked()) {
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    const Bool* data = array.getStorage (deleteIt);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (!data[i]) {
		  nfalse++;
	       }
	    }
	    array.freeStorage (data, deleteIt);
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<Bool> maskiter(latExpr, latExpr.niceCursorShape());
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* data = array.getStorage (deleteIt);
	    const Bool* maskdata = mask.getStorage (deleteMask);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (!data[i] && maskdata[i]) {
		  nfalse++;
	       }
	    }
	    array.freeStorage (data, deleteIt);
	    mask.freeStorage (maskdata, deleteMask);
	    iter++;
	 }
      }
      return nfalse;
   }
   case LELFunctionEnums::NELEM :
   {
      if (arg_p[0].isScalar()) {
	 return 1;
      }
      if (! arg_p[0].isMasked()) {
         return arg_p[0].shape().product();
      }
      return nMaskedElements (arg_p[0]);
   }
   case LELFunctionEnums::ABS :       
   {
       if (arg_p[0].dataType() == TpDouble) {
          return abs(arg_p[0].getDouble());
       } else {
          return Double(abs(arg_p[0].getDComplex()));
       }
   }
   case LELFunctionEnums::ARG :
       return Double(arg(arg_p[0].getDComplex()));
   case LELFunctionEnums::REAL :
       if (arg_p[0].dataType() == TpDouble) {
          return arg_p[0].getDouble();
       } else {
          return Double(real(arg_p[0].getDComplex()));
       }
   case LELFunctionEnums::IMAG :
       return Double(imag(arg_p[0].getDComplex()));
   case LELFunctionEnums::ATAN2 :
       return atan2(arg_p[0].getDouble(), arg_p[1].getDouble());
   case LELFunctionEnums::POW :
       return pow(arg_p[0].getDouble(), arg_p[1].getDouble());
   case LELFunctionEnums::FMOD :
       return fmod(arg_p[0].getDouble(), arg_p[1].getDouble());
   case LELFunctionEnums::MIN :
       return min(arg_p[0].getDouble(), arg_p[1].getDouble());
   case LELFunctionEnums::MAX :
       return max(arg_p[0].getDouble(), arg_p[1].getDouble());
   default:
      throw(AipsError("LELFunctionDouble::getScalar - unknown function"));
   }
   return LELScalar<Double>();                       // Make compiler happy
}

uInt LELFunctionDouble::nMaskedElements (const LatticeExprNode& expr) const
{
   uInt nelem = 0;
   switch (expr.dataType()) {
   case TpFloat:
   {
      LatticeExpr<Float> latExpr(expr, 0);
      RO_LatticeIterator<Float> iter(latExpr, latExpr.niceCursorShape());
////  RO_LatticeIterator<Bool> maskiter(latExpr, latExpr.niceCursorShape());
      Array<Bool> mask;
      while (! iter.atEnd()) {
	 latExpr.getMaskSlice (mask, iter.position(), iter.cursorShape());
	 nelem += nMaskedOn (mask);
	 iter++;
      }
      break;
   }
   case TpDouble:
   {
      LatticeExpr<Double> latExpr(expr, 0);
      RO_LatticeIterator<Double> iter(latExpr, latExpr.niceCursorShape());
////  RO_LatticeIterator<Bool> maskiter(latExpr, latExpr.niceCursorShape());
      Array<Bool> mask;
      while (! iter.atEnd()) {
	 latExpr.getMaskSlice (mask, iter.position(), iter.cursorShape());
	 nelem += nMaskedOn (mask);
	 iter++;
      }
      break;
   }
   case TpComplex:
   {
      LatticeExpr<Complex> latExpr(expr, 0);
      RO_LatticeIterator<Complex> iter(latExpr, latExpr.niceCursorShape());
////  RO_LatticeIterator<Bool> maskiter(latExpr, latExpr.niceCursorShape());
      Array<Bool> mask;
      while (! iter.atEnd()) {
	 latExpr.getMaskSlice (mask, iter.position(), iter.cursorShape());
	 nelem += nMaskedOn (mask);
	 iter++;
      }
      break;
   }
   case TpDComplex:
   {
      LatticeExpr<DComplex> latExpr(expr, 0);
      RO_LatticeIterator<DComplex> iter(latExpr, latExpr.niceCursorShape());
////  RO_LatticeIterator<Bool> maskiter(latExpr, latExpr.niceCursorShape());
      Array<Bool> mask;
      while (! iter.atEnd()) {
	 latExpr.getMaskSlice (mask, iter.position(), iter.cursorShape());
	 nelem += nMaskedOn (mask);
	 iter++;
      }
      break;
   }
   case TpBool:
   {
      LatticeExpr<Bool> latExpr(expr, 0);
      RO_LatticeIterator<Bool> iter(latExpr, latExpr.niceCursorShape());
////  RO_LatticeIterator<Bool> maskiter(latExpr, latExpr.niceCursorShape());
      Array<Bool> mask;
      while (! iter.atEnd()) {
	 latExpr.getMaskSlice (mask, iter.position(), iter.cursorShape());
	 nelem += nMaskedOn (mask);
	 iter++;
      }
      break;
   }
   default:
      throw (AipsError ("LELFunction2::nMaskedElements, unknown data type"));
   }
   return nelem;
}

uInt LELFunctionDouble::nMaskedOn (const Array<Bool>& mask) const
{
   uInt nelem = 0;
   Bool deleteMask;
   const Bool* maskdata = mask.getStorage (deleteMask);
   uInt n = mask.nelements();
   for (uInt i=0; i<n; i++) {
      if (maskdata[i]) {
	 nelem++;
      }
   }
   mask.freeStorage (maskdata, deleteMask);
   return nelem;
}

Bool LELFunctionDouble::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDouble::prepare" << endl;
#endif

   uInt i;
   for (i=0; i<arg_p.nelements(); i++) {
       Bool invalid = arg_p[i].replaceScalarExpr();
       if (invalid) {
	  if (function_p != LELFunctionEnums::NTRUE
          &&  function_p != LELFunctionEnums::NFALSE
          &&  function_p != LELFunctionEnums::NELEM) {
	     return True;
	  }
       }
   }
   return False;
}

String LELFunctionDouble::className() const
{
   return String("LELFunctionDouble");
}



// LELFunctionComplex
LELFunctionComplex::LELFunctionComplex
                                  (const LELFunctionEnums::Function function,
				   const Block<LatticeExprNode>& exp)
: function_p(function), arg_p(exp)
{
    switch (function_p) {
    case LELFunctionEnums::CONJ :
    {
// Expect 1 Complex argument

       if (arg_p.nelements() != 1) {
          throw (AipsError ("LELFunctionComplex::constructor - functions can only"
                            "have one argument"));
       }
       setAttr(arg_p[0].getAttribute());
       break;
    }
    case LELFunctionEnums::COMPLEX :
    {
// Expect 2 Float arguments

	Block<Int> argType(2);
	argType[0] = TpFloat;
	argType[1] = TpFloat;
	setAttr (LatticeExprNode::checkArg (arg_p, argType, False));
	break;
    }
    case LELFunctionEnums::POW :
    {
// Expect 2 Complex arguments

	Block<Int> argType(2);
	argType[0] = TpComplex;
	argType[1] = TpComplex;
	setAttr (LatticeExprNode::checkArg (arg_p, argType, False));
	break;
    }
    default:
	throw (AipsError ("LELFunctionComplex::constructor - unknown Complex function"));
    }

#if defined(AIPS_TRACE)
   cout << "LELFunctionComplex: constructor" << endl;
#endif
}

LELFunctionComplex::~LELFunctionComplex()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionComplex: destructor" << endl;
#endif
}


void LELFunctionComplex::eval(LELArray<Complex>& result,
			      const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionComplex:: eval" << endl;
#endif

   if (arg_p.nelements() == 1) {
      switch (function_p) {
      case LELFunctionEnums::CONJ :
      {   
         arg_p[0].eval(result, section);
         Array<Complex> tmpC(conj(result.value()));
         result.value().reference(tmpC);
         break;
      }
      default:
         throw (AipsError ("LELFunctionComplex::eval - unknown Complex function"));
      }
 
   } else {
      if (arg_p[0].isScalar()) {
         switch (function_p) {
	 case LELFunctionEnums::COMPLEX :
	 {
	    Float scalarTemp;
	    LELArray<Float> arrayTemp(result.shape());
	    arg_p[0].eval(scalarTemp);
	    arg_p[1].eval(arrayTemp, section);
	    Bool delr, delc;
	    const Float* rptr = arrayTemp.value().getStorage(delr);
	    Complex *cptr = result.value().getStorage(delc);
	    uInt n=arrayTemp.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i].real() = scalarTemp;
		cptr[i].imag() = rptr[i];
	    }
	    arrayTemp.value().freeStorage(rptr, delr);
	    result.value().putStorage(cptr, delc);
	    result.setMask (arrayTemp);
	    break;
	 }
         case LELFunctionEnums::POW :
         {
	    Complex scalarTemp;
	    arg_p[0].eval(scalarTemp);
	    arg_p[1].eval(result, section);
	    Array<Complex> templ (result.shape());
	    templ = scalarTemp;
	    Array<Complex> temp (pow (templ, result.value()));
	    result.value().reference (temp);
	    break;
         }
         default:
            throw (AipsError ("LELFunctionComplex::eval - unknown Complex function"));
         }

      } else if (arg_p[1].isScalar()) {
         switch (function_p) {
	 case LELFunctionEnums::COMPLEX :
	 {
	    Float scalarTemp;
	    LELArray<Float> arrayTemp(result.shape());
	    arg_p[1].eval(scalarTemp);
	    arg_p[0].eval(arrayTemp, section);
	    Bool delr, delc;
	    const Float* rptr = arrayTemp.value().getStorage(delr);
	    Complex *cptr = result.value().getStorage(delc);
	    uInt n=arrayTemp.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i].real() = rptr[i];
		cptr[i].imag() = scalarTemp;
	    }
	    arrayTemp.value().freeStorage(rptr, delr);
	    result.value().putStorage(cptr, delc);
	    result.setMask (arrayTemp);
	    break;
	 }
         case LELFunctionEnums::POW :
         {
	    Complex scalarTemp;
	    arg_p[1].eval(scalarTemp);
	    arg_p[0].eval(result, section);
	    if (scalarTemp.imag() == 0) {
	       Double exponent = scalarTemp.real();
	       if (exponent == 2) {
		  result.value() *= result.value();
	       } else {
		  Array<Complex> temp (pow (result.value(), exponent));
		  result.value().reference (temp);
	       }
	    } else {
	       Array<Complex> exponent (result.shape());
	       exponent = scalarTemp;
	       Array<Complex> temp (pow (result.value(), exponent));
	       result.value().reference (temp);
	    }
	    break;
         }
         default:
            throw (AipsError ("LELFunctionComplex::eval - unknown Complex function"));
         }

      } else {
         switch(function_p) {
	 case LELFunctionEnums::COMPLEX :
	 {
	    LELArray<Float> arrayLeft(result.shape());
	    LELArray<Float> arrayRight(result.shape());
	    arg_p[0].eval(arrayLeft, section);
	    arg_p[1].eval(arrayRight, section);
	    Bool dell, delr, delc;
	    const Float* lptr = arrayLeft.value().getStorage(dell);
	    const Float* rptr = arrayRight.value().getStorage(delr);
	    Complex *cptr = result.value().getStorage(delc);
	    uInt n=arrayLeft.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i].real() = lptr[i];
		cptr[i].imag() = rptr[i];
	    }
	    arrayLeft.value().freeStorage(lptr, dell);
	    arrayRight.value().freeStorage(rptr, delr);
	    result.value().putStorage(cptr, delc);
	    result.setMask (arrayLeft, arrayRight);
	    break;
	 }
         case LELFunctionEnums::POW :
         {
	    LELArray<Complex> tempr(result.shape());
	    arg_p[0].eval(result, section);
	    arg_p[1].eval(tempr, section);
	    result.combineMask (tempr);
            Array<Complex> temp (pow (result.value(), tempr.value()));
            result.value().reference (temp);
            break;
         }
         default:
            throw(AipsError("LELFunctionComplex::eval - unknown function"));
         }
      }
   }
}

LELScalar<Complex> LELFunctionComplex::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionComplex:: getScalar" << endl;
#endif

   switch (function_p) {
   case LELFunctionEnums::CONJ :
       return conj(arg_p[0].getComplex());
   case LELFunctionEnums::COMPLEX :
       return Complex(arg_p[0].getFloat(), arg_p[1].getFloat());
   case LELFunctionEnums::POW :
       return pow(arg_p[0].getComplex(), arg_p[1].getComplex());
   default:
      throw(AipsError("LELFunctionComplex::getScalar - unknown function"));
   }
   return LELScalar<Complex>();                       // Make compiler happy
}

Bool LELFunctionComplex::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionComplex::prepare" << endl;
#endif

   uInt i;
   for (i=0; i<arg_p.nelements(); i++) {
      if (arg_p[i].replaceScalarExpr()) {
	 return True;
      }
   }
   return False;
}

String LELFunctionComplex::className() const
{
   return String("LELFunctionComplex");
}



// LELFunctionDComplex
LELFunctionDComplex::LELFunctionDComplex
                                  (const LELFunctionEnums::Function function,
				   const Block<LatticeExprNode>& exp)
: function_p(function), arg_p(exp)
{
    switch (function_p) {
    case LELFunctionEnums::CONJ :
    {
// Expect 1 Complex argument

       if (arg_p.nelements() != 1) {
          throw (AipsError ("LELFunctionDComplex::constructor - functions can only"
                            "have one argument"));
       }
       setAttr(arg_p[0].getAttribute());
       break;
    }
    case LELFunctionEnums::COMPLEX :
    {
// Expect 2 Double arguments

	Block<Int> argType(2);
	argType[0] = TpDouble;
	argType[1] = TpDouble;
	setAttr (LatticeExprNode::checkArg (arg_p, argType, False));
	break;
    }
    case LELFunctionEnums::POW :
    {
	// Expect 2 DComplex arguments
	Block<Int> argType(2);
	argType[0] = TpDComplex;
	argType[1] = TpDComplex;
	setAttr (LatticeExprNode::checkArg (arg_p, argType, False));
	break;
    }
    default:
	throw (AipsError ("LELFunctionDComplex::constructor - unknown DComplex function"));
    }

#if defined(AIPS_TRACE)
   cout << "LELFunctionDComplex: constructor" << endl;
#endif
}

LELFunctionDComplex::~LELFunctionDComplex()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDComplex: destructor" << endl;
#endif
}


void LELFunctionDComplex::eval(LELArray<DComplex>& result,
			       const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDComplex:: eval" << endl;
#endif

   if (arg_p.nelements() == 1) {
      switch (function_p) {
      case LELFunctionEnums::CONJ :
      {   
         arg_p[0].eval(result, section);
         Array<DComplex> tmpC(conj(result.value()));
         result.value().reference(tmpC);
         break;
      }
      default:
         throw (AipsError ("LELFunctionDComplex::eval - unknown Complex function"));
      }
 
   } else { 
      if (arg_p[0].isScalar()) {
         switch (function_p) {
	 case LELFunctionEnums::COMPLEX :
	 {
	    Double scalarTemp;
	    LELArray<Double> arrayTemp(result.shape());
	    arg_p[0].eval(scalarTemp);
	    arg_p[1].eval(arrayTemp, section);
	    Bool delr, delc;
	    const Double* rptr = arrayTemp.value().getStorage(delr);
	    DComplex *cptr = result.value().getStorage(delc);
	    uInt n=arrayTemp.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i].real() = scalarTemp;
		cptr[i].imag() = rptr[i];
	    }
	    arrayTemp.value().freeStorage(rptr, delr);
	    result.value().putStorage(cptr, delc);
	    result.setMask (arrayTemp);
	    break;
	 }
         case LELFunctionEnums::POW :
         {
	    DComplex scalarTemp;
	    arg_p[0].eval(scalarTemp);
	    arg_p[1].eval(result, section);
	    Array<DComplex> templ (result.shape());
	    templ = scalarTemp;
	    Array<DComplex> temp (pow (templ, result.value()));
	    result.value().reference (temp);
	    break;
         }
         default:
            throw (AipsError ("LELFunctionDComplex::eval - unknown DComplex function"));
         }

      } else if (arg_p[1].isScalar()) {
         switch (function_p) {
	 case LELFunctionEnums::COMPLEX :
	 {
	    Double scalarTemp;
	    LELArray<Double> arrayTemp(result.shape());
	    arg_p[1].eval(scalarTemp);
	    arg_p[0].eval(arrayTemp, section);
	    Bool delr, delc;
	    const Double* rptr = arrayTemp.value().getStorage(delr);
	    DComplex *cptr = result.value().getStorage(delc);
	    uInt n=arrayTemp.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i].real() = rptr[i];
		cptr[i].imag() = scalarTemp;
	    }
	    arrayTemp.value().freeStorage(rptr, delr);
	    result.value().putStorage(cptr, delc);
	    result.setMask (arrayTemp);
	    break;
	 }
         case LELFunctionEnums::POW :
         {
	    DComplex scalarTemp;
	    arg_p[1].eval(scalarTemp);
	    arg_p[0].eval(result, section);
	    if (scalarTemp.imag() == 0) {
	       Double exponent = scalarTemp.real();
	       if (exponent == 2) {
		  result.value() *= result.value();
	       } else {
		  Array<DComplex> temp (pow (result.value(), exponent));
		  result.value().reference (temp);
	       }
	    } else {
	       Array<DComplex> exponent (result.shape());
	       exponent = scalarTemp;
	       Array<DComplex> temp (pow (result.value(), exponent));
	       result.value().reference (temp);
	    }
	    break;
         }
         default:
            throw (AipsError ("LELFunctionDComplex::eval - unknown DComplex function"));
         }

      } else {
         switch(function_p) {
 	 case LELFunctionEnums::COMPLEX :
	 {
	    LELArray<Double> arrayLeft(result.shape());
	    LELArray<Double> arrayRight(result.shape());
	    arg_p[0].eval(arrayLeft, section);
	    arg_p[1].eval(arrayRight, section);
	    Bool dell, delr, delc;
	    const Double* lptr = arrayLeft.value().getStorage(dell);
	    const Double* rptr = arrayRight.value().getStorage(delr);
	    DComplex *cptr = result.value().getStorage(delc);
	    uInt n=arrayLeft.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i].real() = lptr[i];
		cptr[i].imag() = rptr[i];
	    }
	    arrayLeft.value().freeStorage(lptr, dell);
	    arrayRight.value().freeStorage(rptr, delr);
	    result.value().putStorage(cptr, delc);
	    result.setMask (arrayLeft, arrayRight);
	    break;
	 }
        case LELFunctionEnums::POW :
         {
	    LELArray<DComplex> tempr(result.shape());
	    arg_p[0].eval(result, section);
	    arg_p[1].eval(tempr, section);
	    result.combineMask (tempr);
            Array<DComplex> temp (pow (result.value(), tempr.value()));
            result.value().reference (temp);
            break;
         }
         default:
            throw(AipsError("LELFunctionDComplex::eval - unknown function"));
         }
      }
   }
}

LELScalar<DComplex> LELFunctionDComplex::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDComplex:: getScalar" << endl;
#endif

   switch (function_p) {
   case LELFunctionEnums::CONJ :
       return conj(arg_p[0].getDComplex());
   case LELFunctionEnums::COMPLEX :
       return DComplex(arg_p[0].getDouble(), arg_p[1].getDouble());
   case LELFunctionEnums::POW :
       return pow(arg_p[0].getDComplex(), arg_p[1].getDComplex());
   default:
      throw(AipsError("LELFunctionDComplex::getScalar - unknown function"));
   }
   return LELScalar<DComplex>();                       // Make compiler happy
}

Bool LELFunctionDComplex::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDComplex::prepare" << endl;
#endif

   uInt i;
   for (i=0; i<arg_p.nelements(); i++) {
      if (arg_p[i].replaceScalarExpr()) {
	 return True;
      }
   }
   return False;
}

String LELFunctionDComplex::className() const
{
   return String("LELFunctionDComplex");
}



// LELFunctionBool
LELFunctionBool::LELFunctionBool(const LELFunctionEnums::Function function,
				 const Block<LatticeExprNode>& exp)
: function_p(function), arg_p(exp)
{
    switch (function_p) {
    case LELFunctionEnums::ALL :
    case LELFunctionEnums::ANY :
    {
	Block<Int> argType(1);
	argType[0] = TpBool;
	LatticeExprNode::checkArg (arg_p, argType, True); // expect 1 Bool array
	setAttr (LELAttribute());                         // result is scalar
	break;
    }
    default:
	throw (AipsError ("LELFunctionBool::constructor - unknown Bool function"));
    }

#if defined(AIPS_TRACE)
   cout << "LELFunctionBool: constructor" << endl;
#endif
}

LELFunctionBool::~LELFunctionBool()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionBool: destructor" << endl;
#endif
}


void LELFunctionBool::eval(LELArray<Bool>&,
			   const Slicer&) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionBool:: eval" << endl;
#endif

// All Bool function result in a scalar, so cannot be used.
   throw (AipsError ("LELFunctionBool::eval - cannot be used"));
}

LELScalar<Bool> LELFunctionBool::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionBool:: getScalar" << endl;
#endif

// Apply the  function
   switch(function_p) {
   case LELFunctionEnums::ALL :
   {
      Bool deleteIt, deleteMask;
      LatticeExpr<Bool> latExpr(arg_p[0], 0);
      RO_LatticeIterator<Bool> iter(latExpr, latExpr.niceCursorShape());
      if (! arg_p[0].isMasked()) {
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    const Bool* data = array.getStorage (deleteIt);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (!data[i]) {
		  array.freeStorage (data, deleteIt);
		  return False;
	       }
	    }
	    array.freeStorage (data, deleteIt);
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<Bool> maskiter(latExpr, latExpr.niceCursorShape());
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* data = array.getStorage (deleteIt);
	    const Bool* maskdata = mask.getStorage (deleteMask);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (!data[i] && maskdata[i]) {
		  array.freeStorage (data, deleteIt);
		  mask.freeStorage (maskdata, deleteMask);
		  return False;
	       }
	    }
	    array.freeStorage (data, deleteIt);
	    mask.freeStorage (maskdata, deleteMask);
	    iter++;
	 }
      }
      return True;
   }
   case LELFunctionEnums::ANY :
   {
      Bool deleteIt, deleteMask;
      LatticeExpr<Bool> latExpr(arg_p[0], 0);
      RO_LatticeIterator<Bool> iter(latExpr, latExpr.niceCursorShape());
      if (! arg_p[0].isMasked()) {
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    const Bool* data = array.getStorage (deleteIt);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (data[i]) {
		  array.freeStorage (data, deleteIt);
		  return True;
	       }
	    }
	    array.freeStorage (data, deleteIt);
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<Bool> maskiter(latExpr, latExpr.niceCursorShape());
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* data = array.getStorage (deleteIt);
	    const Bool* maskdata = mask.getStorage (deleteMask);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (data[i] && maskdata[i]) {
		  array.freeStorage (data, deleteIt);
		  mask.freeStorage (maskdata, deleteMask);
		  return True;
	       }
	    }
	    array.freeStorage (data, deleteIt);
	    mask.freeStorage (maskdata, deleteMask);
	    iter++;
	 }
      }
      return False;
   }
   default:
      throw(AipsError("LELFunctionBool::getScalar - unknown function"));
   }
   return LELScalar<Bool>();                       // Make compiler happy
}

Bool LELFunctionBool::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionBool::prepare" << endl;
#endif

   uInt i;
   for (i=0; i<arg_p.nelements(); i++) {
       Bool invalid = arg_p[i].replaceScalarExpr();
       if (invalid) {
	  if (function_p != LELFunctionEnums::ALL
          &&  function_p != LELFunctionEnums::ANY) {
	     return True;
	  }
      }
   }
   return False;
}

String LELFunctionBool::className() const
{
   return String("LELFunctionBool");
}
