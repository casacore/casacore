//# LELFunction2.cc:  this defines non-templated classes in LELFunction.h
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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

#include <casacore/lattices/LEL/LELFunction.h>
#include <casacore/lattices/LEL/LELFunctionEnums.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/LEL/LELScalar.h>
#include <casacore/lattices/LatticeMath/LatticeFractile.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/Lattices/MaskedLatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h> 



namespace casacore { //# NAMESPACE CASACORE - BEGIN

// LELFunctionFloat
LELFunctionFloat::LELFunctionFloat(const LELFunctionEnums::Function function,
				   const Block<LatticeExprNode>& exp)
: function_p(function)
{
    switch (function_p) {
    case LELFunctionEnums::ABS :
    case LELFunctionEnums::ARG :
    case LELFunctionEnums::REAL :
    case LELFunctionEnums::IMAG :
    case LELFunctionEnums::NDIM :
    {
       if (exp.nelements() != 1) {
          throw (AipsError ("LELFunctionFloat::constructor - "
			    "function can only have one argument"));
       }
       if (function_p == LELFunctionEnums::NDIM) {
	   setAttr (LELAttribute());                     // result is scalar
       } else {
	   setAttr(exp[0].getAttribute());
       }
       break;
    }
    case LELFunctionEnums::LENGTH :
    {
       if (exp.nelements() != 2) {
          throw (AipsError ("LELFunctionFloat::constructor - "
			    "length function should have 2 arguments"));
       }
       if (! (exp[1].isScalar()  &&
            (exp[1].dataType()==TpFloat || exp[1].dataType()==TpDouble))) {
          throw (AipsError ("LELFunctionFloat::constructor - "
			    "2nd argument of length function "
			    "should be a real scalar"));
       }
       setAttr (LELAttribute());                         // result is scalar
       break;
    }
    case LELFunctionEnums::SIGN :
    {
       // Expect 1 Float argument
       Block<Int> argType(1);
       argType[0] = TpFloat;
       setAttr (LatticeExprNode::checkArg (exp, argType, False));
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
       setAttr (LatticeExprNode::checkArg (exp, argType, False));
       break;
    }
    case LELFunctionEnums::FRACTILE1D :
    {
       if (exp.nelements() != 2) {
          throw (AipsError ("LELFunctionFloat::constructor - "
			    "fractile function should have 2 arguments"));
       }
       if (! (exp[1].isScalar()  &&  exp[1].dataType()==TpFloat)) {
          throw (AipsError ("LELFunctionFloat::constructor - "
			    "2nd argument of fractile function "
			    "should be a float scalar"));
       }
       setAttr (LELAttribute());                         // result is scalar
       break;
    }
    case LELFunctionEnums::FRACTILERANGE1D :
    {
       if (exp.nelements() != 2  &&  exp.nelements() != 3) {
          throw (AipsError ("LELFunctionFloat::constructor - "
		       "fractilerange function should have 2 or 3 arguments"));
       }
       if (exp[0].isScalar()) {
	  throw (AipsError ("LELFunctionFloat::constructor - "
			    "1st argument of fractilerange function "
			    "should be a lattice"));
       }
       for (uInt i=1; i<exp.nelements(); i++) {
	  if (! (exp[i].isScalar()  &&  exp[i].dataType()==TpFloat)) {
	     throw (AipsError ("LELFunctionFloat::constructor - "
			    "2nd and 3rd argument of fractilerange function "
			    "should be a float scalar"));
	  }
       }
       setAttr (LELAttribute());                         // result is scalar
       break;
    }
    default:
	throw (AipsError ("LELFunctionFloat::constructor - "
			  "unknown Float function"));
    }
   // Fill the node block here, so an exception does
   // not leave the nodes undestructed.
   arg_p = exp;
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
            LELArrayRef<Complex> tmpC(result.shape());
            arg_p[0].evalRef(tmpC, section);
	    result.setMask(tmpC);
   	    amplitude(result.value(), tmpC.value());
         }
         break;
      }
      case LELFunctionEnums::ARG :
      {
         LELArrayRef<Complex> tmpC(result.shape());
         arg_p[0].evalRef(tmpC, section);
	 result.setMask(tmpC);
         phase(result.value(), tmpC.value());
         break;
      }
      case LELFunctionEnums::REAL :
      {
         if (arg_p[0].dataType() == TpFloat) {
            arg_p[0].eval(result, section);
         } else {
            LELArrayRef<Complex> tmpC(result.shape());
            arg_p[0].evalRef(tmpC, section);
	    result.setMask(tmpC);
   	    real(result.value(), tmpC.value());
         }
         break;
      }
      case LELFunctionEnums::IMAG :
      {
         LELArrayRef<Complex> tmpC(result.shape());
         arg_p[0].evalRef(tmpC, section);
	 result.setMask(tmpC);
         imag(result.value(), tmpC.value());
         break;
      }
      case LELFunctionEnums::SIGN :
      {
	 arg_p[0].eval(result, section);
	 Bool deleteIt;
	 Float* data = result.value().getStorage (deleteIt);
	 uInt nr = result.value().nelements();
	 for (uInt i=0; i<nr; i++) {
	    if (data[i] < 0) {
	       data[i] = -1;
	    } else if (data[i] > 0) {
	       data[i] = 1;
	    }
	 }
	 result.value().putStorage (data, deleteIt);
         break;
      }
      default:
         throw (AipsError ("LELFunctionFloat::eval - "
			   "unknown Float function"));
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
            throw (AipsError ("LELFunctionFloat::eval - "
			      "unknown Float function"));
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
	    throw (AipsError ("LELFunctionFloat::eval - "
			      "unknown Float function"));
         }

      } else {
         LELArrayRef<Float> tempr(result.shape());
         arg_p[0].eval(result, section);
         arg_p[1].evalRef(tempr, section);
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
	    throw(AipsError("LELFunctionFloat::eval - "
			    "unknown function"));
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
   case LELFunctionEnums::NDIM :
   {
       if (arg_p[0].isScalar()) {
	   return 0;
       }
       return arg_p[0].shape().nelements();
   }
   case LELFunctionEnums::LENGTH :       
   {
      Double daxis;
      if (arg_p[1].dataType() == TpFloat) {
	 daxis = arg_p[1].getFloat();
      } else {
	 daxis = arg_p[1].getDouble();
      }
      Int axis = Int(daxis+0.499);    // add  for rounding
      if (axis < 0) {
	 throw (AipsError ("Axis argument in length function is < 0; "
			   "(note axis is 0-relative!)"));
      }
      if (arg_p[0].isScalar()) {
	 return 1;
      }
      const IPosition& shape = arg_p[0].shape();
      if (axis >= Int(shape.nelements())) {
	 return 1;
      }
      return shape(axis);
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
   case LELFunctionEnums::SIGN :
   {
       Float value = arg_p[0].getFloat();
       if (value < 0) {
	   value = -1;
       } else if (value > 0) {
	   value = 1;
       }
       return value;
   }
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
   case LELFunctionEnums::FRACTILE1D :
   {
      if (arg_p[0].isScalar()) {
         return arg_p[0].getFloat();
      }
      Vector<Float> fractile = LatticeFractile<Float>::maskedFractile
                                             (LatticeExpr<Float>(arg_p[0]),
					      arg_p[1].getFloat());
      if (fractile.nelements() == 0) {
	 return LELScalar<Float>();             // no valid fractile found
      }
      return fractile(0);
   }
   case LELFunctionEnums::FRACTILERANGE1D :
   {
      Float fraction1 = arg_p[1].getFloat();
      Float fraction2 = fraction1;
      if (arg_p.nelements() > 2) {
	 fraction2 = arg_p[2].getFloat();
      }
      Vector<Float> fractiles = LatticeFractile<Float>::maskedFractiles
                                              (LatticeExpr<Float>(arg_p[0]),
					       fraction1, fraction2);
      if (fractiles.nelements() < 2) {
	 return LELScalar<Float>();             // no valid fractiles found
      }
      return fractiles(1) - fractiles(0);
   }
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
       Bool invalid = arg_p[i].replaceScalarExpr();
       if (invalid) {
	  if (i > 0
          ||  (function_p != LELFunctionEnums::NDIM
	   &&  function_p != LELFunctionEnums::LENGTH)) {
	     return True;
	  }
      }
   }
   return False;
}

String LELFunctionFloat::className() const
{
   return String("LELFunctionFloat");
}

Bool LELFunctionFloat::lock (FileLocker::LockType type, uInt nattempts)
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].lock (type, nattempts)) {
      return False;
    }
  }
  return True;
}
void LELFunctionFloat::unlock()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].unlock();
  }
}
Bool LELFunctionFloat::hasLock (FileLocker::LockType type) const
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].hasLock (type)) {
      return False;
    }
  }
  return True;
}
void LELFunctionFloat::resync()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].resync();
  }
}



// LELFunctionDouble
LELFunctionDouble::LELFunctionDouble(const LELFunctionEnums::Function function,
  				     const Block<LatticeExprNode>& exp)
: function_p(function)
{
    switch (function_p) {
    case LELFunctionEnums::ABS :  
    case LELFunctionEnums::ARG :
    case LELFunctionEnums::REAL :
    case LELFunctionEnums::IMAG :
    case LELFunctionEnums::NELEM :

// Returns a real number

    {
       if (exp.nelements() != 1) {
          throw (AipsError ("LELFunctionDouble::constructor - "
			    "function can only have one argument"));
       }
       if (function_p == LELFunctionEnums::NELEM) {
	   setAttr (LELAttribute());                    // result is scalar
       } else {
	   setAttr(exp[0].getAttribute());
       }
       break;
    }
    case LELFunctionEnums::NTRUE :
    case LELFunctionEnums::NFALSE :
    {
	Block<Int> argType(1);
	argType[0] = TpBool;
	LatticeExprNode::checkArg (exp, argType, True); // expect 1 Bool array
	setAttr (LELAttribute());                       // result is scalar
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
	setAttr (LatticeExprNode::checkArg (exp, argType, False));
	break;
    }
    case LELFunctionEnums::FRACTILE1D :
    {
       if (exp.nelements() != 2) {
          throw (AipsError ("LELFunctionDouble::constructor - "
			    "fractile function should have 2 arguments"));
       }
       if (! (exp[1].isScalar()  &&  exp[1].dataType()==TpFloat)) {
          throw (AipsError ("LELFunctionDouble::constructor - "
			    "2nd argument of fractile function "
			    "should be a float scalar"));
       }
       setAttr (LELAttribute());                         // result is scalar
       break;
    }
    case LELFunctionEnums::FRACTILERANGE1D :
    {
       if (exp.nelements() != 2  &&  exp.nelements() != 3) {
          throw (AipsError ("LELFunctionDouble::constructor - "
		       "fractilerange function should have 2 or 3 arguments"));
       }
       if (exp[0].isScalar()) {
	  throw (AipsError ("LELFunctionDouble::constructor - "
			    "1st argument of fractilerange function "
			    "should be a lattice"));
       }
       for (uInt i=1; i<exp.nelements(); i++) {
	  if (! (exp[i].isScalar()  &&  exp[i].dataType()==TpFloat)) {
	     throw (AipsError ("LELFunctionDouble::constructor - "
			    "2nd and 3rd argument of fractilerange function "
			    "should be a float scalar"));
	  }
       }
       setAttr (LELAttribute());                         // result is scalar
       break;
    }
    default:
	throw (AipsError ("LELFunctionDouble::constructor - "
			  "unknown Double function"));
    }
   // Fill the node block here, so an exception does
   // not leave the nodes undestructed.
   arg_p = exp;
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
            LELArrayRef<DComplex> tmpC(result.shape());
            arg_p[0].evalRef(tmpC, section);
	    result.setMask(tmpC);
            amplitude(result.value(), tmpC.value());
         }
         break;
      }
      case LELFunctionEnums::ARG :
      {
         LELArrayRef<DComplex> tmpC(result.shape());
         arg_p[0].evalRef(tmpC, section);
	 result.setMask(tmpC);
         phase(result.value(), tmpC.value());
         break;
      }
      case LELFunctionEnums::REAL :
      {
         if (arg_p[0].dataType() == TpDouble) {
            arg_p[0].eval(result, section);
         } else {
            LELArrayRef<DComplex> tmpC(result.shape());
            arg_p[0].evalRef(tmpC, section);
	    result.setMask(tmpC);
   	    real(result.value(), tmpC.value());
         }
         break;
      }
      case LELFunctionEnums::IMAG :
      {
         LELArrayRef<DComplex> tmpC(result.shape());
         arg_p[0].evalRef(tmpC, section);
	 result.setMask(tmpC);
         imag(result.value(), tmpC.value());
         break;
      }
      default:
         throw (AipsError ("LELFunctionDouble::eval - "
			   "unknown Double function"));
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
	    throw (AipsError ("LELFunctionDouble::eval - "
			      "unknown Double function"));
         }

      } else if (arg_p[1].isScalar()) {
         Double scalarTemp = 0;
	 if (function_p != LELFunctionEnums::FRACTILE1D
	 &&  function_p != LELFunctionEnums::FRACTILERANGE1D) {
	    arg_p[1].eval(scalarTemp);
	    arg_p[0].eval(result, section);
	 }
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
            throw (AipsError ("LELFunctionDouble::eval - "
			      "unknown Double function"));
         }

      } else {
         LELArrayRef<Double> tempr(result.shape());
         arg_p[0].eval(result, section);
         arg_p[1].evalRef(tempr, section);
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
            throw(AipsError("LELFunctionDouble::eval - "
			    "unknown function"));
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
      LatticeExpr<Bool> latExpr(arg_p[0]);
      if (! arg_p[0].isMasked()) {
	 RO_LatticeIterator<Bool> iter(latExpr);
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
	 RO_MaskedLatticeIterator<Bool> iter(latExpr);
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    iter.getMask (mask);
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
      LatticeExpr<Bool> latExpr(arg_p[0]);
      if (! arg_p[0].isMasked()) {
	 RO_LatticeIterator<Bool> iter(latExpr);
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
	 RO_MaskedLatticeIterator<Bool> iter(latExpr);
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    iter.getMask (mask);
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
   case LELFunctionEnums::FRACTILE1D :
   {
      if (arg_p[0].isScalar()) {
         return arg_p[0].getDouble();
      }
      Vector<Double> fractile = LatticeFractile<Double>::maskedFractile
                                             (LatticeExpr<Double>(arg_p[0]),
					      arg_p[1].getFloat());
      if (fractile.nelements() == 0) {
	 return LELScalar<Double>();             // no valid fractile found
      }
      return fractile(0);
   }
   case LELFunctionEnums::FRACTILERANGE1D :
   {
      Float fraction1 = arg_p[1].getFloat();
      Float fraction2 = fraction1;
      if (arg_p.nelements() > 2) {
	 fraction2 = arg_p[2].getFloat();
      }
      Vector<Double> fractiles = LatticeFractile<Double>::maskedFractiles
                                              (LatticeExpr<Double>(arg_p[0]),
					       fraction1, fraction2);
      if (fractiles.nelements() < 2) {
	 return LELScalar<Double>();             // no valid fractiles found
      }
      return fractiles(1) - fractiles(0);
   }
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
      LatticeExpr<Float> latExpr(expr);
      RO_MaskedLatticeIterator<Float> iter(latExpr);
      Array<Bool> mask;
      while (! iter.atEnd()) {
	 iter.getMask (mask);
	 nelem += nMaskedOn (mask);
	 iter++;
      }
      break;
   }
   case TpDouble:
   {
      LatticeExpr<Double> latExpr(expr);
      RO_MaskedLatticeIterator<Double> iter(latExpr);
      Array<Bool> mask;
      while (! iter.atEnd()) {
	 iter.getMask (mask);
	 nelem += nMaskedOn (mask);
	 iter++;
      }
      break;
   }
   case TpComplex:
   {
      LatticeExpr<Complex> latExpr(expr);
      RO_MaskedLatticeIterator<Complex> iter(latExpr);
      Array<Bool> mask;
      while (! iter.atEnd()) {
	 iter.getMask (mask);
	 nelem += nMaskedOn (mask);
	 iter++;
      }
      break;
   }
   case TpDComplex:
   {
      LatticeExpr<DComplex> latExpr(expr);
      RO_MaskedLatticeIterator<DComplex> iter(latExpr);
      Array<Bool> mask;
      while (! iter.atEnd()) {
	 iter.getMask (mask);
	 nelem += nMaskedOn (mask);
	 iter++;
      }
      break;
   }
   case TpBool:
   {
      LatticeExpr<Bool> latExpr(expr);
      RO_MaskedLatticeIterator<Bool> iter(latExpr);
      Array<Bool> mask;
      while (! iter.atEnd()) {
	 iter.getMask (mask);
	 nelem += nMaskedOn (mask);
	 iter++;
      }
      break;
   }
   default:
      throw (AipsError ("LELFunction2::nMaskedElements - unknown data type"));
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

Bool LELFunctionDouble::lock (FileLocker::LockType type, uInt nattempts)
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].lock (type, nattempts)) {
      return False;
    }
  }
  return True;
}
void LELFunctionDouble::unlock()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].unlock();
  }
}
Bool LELFunctionDouble::hasLock (FileLocker::LockType type) const
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].hasLock (type)) {
      return False;
    }
  }
  return True;
}
void LELFunctionDouble::resync()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].resync();
  }
}



// LELFunctionComplex
LELFunctionComplex::LELFunctionComplex
                                  (const LELFunctionEnums::Function function,
				   const Block<LatticeExprNode>& exp)
: function_p(function)
{
    switch (function_p) {
    case LELFunctionEnums::CONJ :
    {
// Expect 1 Complex argument

       if (exp.nelements() != 1) {
          throw (AipsError ("LELFunctionComplex::constructor - "
			    "function can only have one argument"));
       }
       setAttr(exp[0].getAttribute());
       break;
    }
    case LELFunctionEnums::COMPLEX :
    {
// Expect 2 Float arguments

	Block<Int> argType(2);
	argType[0] = TpFloat;
	argType[1] = TpFloat;
	setAttr (LatticeExprNode::checkArg (exp, argType, False));
	break;
    }
    case LELFunctionEnums::POW :
    {
// Expect 2 Complex arguments

	Block<Int> argType(2);
	argType[0] = TpComplex;
	argType[1] = TpComplex;
	setAttr (LatticeExprNode::checkArg (exp, argType, False));
	break;
    }
    default:
	throw (AipsError ("LELFunctionComplex::constructor - "
			  "unknown Complex function"));
    }
   // Fill the node block here, so an exception does
   // not leave the nodes undestructed.
   arg_p = exp;
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
         throw (AipsError ("LELFunctionComplex::eval - "
			   "unknown Complex function"));
      }
 
   } else {
      if (arg_p[0].isScalar()) {
         switch (function_p) {
	 case LELFunctionEnums::COMPLEX :
	 {
	    Float scalarTemp;
	    LELArrayRef<Float> arrayTemp(result.shape());
	    arg_p[0].eval(scalarTemp);
	    arg_p[1].evalRef(arrayTemp, section);
	    Bool delr, delc;
	    const Float* rptr = arrayTemp.value().getStorage(delr);
	    Complex *cptr = result.value().getStorage(delc);
	    uInt n=arrayTemp.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i] = Complex (scalarTemp, rptr[i]);
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
            throw (AipsError ("LELFunctionComplex::eval - "
			      "unknown Complex function"));
         }

      } else if (arg_p[1].isScalar()) {
         switch (function_p) {
	 case LELFunctionEnums::COMPLEX :
	 {
	    Float scalarTemp;
	    LELArrayRef<Float> arrayTemp(result.shape());
	    arg_p[1].eval(scalarTemp);
	    arg_p[0].evalRef(arrayTemp, section);
	    Bool delr, delc;
	    const Float* rptr = arrayTemp.value().getStorage(delr);
	    Complex *cptr = result.value().getStorage(delc);
	    uInt n=arrayTemp.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i] = Complex (rptr[i], scalarTemp);
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
            throw (AipsError ("LELFunctionComplex::eval - "
			      "unknown Complex function"));
         }

      } else {
         switch(function_p) {
	 case LELFunctionEnums::COMPLEX :
	 {
	    LELArrayRef<Float> arrayLeft(result.shape());
	    LELArrayRef<Float> arrayRight(result.shape());
	    arg_p[0].evalRef(arrayLeft, section);
	    arg_p[1].evalRef(arrayRight, section);
	    Bool dell, delr, delc;
	    const Float* lptr = arrayLeft.value().getStorage(dell);
	    const Float* rptr = arrayRight.value().getStorage(delr);
	    Complex *cptr = result.value().getStorage(delc);
	    uInt n=arrayLeft.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i] = Complex (lptr[i], rptr[i]);
	    }
	    arrayLeft.value().freeStorage(lptr, dell);
	    arrayRight.value().freeStorage(rptr, delr);
	    result.value().putStorage(cptr, delc);
	    result.setMask (arrayLeft, arrayRight);
	    break;
	 }
         case LELFunctionEnums::POW :
         {
	    LELArrayRef<Complex> tempr(result.shape());
	    arg_p[0].eval(result, section);
	    arg_p[1].evalRef(tempr, section);
	    result.combineMask (tempr);
            Array<Complex> temp (pow (result.value(), tempr.value()));
            result.value().reference (temp);
            break;
         }
         default:
            throw(AipsError("LELFunctionComplex::eval - "
			    "unknown function"));
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

Bool LELFunctionComplex::lock (FileLocker::LockType type, uInt nattempts)
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].lock (type, nattempts)) {
      return False;
    }
  }
  return True;
}
void LELFunctionComplex::unlock()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].unlock();
  }
}
Bool LELFunctionComplex::hasLock (FileLocker::LockType type) const
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].hasLock (type)) {
      return False;
    }
  }
  return True;
}
void LELFunctionComplex::resync()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].resync();
  }
}



// LELFunctionDComplex
LELFunctionDComplex::LELFunctionDComplex
                                  (const LELFunctionEnums::Function function,
				   const Block<LatticeExprNode>& exp)
: function_p(function)
{
    switch (function_p) {
    case LELFunctionEnums::CONJ :
    {
// Expect 1 Complex argument

       if (exp.nelements() != 1) {
          throw (AipsError ("LELFunctionDComplex::constructor - "
			    "functions can only have one argument"));
       }
       setAttr(exp[0].getAttribute());
       break;
    }
    case LELFunctionEnums::COMPLEX :
    {
// Expect 2 Double arguments

	Block<Int> argType(2);
	argType[0] = TpDouble;
	argType[1] = TpDouble;
	setAttr (LatticeExprNode::checkArg (exp, argType, False));
	break;
    }
    case LELFunctionEnums::POW :
    {
	// Expect 2 DComplex arguments
	Block<Int> argType(2);
	argType[0] = TpDComplex;
	argType[1] = TpDComplex;
	setAttr (LatticeExprNode::checkArg (exp, argType, False));
	break;
    }
    default:
	throw (AipsError ("LELFunctionDComplex::constructor - "
			  "unknown DComplex function"));
    }
   // Fill the node block here, so an exception does
   // not leave the nodes undestructed.
   arg_p = exp;
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
         throw (AipsError ("LELFunctionDComplex::eval - "
			   "unknown Complex function"));
      }
 
   } else { 
      if (arg_p[0].isScalar()) {
         switch (function_p) {
	 case LELFunctionEnums::COMPLEX :
	 {
	    Double scalarTemp;
	    LELArrayRef<Double> arrayTemp(result.shape());
	    arg_p[0].eval(scalarTemp);
	    arg_p[1].evalRef(arrayTemp, section);
	    Bool delr, delc;
	    const Double* rptr = arrayTemp.value().getStorage(delr);
	    DComplex *cptr = result.value().getStorage(delc);
	    uInt n=arrayTemp.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i] = DComplex (scalarTemp, rptr[i]);
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
            throw (AipsError ("LELFunctionDComplex::eval - "
			      "unknown DComplex function"));
         }

      } else if (arg_p[1].isScalar()) {
         switch (function_p) {
	 case LELFunctionEnums::COMPLEX :
	 {
	    Double scalarTemp;
	    LELArrayRef<Double> arrayTemp(result.shape());
	    arg_p[1].eval(scalarTemp);
	    arg_p[0].evalRef(arrayTemp, section);
	    Bool delr, delc;
	    const Double* rptr = arrayTemp.value().getStorage(delr);
	    DComplex *cptr = result.value().getStorage(delc);
	    uInt n=arrayTemp.value().nelements();
	    for (uInt i=0; i<n; i++) {
		cptr[i] = DComplex (rptr[i], scalarTemp);
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
            throw (AipsError ("LELFunctionDComplex::eval -"
			      "unknown DComplex function"));
         }

      } else {
         switch(function_p) {
 	 case LELFunctionEnums::COMPLEX :
	 {
	    LELArrayRef<Double> arrayLeft(result.shape());
	    LELArrayRef<Double> arrayRight(result.shape());
	    arg_p[0].evalRef(arrayLeft, section);
	    arg_p[1].evalRef(arrayRight, section);
	    Bool dell, delr, delc;
	    const Double* lptr = arrayLeft.value().getStorage(dell);
	    const Double* rptr = arrayRight.value().getStorage(delr);
	    DComplex *cptr = result.value().getStorage(delc);
	    uInt n=arrayLeft.value().nelements();
	    for (uInt i=0; i<n; i++) {
	      cptr[i] = DComplex (lptr[i], rptr[i]);
	    }
	    arrayLeft.value().freeStorage(lptr, dell);
	    arrayRight.value().freeStorage(rptr, delr);
	    result.value().putStorage(cptr, delc);
	    result.setMask (arrayLeft, arrayRight);
	    break;
	 }
        case LELFunctionEnums::POW :
         {
	    LELArrayRef<DComplex> tempr(result.shape());
	    arg_p[0].eval(result, section);
	    arg_p[1].evalRef(tempr, section);
	    result.combineMask (tempr);
            Array<DComplex> temp (pow (result.value(), tempr.value()));
            result.value().reference (temp);
            break;
         }
         default:
            throw(AipsError("LELFunctionDComplex::eval - "
			    "unknown function"));
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

Bool LELFunctionDComplex::lock (FileLocker::LockType type, uInt nattempts)
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].lock (type, nattempts)) {
      return False;
    }
  }
  return True;
}
void LELFunctionDComplex::unlock()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].unlock();
  }
}
Bool LELFunctionDComplex::hasLock (FileLocker::LockType type) const
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].hasLock (type)) {
      return False;
    }
  }
  return True;
}
void LELFunctionDComplex::resync()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].resync();
  }
}



// LELFunctionBool
LELFunctionBool::LELFunctionBool(const LELFunctionEnums::Function function,
				 const Block<LatticeExprNode>& exp)
: function_p(function)
{
    switch (function_p) {
    case LELFunctionEnums::ISNAN :
    {
        if (exp.nelements() != 1) {
	    throw (AipsError ("LELFunctionBool::constructor - "
			      "function can only have one argument"));
	}
	if (exp[0].dataType() == TpBool) {
	    throw (AipsError ("LELFunctionBool::constructor - "
			      "function isNaN cannot have bool argument"));
	}
	setAttr(exp[0].getAttribute());
	break;
    }
    case LELFunctionEnums::ALL :
    case LELFunctionEnums::ANY :
    {
	Block<Int> argType(1);
	argType[0] = TpBool;
	LatticeExprNode::checkArg (exp, argType, True); // expect 1 Bool array
	setAttr (LELAttribute());                       // result is scalar
	break;
    }
    case LELFunctionEnums::INDEXIN :
    {
       // The first argument must be a real scalar.
       if (! (exp[0].isScalar()  &&
              (exp[0].dataType()==TpFloat || exp[0].dataType()==TpDouble))) {
          throw (AipsError ("LELFunctionBool::constructor - "
			    "1st argument of INDEXIN function "
			    "should be a real scalar"));
       }
       // The second argument must be a bool vector.
       if (exp[1].isScalar()  ||  exp[1].dataType()!=TpBool
       ||  exp[1].shape().nelements() != 1) {
          throw (AipsError ("LELFunctionBool::constructor - "
			    "2nd argument of INDEXIN function "
			    "should be a bool vector"));
       }
       // The output shape is unknown.
       setAttr (LELAttribute (False, IPosition(), IPosition(),
			      LELCoordinates()));
       break;
    }
    case LELFunctionEnums::MASK :
    case LELFunctionEnums::VALUE :
    {
       if (exp.nelements() != 1) {
          throw (AipsError ("LELFunctionBool::constructor - "
			    "function can only have one argument"));
       }
       // The value or mask itself is unmasked.
       const LELAttribute& argAttr = exp[0].getAttribute();
       if (argAttr.isScalar()) {
	  setAttr (LELAttribute());
       } else {
	  setAttr (LELAttribute (False, argAttr.shape(), argAttr.tileShape(),
				 argAttr.coordinates()));
       }
       break;
    }
    default:
	throw (AipsError ("LELFunctionBool::constructor - "
			  "unknown Bool function"));
    }
    // Fill the node block here, so an exception does
    // not leave the nodes undestructed.
    arg_p = exp;
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


void LELFunctionBool::eval(LELArray<Bool>& result,
			   const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionBool:: eval" << endl;
#endif

   switch (function_p) {
   case LELFunctionEnums::ISNAN :
   {
      Bool deleteIn, deleteOut;
      Bool* out = result.value().getStorage(deleteOut);
      uInt nr = result.value().nelements();
      if (arg_p[0].dataType() == TpFloat) {
	 LELArrayRef<Float> tmp(result.shape());
	 arg_p[0].evalRef(tmp, section);
	 result.setMask(tmp);
	 const Float* in = tmp.value().getStorage(deleteIn);
	 for (uInt i=0; i<nr; i++) {
	   out[i] = isNaN(in[i]);
	 }
	 tmp.value().freeStorage (in, deleteIn);
      } else if (arg_p[0].dataType() == TpDouble) {
	 LELArrayRef<Double> tmp(result.shape());
	 arg_p[0].evalRef(tmp, section);
	 result.setMask(tmp);
	 const Double* in = tmp.value().getStorage(deleteIn);
	 for (uInt i=0; i<nr; i++) {
	   out[i] = isNaN(in[i]);
	 }
	 tmp.value().freeStorage (in, deleteIn);
      } else if (arg_p[0].dataType() == TpComplex) {
	 LELArrayRef<Complex> tmp(result.shape());
	 arg_p[0].evalRef(tmp, section);
	 result.setMask(tmp);
	 const Complex* in = tmp.value().getStorage(deleteIn);
	 for (uInt i=0; i<nr; i++) {
	   out[i] = isNaN(in[i]);
	 }
	 tmp.value().freeStorage (in, deleteIn);
      } else {
	 LELArrayRef<DComplex> tmp(result.shape());
	 arg_p[0].evalRef(tmp, section);
	 result.setMask(tmp);
	 const DComplex* in = tmp.value().getStorage(deleteIn);
	 for (uInt i=0; i<nr; i++) {
	   out[i] = isNaN(in[i]);
	 }
	 tmp.value().freeStorage (in, deleteIn);
      }
      result.value().putStorage (out, deleteOut);
      break;
   }
   case LELFunctionEnums::INDEXIN :
   {
      Double daxis;
      if (arg_p[0].dataType() == TpFloat) {
	daxis = arg_p[0].getFloat();
      } else {
	daxis = arg_p[0].getDouble();
      }
      Int axis = Int(daxis+0.499);    // add for rounding
      if (axis < 0) {
	throw (AipsError ("Axis argument in INDEXIN function is < 1; "
			  "(note axis is 0-relative!)"));
      } else if (axis >= Int(section.ndim())) {
	throw (AipsError ("Axis argument in INDEXIN function outside array; "
			  "(note axis is 0-relative!)"));
      }
      uInt stinx = section.start()[axis];
      Array<Bool> tmp(section.length());
      const IPosition& shp = tmp.shape();
      uInt nrinx = stinx + shp[axis];
      uInt nr1 = 1;
      for (Int i=0; i<axis; i++) {
	nr1 *= shp[i];
      }
      uInt nr2 = 1;
      for (uInt i=axis+1; i<shp.nelements(); i++) {
	nr2 *= shp[i];
      }
      Array<Bool> pixelArr = arg_p[1].getArrayBool();
      Bool deletePix;
      const Bool* pixels = pixelArr.getStorage (deletePix);
      uInt nrpix = pixelArr.nelements();
      Bool deleteIt;
      Bool* tmpp = tmp.getStorage (deleteIt);
      uInt inx = 0;
      if (nr1 == 1) {
	for (uInt i1=0; i1<nr2; i1++) {
	  for (uInt i2=stinx; i2<nrinx; i2++) {
	    tmpp[inx++] = (i2<nrpix  ?  pixels[i2] : False);
	  }
	}
      } else {
	for (uInt i1=0; i1<nr2; i1++) {
	  for (uInt i2=stinx; i2<nrinx; i2++) {
	    Bool flag = (i2<nrpix  ?  pixels[i2] : False);
	    for (uInt i3=0; i3<nr1; i3++) {
	      tmpp[inx++] = flag;
	    }
	  }
	}
      }
      pixelArr.freeStorage (pixels, deletePix);
      tmp.putStorage (tmpp, deleteIt);
      result.value().reference (tmp);
      break;
   }
   case LELFunctionEnums::MASK :
   {
      result.removeMask();
      if (! arg_p[0].isMasked()) {
	 result.value() = True;
      } else {
	 switch (arg_p[0].dataType()) {
	 case TpFloat:
	 {
	    LELArrayRef<Float> tmp(result.shape());
	    arg_p[0].evalRef(tmp, section);
	    result.value() = tmp.mask();
	    break;
	 }
	 case TpDouble:
	 {
	    LELArrayRef<Double> tmp(result.shape());
	    arg_p[0].evalRef(tmp, section);
	    result.value() = tmp.mask();
	    break;
	 }
	 case TpComplex:
	 {
	    LELArrayRef<Complex> tmp(result.shape());
	    arg_p[0].evalRef(tmp, section);
	    result.value() = tmp.mask();
	    break;
	 }
	 case TpDComplex:
	 {
	    LELArrayRef<DComplex> tmp(result.shape());
	    arg_p[0].evalRef(tmp, section);
	    result.value() = tmp.mask();
	    break;
	 }
	 case TpBool:
	 {
	    LELArrayRef<Bool> tmp(result.shape());
	    arg_p[0].evalRef(tmp, section);
	    result.value() = tmp.mask();
	    break;
	 }
	 default:
	    throw (AipsError ("LELFunction2::eval - unknown data type"));
	 }
      }
      break;
   }
   case LELFunctionEnums::VALUE :
   {
      arg_p[0].eval (result, section);
      result.removeMask();
      break;
   }
   default:
      throw(AipsError("LELFunctionBool::eval - unknown function"));
   }
}

LELScalar<Bool> LELFunctionBool::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionBool:: getScalar" << endl;
#endif

// Apply the  function
   switch(function_p) {
   case LELFunctionEnums::ISNAN :
   {
      if (arg_p[0].dataType() == TpFloat) {
	 return isNaN (arg_p[0].getFloat());
      } else if (arg_p[0].dataType() == TpDouble) {
	 return isNaN (arg_p[0].getDouble());
      } else if (arg_p[0].dataType() == TpComplex) {
	 return isNaN (arg_p[0].getComplex());
      } else {
	 return isNaN (arg_p[0].getDComplex());
      }
      break;
   }
   case LELFunctionEnums::ALL :
   {
      Bool deleteIt, deleteMask;
      LatticeExpr<Bool> latExpr(arg_p[0]);
      if (! arg_p[0].isMasked()) {
	 RO_LatticeIterator<Bool> iter(latExpr);
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
	 RO_MaskedLatticeIterator<Bool> iter(latExpr);
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    iter.getMask (mask);
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
      LatticeExpr<Bool> latExpr(arg_p[0]);
      if (! arg_p[0].isMasked()) {
	 RO_LatticeIterator<Bool> iter(latExpr);
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
	 RO_MaskedLatticeIterator<Bool> iter(latExpr);
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<Bool>& array = iter.cursor();
	    iter.getMask (mask);
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
   case LELFunctionEnums::MASK :
      return  (! arg_p[0].isInvalidScalar());
   case LELFunctionEnums::VALUE :
      return arg_p[0].getBool();
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
          &&  function_p != LELFunctionEnums::ANY
          &&  function_p != LELFunctionEnums::MASK) {
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

Bool LELFunctionBool::lock (FileLocker::LockType type, uInt nattempts)
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].lock (type, nattempts)) {
      return False;
    }
  }
  return True;
}
void LELFunctionBool::unlock()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].unlock();
  }
}
Bool LELFunctionBool::hasLock (FileLocker::LockType type) const
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].hasLock (type)) {
      return False;
    }
  }
  return True;
}
void LELFunctionBool::resync()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].resync();
  }
}

} //# NAMESPACE CASACORE - END

