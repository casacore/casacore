//# LELFunction.cc:  this defines non-templated classes in LELFunction.h
//# Copyright (C) 1997,1998
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

// Returns a real number

    {
       if (arg_p.nelements() != 1) {
          throw (AipsError ("LELFunctionFloat::constructor - functions can only"
                            "have one argument"));
       }
       setAttr(arg_p[0].getAttribute());
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
	if (function_p == LELFunctionEnums::POW) {
	    if (arg_p[0].isScalar()  &&  ! arg_p[1].isScalar()) {
		throw (AipsError ("LELFunctionFloat::constructor - POW(scalar,lattice) "
				  "is not possible"));
	    }
	} else if (function_p != LELFunctionEnums::MIN  &&
		   function_p != LELFunctionEnums::MAX) {
	    if (arg_p[0].isScalar()  !=  arg_p[1].isScalar()) {
		throw (AipsError ("LELFunctionFloat::constructor - arguments for function "
				  "should be both scalar or both array"));
	    }
	}
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


void LELFunctionFloat::eval(Array<Float>& result,
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
            Array<Float> tmp(abs(result));
            result.reference(tmp);
         } else {
            Array<Complex> tmpC(result.shape());
            arg_p[0].eval(tmpC, section);
   	    amplitude(result, tmpC);
         }
         break;
      }
      case LELFunctionEnums::ARG :
      {
         Array<Complex> tmpC(result.shape());
         arg_p[0].eval(tmpC, section);
         phase(result, tmpC);
         break;
      }
      case LELFunctionEnums::REAL :
      {
         if (arg_p[0].dataType() == TpFloat) {
            arg_p[0].eval(result, section);      // The real part of a real is the real !
         } else {
            Array<Complex> tmpC(result.shape());
            arg_p[0].eval(tmpC, section);
   	    real(result, tmpC);
         }
         break;
      }
      case LELFunctionEnums::IMAG :
      {
         Array<Complex> tmpC(result.shape());
         arg_p[0].eval(tmpC, section);
         imag(result, tmpC);
         break;
      }
      default:
         throw (AipsError ("LELFunctionFloat::eval - unknown Float function"));
      }
   } else {   
      if (arg_p[0].isScalar()) {
         Float scalarTemp;
         arg_p[0].eval(scalarTemp);
         Array<Float> arrayTemp(result.shape());
         arg_p[1].eval(arrayTemp, section);
         switch (function_p) {
         case LELFunctionEnums::MIN :
   	    min (result, arrayTemp, scalarTemp);
   	    break;
         case LELFunctionEnums::MAX :
   	    max (result, arrayTemp, scalarTemp);
   	    break;
         default:
            throw (AipsError ("LELFunctionFloat::eval - unknown Float function"));
         }

      } else if (arg_p[1].isScalar()) {
         Float scalarTemp;
         arg_p[1].eval(scalarTemp);
         Array<Float> arrayTemp(result.shape());
         arg_p[0].eval(arrayTemp, section);
         switch (function_p) {
         case LELFunctionEnums::MIN :
	    min (result, arrayTemp, scalarTemp);
   	    break;
         case LELFunctionEnums::MAX :
	    max (result, arrayTemp, scalarTemp);
	    break;
         case LELFunctionEnums::POW :
         {
   	    Array<Float> temp (pow (arrayTemp, Double(scalarTemp)));
	    result.reference (temp);
	    break;
         }
         default:
	    throw (AipsError ("LELFunctionFloat::eval - unknown Float function"));
         }

      } else {
         Array<Float> tempLeft(result.shape());
         Array<Float> tempRight(result.shape());
         arg_p[0].eval(tempLeft, section);
         arg_p[1].eval(tempRight, section);
         switch(function_p) {
         case LELFunctionEnums::ATAN2 :
         {
   	    Array<Float> temp (atan2 (tempLeft, tempRight));
   	    result.reference (temp);
	    break;
         }
         case LELFunctionEnums::POW :
         {
   	    Array<Float> temp (pow (tempLeft, tempRight));
	    result.reference (temp);
	    break;
         }
         case LELFunctionEnums::FMOD :
         {
   	    Array<Float> temp (fmod (tempLeft, tempRight));
	    result.reference (temp);
	    break;
         }
         case LELFunctionEnums::MIN :
	    min(result, tempLeft, tempRight);
	    break;
         case LELFunctionEnums::MAX :
   	    max(result, tempLeft, tempRight);
	    break;
         default:
	    throw(AipsError("LELFunctionFloat::eval - unknown function"));
         }
      }
   }
}

Float LELFunctionFloat::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionFloat:: getScalar" << endl;
#endif

   switch (function_p) {
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
   return 0;                            // Make compiler happy
}

void LELFunctionFloat::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionFloat::prepare" << endl;
#endif

   for (uInt i=0; i<arg_p.nelements(); i++) {
       arg_p[i].replaceScalarExpr();
   }
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

// Returns a real number

    {
       if (arg_p.nelements() != 1) {
          throw (AipsError ("LELFunctionDouble::constructor - functions can only"
                            "have one argument"));
       }
       setAttr(arg_p[0].getAttribute());
       break;
    }
    case LELFunctionEnums::NELEM :
    {
	setAttr (LELAttribute());                         // result is scalar
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
	if (function_p == LELFunctionEnums::POW) {
	    if (arg_p[0].isScalar()  &&  ! arg_p[1].isScalar()) {
		throw (AipsError ("LELFunctionDouble::constructor - POW(scalar,lattice) "
				  "is not possible"));
	    }
	} else if (function_p != LELFunctionEnums::MIN  &&
		   function_p != LELFunctionEnums::MAX) {
	    if (arg_p[0].isScalar()  !=  arg_p[1].isScalar()) {
		throw (AipsError ("LELFunctionDouble::constructor - arguments for function "
				  "should be both scalar or both array"));
	    }
	}
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


void LELFunctionDouble::eval(Array<Double>& result,
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
            Array<Double> tmp(abs(result));
            result.reference(tmp);
         } else {
            Array<DComplex> tmpC(result.shape());
            arg_p[0].eval(tmpC, section);
            amplitude(result, tmpC);
         }
         break;
      }
      case LELFunctionEnums::ARG :
      {
         Array<DComplex> tmpC(result.shape());
         arg_p[0].eval(tmpC, section);
         phase(result, tmpC);
         break;
      }
      case LELFunctionEnums::REAL :
      {
         if (arg_p[0].dataType() == TpDouble) {
            arg_p[0].eval(result, section);      // The real part of a real is the real !
         } else {
            Array<DComplex> tmpC(result.shape());
            arg_p[0].eval(tmpC, section);
   	    real(result, tmpC);
         }
         break;
      }
      case LELFunctionEnums::IMAG :
      {
         Array<DComplex> tmpC(result.shape());
         arg_p[0].eval(tmpC, section);
         imag(result, tmpC);
         break;
      }
      default:
         throw (AipsError ("LELFunctionDouble::eval - unknown Double function"));
      }
   } else {
      if (arg_p[0].isScalar()) {
         Double scalarTemp;
         arg_p[0].eval(scalarTemp);
         Array<Double> arrayTemp(result.shape());
         arg_p[1].eval(arrayTemp, section);
         switch (function_p) {
         case LELFunctionEnums::MIN :
   	    min (result, arrayTemp, scalarTemp);
   	    break;
         case LELFunctionEnums::MAX :
	    max (result, arrayTemp, scalarTemp);
	    break;
         default:
	    throw (AipsError ("LELFunctionDouble::eval - unknown Double function"));
         }

      } else if (arg_p[1].isScalar()) {
         Double scalarTemp;
         arg_p[1].eval(scalarTemp);
         Array<Double> arrayTemp(result.shape());
         arg_p[0].eval(arrayTemp, section);
         switch (function_p) {
         case LELFunctionEnums::MIN :
            min (result, arrayTemp, scalarTemp);
            break;
         case LELFunctionEnums::MAX :
	    max (result, arrayTemp, scalarTemp);
	    break;
         case LELFunctionEnums::POW :
         {
            Array<Double> temp (pow (arrayTemp, scalarTemp));
            result.reference (temp);
            break;
         }
         default:
            throw (AipsError ("LELFunctionDouble::eval - unknown Double function"));
         }

      } else {
         Array<Double> tempLeft(result.shape());
         Array<Double> tempRight(result.shape());
         arg_p[0].eval(tempLeft, section);
         arg_p[1].eval(tempRight, section);
         switch(function_p) {
         case LELFunctionEnums::ATAN2 :
         {
            Array<Double> temp (atan2 (tempLeft, tempRight));
            result.reference (temp);
   	    break;
         }
         case LELFunctionEnums::POW :
         {
            Array<Double> temp (pow (tempLeft, tempRight));
            result.reference (temp);
            break;
         }
         case LELFunctionEnums::FMOD :
         {
            Array<Double> temp (fmod (tempLeft, tempRight)); 
            result.reference (temp);
            break;
         }
         case LELFunctionEnums::MIN :
            min(result, tempLeft, tempRight);
            break;
         case LELFunctionEnums::MAX :
            max(result, tempLeft, tempRight);
            break;
         default:
            throw(AipsError("LELFunctionDouble::eval - unknown function"));
         }
      }
   }
}

Double LELFunctionDouble::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDouble:: getScalar" << endl;
#endif

   switch (function_p) {
   case LELFunctionEnums::NTRUE :
   {
      uInt ntrue = 0;
      Bool deleteIt;
      LatticeExpr<Bool> latExpr(arg_p[0], 0);
      RO_LatticeIterator<Bool> iter(latExpr, latExpr.niceCursorShape());
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
      return ntrue;
   }
   case LELFunctionEnums::NFALSE :
   {
      uInt nfalse = 0;
      Bool deleteIt;
      LatticeExpr<Bool> latExpr(arg_p[0], 0);
      RO_LatticeIterator<Bool> iter(latExpr, latExpr.niceCursorShape());
      while (! iter.atEnd()) {
	 const Array<Bool>& array = iter.cursor();
	 const Bool* data = array.getStorage (deleteIt);
	 uInt n = array.nelements();
	 for (uInt i=0; i<n; i++) {
	    if (! data[i]) {
	       nfalse++;
	    }
	 }
	 array.freeStorage (data, deleteIt);
	 iter++;
      }
      return nfalse;
   }
   case LELFunctionEnums::NELEM :
       if (arg_p[0].isScalar()) {
	  return 1;
       }
       return arg_p[0].shape().product();
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
   return 0;                          // Make compiler happy
}

void LELFunctionDouble::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDouble::prepare" << endl;
#endif

   for (uInt i=0; i<arg_p.nelements(); i++) {
       arg_p[i].replaceScalarExpr();
   }
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
    case LELFunctionEnums::POW :
    {
// Expect 2 Complex arguments

	Block<Int> argType(2);
	argType[0] = TpComplex;
	argType[1] = TpComplex;
	setAttr (LatticeExprNode::checkArg (arg_p, argType, False));
	if (arg_p[0].isScalar()  &&  ! arg_p[1].isScalar()) {
	   throw (AipsError ("LELFunctionComplex::constructor - POW(scalar,lattice) "
			     "is not possible"));
	}
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


void LELFunctionComplex::eval(Array<Complex>& result,
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
         Array<Complex> tmpC(conj(result));
         result.reference(tmpC);
         break;
      }
      default:
         throw (AipsError ("LELFunctionComplex::eval - unknown Complex function"));
      }
 
   } else {
      if (arg_p[0].isScalar()) {
         throw (AipsError ("LELFunctionComplex::eval - unknown Complex function"));

      } else if (arg_p[1].isScalar()) {
         Complex scalarTemp;
         arg_p[1].eval(scalarTemp);
         if (scalarTemp.imag() != 0) {
   	    throw (AipsError ("LELFunctionComplex::eval - When raising a complex lattice to a "
			      "power the scalar exponent should be real"));
         }
         Double exponent = scalarTemp.real();
         Array<Complex> arrayTemp(result.shape());
         arg_p[0].eval(arrayTemp, section);
         switch (function_p) {
         case LELFunctionEnums::POW :
         {
            Array<Complex> temp (pow (arrayTemp, exponent));
            result.reference (temp);
            break;
         }
         default:
            throw (AipsError ("LELFunctionComplex::eval - unknown Complex function"));
         }

      } else {
         Array<Complex> tempLeft(result.shape());
         Array<Complex> tempRight(result.shape());
         arg_p[0].eval(tempLeft, section);
         arg_p[1].eval(tempRight, section);
         switch(function_p) {
         case LELFunctionEnums::POW :
         {
            Array<Complex> temp (pow (tempLeft, tempRight));
            result.reference (temp);
            break;
         }
         default:
            throw(AipsError("LELFunctionComplex::eval - unknown function"));
         }
      }
   }
}

Complex LELFunctionComplex::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionComplex:: getScalar" << endl;
#endif

   switch (function_p) {
   case LELFunctionEnums::CONJ :
       return conj(arg_p[0].getComplex());
   case LELFunctionEnums::POW :
       return pow(arg_p[0].getComplex(), arg_p[1].getComplex());
   default:
      throw(AipsError("LELFunctionComplex::getScalar - unknown function"));
   }
   return False;                       // Make compiler happy
}

void LELFunctionComplex::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionComplex::prepare" << endl;
#endif

   for (uInt i=0; i<arg_p.nelements(); i++) {
       arg_p[i].replaceScalarExpr();
   }
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
    case LELFunctionEnums::POW :
    {
	// Expect 2 DComplex arguments
	Block<Int> argType(2);
	argType[0] = TpDComplex;
	argType[1] = TpDComplex;
	setAttr (LatticeExprNode::checkArg (arg_p, argType, False));
	if (arg_p[0].isScalar()  &&  ! arg_p[1].isScalar()) {
	   throw (AipsError ("LELFunctionDComplex::constructor - POW(scalar,lattice) "
			     "is not possible"));
	}
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


void LELFunctionDComplex::eval(Array<DComplex>& result,
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
         Array<DComplex> tmpC(conj(result));
         result.reference(tmpC);
         break;
      }
      default:
         throw (AipsError ("LELFunctionDComplex::eval - unknown Complex function"));
      }
 
   } else { 
      if (arg_p[0].isScalar()) {
         throw (AipsError ("LELFunctionDComplex::eval - unknown DComplex function"));

      } else if (arg_p[1].isScalar()) {
         DComplex scalarTemp;
         arg_p[1].eval(scalarTemp);
         if (scalarTemp.imag() != 0) {
            throw (AipsError ("LELFunctionDComplex::eval - When raising a complex lattice to a "
                             "power the scalar exponent should be real"));
         }
         Double exponent = scalarTemp.real();
         Array<DComplex> arrayTemp(result.shape());
         arg_p[0].eval(arrayTemp, section);
         switch (function_p) {
         case LELFunctionEnums::POW :
         {
            Array<DComplex> temp (pow (arrayTemp, exponent));
            result.reference (temp);
            break;
         }
         default:
            throw (AipsError ("LELFunctionDComplex::eval - unknown DComplex function"));
         }

      } else {
         Array<DComplex> tempLeft(result.shape());
         Array<DComplex> tempRight(result.shape());
         arg_p[0].eval(tempLeft, section);
         arg_p[1].eval(tempRight, section);
         switch(function_p) {
         case LELFunctionEnums::POW :
         {
            Array<DComplex> temp (pow (tempLeft, tempRight));
            result.reference (temp);
            break;
         }
         default:
            throw(AipsError("LELFunctionDComplex::eval - unknown function"));
         }
      }
   }
}

DComplex LELFunctionDComplex::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDComplex:: getScalar" << endl;
#endif

   switch (function_p) {
   case LELFunctionEnums::CONJ :
       return conj(arg_p[0].getDComplex());
   case LELFunctionEnums::POW :
       return pow(arg_p[0].getDComplex(), arg_p[1].getDComplex());
   default:
      throw(AipsError("LELFunctionDComplex::getScalar - unknown function"));
   }
   return False;                       // Make compiler happy
}

void LELFunctionDComplex::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionDComplex::prepare" << endl;
#endif

   for (uInt i=0; i<arg_p.nelements(); i++) {
       arg_p[i].replaceScalarExpr();
   }
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


void LELFunctionBool::eval(Array<Bool>&,
			   const Slicer&) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionBool:: eval" << endl;
#endif

// All Bool function result in a scalar, so cannot be used.
   throw (AipsError ("LELFunctionBool::eval - cannot be used"));
}

Bool LELFunctionBool::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionBool:: getScalar" << endl;
#endif

// Apply the  function
   switch(function_p) {
   case LELFunctionEnums::ALL :
   {
      Bool deleteIt;
      LatticeExpr<Bool> latExpr(arg_p[0], 0);
      RO_LatticeIterator<Bool> iter(latExpr, latExpr.niceCursorShape());
      while (! iter.atEnd()) {
	 const Array<Bool>& array = iter.cursor();
	 const Bool* data = array.getStorage (deleteIt);
	 uInt n = array.nelements();
	 for (uInt i=0; i<n; i++) {
	    if (! data[i]) {
	       array.freeStorage (data, deleteIt);
	       return False;
	    }
	 }
	 array.freeStorage (data, deleteIt);
	 iter++;
      }
      return True;
   }
   case LELFunctionEnums::ANY :
   {
      Bool deleteIt;
      LatticeExpr<Bool> latExpr(arg_p[0], 0);
      RO_LatticeIterator<Bool> iter(latExpr, latExpr.niceCursorShape());
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
      return False;
   }
   default:
      throw(AipsError("LELFunctionBool::getScalar - unknown function"));
   }
   return False;                       // Make compiler happy
}

void LELFunctionBool::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionBool::prepare" << endl;
#endif

   for (uInt i=0; i<arg_p.nelements(); i++) {
       arg_p[i].replaceScalarExpr();
   }
}

String LELFunctionBool::className() const
{
   return String("LELFunctionBool");
}
