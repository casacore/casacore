//# LELBinary.cc:  this defines templated classes in LELBinary.h
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

#include <trial/Lattices/LELBinary.h>
#include <trial/Lattices/PixelRegion.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h> 



template <class T>
LELBinary<T>::LELBinary(const LELBinaryEnums::Operation op,
			const CountedPtr<LELInterface<T> >& pLeftExpr,
			const CountedPtr<LELInterface<T> >& pRightExpr)
: op_p(op),  pLeftExpr_p(pLeftExpr),  pRightExpr_p(pRightExpr)
{
   setAttr (LELAttribute(pLeftExpr_p->getAttribute(),
			 pRightExpr_p->getAttribute()));

#if defined(AIPS_TRACE)
   cout << "LELBinary: constructor" << endl;
   cout << "LELBinary: left.name = " << pLeftExpr->className() << endl;
   cout << "LELBinary: right.name = " << pRightExpr->className() << endl;
#endif
}


template <class T>
LELBinary<T>::~LELBinary()
{
#if defined(AIPS_TRACE)
   cout << "LELBinary: destructor" << endl;
#endif
}

template <class T>
void LELBinary<T>::eval(Array<T>& result,
			const PixelRegion& region) const
{
#if defined(AIPS_TRACE)
   cout << "LELBinary: eval " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::ADD :
       if (pLeftExpr_p->isScalar()) {
          pRightExpr_p->eval(result, region);
	  result += pLeftExpr_p->getScalar();
       } else if (pRightExpr_p->isScalar()) {
          pLeftExpr_p->eval(result, region);
	  result += pRightExpr_p->getScalar();
       } else {
          Array<T> temp(result.shape());
	  pLeftExpr_p->eval(result, region);
	  pRightExpr_p->eval(temp, region);
	  result += temp;
       }
       break;
   case LELBinaryEnums::SUBTRACT:
       if (pLeftExpr_p->isScalar()) {
          pRightExpr_p->eval(result, region);
	  result = pLeftExpr_p->getScalar() - result;
       } else if (pRightExpr_p->isScalar()) {
          pLeftExpr_p->eval(result, region);
	  result -= pRightExpr_p->getScalar();
       } else {
          Array<T> temp(result.shape());
	  pLeftExpr_p->eval(result, region);
	  pRightExpr_p->eval(temp, region);
	  result -= temp;
       }
       break;
   case LELBinaryEnums::MULTIPLY:
       if (pLeftExpr_p->isScalar()) {
          pRightExpr_p->eval(result, region);
	  result *= pLeftExpr_p->getScalar();
       } else if (pRightExpr_p->isScalar()) {
          pLeftExpr_p->eval(result, region);
	  result *= pRightExpr_p->getScalar();
       } else {
          Array<T> temp(result.shape());
	  pLeftExpr_p->eval(result, region);
	  pRightExpr_p->eval(temp, region);
	  result *= temp;
       }
       break;
   case LELBinaryEnums::DIVIDE:
       if (pLeftExpr_p->isScalar()) {
          pRightExpr_p->eval(result, region);
	  result = pLeftExpr_p->getScalar() / result;
       } else if (pRightExpr_p->isScalar()) {
          pLeftExpr_p->eval(result, region);
	  result /= pRightExpr_p->getScalar();
       } else {
          Array<T> temp(result.shape());
	  pLeftExpr_p->eval(result, region);
	  pRightExpr_p->eval(temp, region);
	  result /= temp;
       }
       break;
   default:
       throw(AipsError("LELBinary::eval - unknown operation"));
   }

}


template <class T>
T LELBinary<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELBinary: getScalar " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::ADD :
      return pLeftExpr_p->getScalar() + pRightExpr_p->getScalar();
   case LELBinaryEnums::SUBTRACT :
      return pLeftExpr_p->getScalar() - pRightExpr_p->getScalar();
   case LELBinaryEnums::MULTIPLY :
      return pLeftExpr_p->getScalar() * pRightExpr_p->getScalar();
   case LELBinaryEnums::DIVIDE :
      return pLeftExpr_p->getScalar() / pRightExpr_p->getScalar();
   default:
       throw(AipsError("LELBinary::getScalar - unknown operation"));
   }
   return 0;
}


template <class T>
void LELBinary<T>::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELBinary::prepare" << endl;
#endif

    LELInterface<T>::replaceScalarExpr (pLeftExpr_p);
    LELInterface<T>::replaceScalarExpr (pRightExpr_p);
}


template <class T>
String LELBinary<T>::className() const
{
   return String("LELBinary");
}




// LELBinaryCmp
template <class T>
LELBinaryCmp<T>::LELBinaryCmp(const LELBinaryEnums::Operation op,
			      const CountedPtr<LELInterface<T> >& pLeftExpr,
			      const CountedPtr<LELInterface<T> >& pRightExpr)
: op_p(op), pLeftExpr_p(pLeftExpr), pRightExpr_p(pRightExpr)
{
   setAttr (LELAttribute(pLeftExpr_p->getAttribute(),
			 pRightExpr_p->getAttribute()));

#if defined(AIPS_TRACE)
   cout << "LELBinaryCmp: constructor" << endl;
   cout << "LELBinaryCmp: left.name = " << pLeftExpr->className() << endl;
   cout << "LELBinaryCmp: right.name = " << pRightExpr->className() << endl;
#endif
}


template <class T>
LELBinaryCmp<T>::~LELBinaryCmp()
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryCmp: destructor" << endl;
#endif
}


template <class T>
void LELBinaryCmp<T>::eval(Array<Bool>& result,
			   const PixelRegion& region) const
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryCmp: eval " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::EQ :
       if (pLeftExpr_p->isScalar()) {
	  Array<T> temp(result.shape());
	  pRightExpr_p->eval (temp, region);
	  Array<Bool> res(pLeftExpr_p->getScalar() == temp);
	  result.reference (res);
       } else if (pRightExpr_p->isScalar()) {
	  Array<T> temp(result.shape());
	  pLeftExpr_p->eval (temp, region);
	  Array<Bool> res(temp == pRightExpr_p->getScalar());
	  result.reference (res);
       } else {
          Array<T> templ(result.shape());
	  Array<T> tempr(result.shape());
	  pLeftExpr_p->eval (templ, region);
	  pRightExpr_p->eval(tempr, region);
	  Array<Bool> res(templ == tempr);
	  result.reference (res);
       }
       break;
   case LELBinaryEnums::GT :
       if (pLeftExpr_p->isScalar()) {
	  Array<T> temp(result.shape());
	  pRightExpr_p->eval (temp, region);
	  Array<Bool> res(pLeftExpr_p->getScalar() > temp);
	  result.reference (res);
       } else if (pRightExpr_p->isScalar()) {
	  Array<T> temp(result.shape());
	  pLeftExpr_p->eval (temp, region);
	  Array<Bool> res(temp > pRightExpr_p->getScalar());
	  result.reference (res);
       } else {
          Array<T> templ(result.shape());
	  Array<T> tempr(result.shape());
	  pLeftExpr_p->eval (templ, region);
	  pRightExpr_p->eval(tempr, region);
	  Array<Bool> res(templ > tempr);
	  result.reference (res);
       }
       break;
   case LELBinaryEnums::GE :
       if (pLeftExpr_p->isScalar()) {
	  Array<T> temp(result.shape());
	  pRightExpr_p->eval (temp, region);
	  Array<Bool> res(pLeftExpr_p->getScalar() >= temp);
	  result.reference (res);
       } else if (pRightExpr_p->isScalar()) {
	  Array<T> temp(result.shape());
	  pLeftExpr_p->eval (temp, region);
	  Array<Bool> res(temp >= pRightExpr_p->getScalar());
	  result.reference (res);
       } else {
          Array<T> templ(result.shape());
	  Array<T> tempr(result.shape());
	  pLeftExpr_p->eval (templ, region);
	  pRightExpr_p->eval(tempr, region);
	  Array<Bool> res(templ >= tempr);
	  result.reference (res);
       }
       break;
   case LELBinaryEnums::NE :
       if (pLeftExpr_p->isScalar()) {
	  Array<T> temp(result.shape());
	  pRightExpr_p->eval (temp, region);
	  Array<Bool> res(pLeftExpr_p->getScalar() != temp);
	  result.reference (res);
       } else if (pRightExpr_p->isScalar()) {
	  Array<T> temp(result.shape());
	  pLeftExpr_p->eval (temp, region);
	  Array<Bool> res(temp != pRightExpr_p->getScalar());
	  result.reference (res);
       } else {
          Array<T> templ(result.shape());
	  Array<T> tempr(result.shape());
	  pLeftExpr_p->eval (templ, region);
	  pRightExpr_p->eval(tempr, region);
	  Array<Bool> res(templ != tempr);
	  result.reference (res);
       }
       break;
   default:
       throw(AipsError("LELBinaryCmp::eval - unknown operation"));
   }

}


template <class T>
Bool LELBinaryCmp<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryCmp: getScalar " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::EQ :
      return ToBool (pLeftExpr_p->getScalar() == pRightExpr_p->getScalar());
   case LELBinaryEnums::GT :
      return ToBool (pLeftExpr_p->getScalar() > pRightExpr_p->getScalar());
   case LELBinaryEnums::GE :
      return ToBool (pLeftExpr_p->getScalar() >= pRightExpr_p->getScalar());
   case LELBinaryEnums::NE :
      return ToBool (pLeftExpr_p->getScalar() != pRightExpr_p->getScalar());
   default:
       throw(AipsError("LELBinaryCmp::eval - unknown operation"));
   }
   return False;
}


template <class T>
void LELBinaryCmp<T>::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryCmp::prepare" << endl;
#endif

    LELInterface<T>::replaceScalarExpr (pLeftExpr_p);
    LELInterface<T>::replaceScalarExpr (pRightExpr_p);
}


template <class T>
String LELBinaryCmp<T>::className() const
{
   return String("LELBinaryCmp");
}
