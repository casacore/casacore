//# LELBinary.cc:  this defines templated classes in LELBinary.h
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

#ifndef LATTICES_LELBINARY_TCC
#define LATTICES_LELBINARY_TCC

#include <casacore/lattices/LEL/LELBinary.h>
#include <casacore/lattices/LEL/LELScalar.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h> 


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
LELBinary<T>::LELBinary(const LELBinaryEnums::Operation op,
			const CountedPtr<LELInterface<T> >& pLeftExpr,
			const CountedPtr<LELInterface<T> >& pRightExpr)
: op_p(op)
{
   setAttr (LELAttribute(pLeftExpr->getAttribute(),
			 pRightExpr->getAttribute()));
   // Fill these variables here, so an exception in setAttr does
   // not leave them undestructed.
   pLeftExpr_p  = pLeftExpr;
   pRightExpr_p = pRightExpr;

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
void LELBinary<T>::eval(LELArray<T>& result,
			const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELBinary: eval " << endl;
#endif

// Evaluate the expression.      
// We are sure that the operands do not have an all false mask,
// so in the scalar case the possible mask is not changed.
// If both operands are arrays, the masks are combined.

   switch(op_p) {
   case LELBinaryEnums::ADD :
       if (pLeftExpr_p->isScalar()) {
	  pRightExpr_p->eval(result, section);
	  result.value() += pLeftExpr_p->getScalar().value();
       } else if (pRightExpr_p->isScalar()) {
	  pLeftExpr_p->eval(result, section);
	  result.value() += pRightExpr_p->getScalar().value();
       } else {
	  pLeftExpr_p->eval(result, section);
	  LELArrayRef<T> temp(result.shape());
	  pRightExpr_p->evalRef(temp, section);
	  result.value() += temp.value();
	  result.combineMask (temp);
       }
       break;

   case LELBinaryEnums::SUBTRACT:
       if (pLeftExpr_p->isScalar()) {
          pRightExpr_p->eval(result, section);
	  result.value() = pLeftExpr_p->getScalar().value() - result.value();
       } else if (pRightExpr_p->isScalar()) {
          pLeftExpr_p->eval(result, section);
	  result.value() -= pRightExpr_p->getScalar().value();
       } else {
	  pLeftExpr_p->eval(result, section);
          LELArrayRef<T> temp(result.shape());
	  pRightExpr_p->evalRef(temp, section);
	  result.value() -= temp.value();
	  result.combineMask (temp);
       }
       break;
   case LELBinaryEnums::MULTIPLY:
       if (pLeftExpr_p->isScalar()) {
	  pRightExpr_p->eval(result, section);
	  result.value() *= pLeftExpr_p->getScalar().value();
       } else if (pRightExpr_p->isScalar()) {
	  pLeftExpr_p->eval(result, section);
	  result.value() *= pRightExpr_p->getScalar().value();
       } else {
	  pLeftExpr_p->eval(result, section);
	  LELArrayRef<T> temp(result.shape());
	  pRightExpr_p->evalRef(temp, section);
	  result.value() *= temp.value();
	  result.combineMask (temp);
       }
       break;
   case LELBinaryEnums::DIVIDE:
       if (pLeftExpr_p->isScalar()) {
          pRightExpr_p->eval(result, section);
	  result.value() = pLeftExpr_p->getScalar().value() / result.value();
       } else if (pRightExpr_p->isScalar()) {
          pLeftExpr_p->eval(result, section);
	  result.value() /= pRightExpr_p->getScalar().value();
       } else {
	  pLeftExpr_p->eval(result, section);
          LELArrayRef<T> temp(result.shape());
	  pRightExpr_p->evalRef(temp, section);
	  result.value() /= temp.value();
	  result.combineMask (temp);
       }
       break;
   default:
       throw(AipsError("LELBinary::eval - unknown operation"));
   }

}


template <class T>
LELScalar<T> LELBinary<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELBinary: getScalar " << endl;
#endif

   LELScalar<T> temp = pLeftExpr_p->getScalar();
   switch(op_p) {
   case LELBinaryEnums::ADD :
      temp.value() += pRightExpr_p->getScalar().value();
      break;
   case LELBinaryEnums::SUBTRACT :
      temp.value() -= pRightExpr_p->getScalar().value();
      break;
   case LELBinaryEnums::MULTIPLY :
      temp.value() *= pRightExpr_p->getScalar().value();
      break;
   case LELBinaryEnums::DIVIDE :
      temp.value() /= pRightExpr_p->getScalar().value();
      break;
   default:
       throw(AipsError("LELBinary::getScalar - unknown operation"));
   }
   return temp;
}


template <class T>
Bool LELBinary<T>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELBinary::prepare" << endl;
#endif

   if (LELInterface<T>::replaceScalarExpr (pLeftExpr_p)) {
      return True;
   }
   if (LELInterface<T>::replaceScalarExpr (pRightExpr_p)) {
      return True;
   }
   return False;
}


template <class T>
String LELBinary<T>::className() const
{
   return String("LELBinary");
}


template<class T>
Bool LELBinary<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  if (! pLeftExpr_p->lock (type, nattempts)) {
    return False;
  }
  return pRightExpr_p->lock (type, nattempts);
}
template<class T>
void LELBinary<T>::unlock()
{
    pLeftExpr_p->unlock();
    pRightExpr_p->unlock();
}
template<class T>
Bool LELBinary<T>::hasLock (FileLocker::LockType type) const
{
    return pLeftExpr_p->hasLock (type)  &&   pRightExpr_p->hasLock (type);
}
template<class T>
void LELBinary<T>::resync()
{
    pLeftExpr_p->resync();
    pRightExpr_p->resync();
}



// LELBinaryCmp
template <class T>
LELBinaryCmp<T>::LELBinaryCmp(const LELBinaryEnums::Operation op,
			      const CountedPtr<LELInterface<T> >& pLeftExpr,
			      const CountedPtr<LELInterface<T> >& pRightExpr)
: op_p(op)
{
   setAttr (LELAttribute(pLeftExpr->getAttribute(),
			 pRightExpr->getAttribute()));
   // Fill these variables here, so an exception in setAttr does
   // not leave them undestructed.
   pLeftExpr_p  = pLeftExpr;
   pRightExpr_p = pRightExpr;

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
void LELBinaryCmp<T>::eval(LELArray<Bool>& result,
			   const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryCmp: eval " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::EQ :
       if (pLeftExpr_p->isScalar()) {
	  LELArrayRef<T> temp(result.shape());
	  pRightExpr_p->evalRef (temp, section);
	  Array<Bool> res(pLeftExpr_p->getScalar().value() == temp.value());
	  result.value().reference (res);
	  result.setMask (temp);
       } else if (pRightExpr_p->isScalar()) {
	  LELArrayRef<T> temp(result.shape());
	  pLeftExpr_p->evalRef (temp, section);
	  Array<Bool> res(temp.value() == pRightExpr_p->getScalar().value());
	  result.value().reference (res);
	  result.setMask (temp);
       } else {
          LELArrayRef<T> templ(result.shape());
	  LELArrayRef<T> tempr(result.shape());
	  pLeftExpr_p->evalRef (templ, section);
	  pRightExpr_p->evalRef(tempr, section);
	  Array<Bool> res(templ.value() == tempr.value());
	  result.value().reference (res);
	  result.setMask (templ, tempr);
       }
       break;
   case LELBinaryEnums::GT :
       if (pLeftExpr_p->isScalar()) {
	  LELArrayRef<T> temp(result.shape());
	  pRightExpr_p->evalRef (temp, section);
	  Array<Bool> res(pLeftExpr_p->getScalar().value() > temp.value());
	  result.value().reference (res);
	  result.setMask (temp);
       } else if (pRightExpr_p->isScalar()) {
	  LELArrayRef<T> temp(result.shape());
	  pLeftExpr_p->evalRef (temp, section);
	  Array<Bool> res(temp.value() > pRightExpr_p->getScalar().value());
	  result.value().reference (res);
	  result.setMask (temp);
       } else {
          LELArrayRef<T> templ(result.shape());
	  LELArrayRef<T> tempr(result.shape());
	  pLeftExpr_p->evalRef (templ, section);
	  pRightExpr_p->evalRef(tempr, section);
	  Array<Bool> res(templ.value() > tempr.value());
	  result.value().reference (res);
	  result.setMask (templ, tempr);
       }
       break;
   case LELBinaryEnums::GE :
       if (pLeftExpr_p->isScalar()) {
	  LELArrayRef<T> temp(result.shape());
	  pRightExpr_p->evalRef (temp, section);
	  Array<Bool> res(pLeftExpr_p->getScalar().value() >= temp.value());
	  result.value().reference (res);
	  result.setMask (temp);
       } else if (pRightExpr_p->isScalar()) {
	  LELArrayRef<T> temp(result.shape());
	  pLeftExpr_p->evalRef (temp, section);
	  Array<Bool> res(temp.value() >= pRightExpr_p->getScalar().value());
	  result.value().reference (res);
	  result.setMask (temp);
       } else {
          LELArrayRef<T> templ(result.shape());
	  LELArrayRef<T> tempr(result.shape());
	  pLeftExpr_p->evalRef (templ, section);
	  pRightExpr_p->evalRef(tempr, section);
	  Array<Bool> res(templ.value() >= tempr.value());
	  result.value().reference (res);
	  result.setMask (templ, tempr);
       }
       break;
   case LELBinaryEnums::NE :
       if (pLeftExpr_p->isScalar()) {
	  LELArrayRef<T> temp(result.shape());
	  pRightExpr_p->evalRef (temp, section);
	  Array<Bool> res(pLeftExpr_p->getScalar().value() != temp.value());
	  result.value().reference (res);
	  result.setMask (temp);
       } else if (pRightExpr_p->isScalar()) {
	  LELArrayRef<T> temp(result.shape());
	  pLeftExpr_p->evalRef (temp, section);
	  Array<Bool> res(temp.value() != pRightExpr_p->getScalar().value());
	  result.value().reference (res);
	  result.setMask (temp);
       } else {
          LELArrayRef<T> templ(result.shape());
	  LELArrayRef<T> tempr(result.shape());
	  pLeftExpr_p->evalRef (templ, section);
	  pRightExpr_p->evalRef(tempr, section);
	  Array<Bool> res(templ.value() != tempr.value());
	  result.value().reference (res);
	  result.setMask (templ, tempr);
       }
       break;
   default:
       throw(AipsError("LELBinaryCmp::eval - unknown operation"));
   }

}


template <class T>
LELScalar<Bool> LELBinaryCmp<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryCmp: getScalar " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::EQ :
      return  (pLeftExpr_p->getScalar().value() ==
		     pRightExpr_p->getScalar().value());
   case LELBinaryEnums::GT :
      return  (pLeftExpr_p->getScalar().value() >
		     pRightExpr_p->getScalar().value());
   case LELBinaryEnums::GE :
      return  (pLeftExpr_p->getScalar().value() >=
		     pRightExpr_p->getScalar().value());
   case LELBinaryEnums::NE :
      return  (pLeftExpr_p->getScalar().value() !=
		     pRightExpr_p->getScalar().value());
   default:
       throw(AipsError("LELBinaryCmp::eval - unknown operation"));
   }
   return False;
}


template <class T>
Bool LELBinaryCmp<T>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryCmp::prepare" << endl;
#endif

   if (LELInterface<T>::replaceScalarExpr (pLeftExpr_p)) {
      return True;
   }
   if (LELInterface<T>::replaceScalarExpr (pRightExpr_p)) {
      return True;
   }
   return False;
}


template <class T>
String LELBinaryCmp<T>::className() const
{
   return String("LELBinaryCmp");
}


template<class T>
Bool LELBinaryCmp<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  if (! pLeftExpr_p->lock (type, nattempts)) {
    return False;
  }
  return pRightExpr_p->lock (type, nattempts);
}
template<class T>
void LELBinaryCmp<T>::unlock()
{
    pLeftExpr_p->unlock();
    pRightExpr_p->unlock();
}
template<class T>
Bool LELBinaryCmp<T>::hasLock (FileLocker::LockType type) const
{
    return pLeftExpr_p->hasLock (type)  &&   pRightExpr_p->hasLock (type);
}
template<class T>
void LELBinaryCmp<T>::resync()
{
    pLeftExpr_p->resync();
    pRightExpr_p->resync();
}

} //# NAMESPACE CASACORE - END


#endif
