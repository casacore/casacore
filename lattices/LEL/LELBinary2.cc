//# LELBinary.cc:  this defines non-templated classes in LELBinary.h
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

#include <casacore/lattices/LEL/LELBinary.h>
#include <casacore/lattices/LEL/LELScalar.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h> 


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LELBinaryBool::LELBinaryBool(const LELBinaryEnums::Operation op,
			     const CountedPtr<LELInterface<bool> >& pLeftExpr,
			     const CountedPtr<LELInterface<bool> >& pRightExpr)
: op_p(op)
{
   if (op == LELBinaryEnums::EQ  ||  op == LELBinaryEnums::NE) {
       if (pLeftExpr->isScalar() != pRightExpr->isScalar()) {
	   throw (AipsError ("LELBinaryBool::constructor - "
			     "comparison between bool scalar and "
			     "array not possible; use function ANY or ALL"));
       }
   }
   setAttr (LELAttribute(pLeftExpr->getAttribute(),
			 pRightExpr->getAttribute()));
   // Fill these variables here, so an exception in setAttr does
   // not leave them undestructed.
   pLeftExpr_p  = pLeftExpr;
   pRightExpr_p = pRightExpr;
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool: constructor" << endl;
   cout << "LELBinaryBool: left.name = " << pLeftExpr->className() << endl;
   cout << "LELBinaryBool: right.name = " << pRightExpr->className() << endl;
#endif
}


LELBinaryBool::~LELBinaryBool()
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool: destructor" << endl;
#endif
}


void LELBinaryBool::eval(LELArray<bool>& result,
			 const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool: eval " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::EQ :
       {
          LELArrayRef<bool> templ(result.shape());
	  LELArrayRef<bool> tempr(result.shape());
	  pLeftExpr_p->evalRef (templ, section);
	  pRightExpr_p->evalRef(tempr, section);
	  Array<bool> res(templ.value() == tempr.value());
	  result.value().reference (res);
	  result.setMask (templ, tempr);
       }
       break;
   case LELBinaryEnums::NE :
       {
          LELArrayRef<bool> templ(result.shape());
	  LELArrayRef<bool> tempr(result.shape());
	  pLeftExpr_p->evalRef (templ, section);
	  pRightExpr_p->evalRef(tempr, section);
	  Array<bool> res(templ.value() != tempr.value());
	  result.value().reference (res);
	  result.setMask (templ, tempr);
       }
       break;
   case LELBinaryEnums::OR :
       if (pLeftExpr_p->isScalar()) {
	  LELScalar<bool> temp = pLeftExpr_p->getScalar();
	  // Note that having a false mask is in fact an Unknown value.
	  // If true scalar value, result is all true.
	  if (temp.value()  &&  temp.mask()) {
	     result.value() = true;
	     result.removeMask();
          } else {
	     pRightExpr_p->eval(result, section);
	     // If false scalar value, result array is same as original.
	     // If Unknown scalar, result is Unknown where not true.
	     if (! temp.mask()) {
		result.combineOrAnd (true, result.value());
	     }
	  }
       } else if (pRightExpr_p->isScalar()) {
	  LELScalar<bool> temp = pRightExpr_p->getScalar();
	  if (temp.value()  &&  temp.mask()) {
	     result.value() = true;
	     result.removeMask();
          } else {
	     pLeftExpr_p->eval(result, section);
	     if (! temp.mask()) {
		result.combineOrAnd (true, result.value());
	     }
	  }
       } else {
          LELArrayRef<bool> temp(result.shape());
	  pLeftExpr_p->eval(result, section);
	  pRightExpr_p->evalRef(temp, section);
	  if (temp.isMasked()) {
	     result.combineOrAnd (true, result.value(), temp.value(),
				  temp.mask());
	  } else {
	     result.combineOrAnd (true, result.value(), temp.value());
	  }
       }
       break;
   case LELBinaryEnums::AND :
       if (pLeftExpr_p->isScalar()) {
	  LELScalar<bool> temp = pLeftExpr_p->getScalar();
	  // Note that having a false mask is in fact an Unknown value.
	  // If false scalar value, result is all false.
	  if (!temp.value()  &&  temp.mask()) {
	     result.value() = false;
	     result.removeMask();
          } else {
	     pRightExpr_p->eval(result, section);
	     // If true scalar value, result array is same as original.
	     // If Unknown scalar, result is Unknown where not false.
	     if (! temp.mask()) {
		result.combineOrAnd (false, result.value());
	     }
	  }
       } else if (pRightExpr_p->isScalar()) {
	  LELScalar<bool> temp = pRightExpr_p->getScalar();
	  if (!temp.value()  &&  temp.mask()) {
	     result.value() = false;
	     result.removeMask();
          } else {
	     pLeftExpr_p->eval(result, section);
	     if (! temp.mask()) {
		result.combineOrAnd (false, result.value());
	     }
	  }
       } else {
          LELArrayRef<bool> temp(result.shape());
	  pLeftExpr_p->eval(result, section);
	  pRightExpr_p->evalRef(temp, section);
	  if (temp.isMasked()) {
	     result.combineOrAnd (false, result.value(), temp.value(),
				  temp.mask());
	  } else {
	     result.combineOrAnd (false, result.value(), temp.value());
	  }
       }
       break;
   default:
       throw(AipsError("LELBinaryBool::eval - unknown operation"));
   }

}


LELScalar<bool> LELBinaryBool::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool: getScalar " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::EQ :
      return  (pLeftExpr_p->getScalar().value() ==
		     pRightExpr_p->getScalar().value());
   case LELBinaryEnums::NE :
      return  (pLeftExpr_p->getScalar().value() !=
		     pRightExpr_p->getScalar().value());
   case LELBinaryEnums::OR :
   {
      LELScalar<bool> templ = pLeftExpr_p->getScalar();
      if (templ.value()  &&  templ.mask()) {
	 return true;
      }
      LELScalar<bool> tempr = pRightExpr_p->getScalar();
      if (tempr.value()  &&  tempr.mask()) {
	 return true;
      }
      return LELScalar<bool> ( (templ.value() || tempr.value()),
			       (templ.mask() && tempr.mask()));
   }
   case LELBinaryEnums::AND :
   {
      LELScalar<bool> templ = pLeftExpr_p->getScalar();
      if (!templ.value()  &&  templ.mask()) {
	 return false;
      }
      LELScalar<bool> tempr = pRightExpr_p->getScalar();
      if (!tempr.value()  &&  tempr.mask()) {
	 return false;
      }
      return LELScalar<bool> ( (templ.value() && tempr.value()),
			       (templ.mask() && tempr.mask()));
   }
   default:
       throw(AipsError("LELBinaryBool::eval - unknown operation"));
   }
   return false;
}


bool LELBinaryBool::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool::prepare" << endl;
#endif

// In case of OR and AND, the result is invalid if both operands are invalid.
// In case of EQ and NE, the result is invalid if one operand is invalid.
   uint32_t nrinv = 0;
   if (LELInterface<bool>::replaceScalarExpr (pLeftExpr_p)) {
      nrinv++;
      if (op_p != LELBinaryEnums::OR  &&  op_p != LELBinaryEnums::AND) {
	 return true;
      }
   }
   if (LELInterface<bool>::replaceScalarExpr (pRightExpr_p)) {
      nrinv++;
      if (op_p != LELBinaryEnums::OR  &&  op_p != LELBinaryEnums::AND) {
	 return true;
      }
   }
   return  (nrinv==2);
}


String LELBinaryBool::className() const
{
   return String("LELBinaryBool");
}


bool LELBinaryBool::lock (FileLocker::LockType type, uint32_t nattempts)
{
  if (! pLeftExpr_p->lock (type, nattempts)) {
    return false;
  }
  return pRightExpr_p->lock (type, nattempts);
}
void LELBinaryBool::unlock()
{
    pLeftExpr_p->unlock();
    pRightExpr_p->unlock();
}
bool LELBinaryBool::hasLock (FileLocker::LockType type) const
{
    return pLeftExpr_p->hasLock (type)  &&   pRightExpr_p->hasLock (type);
}
void LELBinaryBool::resync()
{
    pLeftExpr_p->resync();
    pRightExpr_p->resync();
}

} //# NAMESPACE CASACORE - END

