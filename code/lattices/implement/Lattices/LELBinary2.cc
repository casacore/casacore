//# LELBinary.cc:  this defines non-templated classes in LELBinary.h
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

#include <trial/Lattices/LELBinary.h>
#include <trial/Lattices/LELScalar.h>
#include <trial/Lattices/LELArray.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h> 


typedef LELScalar<Bool> gppbug_lelbinary2;


LELBinaryBool::LELBinaryBool(const LELBinaryEnums::Operation op,
			     const CountedPtr<LELInterface<Bool> >& pLeftExpr,
			     const CountedPtr<LELInterface<Bool> >& pRightExpr)
: op_p(op), pLeftExpr_p(pLeftExpr), pRightExpr_p(pRightExpr)
{
   setAttr (LELAttribute(pLeftExpr_p->getAttribute(),
			 pRightExpr_p->getAttribute()));
   if (op == LELBinaryEnums::EQ  ||  op == LELBinaryEnums::NE) {
       if (pLeftExpr_p->isScalar() != pRightExpr_p->isScalar()) {
	   throw (AipsError ("LELBinaryBool::constructor - comparison between Bool scalar and "
			     " array not possible; use function ANY or ALL"));
       }
   }
   setAttr (LELAttribute(pLeftExpr_p->getAttribute(),
			 pRightExpr_p->getAttribute()));

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


void LELBinaryBool::eval(LELArray<Bool>& result,
			 const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool: eval " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::EQ :
       {
          LELArray<Bool> templ(result.shape());
	  LELArray<Bool> tempr(result.shape());
	  pLeftExpr_p->eval (templ, section);
	  pRightExpr_p->eval(tempr, section);
	  Array<Bool> res(templ.value() == tempr.value());
	  result.value().reference (res);
	  result.setMask (templ, tempr);
       }
       break;
   case LELBinaryEnums::NE :
       {
          LELArray<Bool> templ(result.shape());
	  LELArray<Bool> tempr(result.shape());
	  pLeftExpr_p->eval (templ, section);
	  pRightExpr_p->eval(tempr, section);
	  Array<Bool> res(templ.value() != tempr.value());
	  result.value().reference (res);
	  result.setMask (templ, tempr);
       }
       break;
   case LELBinaryEnums::OR :
       if (pLeftExpr_p->isScalar()) {
	  LELScalar<Bool> temp = pLeftExpr_p->getScalar();
	  // Note that having a False mask is in fact an Unknown value.
	  // If True scalar value, result is all True.
	  if (temp.value()  &&  temp.mask()) {
	     result.value() = True;
	     result.removeMask();
          } else {
	     pRightExpr_p->eval(result, section);
	     // If False scalar value, result array is same as original.
	     // If Unknown scalar, result is Unknown where not True.
	     if (! temp.mask()) {
		result.combineOrAnd (True, result.value());
	     }
	  }
       } else if (pRightExpr_p->isScalar()) {
	  LELScalar<Bool> temp = pRightExpr_p->getScalar();
	  if (temp.value()  &&  temp.mask()) {
	     result.value() = True;
	     result.removeMask();
          } else {
	     pLeftExpr_p->eval(result, section);
	     if (! temp.mask()) {
		result.combineOrAnd (True, result.value());
	     }
	  }
       } else {
          LELArray<Bool> temp(result.shape());
	  pLeftExpr_p->eval(result, section);
	  pRightExpr_p->eval(temp, section);
	  if (temp.isMasked()) {
	     result.combineOrAnd (True, result.value(), temp.value(),
				  temp.mask());
	  } else {
	     result.combineOrAnd (True, result.value(), temp.value());
	  }
       }
       break;
   case LELBinaryEnums::AND :
       if (pLeftExpr_p->isScalar()) {
	  LELScalar<Bool> temp = pLeftExpr_p->getScalar();
	  // Note that having a False mask is in fact an Unknown value.
	  // If False scalar value, result is all False.
	  if (!temp.value()  &&  temp.mask()) {
	     result.value() = False;
	     result.removeMask();
          } else {
	     pRightExpr_p->eval(result, section);
	     // If True scalar value, result array is same as original.
	     // If Unknown scalar, result is Unknown where not False.
	     if (! temp.mask()) {
		result.combineOrAnd (False, result.value());
	     }
	  }
       } else if (pRightExpr_p->isScalar()) {
	  LELScalar<Bool> temp = pRightExpr_p->getScalar();
	  if (!temp.value()  &&  temp.mask()) {
	     result.value() = False;
	     result.removeMask();
          } else {
	     pLeftExpr_p->eval(result, section);
	     if (! temp.mask()) {
		result.combineOrAnd (False, result.value());
	     }
	  }
       } else {
          LELArray<Bool> temp(result.shape());
	  pLeftExpr_p->eval(result, section);
	  pRightExpr_p->eval(temp, section);
	  if (temp.isMasked()) {
	     result.combineOrAnd (False, result.value(), temp.value(),
				  temp.mask());
	  } else {
	     result.combineOrAnd (False, result.value(), temp.value());
	  }
       }
       break;
   default:
       throw(AipsError("LELBinaryBool::eval - unknown operation"));
   }

}


LELScalar<Bool> LELBinaryBool::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool: getScalar " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::EQ :
      return ToBool (pLeftExpr_p->getScalar().value() ==
		     pRightExpr_p->getScalar().value());
   case LELBinaryEnums::NE :
      return ToBool (pLeftExpr_p->getScalar().value() !=
		     pRightExpr_p->getScalar().value());
   case LELBinaryEnums::OR :
   {
      LELScalar<Bool> templ = pLeftExpr_p->getScalar();
      if (templ.value()  &&  templ.mask()) {
	 return True;
      }
      LELScalar<Bool> tempr = pRightExpr_p->getScalar();
      if (tempr.value()  &&  tempr.mask()) {
	 return True;
      }
      return LELScalar<Bool> (ToBool (templ.value() || tempr.value()),
			      ToBool (templ.mask() && tempr.mask()));
   }
   case LELBinaryEnums::AND :
   {
      LELScalar<Bool> templ = pLeftExpr_p->getScalar();
      if (!templ.value()  &&  templ.mask()) {
	 return False;
      }
      LELScalar<Bool> tempr = pRightExpr_p->getScalar();
      if (!tempr.value()  &&  tempr.mask()) {
	 return False;
      }
      return LELScalar<Bool> (ToBool (templ.value() && tempr.value()),
			      ToBool (templ.mask() && tempr.mask()));
   }
   default:
       throw(AipsError("LELBinaryBool::eval - unknown operation"));
   }
   return False;
}


Bool LELBinaryBool::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool::prepare" << endl;
#endif

// In case of OR and AND, the result is invalid if both operands are invalid.
// In case of EQ and NE, the result is invalid if one operand is invalid.
   uInt nrinv = 0;
   if (LELInterface<Bool>::replaceScalarExpr (pLeftExpr_p)) {
      nrinv++;
      if (op_p != LELBinaryEnums::OR  &&  op_p != LELBinaryEnums::AND) {
	 return True;
      }
   }
   if (LELInterface<Bool>::replaceScalarExpr (pRightExpr_p)) {
      nrinv++;
      if (op_p != LELBinaryEnums::OR  &&  op_p != LELBinaryEnums::AND) {
	 return True;
      }
   }
   return ToBool (nrinv==2);
}


String LELBinaryBool::className() const
{
   return String("LELBinaryBool");
}
