//# LELBinary.cc:  this defines non-templated classes in LELBinary.h
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
#include <aips/Lattices/Slicer.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h> 



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


void LELBinaryBool::eval(Array<Bool>& result,
			 const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool: eval " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::EQ :
       {
          Array<Bool> templ(result.shape());
	  Array<Bool> tempr(result.shape());
	  pLeftExpr_p->eval (templ, section);
	  pRightExpr_p->eval(tempr, section);
	  Array<Bool> res(templ == tempr);
	  result.reference (res);
       }
       break;
   case LELBinaryEnums::NE :
       {
          Array<Bool> templ(result.shape());
	  Array<Bool> tempr(result.shape());
	  pLeftExpr_p->eval (templ, section);
	  pRightExpr_p->eval(tempr, section);
	  Array<Bool> res(templ != tempr);
	  result.reference (res);
       }
       break;
   case LELBinaryEnums::OR :
       if (pLeftExpr_p->isScalar()) {
	  if (pLeftExpr_p->getScalar()) {
	     result = True;
          } else {
	     pRightExpr_p->eval(result, section);
	  }
       } else if (pRightExpr_p->isScalar()) {
	  if (pRightExpr_p->getScalar()) {
	     result = True;
          } else {
	     pLeftExpr_p->eval(result, section);
	  }
       } else {
          Array<Bool> temp(result.shape());
	  pLeftExpr_p->eval(result, section);
	  pRightExpr_p->eval(temp, section);
	  result = result || temp;
       }
       break;
   case LELBinaryEnums::AND :
       if (pLeftExpr_p->isScalar()) {
	  if (! pLeftExpr_p->getScalar()) {
	     result = False;
          } else {
	     pRightExpr_p->eval(result, section);
	  }
       } else if (pRightExpr_p->isScalar()) {
	  if (! pRightExpr_p->getScalar()) {
	     result = False;
          } else {
	     pLeftExpr_p->eval(result, section);
	  }
       } else {
          Array<Bool> temp(result.shape());
	  pLeftExpr_p->eval(result, section);
	  pRightExpr_p->eval(temp, section);
	  result = result && temp;
       }
       break;
   default:
       throw(AipsError("LELBinaryBool::eval - unknown operation"));
   }

}


Bool LELBinaryBool::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool: getScalar " << endl;
#endif

   switch(op_p) {
   case LELBinaryEnums::OR :
      if (pLeftExpr_p->getScalar()) return True;
      return pRightExpr_p->getScalar();
   case LELBinaryEnums::AND :
      if (! pLeftExpr_p->getScalar()) return False;
      return pRightExpr_p->getScalar();
   default:
       throw(AipsError("LELBinaryBool::eval - unknown operation"));
   }
   return False;
}


void LELBinaryBool::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELBinaryBool::prepare" << endl;
#endif

    LELInterface<Bool>::replaceScalarExpr (pLeftExpr_p);
    LELInterface<Bool>::replaceScalarExpr (pRightExpr_p);
}


String LELBinaryBool::className() const
{
   return String("LELBinaryBool");
}
