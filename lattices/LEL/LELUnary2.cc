//# LELUnary2.cc:  this defines non-templated classes in LELUnary.h
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

#include <casacore/lattices/LEL/LELUnary.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h> 


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LELUnaryBool::LELUnaryBool(const LELUnaryEnums::Operation op,
			   const CountedPtr<LELInterface<Bool> >& pExpr)
: op_p(op), pExpr_p(pExpr)
{
   setAttr(pExpr_p->getAttribute());

#if defined(AIPS_TRACE)
   cout << "LELUnaryBool:: constructor" << endl;
#endif
}

LELUnaryBool::~LELUnaryBool()
{
#if defined(AIPS_TRACE)
   cout << "LELUnaryBool:: destructor " << endl;
#endif
}


void LELUnaryBool::eval(LELArray<Bool>& result,
			const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELUnaryBool:: eval " << endl;
#endif

// Get the value and apply the unary operation
   pExpr_p->eval(result, section);
   switch(op_p) {
   case LELUnaryEnums::NOT :
   {
      Array<Bool> tmp(!result.value());
      result.value().reference(tmp);
      break;
   }
   default:
      throw(AipsError("LELUnaryBool::eval - unknown operation"));
   }
}

LELScalar<Bool> LELUnaryBool::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELUnaryBool::getScalar" << endl;
#endif

   LELScalar<Bool> temp (pExpr_p->getScalar());
   switch(op_p) {
   case LELUnaryEnums::NOT :
      temp.value() =  (!(temp.value()));
      break;
   default:
      throw(AipsError("LELUnaryBool::getScalar - unknown operation"));
   }
   return temp;
}

Bool LELUnaryBool::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELUnaryBool::prepare" << endl;
#endif

   return LELInterface<Bool>::replaceScalarExpr (pExpr_p);
}

String LELUnaryBool::className() const
{
   return String("LELUnaryBool");
}


Bool LELUnaryBool::lock (FileLocker::LockType type, uInt nattempts)
{
  return pExpr_p->lock (type, nattempts);
}
void LELUnaryBool::unlock()
{
    pExpr_p->unlock();
}
Bool LELUnaryBool::hasLock (FileLocker::LockType type) const
{
    return pExpr_p->hasLock (type);
}
void LELUnaryBool::resync()
{
    pExpr_p->resync();
}

} //# NAMESPACE CASACORE - END

