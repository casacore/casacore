//# LELConvert.cc:  LELConvert.cc
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


#include <trial/Lattices/LELConvert.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>


template <class T, class F>
LELConvert<T,F>::LELConvert(const CountedPtr<LELInterface<F> >& expr)
: pExpr_p (expr)
{
   setAttr (expr->getAttribute());

#if defined(AIPS_TRACE)
   cout << "LELConvert:: constructor" << endl;
#endif
}


template <class T, class F>
LELConvert<T,F>::~LELConvert()
{
#if defined(AIPS_TRACE)
   cout << "LELConvert:: destructor" << endl;
#endif
}


template <class T, class F>
void LELConvert<T,F>::eval (Array<T>& result,
                      const PixelRegion& region) const
{
#if defined(AIPS_TRACE)
   cout << "LELConvert::eval" << endl;
#endif

   Array<F> temp(result.shape());
   pExpr_p->eval (temp, region);
   convertArray (result, temp);
}


template <class T, class F>
T LELConvert<T,F>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELConvert::getScalar" << endl;
#endif

//# This is the only way the compiler does not complain about
//# ambiguity between Complex conversion operator and constructor.
   T tmp;
   tmp = pExpr_p->getScalar();
   return tmp;
}


template <class T, class F>
void LELConvert<T,F>::prepare()
{
#if defined(AIPS_TRACE)
   cout << "LELConvert::prepare" << endl;
#endif

   LELInterface<F>::replaceScalarExpr (pExpr_p);
}


template <class T, class F>
String LELConvert<T,F>::className() const
{
   return "LELConvert";
}
