//# Functionals.h: A module that represents various function-like classes.
//# Copyright (C) 1995,1996
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


#if !defined (AIPS_FUNCTIONALS_H)
#define AIPS_FUNCTIONALS_H

//# Base classes
#include <aips/Functionals/Functional.h>
#include <aips/Functionals/Parameterized.h>
#include <aips/Functionals/Function1D.h>
#include <aips/Functionals/FunctionND.h>

//# 1-D Functions
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Functionals/Polynomial.h>

//#1D or ND Functions
#include <aips/Functionals/SumFunction.h>

// <module>
//
// <summary>
// A module that represents various function-like classes.
// </summary>

// <reviewed reviewer="tcornwel" date="1996/02/13" demos="">
// </reviewed>

// <etymology>
// The term ``Functional'' was chosen to roughly follow the usage in Barton and
// Nackman's ``Scientific and Engineering C++.'' Functional classes map a
// Domain object into a Range object, rather like a mathematical
// ``function''. They use <src>operator()</src>, so they look much like single
// argument C++ ``functions''.
// </etymology>

//
// <synopsis>
// Functional classes map an input object of some ``Domain'' type into an
// output object of some ``Range'' type. The Domain and Range types will
// normally be numeric types, and will often be the same type. The mapping
// occurs via <src>operator()</src>.
//
// The fundamental Functional classes are as follows:

// <ul>
//    <li> <linkto class="Functional">Functional<Domain,Range></linkto>: This
//    base class represents functions that map a <src>Domain</src> object into
//    a <src>Range</src> object via <src>operator()</src>.
//    <li> <linkto class="FunctionND">FunctionND<Domain,Range></linkto>: This
//    base class is used for N-dimensional functions, that is it maps a <linkto
//    class="Vector">Vector<Domain></linkto> into a <src>Range</src> object via
//    operator(). FunctionND objects also have adjustable parameters.
//    <src>FunctionND<Domain,Range></src> inherits from
//    <src>Functional<Vector<Domain>, Range></src>.
//    <li> <linkto class="Function1D">Function1D<Domain,Range></linkto>: This
//    class is much like <src>FunctionND</src>, however it is strictly for
//    one dimensional functions.
//    <li> <linkto class="Parameterized">Parameterized</linkto>: This base class
//         provides the interface for adjustable parameters for
//         <src>Function1D</src> and <src>FunctionND</src>.
// </ul>
//
// Presently, the following Function1D classes are implemented:
// <ul>
//      <li> <linkto class="Polynomial">Polynomial<T></linkto>: A 1-Dimensional
//      polynomial. The polynomial coefficients are its adjustable parameters.
//      <li> <linkto class="Gaussian1D">Gaussian1D<T></linkto>: A 1-dimensional
//      Gaussian. The height, center, and width are the parameters of the
//      Gaussian.
//      <li> <linkto class="SumFunction1D">SumFunction1D<T></linkto>: A 
//      Function which is made up of the sum of other Functions.
// </ul>
// </synopsis>

// <example>
// A function to find a bracketed root by bisection could be written as follows:
// <srcBlock>
//    template<class Domain, class Range> 
//      Domain findRoot(const Functional<Domain,Range> &func, Domain left, 
//                      Domain right, Domain tol)
//      {
//          Range fr = func(right);
//          Range fl = func(left);
//          Range sign = fr > 0 ? 1 : -1 ;
//          AlwaysAssertExit(fl*fr < 0.0 && right > left);
//          while (right - left > tol) {
//              Domain mid = (left + right) / 2;
//              Range fmid = func(mid);
//              if (sign*fmid > 0.0)
//                  right = mid;
//              else
//                  left = mid;
//          }
//          return (left + right)/2;
//      }
// </srcBlock>
// Since Function1D is derived from Functional, the
// above function will also work with classes derived from Function1D. To
// behave sensibly, the Domain and Range types should be real, <e>i.e.</e>,
// Float or Double.  (This example should likely be made a real function in the
// system!)
// </example>

// <motivation>
// The immediate motivations for this module were:
// <ol>
//    <li> To represent functions which are used in linear and non-linear least
//         squares fitting (1-Dimensional).
//    <li> For functions which model the sky brightness, and the Fourier
//    transform of the sky brightness. (Not currently (early 1996) in use).
// </ol>
// </motivation>

// <todo asof="1995/08/30">
//   <li> REPLACE THE MATHFUNC CLASSES!!
//   <li> It would be very convenient to have a letter/envelope class, and to 
//        define ``function arithmetic.''
// </todo>

// </module>

#endif

