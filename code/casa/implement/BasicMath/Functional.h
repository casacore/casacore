//# Functional.h: Objects which map a domain object into a range object via operator().
//# Copyright (C) 1995,1996,1999
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

#if !defined(AIPS_FUNCTIONAL_H)
#define AIPS_FUNCTIONAL_H

#include <aips/aips.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>

//# Forward declaration
template<class T> class Lattice;

// <summary>
// Objects which map a domain object into a range object via operator().
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <etymology> The term ``Functional'' was chosen to follow the usage
// in Barton and Nackman's ``Scientific and Engineering C++.''
// </etymology>
//
// <synopsis>
// A Functional<Domain,Range> is an abstract base class which encapsulates the
// mapping of an object of type ``Domain'' into one an object of type
// ``Range''. This operation is invoked via operator() to make it look more like
// a function call.
//
// While these functions are ``function-like'', there is no guarantee that
// evaluations of the same parameter will yield the same result (the implementor
// of a particular class could, for example, merely choose to emit a random
// number). However implementors of Functional classes are strongly encouraged 
// to implement (visible) side-effect free semantics in their classes.
// 
// A Functional object is used in circumstances similar to those in which an
// ordinary C-function pointer would be used. An advantage of the Functional
// objects is that it is possible to have more than one of them at the same
// Time. With a C-pointer one normally has to manipulate global variables to
// simulate multiple objects. Another potential advantage (not yet
// implemented) is that it will be possible to perform functional
// composition at run time, e.g. a=b+c where a,b, and c are
// Functionals. Another advantage is that since the Functional
// implementations are likely going to be templated, it is likely that the
// same source code would yield instantiations for all the numeric types.
//
// To be of greatest utility, a library of functions that do mathematics, 
// plotting, etc. on Functional objects needs to be developed.
// </synopsis>
//
// <example>
// The following simple example shows how you can write a function that uses a
// Functional object. 
// <srcBlock>
// Double integrate1D(const Functional<Float,Float> &f,
//                    Double x1, Double x2, Double dx) {
//     uInt n = (xend - xstart) / dx;
//     Double sum = 0.0;
//     for (uInt i=0; i < n; i++) {
//       sum += f(x1 + i*dx) * dx;
//     }
//     return sum; 
// }
// </srcBlock>
// Obviously this isn't a very serious algorithm!
// </example>
// 
// <motivation>
// The specific application that caused the implementation of these Functional
// classes was the creation of the <linkto module="Fitting">Fitting
// module</linkto>, which needed classes to represent the fitting functions.
// </motivation>
//
// <templating arg=Domain>
//    <li> Accessible default and copy constructors, assignment operators, 
//         and destructors will almost always also be required.
// </templating>
//
// <templating arg=Range>
//    <li> A copy constructor is absolutely required for Range objects because
//         operator() returns Range objects by value.
//    <li> Accessible default constructors, assignment operators, and destructors
//         will almost always also be required.
// </templating>
//
// <todo asof="1996/02/22">
//   <li> My guess is that Functional classes will most often be accessed
//   polymorphically through more derived classes, such as Function1D and
//   FunctionND. If this is not the case, then a clone() member function will be
//   needed here (until covariant returns are available in standard compilers).
//   <li> A Functional class which merely wraps up a pointer-to-function will
//   doubtless be convenient eventually.
// </todo>

template<class Domain, class Range> class Functional
{
public:
    virtual ~Functional();

    // Map a Domain ``x'' into a Range ``y'' value. Must be overridden in every
    // derived class.
    virtual Range operator()(const Domain &x) const = 0;
};

#endif
