//# Functional.h: Map a domain object into a range object via operator().
//# Copyright (C) 1995,1996,1999-2001
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

#ifndef CASA_FUNCTIONAL_H
#define CASA_FUNCTIONAL_H

//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declaration
template<class T> class Lattice;

// <summary> Map a domain object into a range object via operator().
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <etymology> The term ``Functional'' was chosen to follow the usage
// in Barton and Nackman's ``Scientific and Engineering C++.''
// </etymology>
//
// <synopsis>
// A <src>Functional<Domain,Range></src> is an abstract base class which
// encapsulates the mapping of an object of type <src>Domain</src> into an
// object of type <src>Range</src>.
// This operation is invoked via operator() to make it look like
// a function call.
//
// While these functions are <src>function-like</src>, there is no guarantee
// that evaluations of the same parameter will yield the same result
// (the implementor of a particular class could, for example, merely choose
// to emit a random number).
// However implementors of <src>Functional</src> classes are strongly
// encouraged to implement (visible) side-effect free semantics in their
// classes.
// 
// A <src>Functional</src> object is used in circumstances similar to those
// in which a function pointer could be used. An advantage of the
// <src>Functional</src> objects is that it is possible to have more than
// one of them at the same time.
// Another potential advantage (not yet
// implemented) is that it will be possible to perform functional
// composition at run time, e.g. a=b+c where a,b, and c are
// <src>Functionals</src>.
// Another advantage is that since the Functional implementations
// will in general be templated, the same source code would yield
// instantiations for all the numeric types and for specializations like
// automatic derivatives.
//
// To be of greatest utility, a library of functions that do mathematics, 
// plotting, etc. on Functional objects needs to be developed.
// </synopsis>
//
// <example>
// The following simple example shows how you can write a function that uses a
// Functional object. 
// <srcblock>
// Double integrate1D(const Functional<Float,Float> &f,
//                    Double x1, Double x2, Double dx) {
//     uInt n = (xend - xstart) / dx;
//     Double sum = 0.0;
//     for (uInt i=0; i < n; i++) sum += f(x1 + i*dx) * dx;
//     return sum; 
// }
// </srcblock>
// Obviously this isn't a very serious algorithm!
// </example>
// 
// <motivation>
// The specific application that caused the implementation of these 
// <src>Functional</src>
// classes was the creation of the <linkto module="Fitting">Fitting
// </linkto> module, which needed classes to represent the fitting functions.
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
//    <li> Accessible default constructors, assignment operators,
//	   and destructors will almost always also be required.
// </templating>
//
// <todo asof="2001/08/29">
//   <li> For polymorphic access it could be that a <src>clone()</src> function
//		is needed at this level.
// </todo>

template<class Domain, class Range> class Functional {
 public:
  //# Constructors
  // Destructor
  virtual ~Functional();

  //# Operators
  // Map a Domain <src>x</src> into a Range <src>y</src> value.
  virtual Range operator()(const Domain &x) const = 0;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/BasicMath/Functional.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
