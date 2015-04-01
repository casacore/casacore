//# SampledFunctional.h:
//# Copyright (C) 1996,1999
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

#ifndef SCIMATH_SAMPLEDFUNCTIONAL_H
#define SCIMATH_SAMPLEDFUNCTIONAL_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Functional.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> A base class for indexing into arbitrary data types </summary>

// <use visibility=export>

// <reviewed reviewer="wyoung" date="1996/10/10" tests="tSampledFunctional.cc">

// <prerequisite>
//   <li> Functional
// </prerequisite>

// <etymology>
// A Functional is simply a mapping from a Domain type to a Range
// type. Experimental data is usually sampled, and is can be represented as
// a mapping from the unsigned integers to an arbitrary Domain.
// </etymology>

// <synopsis>
// This abstract class defines an interface for functions that map from the
// unsigned integers to an arbitrary type. It defines two functions: the
// operator() function which it inherits from the Functional class, and the
// nelements function which is necessary to know how many data elements. 
//
// This class is useful for freeing the writer of other classes from having
// to know how how a linear data set is stored or represented. For example,
// four floating point numbers will probably be stored as a Vector<Float>,
// and kept in memory for fast access. But 400 million floating point
// numbers cannot usually be kept in memory, and may be stored on disk as a
// Table. By using a SampledFunctional writers of other classes
// (Interpolate1D is an example), can ignore these details if all they are
// interested in is random access to individual elements of the data.
// </synopsis>

// <example>
// Because this is an abstract class the example will be inside a function
// <srcblock>
// T sum(SampledFunctional<T> data)
// {
//   T result = 0;
//   for (uInt i = 0; i < data.nelements(); i++)
//     result += data(i);
//   return result;
// }
// </srcblock>
// </example>

// <motivation>
// If all you need to do is random access indexing into arbitrary data sets
// this class provides a suitable abstraction of that functionality.
// </motivation>

// <templating arg=Range>
// <li> Templating restrictions will depend on the actual derived class that is
// used. 
// </templating>

// <thrown>
// <li> Exceptions will depend on derived classes and the templating
// arguements. This abstract class only defines an interface and does not
// throw any exceptions. 
// </thrown>

// <todo asof="1996/10/19">
//   <li> I cannot think of anything
// </todo>

template <class Range> class SampledFunctional: 
  public Functional<uInt, Range>
{
public:
  // Access the specified element of the data
  virtual Range operator()(const uInt &index) const = 0;
  // Return the total size of the data set.
  virtual uInt nelements() const = 0;
  // The virtual destructor does nothing
  virtual ~SampledFunctional(){}
};


} //# NAMESPACE CASACORE - END

#endif
