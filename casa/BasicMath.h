//# BasicMath.h: Basic math related classes.
//# Copyright (C) 2005
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


#ifndef CASA_BASICMATH_H
#define CASA_BASICMATH_H

#include <casacore/casa/aips.h>

#include <casacore/casa/BasicMath/ConvertScalar.h>
#include <casacore/casa/BasicMath/Functional.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicMath/Random.h>
#include <casacore/casa/BasicMath/Primes.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary> Classes and global functions for basic math use </summary>

// <reviewed reviewer="" date="" demos="">
// </reviewed>

// <synopsis>
//
// This module is a bag of related basic math classes and
// global functions.
//
// The following functionality is available:
// <ul>
//  <li> Templated functors that can be used with <src>std::transform</src>
//       to apply functions like sin, near, sqrt, etc. to iterators on
//       sequences like Array, Block, std::vector, etc.
//  <li> Templated functions <linkto group="ConvertScalar.h#Scalar conversion">
//       ConvertScalar</linkto>
//       to convert scalars from one type to another.
//  <li> Class <linkto class=Functional:description>
//       Functional</linkto>
//       to map a domain object into a range object.
//  <li> Functions <linkto group="Math.h#Math interface for casacore>
//       Math</linkto>
//       to interface to math.h and other scalar math functions.
//  <li> Class <linkto class=Random:description>
//       Random</linkto>
//       to offer random number generators.
// <li> <linkto class="Primes">Prime</linkto> numbers
// </ul>
//
// <note role=tip> You may want to look at the individual header files
// to see whether you might not prefer to include only the header
// files you really need; it may be more efficient to do so.
// </note>
//
// </synopsis>

//# <todo asof="2005/06/08">
//#   <li>
//# </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif

