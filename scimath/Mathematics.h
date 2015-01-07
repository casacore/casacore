//# Mathematics.h: Module header for Mathematical operations
//# Copyright (C) 1995,1996,1997,1998,1999
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

#ifndef SCIMATH_MATHEMATICS_H
#define SCIMATH_MATHEMATICS_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/scimath/Mathematics/Convolver.h>
#include <casacore/scimath/Mathematics/FFTPack.h>
#include <casacore/scimath/Mathematics/FFTServer.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/BasicMath/Random.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>

// <summary> Mathematical types, constants, operations </summary>

// <prerequisite>
// <li> At least high school (and preferably undergraduate
// level) understanding of mathematics.
// </prerequisite>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" demos="">
// </reviewed>

// <etymology> 
// Mathematicians may argue that everything is mathematics, and hence all of
// Casacore should be in this module. However this module will only contain
// core mathematical operations that are independent of astronomical
// applications. 
// </etymology>

// <synopsis>

// The Mathematics module has a variety of mathematical classes and functions.
// Not all numerical operations are found herein. Very complicated operations
// might be in their own module. such as deconvolution.
// Many whole array operations
// are in the <linkto file="ArrayMath.h">ArrayMath</linkto> global functions
// (part of the <linkto module="Arrays">Arrays</linkto> module). Mathematical
// operations on Lattices are found in the <linkto
// module="Lattices">Lattices</linkto> module. A wide variety of special
// Mathematical functions is planned for the <linkto
// module="Functionals">Functionals</linkto> module.
//
// The classes presently in this module fall into the following categories:
// <ul>
// <li> A wrapper around the system math.h functions called 
//      <linkto file="Math.h">Math.h</linkto>. This contains generic
//      mathematical functions. It is required that you always include this
//      file rather than the system math.h file as deficiencies in the system
//      math.h file will be implemented here.
// <li> <linkto file=NumericTraits.h>Relationships</linkto>
//      between different numerical data types.
// <li> Multi-dimensional Fourier transforms are done in the 
//      <linkto class="FFTServer">FFTServer</linkto> class.  This decomposes
//      the transforms into a one-dimensional transforms which are done using
//      the functions in the <linkto class="FFTPack">FFTPack</linkto>
//      class. The FFTPack class also contains functions for doing
//      one-dimensional sine, cosine, and real-symmetric transforms.
// <li> Numerical <linkto class="Convolver">Convolution</linkto> 
//      (both linear and circular) of multi-dimensional Arrays.
// <li> <linkto file="Random.h">Random</linkto> numbers in a wide 
//      variety of distributions. 
// <li> Interpolation in one dimension is performed by the 
//      <linkto class="Interpolate1D">Interpolate1D</linkto> class in 
//      the <linkto module="Functionals">Functionals</linkto> module
// </ul>
// </synopsis>

// </module>


} //# NAMESPACE CASACORE - END

#endif
