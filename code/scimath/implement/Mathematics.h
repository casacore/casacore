//# Mathematics.h: Module header for Mathematical operations
//# Copyright (C) 1995,1996,1997,1998
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

#if !defined (AIPS_MATHEMATICS_H)
#define AIPS_MATHEMATICS_H

#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Convolver.h>
#include <aips/Mathematics/FFTServer.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/NumericTraits.h>
//# No need to include NumericTraits2.h as it is included by NumericTraits.h
#include <aips/Mathematics/Primes.h>
#include <aips/Mathematics/Random.h>
//# The following will be renamed FFTPack in a future release
#include <aips/Mathematics/extern_fft.h>
// The following are scheduled to be removed in a future AIPS++ release
#include <aips/Mathematics/ConvVector.h>
#include <aips/Mathematics/DFTServer.h>
#include <aips/Mathematics/FourierTool.h>
#include <aips/Mathematics/GridTool.h>
#include <aips/Mathematics/MathFunc.h>

// <module>

// <summary> Mathematical types, constants, operations, objects, etc.</summary>

// <prerequisite> At least high school (and preferably undergraduate
// level) understanding of mathematics. </prerequisite>

//# // <reviewed reviewer="" date="yyyy/mm/dd" demos="">
//# // </reviewed>

// <etymology> 
// Mathematicians may argue that everything is mathematics, and hence all of
// AIPS++ should be in this module. However this module will only contain
// core mathematical operations that are independent of astronomical
// applications. 
// </etymology>

// <synopsis>
// The Mathematics module has a variety of mathematical classes and functions.
// Not all numerical operations are found herein. Very complicated operations
// might be in their own module, such as 
// <linkto module="Deconvolution">Deconvolution</linkto>. 
// Many whole array operations are in the
// <linkto file="ArrayMath.h">ArrayMath</linkto> global functions (part of the
// <linkto module="Arrays">Arrays</linkto> module).
//
// The classes presently in this module fall into the following categories:
// <ul>
// <li> <linkto file="Math.h">Generic</linkto> mathematical functions
//      (including the system math.h functions) 
// <li> <linkto file="Complex.h">Complex</linkto> numbers
// <li> Mathematical, numerical and physical 
//      <linkto file="Constants.h">constants </linkto> like 
//      pi, FLT_MAX and h (Planks constant). Many of the Physical 
//      constants are also available with units in the 
//      <linkto class="QC">Measures</linkto> Module.
// <li> <linkto file=NumericTraits.h>Relationships</linkto>
//      between different numerical data types.
// <li> Numerical Fourier transforms, both 
//      <linkto class="DFTServer">Direct Transforms</linkto>
//      (for use with ungridded data) and 
//      <linkto class="FFTServer">Fast Transforms</linkto> 
//      (for gridded data)
// <li> Numerical <linkto class="Convolver">Convolution</linkto> 
//      (both linear and circular) of multi-dimensional Arrays.
// <li> <linkto class="GridTool">Gridding</linkto> of irregular data 
// <li> <linkto file="Random.h">Random</linkto> numbers in a wide 
//      variety of distributions. 
// <li> <linkto class="Primes">Prime</linkto> numbers
// <li> Interpolation in one dimension is performed by the 
//      <linkto class="Interpolate1D">Interpolate1D</linkto> class in 
//      the <linkto module="Functionals">Functionals</linkto> module
// <li> ``Special'' <linkto file="MathFunc.h">function</linkto> classes
// </ul>
// A number of classes in this module are very old and are either no longer
// required or have been superceeded by newer classes. The following classes
// will be removed in future AIPS++ releases.
// <ul>
// <li> ConvVector (replaced by Convolver)
// <li> DFTServer (ungridded Fourier Transforms are not required)
// <li> FourierTool (replaced by the updated FFTServer class)
// <li> GridTool (replaced by classes in the trial package)
// <li> MathFunc (this code will move into the Functionals module)
// </ul>
// In addition the extern_fft class will be renamed FFTPack and the Constants
// class will move into the OS Module in a future AIPS++ release.
// </synopsis>

//# // <example>
//# // </example>

//# // <motivation>
//# // </motivation>

//# // <todo asof="yyyy/mm/dd">
//# // </todo>

//
// </module>

#endif
