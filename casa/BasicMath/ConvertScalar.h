//# ConvertScalar.h: Templated functions to convert scalars
//# Copyright (C) 2000,2002
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

#ifndef CASA_CONVERTSCALAR_H
#define CASA_CONVERTSCALAR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Templated functions to convert scalars from one type to another.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <synopsis> 
// Templated functions to convert scalars from one type to another.
// They are useful to be able to convert a DComplex to a Complex
// in templated classes.
// A Complex,DComplex specialisation is necessary because the complex
// class in the standard C++ library does not have such an automatic
// conversion.
// </synopsis>

// <group name="Scalar conversion">
template<class T, class F> inline void convertScalar (T& out, F in)
  { out = static_cast<T>(in); }
inline void convertScalar (Complex& out, DComplex in)
  { out = Complex(in.real(), in.imag()); }
// </group>


} //# NAMESPACE CASACORE - END

#endif
