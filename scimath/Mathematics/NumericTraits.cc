//# NumericTraits.cc:
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

#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/BasicSL/Constants.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

const Double & NumericTraits<Float>::epsilon = C::flt_epsilon;
const Double & NumericTraits<Double>::epsilon = C::dbl_epsilon;
const Double & NumericTraits<Complex>::epsilon = C::flt_epsilon;
const Double & NumericTraits<DComplex>::epsilon = C::dbl_epsilon;

const Double & NumericTraits<Float>::minimum = C::flt_min;
const Double & NumericTraits<Double>::minimum = C::dbl_min;
const Double & NumericTraits<Complex>::minimum = C::flt_min;
const Double & NumericTraits<DComplex>::minimum = C::dbl_min;

const Double & NumericTraits<Float>::maximum = C::flt_max;
const Double & NumericTraits<Double>::maximum = C::dbl_max;
const Double & NumericTraits<Complex>::maximum = C::flt_max;
const Double & NumericTraits<DComplex>::maximum = C::dbl_max;

} //# NAMESPACE CASACORE - END

