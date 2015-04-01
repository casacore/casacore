//# NumericTraits2.h: Specialisations of the NumericTraits class
//# Copyright (C) 1996,1997,2000
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
//#
//# $Id$

#ifndef SCIMATH_NUMERICTRAITS2_H
#define SCIMATH_NUMERICTRAITS2_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> A specialisation for T=Float of the NumericTraits class </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
template <> class NumericTraits<Float> {
public:
  typedef Complex ConjugateType;
  typedef Double PrecisionType;
  static const Double & epsilon;
  static const Double & minimum;
  static const Double & maximum;
};

// <summary> A specialisation for T=Double of the NumericTraits class </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
template <> class NumericTraits<Double> {
public:
  typedef DComplex ConjugateType;
  typedef Double PrecisionType;
  static const Double & epsilon;
  static const Double & minimum;
  static const Double & maximum;
};

// <summary> A specialisation for T=Complex of the NumericTraits class </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
template <> class NumericTraits<Complex> {
public:
  typedef Float ConjugateType;
  typedef DComplex PrecisionType;
  static const Double & epsilon;
  static const Double & minimum;
  static const Double & maximum;
};

// <summary> A specialisation for T=DComplex of the NumericTraits class </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <use visibility=local>
//
// <synopsis>
// This file contains specialisations of the templated NumericTraits class. 
// See the description in 
// <linkto file="NumericTraits.h#summary">NumericTraits.h</linkto> 
// for a summary of the purpose and usage of this class and its
// specialisations.
// </synopsis>

template <> class NumericTraits<DComplex> {
public:
  typedef Double ConjugateType;
  typedef DComplex PrecisionType;
  static const Double & epsilon;
  static const Double & minimum;
  static const Double & maximum;
};


} //# NAMESPACE CASACORE - END

#endif
