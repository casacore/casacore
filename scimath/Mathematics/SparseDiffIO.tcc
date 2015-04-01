//# SparseDiffIO.cc: text output for SparseDiff
//# Copyright (C) 2007
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
//# $Id: SparseDiffIO.cc,v 1.1 2007/11/16 04:34:46 wbrouw Exp $

#ifndef SCIMATH_SPARSEDIFFIO_TCC
#define SCIMATH_SPARSEDIFFIO_TCC

//# Includes
#include <casacore/scimath/Mathematics/SparseDiffIO.h>
#include <casacore/scimath/Mathematics/SparseDiff.h>
#include <casacore/casa/vector.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  template<class T>
  ostream &operator<<(ostream &os, const SparseDiff<T> &ad) {
    os << "(" << ad.value();
    for (uInt i=0; i<ad.nDerivatives(); ++i) {
      os << ", " << ad.derivatives()[i].first << "|" << 
	ad.derivatives()[i].second;
    }
    os << ")";
    return os;
  }



} //# NAMESPACE CASACORE - END


#endif
