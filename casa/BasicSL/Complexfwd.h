//# Complexfwd.h: Forward declaration complex classes
//# Copyright (C) 2000,2001,2002
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

#ifndef CASA_COMPLEXFWD_H
#define CASA_COMPLEXFWD_H

//# Includes

#include <casacore/casa/aips.h>

// <summary> Forward declaration complex classes</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <synopsis>
// The Complexfwd.h include file can be used where a forward declaration
// of the Casacore complex classes could suffice (cf the system's iosfwd).
// </synopsis>

// <group name=Complexfwd>

#include <complex>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

typedef std::complex<Float>  Complex;
typedef std::complex<Double> DComplex;

class IComplex;
// </group>

} //# NAMESPACE CASACORE - END

#endif
