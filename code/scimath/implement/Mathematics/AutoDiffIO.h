//# AutoDiffIO.h: test output for AutoDiff objects
//# Copyright (C) 1995,1999,2000
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

#if !defined (AIPS_AUTO_DIFF__IO_H)
#define AIPS_AUTO_DIFF__IO_H


//# Includes
#include <aips/aips.h>
#include <trial/Mathematics/AutoDiff.h>

//# Forward declarations
#if defined(AIPS_STDLIB)
#include <iosfwd>
#else
class ostream;
#endif

// <summary>
// Implements all IO operators and functions for AutoDiff.
// </summary>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
// <li> AutoDiff
// </prerequisite>
//
// <etymology>
// Implements all IO operators and functions for AutoDiff.
// </etymology>
//
// <todo asof="yyyy/mm/dd">
// </todo>
 
// <group name="AutoDiff IO operations">
template<class T> ostream &operator << (ostream &, const AutoDiff<T> &);
// </group>


#endif
