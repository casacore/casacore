//# VectorCollapser.h: base class to collapse profiles
//# Copyright (C) 1996,1997
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
#if !defined(AIPS_VECTORCOLLAPSER_H)
#define AIPS_VECTORCOLLAPSER_H
 
#if defined(_AIX)
#pragma implementation ("VectorCollapser.h")
#endif

#include <aips/aips.h>
template <class T> class Vector;
class IPosition;

// <summary> Abstract base class for LatticeApply function signatures </summary>
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> LatticeApply
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// This is an abstract base class for the collapsing of profiles. The application
// programmer implements a derived class and it is used by a LatticeApply function
// </synopsis>
//
// <example>
// <srcBlock>
// </srcBlock>
// </example>
// 
// <note role=caution>
// </note>
//      
// <note role=tip>
// </note>
//      
//      
// <motivation>
// </motivation>
//      
// <todo asof="1997/08/01">   
//   <li> 
// </todo>
//
template <class T> class VectorCollapser
{
public:
// Collapse the given vector and return one value from that operation.
// The position in the Lattice at the start of the vector is input
// as well.
  virtual T collapse(const Vector<T> &vector,
                     const IPosition& pos) = 0;

// Collapse the given vector and return a vector of values from that operation.
// The position in the Lattice at the start of the vector is input
// as well.
  virtual Vector<T>& multiCollapse(const Vector<T> &vector,
                                   const IPosition& pos) = 0;
};

#endif
