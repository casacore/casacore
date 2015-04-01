//# LELUnaryEnums.h: Enums of unary operators
//# Copyright (C) 1997
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
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

#ifndef LATTICES_LELUNARYENUMS_H
#define LATTICES_LELUNARYENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Each LEL unary operation is described in this enum  </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
//   <li> <linkto class="LELUnary"> LELUnary</linkto>
// </prerequisite>
//
// <etymology>
//  This enum provides a value for each unary operation accepted
//  by the Lattice Expression Language classes.
// </etymology>
//
// <synopsis>
//  Each unary operator accepted by the bridging class LatticeExprNode
//  and passed  on to the LELUnary letter classes is labelled internally 
//  with a value from this enum.  
// </synopsis> 
//
// <todo asof="1998/01/21">
// </todo>


class LELUnaryEnums 
{
public:
   enum Operation{

// Unary plus
   PLUS, 

// Unary minus
   MINUS, 

// Unary not
   NOT, 

// Total number
   NOPS

};

};


} //# NAMESPACE CASACORE - END

#endif
