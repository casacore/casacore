//# LEL.h: Lattice expression
//# Copyright (C) 1996,1997,1998,1999,2003
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
//# $Id: Lattices.h 21521 2014-12-10 08:06:42Z gervandiepen $

#ifndef LATTICES_LEL_H
#define LATTICES_LEL_H


#include <casacore/lattices/LEL/LatticeExpr.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>

// <summary>
// Lattice expressions.
// </summary>

// <prerequisite>
//   <li> module <linkto module=Lattices>Lattices</linkto>
// </prerequisite>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" demos="">
// </reviewed>

// <etymology>
// LEL: Lattice Expression Language.
// </etymology>

// <synopsis>
// A <linkto class=LatticeExpr>LatticeExpr</linkto> represents
// a mathematical expression of lattices. All standard operators, regions,
// and many, many <linkto class=LatticeExprNode>functions</linkto>
// can be used in an expression.
// <br> An expression is calculated on-the-fly. Thus only when
// the user gets a part of the lattice, is the expression calculated
// for that part. Subexpressions resulting in a scalar are calculated
// only once, on a get of the first part of the lattice expression.
// <br> Note that a lattice expression is not writable, thus using
// the put function on such a lattice results in an exception.
// <br> <a href="../notes/223.html">Note 223</a>
// gives a more detailed
// explanation of the capabilities of LEL (Lattice Expression Language).
// <p>
// If the expression consists of images, the result can also be
// treated as an image using class <linkto class=ImageExpr>ImageExpr</linkto>.
// With the <src>command</src> function in
// <linkto class=ImageExprParse>ImageExprParse</linkto> it is possible
// to parse and execute a LEL expression given as as a string.

// </module>


} //# NAMESPACE CASACORE - END

#endif
