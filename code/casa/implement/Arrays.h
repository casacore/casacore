//# Arrays.h:  A module implementing multidimensional arrays and operations
//# Copyright (C) 1995
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

#if !defined (AIPS_ARRAYS_H)
#define AIPS_ARRAYS_H

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayPosIter.h>
#include <aips/Arrays/ArrayRtti.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/CubeRtti.h>
#include <aips/Arrays/LogiArray.h>
#include <aips/Arrays/LogiCube.h>
#include <aips/Arrays/LogiMatrix.h>
#include <aips/Arrays/LogiVector.h>
#include <aips/Arrays/MaskArrIO.h>
#include <aips/Arrays/MaskArrLogi.h>
#include <aips/Arrays/MaskArrMath.h>
#include <aips/Arrays/MaskLogiArr.h>
#include <aips/Arrays/MaskedArray.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/MatrixIter.h>
#include <aips/Arrays/MatrixMath.h>
#include <aips/Arrays/MatrixRtti.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/VectorIter.h>
#include <aips/Arrays/VectorRtti.h>


// <module>
//
// <summary>
// A module implementing multidimensional arrays and operations.
// </summary>

// <prerequisite>
//   <li> <linkto class=IPosition>IPosition</linkto>
// </prerequisite>
//

// <reviewed reviewer="None yet" date="yyyy/mm/dd" demos="">
// </reviewed>

// <etymology>
// This module provides classes and global functions for multidimensional
// arrays.
// </etymology>
//
// <synopsis>
// Arrays have traditionally played an important role in scientific
// computation. While it is certainly true that some of the reliance on
// arrays was due to the paucity of other data structures in FORTRAN, it
// is also true that computation on arrays reflects the common occurrence
// of regularly sampled multi-dimensioned data in science.
//
// <linkto class=Array>Array</linkto> is the basic array class.
//
// <linkto class=Vector>Vector</linkto>,
// <linkto class=Matrix>Matrix</linkto>, and
// <linkto class=Cube>Cube</linkto>
// are the one, two, and three dimensional specializations respectively of
// Array.
//
// <linkto class=MaskedArray>MaskedArray</linkto> is the class used to mask
// an Array for operations on that Array.
//
// <linkto class=ArrayError>ArrayError</linkto> is the base class for all
// Array exception classes.
//
// <linkto class=ArrayIterator>ArrayIterator</linkto> is used to iterate
// through an Array.
//
// Mathematical, logical, IO, and other useful operations are provided for
// Arrays and MaskedArrays.
//
// The <linkto module=Arrays:classes>detailed discussions</linkto> for the
// classes and global functions will describe how to use them.
// </synopsis>
//
// </module>

#endif
