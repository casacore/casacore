//# Arrays.h:  A module implementing multidimensional arrays and operations
//# Copyright (C) 1995,1999
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

#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Arrays/Slice.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>

#include <aips/Arrays/ArrayIter.h>
#include <aips/Arrays/MatrixIter.h>
#include <aips/Arrays/VectorIter.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/MatrixMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/ArrayError.h>

#include <aips/Arrays/LogiArray.h>
#include <aips/Arrays/LogiVector.h>
#include <aips/Arrays/LogiMatrix.h>
#include <aips/Arrays/LogiCube.h>

#include <aips/Arrays/MaskedArray.h>
#include <aips/Arrays/MaskArrMath.h>
#include <aips/Arrays/MaskArrLogi.h>
#include <aips/Arrays/MaskArrIO.h>
#include <aips/Arrays/MaskLogiArr.h>


// <module>
//
// <summary>
// A module implementing multidimensional arrays and operations.
// </summary>

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
// The <linkto module=Lattices>Lattices</linkto> are a generalization
// of Arrays. They can handle memory- and disk-based arrays as well
// as other types of arrays (eg. expressions).
//
// The module consists of various parts:
// <ul>

// <li>
// <linkto class=Array>Array</linkto> is the basic array class.
//
// <linkto class=Vector>Vector</linkto>,
// <linkto class=Matrix>Matrix</linkto>, and
// <linkto class=Cube>Cube</linkto>
// are the one, two, and three dimensional specializations respectively of
// Array.
//
// <li>
// <linkto class=MaskedArray>MaskedArray</linkto> is the class used to mask
// an Array for operations on that Array.
//
// <li>
// <linkto class=ArrayError>ArrayError</linkto> is the base class for all
// Array exception classes.
//
// <li>
// <linkto class=ArrayIterator>ArrayIterator</linkto> can be used to iterate
// in a simple way through an Array. Note that
// <linkto class=LatticeIterator>LatticeIterator</linkto> can be used on a
// <linkto class=ArrayLattice>ArrayLattice</linkto> object for more
// advanced iteration.
//
// <li>
// <linkto class=ArrayMath>Mathematical</linkto>,
// <linkto class=ArrayLogical>logical</linkto>,
// <linkto class=ArrayIO>IO</linkto>,
// and other useful operations are provided for
// Arrays and MaskedArrays.
//
// <li>
// Orthogonal n-space descriptors - useful when a shape of a Lattice is
// needed or when a sub-region within a Lattice is required.
// <ul>
//   <li> The <linkto class="IPosition">IPosition</linkto> class name is a
//   concatenation of "Integer Position."  IPosition objects are normally
//   used to index into, and define the shapes of, Arrays and Lattices. For
//   example, if you have a 5-dimensional array, you need an IPosition of
//   length 5 to index into the array (or to define its shape, etc.).  It is
//   essentially a vector of integers.  The IPosition vector may point to
//   the "top right corner" of some shape, or it may be an indicator of a
//   specific position in n-space.  The interpretation is context dependent.
//   The constructor consists of an initial argument which specifies the
//   number of axes, followed by the appropriate number of respective axis
//   lengths.  Thus the constructor needs N+1 arguments for an IPosition
//   of length N. IPositions have the standard integer math relationships
//   defined. The dimensionality of the operator arguments must be the
//   same.
//<srcblock>
// // Make a shape with three axes, x = 24, y = 48, z = 16;
// IPosition threeSpace(3, 24, 48, 16);
//
// // get the value of the ith axis (note: C++ is zero based!)
// Int xShape = threeSpace(0);
// Int zShape = threeSpace(2);
//
// // construct another with all three axes values equal to 666;
// IPosition threeSpaceAlso(3,666);
//
// // do math with the IPositions...
// threeSpace += threeSpaceAlso;
// AlwaysAssert(threeSpace(1) == 714, AipsError);
// </srcblock>
//
//   <li> The <linkto class="Slicer">Slicer</linkto> class name may be
//   thought of as a short form of "n-Dimensional Slice Specifier."  
//   This object is used to bundle into one place all the information
//   necessary to specify a regular subregion within an Array or Lattice.
//   In other words, Slicer holds the location of a "slice" of a
//   greater whole.  Construction is with up to 3 IPositions: the start 
//   location of the subspace within the greater space; the shape or end
//   location of the subspace within the greater space; and the stride,
//   or multiplier to be used for each axis.  The stride gives the user
//   the chance to use every i-th piece of data, rather than every
//   position on the axis.
//   <br>
//   It is possible to leave some values in the given start or end/length
//   unspecified. Such unspecified values default to the boundaries of the
//   array to which the slicer will be applied.
//   It is also possible to use a non-zero origin when applying the slicer
//   to an array.
//
// <srcblock>
// // Define the shape of an array.
// IPosition shape(2,20,30);
//
// // Also define an origin.
// IPosition origin(2,-5,15);
//
// // Now define some Slicers, initially only specify the start
// // Its length and stride will be 1.
// Slicer ns0(IPosition(2,0,24));
//
// // make some IPositions as holders for the rest of the information
// IPosition blc,trc,inc;
//
// // Use the shape and origin to fill our holders assuming we want to use
// // as much of the Array as possible.
// ns0.inferShapeFromSource (shape, origin, blc,trc,inc);
//
// // print out the new info ie. blc=[5,9],trc=[5,9],inc=[1,1]
// cout << blc << trc << inc << endl;
//
// // Build a slicer with temporaries for arguments. The arguments are:
// // start position, end position and step increment. The Slicer::endIsLast
// // argument specifies that the end position is the trc. The alternative
// // is Slicer::endIsLength which specifies that the end argument is the
// // shape of the resulting subregion.
// //
// Slicer ns1(IPosition(2,3,5), IPosition(2,13,21), IPosition(2,3,2),
//            Slicer::endIsLast);
// IPosition shp = ns1.inferShapeFromSource (shape, blc,trc,inc);
// //
// // print out the new info ie. shp=[4,9],blc=[3,5],trc=[12,21],inc=[3,2]
// cout << shp << blc << trc << inc << endl;
// </srcblock>
//   </ul>

// The <linkto module=Arrays:classes>detailed discussions</linkto> for the
// classes and global functions will describe how to use them.
// </synopsis>
//
// </module>

#endif
