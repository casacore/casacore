//# LatticeExpr.h:  LatticeExpr
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

#if !defined(AIPS_LATTICEEXPR_H)
#define AIPS_LATTICEEXPR_H


//# Includes
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/LatticeExprNode.h>

//# Forward Declarations
class PixelRegion;
template <class T> class Array;


// <summary>
// Class to allow C++ expressions involving lattices
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <etymology>
// The name is derived from the fact that this class provides
// an expression interface to the user which s/he may use to
// write C++ expressions involving Lattices.
// </etymology>

// <synopsis>
//    This class provides an interface which allows the C++ programmer
//    to enter expressions such as "sin(a)+b" where "a" and "b"
//    are Lattices.   
//
//    This class is termed an envelope class, and inside it are the
//    letter classes which do the real work.  These classes iterate
//    through the Lattice and evaluate the expression for each
//    chunk of the iteration (usually a tile shape).
// </synopsis> 

// <example>
// <srcblock>
//  ArrayLattice<Float> a (IPosition (2,nx,ny));
//  ArrayLattice<Float> b (IPosition (2,nx,ny));
//  ArrayLattice<Float> c (IPosition (2,nx,ny));
//  ArrayLattice<Float> d (IPosition (2,nx,ny));
//  a.set(0.0); b.set(1.0); c.set(2.0); d.set(3.0); 
//  a.copyData( (3.5*b) + (cos(c)) - (10/min(c,b)*(-d)*log(d)) - (C::pi) );
// </srcblock>
//  
//  In this rather silly example, we create 4 small ArrayLattices and fill them
//  with some constants.  Then we fill Lattice "a" with the result of
//  the expression.  The expression shows the use
//  of scalars, unary operations, binary operations, 1D and 2D functions,
//  and constants.
//
// </example>

// <motivation>
//  Brian said I had to do this before I was allowed my icecream.
// </motivation>

// <todo asof="1996/07/01">
//   <li> masks
//   <li> mixed data types (e.g. don't lose double precision)
// </todo>


template <class T> class LatticeExpr : public Lattice<T>
{
public:

// Default constructor
   LatticeExpr();

// Constructor from an arbitrary lattice expression object.
// An exception is thrown if the expression data type mismatches
// the template data type.
// Note that the 2nd argument is included to be sure that the compiler
// does not take this constructor into account for automatic conversions
// (since the keyword explicit is not implemented by all compilers yet).
// The conversion operators in class LatticeExprNode take care of
// automatic conversions.
   LatticeExpr (const LatticeExprNode& expr, uInt dummy);

// Copy constructor (reference semantics)
   LatticeExpr(const LatticeExpr<T>& other);

// Destructor, does nothing
   virtual ~LatticeExpr();

// Assignment (reference semantics)
   LatticeExpr<T>& operator=(const LatticeExpr<T>& other);

  // Make a copy of the derived object (reference semantics).
   virtual Lattice<T>* clone() const;

  // The lattice is not writable.
   virtual Bool isWritable() const;

  // returns the shape of the Lattice including all degenerate axes
  // (ie. axes with a length of one)
   virtual IPosition shape() const;
  
  // Functions which extract an Array of values from a Lattice. All the
  // IPosition arguments must have the same number of axes as the underlying
  // Lattice, otherwise, an exception is thrown. <br>
  // The parameters are:
  // <ul>
  // <li> buffer: a <src>COWPtr<Array<T>></src> or an
  //      <src>Array<T></src>. See example two above for an examples.
  // <li> start: The starting position (or Bottom Left Corner), within 
  //      the Lattice, of the data to be extracted.
  // <li> shape: The shape of the data to be extracted.  This is not a
  //      position within the Lattice but the actual shape the buffer will 
  //      have after this function is called.  This argument added
  //      to the "start" argument should be the "Top Right Corner".
  // <li> stride: The increment for each axis.  A stride of
  //      one will return every data element, a stride of two will return
  //      every other element.  The IPosition elements may be different for
  //      each respective axis.  Thus, a stride of IPosition(3,1,2,3) says:
  //      fill the buffer with every element whose position has a first 
  //      index between start(0) and start(0)+shape(0), a second index
  //      which is every other element between start(1) and 
  //      (start(1)+shape(1))*2, and a third index of every third element 
  //      between start(2) and (start(2)+shape(2))*3.
  // <li> section: Another way of specifying the start, shape and stride
  // <li> removeDegenerateAxes: a Bool which dictates whether to remove 
  //      "empty" axis created in buffer. (e.g. extracting an n-dimensional 
  //      from an (n+1)-dimensional will fill 'buffer' with an array that 
  //      has a degenerate axis (i.e. one axis will have a length = 1.) 
  //      Setting removeDegenerateAxes = True will return a buffer with 
  //      a shape that doesn't reflect these superfluous axes.)
  // </ul>
  // 
  // The derived implementations of these functions return
  // 'True' if "buffer" is a reference to Lattice data and 'False' if it  
  // is a copy. 
  // <group>   
   virtual Bool getSlice (COWPtr<Array<T> >& buffer, const IPosition& start, 
			  const IPosition& shape, const IPosition& stride, 
			  Bool removeDegenerateAxes=False) const;
   virtual Bool getSlice (COWPtr<Array<T> >& buffer, const Slicer& section, 
			  Bool removeDegenerateAxes=False) const;
   virtual Bool getSlice (Array<T>& buffer, const IPosition& start,
			  const IPosition& shape, const IPosition& stride,
			  Bool removeDegenerateAxes=False);
   virtual Bool getSlice (Array<T>& buffer, const Slicer& section, 
			  Bool removeDegenerateAxes=False);
  // </group>

  // An expression is not writable.
   virtual void putSlice (const Array<T>& sourceBuffer, const IPosition& where,
			  const IPosition& stride);
   virtual void putSlice (const Array<T>& sourceBuffer, const IPosition& where);

  // Copy the data from this lattice to the given lattice.
   virtual void copyDataTo (Lattice<T>& to) const;

private:
   LatticeExprNode expr_p;
};


#endif
