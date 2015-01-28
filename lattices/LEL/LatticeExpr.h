//# LatticeExpr.h:  LatticeExpr.h
//# Copyright (C) 1997,1998,1999,2000,2003
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

#ifndef LATTICES_LATTICEEXPR_H
#define LATTICES_LATTICEEXPR_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/casa/Arrays/Slicer.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Array;
template <class T> class LELArray;


// <summary> Class to allow C++ expressions involving lattices </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//
// </prerequisite>
//
// <etymology>
// The name is derived from the fact that this class provides
// an expression interface to the user which s/he may use to
// write C++ expressions involving Lattices.
// </etymology>
//
// <synopsis>
//    This class provides an interface which allows the C++ programmer
//    to enter expressions such as "sin(a)+b" where "a" and "b"
//    are Lattices.   
//
//    This class is termed an envelope class, and inside it are the
//    letter classes which do the real work.    In reality, the letter
//    classes are actually accessed via a bridging class called 
//    LatticeExprNode, which exists to handle type conversions.
//    The letter classes iterate through the Lattices and evaluate the 
//    expression for each chunk of the iteration (usually a tile shape).
//
//    It is in the LatticeExprNode class that all the available expression
//    operations are defined, so you should look there to see what 
//    functionality is available.
//
//    A description of the implementation details of these classes can
//    be found in
//    <a href="../notes/216.html">Note 216</a>
// </synopsis> 
//
// <example>
// <srcblock>
//  ArrayLattice<Float>   f1(IPosition (2,nx,ny));
//  ArrayLattice<Float>   f2(IPosition (2,nx,ny));
//  f2.set(2.0);
//  f1.copyData(2*f2+f2);
// </srcblock>
//
//  In this example, the values of the pixels in Lattice f1 are set
//  to the values resulting from the expression "2*f2 + f2"
//  I.e. the expression is evaluated for each pixel in the Lattices
//
//  Note that :
//  1) the Lattice::copyData function is expecting a Lattice argument.  
//  2) LatticeExpr inherits from Lattice and therefore a LatticeExpr
//     object is a valid argument object type
//  3) The expression in the copyData call is automatically converted to 
//     a LatticeExprNode by the constructors and operators in LatticeExprNode
//  4) The LatticeExprNode object so created is automatically converted
//     to a LatticeExpr by casting functions in LatticeExprNode.
// </example>
//
// <example>
// <srcblock>
//  ArrayLattice<Float>   f1(IPosition (2,nx,ny));
//  ArrayLattice<Float>   f2(IPosition (2,nx,ny));
//  ArrayLattice<Double>  d(IPosition (2,nx,ny));
//  ArrayLattice<Complex> c(IPosition (2,nx,ny));
//  ArrayLattice<Bool>    b(IPosition (2,nx,ny));
//
//  f2.set(1.0); d.set(2.0); c.set(Complex(2.0,3.0)); b.set(True);
//  f1.copyData( (3.5*f2) + (cos(d)) - (10/min(d,f2)*(-abs(c))*ntrue(b)) - (C::pi) );
// </srcblock>
//  
//  In this rather silly example, we fill Lattice "f1" with the result of the
//  expression.  The expression shows the use of constants, unary operations, 
//  binary operations, 1D and 2D functions.  It also shows how mixed types can 
//  be handled.  The output Lattice is a Float, whereas  mixed into the 
//  expression are subexpressions involving Float, Double, Complex and Bool
//  Lattices.
//
// </example>
//
// <motivation>
//  The Lattice expression classes enable the C++ programmer much simpler 
//  handling of mathematical expressions involving lattices.  In addition, 
//  these classes provide the infrastructure on top of which we can build 
//  an image calculator for Glish users
// </motivation>

// <todo asof="1997/01/15">
//   <li> masks
//   <li> regions
// </todo>


template <class T> class LatticeExpr : public MaskedLattice<T>
{
public:

  // Default constructor
   LatticeExpr();

  // Constructor from an arbitrary LatticeExprNode expression object.
  // An exception is thrown if the expression data type cannot be
  // converted to the template data type.
  // The shape argument is mandatory if the expression has no shape.
  // If the expression has a shape and if shape is given, it is checked
  // if they are equal.
   LatticeExpr (const LatticeExprNode& expr);
   LatticeExpr (const LatticeExprNode& expr, const IPosition& latticeShape);

  // Copy constructor (reference semantics)
   LatticeExpr (const LatticeExpr<T>& other);

  // Destructor, does nothing
   virtual ~LatticeExpr();

  // Assignment (reference semantics)
   LatticeExpr<T>& operator=(const LatticeExpr<T>& other);

  // Make a copy of the derived object (reference semantics).
   virtual MaskedLattice<T>* cloneML() const;

  // Has the object really a mask?
   virtual Bool isMasked() const;

  // Get the region used (always returns 0).
   virtual const LatticeRegion* getRegionPtr() const;

  // Returns False, as the LatticeExpr lattice is not writable.
   virtual Bool isWritable() const;

  // Handle locking of the LatticeExpr which is delegated to all of its parts.
  // <br>hasLock() is True if all parts of the expression return True.
  // <br>It is strongly recommended to use class
  // <linkto class=LatticeLocker>LatticeLocker</linkto> to
  // handle lattice locking. It also contains a more detailed
  // explanation of the locking process.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  // </group>

  // Resynchronize the Lattice object with the lattice file.
  // This function is only useful if no read-locking is used, ie.
  // if the table lock option is UserNoReadLocking or AutoNoReadLocking.
  // In that cases the table system does not acquire a read-lock, thus
  // does not synchronize itself automatically.
  // <br>By default the function does not do anything at all.
  virtual void resync();

  // Returns the shape of the Lattice including all degenerate axes
  // (i.e. axes with a length of one)
   virtual IPosition shape() const;

  // Return the best cursor shape.  
   virtual IPosition doNiceCursorShape (uInt maxPixels) const;

  // Returns the coordinates of the lattice expression.
   virtual LELCoordinates lelCoordinates() const;

  // Do the actual get of the data.
  // The return value is always False, thus the buffer does not reference
  // another array.
   virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual get of the mask data.
  // The return value is always False, thus the buffer does not reference
  // another array.
   virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // An expression is not writable so this functions throws an exception.
   virtual void doPutSlice (const Array<T>& sourceBuffer,
			    const IPosition& where,
			    const IPosition& stride);

  // Copy the data from this lattice to the given lattice.
   virtual void copyDataTo (Lattice<T>& to) const;

  // Handle the Math operators (+=, -=, *=, /=).
  // They work similarly to copyData(To).
  // However, they are not defined for Bool types, thus specialized below.
   virtual void handleMathTo (Lattice<T>& to, int oper) const;

private:
   // Initialize the object from the expression.
   void init (const LatticeExprNode& expr);


   LatticeExprNode expr_p;     //# its shape can be undefined
   IPosition       shape_p;    //# this shape is always defined
   LELArray<T>*    lastChunkPtr_p;
   Slicer          lastSlicer_p;
};


template<> inline
void LatticeExpr<Bool>::handleMathTo (Lattice<Bool>&, int) const
  { throwBoolMath(); }



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LEL/LatticeExpr.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
