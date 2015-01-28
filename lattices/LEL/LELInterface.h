//# LELInterface.h:  Abstract base class for lattice expressions
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

#ifndef LATTICES_LELINTERFACE_H
#define LATTICES_LELINTERFACE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELAttribute.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/IO/FileLocker.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class LELScalar;
template <class T> class LELArray;
template <class T> class LELArrayRef;
class Slicer;


// <summary> This base class provides the interface for Lattice expressions </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
// </prerequisite>

// <etymology>
//  The name means "Lattice Expression Language Interface".
//  This class provides the declaration for the interface for classes
//  that are to provide Lattice expression computational functionality 
// </etymology>

// <synopsis>
//  This class is part of the Letter/envelope scheme which enables
//  the C++ programmer to write mathematical expressions involving 
//  Lattices.   The envelope class LatticeExpr invokes the bridge 
//  class LatticeExprNode.  LatticeExprNode  activates the letter 
//  classes which provide the real functionality. 
//
//  A description of the implementation details of these classes can
//  be found in
//  <a href="../notes/216.html">Note 216</a>
//
//  This class, LELInterface,  is the abstract base class for all of 
//  the letter classes.  Its purpose is to declare the interface inherited 
//  by all of its derived classes which are used polymorphically. The derived 
//  classes offer the functionality to create and evaluate the expression 
//  tree that results  from the compiler parsing the expression.  
//  For example, these derived classes are activated by LatticeExprNode to 
//  handle operations like reading pixels from a Lattice, applying binary 
//  operations to  Lattices, applying mathematical functions to Lattices 
//  and so on.
//
//  The heart of the interface is in the functions <src>eval</src> and
//  <src>getScalar</src>.   These recursively evaluate the result of the 
//  current expression when the result is either an array or a scalar,
//  respectively.   The need for recursion can be understood with a simple
//  example.
//
//  Consider an expression summing two Lattices such as "2*(b+c)".
//  The expression tree consists of nodes (leaves) that 1) get Lattice 
//  pixels from the Lattice (expressions "b" and "c"), 2) add the pixel 
//  values of the Lattices together (operator "+"), and 3) multiply a Lattice
//  by a scalar (operator "*").   At the top of the tree, 
//  we have a scalar (2.0) and a Lattice (the 
//  result of "b+c").  The top-of-the-tree expression has to multiply
//  them together.  That's what the <src>eval</src> function for the "*"
//  operation needs to do.   The key is that each of the "2.0" and 
//  "b+c" are really Lattice expressions themselves and they can be evaluated.
//  So before the "*" <src>eval</src> function can 
//  multiply its two expressions together, it must individually evaluate them.
//  Thus, it first calls the <src>getScalar</src> function of
//  the object housing the expression "2.0".  This will in fact return 
//  the scalar value "2.0".  Then it calls
//  <src>eval</src> on the expression object housing "b+c".  This 
//  object in turn first calls <src>eval</src> on the left ("b") and 
//  right ("c") expressions which results in the pixels for the Lattices 
//  being returned.  It then adds them together, returning the result
//  to the top of the tree where they are multiplied by 2.     You can see
//  that since all these different expression objects call the 
//  <src>eval</src> or <src>getScalar</src> function that they all inherit
//  from LELInterface.  Indeed for our example above, the actual classes
//  involved are are LELLattice (get pixels from Lattice) and LELBinary
//  ("+" and "*" operators) which inherit from LELInterface.  When these
//  objects are constructed, they work out whether the result of their
//  evaluation is a scalar or not.  This is how the classes higher up
//  the tree know whether to call  <src>eval</src> or <src>getScalar</src>.
//
//  The results of the computations are either returned in the buffer in
//  the <src>eval</src> function or by value by <src>getScalar</src>
//
//  The classes evaluate the expression for each specified Lattice
//  chunk (usually tile by tile).    The <src>section</src> argument 
//  in the <src>eval</src> function specifies the section of the 
//  Lattice being evaluated.   The absence of the <src>section</src> 
//  argument in the <src>getScalar</src> function emphasises the 
//  scalar nature; a scalar expression does not have a shape. For most
//  of the letter classes, the <src>section</src> argument is irrelevant;
//  the only one it really matters for is LELLattice which fetches the
//  pixels from the Lattice.  The rest only care about the shape of the
//  buffer in the <src>eval</src> call.
//
// </synopsis> 
//
// <motivation>
// The many letter classes that actually do the computational work
// are used polymorphically.  Therefore, they must have a base 
// class declaring the interface.
// </motivation>

// <todo asof="1998/02/20">
// </todo>


template <class T> class LELInterface
{
public:

// Virtual destructor
   virtual ~LELInterface();

// Evaluate the expression and fill the result array
   virtual void eval (LELArray<T>& result,
                      const Slicer& section) const = 0;
   virtual void evalRef (LELArrayRef<T>& result,
			 const Slicer& section) const;

// Get the result of a scalar subexpression.
   virtual LELScalar<T> getScalar() const = 0;

// Get the result of an array subexpression.
// It does eval for the entire array.
// An exception is thrown if the shape of the subexpression is unknown.
   LELArray<T> getArray() const;

// Do further preparations (e.g. optimization) on the expression.
// It returns True if the expression is an invalid scalar
// (i.e. with a False mask).
// That can happen if the expression has a component with an invalid
// scalar value (e.g. min(lattice) where lattice contains no valid elements).
   virtual Bool prepareScalarExpr() = 0;

// Is the result of evaluating this expression a scalar ?
   Bool isScalar() const {return attr_p.isScalar();}

// Get the shape of the expression result.
   const IPosition& shape() const {return attr_p.shape();}

// Get expression attribute
   const LELAttribute& getAttribute() const {return attr_p;}

// Get class name
   virtual String className() const = 0;

// If the given expression is a valid scalar, replace it by its result.
// It returns False if the expression is no scalar or if the expression
// is an invalid scalar (i.e. with a False mask).
   static Bool replaceScalarExpr (CountedPtr<LELInterface<T> >& expr);

  // Handle locking/syncing of the parts of a lattice expression.
  // <br>By default the functions do not do anything at all.
  // lock() and hasLock return True.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  // </group>

protected:
// Set the expression attributes of this object.
   void setAttr(const LELAttribute& attrib);

private:
   LELAttribute attr_p;
};



} //# NAMESPACE CASACORE - END

//# There is a problem in including LELInterface.tcc, because it needs
//# LELUnary.h which in its turn includes LELInterface.h again.
//# So in a source file including LELUnary.h, LELInterface::replaceScalarExpr
//# fails to compile, because the LELUnary declarations are not seen yet.
//# Therefore LELUnary.h is included here, while LELUnary.h includes
//# LELInterface.tcc.
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LEL/LELUnary.h>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
