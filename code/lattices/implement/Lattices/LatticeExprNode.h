//# LatticeExprNode.h:  LatticeExprNode
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

#if !defined(AIPS_LATTICEEXPRNODE_H)
#define AIPS_LATTICEEXPRNODE_H


//# Includes
#include <trial/Lattices/LELInterface.h>
#include <trial/Lattices/LELAttribute.h>
#include <trial/Lattices/LELBinaryEnums.h>
#include <trial/Lattices/LELUnaryEnums.h>
#include <trial/Lattices/LELFunctionEnums.h>
//#include <trial/Lattices/LatticeExpr.h>
#include <aips/Utilities/CountedPtr.h>
#include <aips/Utilities/DataType.h>

//# Forward Declarations
class PixelRegion;
template <class T> class LatticeExpr;
template <class T> class Lattice;
template <class T> class Array;
template <class T> class Block;


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


class LatticeExprNode
{
// Unary operators
// <group>
   friend LatticeExprNode operator+(const LatticeExprNode& expr);
   friend LatticeExprNode operator-(const LatticeExprNode& expr);
   friend LatticeExprNode operator!(const LatticeExprNode& expr);
// </group>

// Numerical binary operators
// <group>
   friend LatticeExprNode operator+ (const LatticeExprNode& left,
				     const LatticeExprNode& right);
   friend LatticeExprNode operator- (const LatticeExprNode& left,
				     const LatticeExprNode& right);
   friend LatticeExprNode operator* (const LatticeExprNode& left,
                                     const LatticeExprNode& right);
   friend LatticeExprNode operator/ (const LatticeExprNode& left,
                                     const LatticeExprNode& right);
// </group>

// Relational operators
// <group>
   friend LatticeExprNode operator== (const LatticeExprNode& left,
				      const LatticeExprNode& right);
   friend LatticeExprNode operator>  (const LatticeExprNode& left,
				      const LatticeExprNode& right);
   friend LatticeExprNode operator>= (const LatticeExprNode& left,
				      const LatticeExprNode& right);
   friend LatticeExprNode operator<  (const LatticeExprNode& left,
				      const LatticeExprNode& right);
   friend LatticeExprNode operator<= (const LatticeExprNode& left,
				      const LatticeExprNode& right);
   friend LatticeExprNode operator!= (const LatticeExprNode& left,
				      const LatticeExprNode& right);
// </group>

// Logical operators
// <group>
   friend LatticeExprNode operator&& (const LatticeExprNode& left,
				      const LatticeExprNode& right);
   friend LatticeExprNode operator|| (const LatticeExprNode& left,
				      const LatticeExprNode& right);
// </group>

// Numerical functions
// <group>
   friend LatticeExprNode sin  (const LatticeExprNode& expr);
   friend LatticeExprNode sinh (const LatticeExprNode& expr);
   friend LatticeExprNode asin (const LatticeExprNode& expr);
   friend LatticeExprNode cos  (const LatticeExprNode& expr);
   friend LatticeExprNode cosh (const LatticeExprNode& expr);
   friend LatticeExprNode acos (const LatticeExprNode& expr);
   friend LatticeExprNode tan  (const LatticeExprNode& expr);
   friend LatticeExprNode tanh (const LatticeExprNode& expr);
   friend LatticeExprNode atan (const LatticeExprNode& expr);
   friend LatticeExprNode atan2(const LatticeExprNode& left,
				const LatticeExprNode& right);
   friend LatticeExprNode exp  (const LatticeExprNode& expr);
   friend LatticeExprNode log  (const LatticeExprNode& expr);
   friend LatticeExprNode log10(const LatticeExprNode& expr);
   friend LatticeExprNode pow  (const LatticeExprNode& left,
				const LatticeExprNode& right);
   friend LatticeExprNode sqrt (const LatticeExprNode& expr);
   friend LatticeExprNode ceil (const LatticeExprNode& expr);
   friend LatticeExprNode floor(const LatticeExprNode& expr);
   friend LatticeExprNode fmod (const LatticeExprNode& left,
                                const LatticeExprNode& right);
   friend LatticeExprNode min  (const LatticeExprNode& left,
				const LatticeExprNode& right);
   friend LatticeExprNode max  (const LatticeExprNode& left,
				const LatticeExprNode& right);
   friend LatticeExprNode abs  (const LatticeExprNode& expr);
   friend LatticeExprNode arg  (const LatticeExprNode& expr);
   friend LatticeExprNode real (const LatticeExprNode& expr);
   friend LatticeExprNode imag (const LatticeExprNode& expr);
   friend LatticeExprNode conj (const LatticeExprNode& expr);
// </group>

// Specialized numerical functions 
// <group>
// sqrt(a^2+b^2)
   friend LatticeExprNode amp (const LatticeExprNode& left,
                               const LatticeExprNode& right);
// </group>   



// Functions operating on a numeric expression resulting in a scalar
// <group>
   friend LatticeExprNode min   (const LatticeExprNode& expr);
   friend LatticeExprNode max   (const LatticeExprNode& expr);
   friend LatticeExprNode mean  (const LatticeExprNode& expr);
   friend LatticeExprNode sum  (const LatticeExprNode& expr);
// </group>

// Function to get the number of elements in a lattice
   friend LatticeExprNode nelements (const LatticeExprNode& expr);

// Functions operating on a logical expression resulting in a scalar;
// any and all result in a Bool; ntrue and nfalse result in a float.
// <group>
   friend LatticeExprNode any   (const LatticeExprNode& expr);
   friend LatticeExprNode all   (const LatticeExprNode& expr);
   friend LatticeExprNode ntrue (const LatticeExprNode& expr);
   friend LatticeExprNode nfalse(const LatticeExprNode& expr);
// </group>

// Functions to convert to the given data type.
// These are mostly meaningful for down-conversions (e.g. double to float),
// since up-conversions are automatically done to get matching data types
// when needed.
// <group>
   friend LatticeExprNode toFloat   (const LatticeExprNode& expr);
   friend LatticeExprNode toDouble  (const LatticeExprNode& expr);
   friend LatticeExprNode toComplex (const LatticeExprNode& expr);
   friend LatticeExprNode toDComplex(const LatticeExprNode& expr);
// </group>

public:

// Default constructor
   LatticeExprNode();

// Unary expression constructor. Relies on implicit conversion from 
// other types (e.g. Double) with resultant loss of precision
// <group>
   LatticeExprNode (Int constant);
   LatticeExprNode (Float constant);
   LatticeExprNode (Double constant);
   LatticeExprNode (const Complex& constant);
   LatticeExprNode (const DComplex& constant);
   LatticeExprNode (Bool constant);
// </group>

// Lattice expression (gets Lattice chunk) constructor.
// <group>
   LatticeExprNode (const Lattice<Float>& lattice);
   LatticeExprNode (const Lattice<Double>& lattice);
   LatticeExprNode (const Lattice<Complex>& lattice);
   LatticeExprNode (const Lattice<DComplex>& lattice);
   LatticeExprNode (const Lattice<Bool>& lattice);
// </group>

// Copy constructor (reference semantics)
   LatticeExprNode(const LatticeExprNode& other);

// Destructor, does nothing
   virtual ~LatticeExprNode();

// Assignment (reference semantics)
   LatticeExprNode& operator=(const LatticeExprNode& other);

// Convert the expression to another data type.
// <group>
   CountedPtr<LELInterface<Float> >    makeFloat() const;
   CountedPtr<LELInterface<Double> >   makeDouble() const;
   CountedPtr<LELInterface<Complex> >  makeComplex() const;
   CountedPtr<LELInterface<DComplex> > makeDComplex() const;
// </group>

// Evaluate the expression
// <group>
   void eval (Array<Float>& result, const PixelRegion& region) const;
   void eval (Array<Double>& result, const PixelRegion& region) const;
   void eval (Array<Complex>& result, const PixelRegion& region) const;
   void eval (Array<DComplex>& result, const PixelRegion& region) const;
   void eval (Array<Bool>& result, const PixelRegion& region) const;
// </group>

// Evaluate the expression (in case it is a scalar).
// <group>
   void eval (Float& result) const;
   void eval (Double& result) const;
   void eval (Complex& result) const;
   void eval (DComplex& result) const;
   void eval (Bool& result) const;
   Float getFloat() const;
   Double getDouble() const;
   Complex getComplex() const;
   DComplex getDComplex() const;
   Bool getBool() const;
// </group>

// Get the data type of the expression.
   DataType dataType() const
      {return dtype_p;}

// Is the result of eval a scalar?
   Bool isScalar() const
      {return pAttr_p->isScalar();}

// Return the shape of the Lattice including all degenerate axes
// (ie. axes with a length of one)
   const IPosition& shape() const
      {return pAttr_p->shape();}

// Replace a scalar subexpression by its result.
   void replaceScalarExpr();
  
// Make the object from a Counted<LELInterface> pointer.
// Ideally this function is private, but alas it is needed in LELFunction1D,
// operator==, and more (too many to make them friend).
// <group>
   LatticeExprNode(const CountedPtr<LELInterface<Float> >& expr);
   LatticeExprNode(const CountedPtr<LELInterface<Double> >& expr);
   LatticeExprNode(const CountedPtr<LELInterface<Complex> >& expr);
   LatticeExprNode(const CountedPtr<LELInterface<DComplex> >& expr);
   LatticeExprNode(const CountedPtr<LELInterface<Bool> >& expr);
// </group>

// Convert automatically to a LatticeExpr object.
// These functions are added because the g++ compiler did not use
// the LatticeExpr constructor for an automatic conversion.
// <group>
   operator LatticeExpr<Float>();
   operator LatticeExpr<Double>();
   operator LatticeExpr<Complex>();
   operator LatticeExpr<DComplex>();
   operator LatticeExpr<Bool>();
// </group>

// Determine the resulting data type from the given data types.
// An exception is thrown if they are incompatible.
   static DataType resultDataType (DataType left, DataType right);

// Check the arguments of a function and return the resulting attribute object.
   static LELAttribute checkArg (const Block<LatticeExprNode>& arg,
				 const Block<Int>& argType,
				 Bool expectArray);


private:
// Make the object from a LELInterface* pointer.
// <group>
   LatticeExprNode(LELInterface<Float>* expr);
   LatticeExprNode(LELInterface<Double>* expr);
   LatticeExprNode(LELInterface<Complex>* expr);
   LatticeExprNode(LELInterface<DComplex>* expr);
   LatticeExprNode(LELInterface<Bool>* expr);
// </group>

// Create a new node for a numerical unary operation.
// The result has the same data type as the input.

   static LatticeExprNode newNumUnary (LELUnaryEnums::Operation oper,
				       const LatticeExprNode& expr);

// Create a new node for a numerical function with 1 argument.
// The result has the same data type as the input.
   static LatticeExprNode newNumFunc1D (LELFunctionEnums::Function func,
					const LatticeExprNode& expr);

// Create a new node for a real numerical function with 1 argument.
// The result has the same data type as the input.
   static LatticeExprNode newRealFunc1D (LELFunctionEnums::Function func,
					 const LatticeExprNode& expr);

// Create a new node for a complex numerical function with 1
// argument. The result has the same data type as the input.
   static LatticeExprNode newComplexFunc1D (LELFunctionEnums::Function func,
                                            const LatticeExprNode& expr);

// Create a new node for a numerical function with 1 arguments that 
// returns a real number
   static LatticeExprNode newNumReal1D (LELFunctionEnums::Function func,
					const LatticeExprNode& expr);

// Create a new node for a numerical function with 2 arguments.
// The result has the same data type as the combined input type.
   static LatticeExprNode newNumFunc2D (LELFunctionEnums::Function func,
					const LatticeExprNode& left,
					const LatticeExprNode& right);

// Create a new node for a numerical binary operator.
// The result has the same data type as the combined input type.
   static LatticeExprNode newNumBinary (LELBinaryEnums::Operation oper,
					const LatticeExprNode& left,
					const LatticeExprNode& right);

// Create a new node for a comparison binary operator.
// The result has the same data type as the combined input type.
   static LatticeExprNode newBinaryCmp (LELBinaryEnums::Operation oper,
					const LatticeExprNode& left,
					const LatticeExprNode& right);

   
//# Member variables.
   Bool                donePrepare_p;
   DataType            dtype_p;
   const LELAttribute* pAttr_p;
   CountedPtr<LELInterface<Float> >    pExprFloat_p;
   CountedPtr<LELInterface<Double> >   pExprDouble_p;
   CountedPtr<LELInterface<Complex> >  pExprComplex_p;
   CountedPtr<LELInterface<DComplex> > pExprDComplex_p;
   CountedPtr<LELInterface<Bool> >     pExprBool_p;
};

#endif
