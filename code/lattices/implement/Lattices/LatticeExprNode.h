//# LatticeExprNode.h:  LatticeExprNode.h
//# Copyright (C) 1997,1998,1999
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
#include <aips/Utilities/CountedPtr.h>
#include <aips/Utilities/DataType.h>

//# Forward Declarations
template <class T> class LatticeExpr;
template <class T> class Lattice;
template <class T> class MaskedLattice;
template <class T> class Array;
template <class T> class Block;


// <summary>
// Bridging class to allow C++ expressions involving lattices
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
// </prerequisite>
//
// <etymology>
// The name is derived from the fact that this class provides
// an expression interface to the user which s/he may use to
// write C++ expressions involving Lattices.  This class actually
// constructs the nodes of the expression tree, hence its name.
// It is used by the envelope class LatticeExpr and provides a 
// bridge to the letter classes derived from LELInterface.
// </etymology>
//
// <synopsis>
//    This class is part of the interface which allows the C++ programmer
//    to enter mathematical expressions involving Lattices. It is
//    is part of a Letter/envelope scheme.  It's actually a bridge
//    between the envelope class (LatticeExpr) and the letter classes
//    (derived from LELInterface) and it exists largely to handle
//    type conversions.  In a single type environment, the envelope
//    class could have directly called the letter classes.
//
//    The envelope and bridge provide the interface which the programmer
//    sees.  The letter classes do the real work and are hidden from
//    the programmer.
//
//    All the expression manipulation functionality that the user has 
//    access to is viewable in this class; it is here that the operators,
//    functions and constructors are defined.  These allow the programmer
//    to write mathematical expressions which involve Lattices.  The
//    letter classes take care of the optimal traversal of the Lattice
//    and the memory mangement thereof.  Thus the Lattices are iterated
//    through and the expressions evaluated for each chunk (usually 
//    a tile shape) of the iteration.
//
//    A description of the implementation details of these classes can
//    be found in <a href="../../../notes/216/216.html">Note 216</a>
//
//    The available functionality is defined by the global friend functions
//    and operators, plus the public constructors.  The other public members
//    functions are generally not of interest to the user of this class.
//
//    Generally, if one writes an expression such as <src>a.copyData(sin(b))</src>,
//    the expression is automatically converted first to a LatticeExprNode and
//    then to a LatticeExpr (which is a Lattice) before evaluation occurs.
//    However, it may occur that you wish to build an expression from
//    subexpressions.  To do this, you must explcitly create objects of 
//    class LatticeExprNode.  You cannot manipulate subexpressions of type
//    LatticeExpr<T>.  See below for an example.
// </synopsis> 
//
// <example>
// <srcblock>
//  ArrayLattice<Float>   f1(IPosition (2,nx,ny));
//  ArrayLattice<Float>   f2(IPosition (2,nx,ny));
//  f2.set(2.0);
//  f1.copyData(2*f2+f2);
// </srcblock>
//  In this example, the values of the pixels in Lattice f1 are set
//  to the values resulting from the expression "2*f2 + f2"
//  I.e. the expression is evaluated for each pixel in the Lattices
//
//  Note that :
//
//  1) the Lattice::copyData function is expecting a Lattice argument.  
//  2) LatticeExpr inherits from Lattice and therefore a LatticeExpr
//     object is a valid argument object type
//  3) The expression in the copyData call is automatically converted to 
//     a LatticeExprNode by the constructors and operators in LatticeExprNode
//  4) The LatticeExprNode object so created is automatically converted
//     to a LatticeExpr by casting functions in LatticeExprNode.
//
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
// <example>
// <srcblock>
//  ArrayLattice<Float>   f1(IPosition (2,nx,ny));
//  ArrayLattice<Float>   f2(IPosition (2,nx,ny));
//  f2.set(2.0);
//  LatticeExprNode exp1(sin(f2));
//  LatticeExprNode exp2(pow(f2,2.0));
//  f1.copyData(exp1+exp2);
// </srcblock>
//  In this example, the expression is "sin(f2) + pow(f2,2.0)",
//  but we have put it together from two subexpressions contained
//  in LatticeExprNode objects exp1 and exp2.  Again the LatticeExprNode
//  object froemd from summing exp1 and exp2 is automatically converted
//  to a LatticeExpr for consumption by copyData
//
// </example>
//
// <motivation>
//  The Lattice expression classes enable the C++ programmer much simpler 
//  handling of mathematical expressions involving lattices.  In addition, 
//  these classes provide the infrastructure on top of which we can build 
//  an image calculator for Glish users
// </motivation>
//
// <todo asof="1997/01/15">
//   <li> masks
//   <li> regions
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
   friend LatticeExprNode operator% (const LatticeExprNode& left,
                                     const LatticeExprNode& right)
    { return fmod (left, right); }
   friend LatticeExprNode operator^ (const LatticeExprNode& left,
                                     const LatticeExprNode& right)
    { return pow (left, right); }
// </group>

// Relational binary operators
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

// Logical binary operators
// <group>
   friend LatticeExprNode operator&& (const LatticeExprNode& left,
				      const LatticeExprNode& right);
   friend LatticeExprNode operator|| (const LatticeExprNode& left,
				      const LatticeExprNode& right);
// </group>

// Numerical 1-argument functions
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
   friend LatticeExprNode exp  (const LatticeExprNode& expr);
   friend LatticeExprNode log  (const LatticeExprNode& expr);
   friend LatticeExprNode log10(const LatticeExprNode& expr);
   friend LatticeExprNode sqrt (const LatticeExprNode& expr);
   friend LatticeExprNode ceil (const LatticeExprNode& expr);
   friend LatticeExprNode floor(const LatticeExprNode& expr);
   friend LatticeExprNode conj (const LatticeExprNode& expr);
// </group>

// Numerical 2-argument functions
// <group>
   friend LatticeExprNode atan2 (const LatticeExprNode& left,
				 const LatticeExprNode& right);
   friend LatticeExprNode pow  (const LatticeExprNode& left,
				const LatticeExprNode& right);
   friend LatticeExprNode fmod (const LatticeExprNode& left,
                                const LatticeExprNode& right);
   friend LatticeExprNode min  (const LatticeExprNode& left,
				const LatticeExprNode& right);
   friend LatticeExprNode max  (const LatticeExprNode& left,
				const LatticeExprNode& right);
// </group>

// Form a complex number from two real numbers.
   friend LatticeExprNode complex (const LatticeExprNode& left,
				   const LatticeExprNode& right);

// Numerical 1-argument functions which result in a real number
// regardless of input expression type
// <group>
   friend LatticeExprNode abs  (const LatticeExprNode& expr);
   friend LatticeExprNode arg  (const LatticeExprNode& expr);
   friend LatticeExprNode real (const LatticeExprNode& expr);
   friend LatticeExprNode imag (const LatticeExprNode& expr);
// </group>

// 1-argument functions operating on a numeric expression resulting 
// in a scalar
// <group>
   friend LatticeExprNode min   (const LatticeExprNode& expr);
   friend LatticeExprNode max   (const LatticeExprNode& expr);
   friend LatticeExprNode mean  (const LatticeExprNode& expr);
   friend LatticeExprNode sum   (const LatticeExprNode& expr);
// </group>

// 1-argument function to get the number of elements in a lattice.
// If the lattice is masked, only the True elements are counted.
// Results in a scalar Double.
   friend LatticeExprNode nelements (const LatticeExprNode& expr);

// 2-argument function to get the length of an axis.
// Results in a scalar Float.
// The 2nd expression (giving the axis number) has to be a real scalar.
// <note role=caution>
// Axes start counting at 1.
// If the axis is a number < 1, an exception is thrown.
// If the axis is a number exceeding the dimensionality, 1 is returned.
// </note>
   friend LatticeExprNode length (const LatticeExprNode& expr,
				  const LatticeExprNode& axis);

// Functions operating on a logical expression resulting in a scalar;
// Functions "any" (are any pixels "True") and "all" (are all pixels
// "True") result in a Bool; functions "ntrue" and "nfalse" result 
// in a Double.
// <group>
   friend LatticeExprNode any   (const LatticeExprNode& expr);
   friend LatticeExprNode all   (const LatticeExprNode& expr);
   friend LatticeExprNode ntrue (const LatticeExprNode& expr);
   friend LatticeExprNode nfalse(const LatticeExprNode& expr);
// </group>

// This function finds <src>sqrt(left^2+right^2)</src>.  This
// could be used to find the (biased) polarized intensity if
// left and right are images of Stokes Q and U.
   friend LatticeExprNode amp (const LatticeExprNode& left,
                               const LatticeExprNode& right);

// This function finds <src>180/pi*atan2(left,right)/2</src>.  This could be 
// used to find the position of linear polarization if left 
// and right are images of Stokes U and Q, respectively.
   friend LatticeExprNode pa (const LatticeExprNode& left,
                              const LatticeExprNode& right);

// Function resembling the ternary <src>?:</src> construct in C++.
// The argument "condition" has to be a Bool scalar or lattice.
// If an element in "condition" is True, the corresponding element from
// "arg1" is taken, otherwise it is taken from "arg2".
   friend LatticeExprNode iif (const LatticeExprNode& condition,
			       const LatticeExprNode& arg1,
			       const LatticeExprNode& arg2);

// Functions to convert to the given data type.  These are mostly 
// meaningful for down-conversions (e.g. double to float),
// since up-conversions are automatically done to get matching data types
// when needed.  Note that some conversions are not supported, such
// as Complex to Double or Float
// <group>
   friend LatticeExprNode toFloat   (const LatticeExprNode& expr);
   friend LatticeExprNode toDouble  (const LatticeExprNode& expr);
   friend LatticeExprNode toComplex (const LatticeExprNode& expr);
   friend LatticeExprNode toDComplex(const LatticeExprNode& expr);
// </group>

public:

// Default constructor
   LatticeExprNode();

// Unary constant expression constructors.
// <group>
   LatticeExprNode (Int constant);
   LatticeExprNode (Float constant);
   LatticeExprNode (Double constant);
   LatticeExprNode (const Complex& constant);
   LatticeExprNode (const DComplex& constant);
   LatticeExprNode (Bool constant);
// </group>

// Lattice expression (gets Lattice pixels) constructors.
// <group>
   LatticeExprNode (const Lattice<Float>& lattice);
   LatticeExprNode (const Lattice<Double>& lattice);
   LatticeExprNode (const Lattice<Complex>& lattice);
   LatticeExprNode (const Lattice<DComplex>& lattice);
   LatticeExprNode (const Lattice<Bool>& lattice);
   LatticeExprNode (const MaskedLattice<Float>& lattice);
   LatticeExprNode (const MaskedLattice<Double>& lattice);
   LatticeExprNode (const MaskedLattice<Complex>& lattice);
   LatticeExprNode (const MaskedLattice<DComplex>& lattice);
   LatticeExprNode (const MaskedLattice<Bool>& lattice);
// </group>

// Masking operator using a condition.
// The given boolean expression forms a mask for this expression node.
   LatticeExprNode operator[] (const LatticeExprNode& cond) const;

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
//   void eval (Array<Float>& result, const Slicer& section) const;
//   void eval (Array<Double>& result, const Slicer& section) const;
//   void eval (Array<Complex>& result, const Slicer& section) const;
//   void eval (Array<DComplex>& result, const Slicer& section) const;
//   void eval (Array<Bool>& result, const Slicer& section) const;
// </group>

// Evaluate the expression
// <group>
   void eval (LELArray<Float>& result, const Slicer& section) const;
   void eval (LELArray<Double>& result, const Slicer& section) const;
   void eval (LELArray<Complex>& result, const Slicer& section) const;
   void eval (LELArray<DComplex>& result, const Slicer& section) const;
   void eval (LELArray<Bool>& result, const Slicer& section) const;
// </group>

// Evaluate the expression (in case it is a scalar).  The "eval"
// and "get*" functions do the same thing, they just have
// a slightly different interface.
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

// Is the result of "eval" a scalar?
   Bool isScalar() const
      {return pAttr_p->isScalar();}

// Is the result of "eval" masked?
   Bool isMasked() const
      {return pAttr_p->isMasked();}

// Holds the node an invalid scalar?
   Bool isInvalidScalar() const
    {
      if (!donePrepare_p) doPrepare();
      return isInvalid_p;
    }

// Return the shape of the Lattice including all degenerate axes
// (ie. axes with a length of one)
   const IPosition& shape() const
      {return pAttr_p->shape();}

// Get the attribute object of the expression.
   const LELAttribute& getAttribute() const
      {return *pAttr_p;}

// Replace a scalar subexpression by its result.
   Bool replaceScalarExpr();
  
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
// These casting functions are added because the g++ compiler did 
// not use the LatticeExpr constructor for an automatic conversion.
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

// Do the preparation for the evaluation.
   void doPrepare() const;

   
// Member variables.  

   Bool                donePrepare_p;
   DataType            dtype_p;
   Bool                isInvalid_p;
   const LELAttribute* pAttr_p;
   CountedPtr<LELInterface<Float> >    pExprFloat_p;
   CountedPtr<LELInterface<Double> >   pExprDouble_p;
   CountedPtr<LELInterface<Complex> >  pExprComplex_p;
   CountedPtr<LELInterface<DComplex> > pExprDComplex_p;
   CountedPtr<LELInterface<Bool> >     pExprBool_p;
};

#endif
