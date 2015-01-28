//# LatticeExprNode.h:  LatticeExprNode.h
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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

#ifndef LATTICES_LATTICEEXPRNODE_H
#define LATTICES_LATTICEEXPRNODE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELInterface.h>
#include <casacore/lattices/LEL/LELAttribute.h>
#include <casacore/lattices/LEL/LELBinaryEnums.h>
#include <casacore/lattices/LEL/LELUnaryEnums.h>
#include <casacore/lattices/LEL/LELFunctionEnums.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class LatticeExpr;
template <class T> class Lattice;
template <class T> class MaskedLattice;
template <class T> class Array;
template <class T> class Block;
class LCRegion;
class Slicer;
class LattRegionHolder;
class LatticeExprNode;

// Global functions operating on a LatticeExprNode.
// <group name=GlobalLatticeExprNode>
  // Unary functions.
  // <group>
   LatticeExprNode operator+ (const LatticeExprNode& expr);
   LatticeExprNode operator- (const LatticeExprNode& expr);
   LatticeExprNode operator! (const LatticeExprNode& expr);
  // </group>

  // Numerical binary operators
  // <group>
   LatticeExprNode operator+ (const LatticeExprNode& left,
			      const LatticeExprNode& right);
   LatticeExprNode operator- (const LatticeExprNode& left,
			      const LatticeExprNode& right);
   LatticeExprNode operator* (const LatticeExprNode& left,
			      const LatticeExprNode& right);
   LatticeExprNode operator/ (const LatticeExprNode& left,
			      const LatticeExprNode& right);
   LatticeExprNode operator% (const LatticeExprNode& left,
			      const LatticeExprNode& right);
   LatticeExprNode operator^ (const LatticeExprNode& left,
			      const LatticeExprNode& right);
  // </group>

  // Relational binary operators
  // <group>
   LatticeExprNode operator== (const LatticeExprNode& left,
			       const LatticeExprNode& right);
   LatticeExprNode operator>  (const LatticeExprNode& left,
			       const LatticeExprNode& right);
   LatticeExprNode operator>= (const LatticeExprNode& left,
			       const LatticeExprNode& right);
   LatticeExprNode operator<  (const LatticeExprNode& left,
			       const LatticeExprNode& right);
   LatticeExprNode operator<= (const LatticeExprNode& left,
			       const LatticeExprNode& right);
   LatticeExprNode operator!= (const LatticeExprNode& left,
			       const LatticeExprNode& right);
  // </group>

  // Logical binary operators
  // <group>
   LatticeExprNode operator&& (const LatticeExprNode& left,
			       const LatticeExprNode& right);
   LatticeExprNode operator|| (const LatticeExprNode& left,
			       const LatticeExprNode& right);
  // </group>

  // Numerical 1-argument functions
  // <group>
   LatticeExprNode sin  (const LatticeExprNode& expr);
   LatticeExprNode sinh (const LatticeExprNode& expr);
   LatticeExprNode asin (const LatticeExprNode& expr);
   LatticeExprNode cos  (const LatticeExprNode& expr);
   LatticeExprNode cosh (const LatticeExprNode& expr);
   LatticeExprNode acos (const LatticeExprNode& expr);
   LatticeExprNode tan  (const LatticeExprNode& expr);
   LatticeExprNode tanh (const LatticeExprNode& expr);
   LatticeExprNode atan (const LatticeExprNode& expr);
   LatticeExprNode exp  (const LatticeExprNode& expr);
   LatticeExprNode log  (const LatticeExprNode& expr);
   LatticeExprNode log10(const LatticeExprNode& expr);
   LatticeExprNode sqrt (const LatticeExprNode& expr);
   LatticeExprNode sign (const LatticeExprNode& expr);
   LatticeExprNode round(const LatticeExprNode& expr);
   LatticeExprNode ceil (const LatticeExprNode& expr);
   LatticeExprNode floor(const LatticeExprNode& expr);
   LatticeExprNode conj (const LatticeExprNode& expr);
  // </group>

  // Numerical 2-argument functions
  // <group>
   LatticeExprNode atan2 (const LatticeExprNode& left,
			  const LatticeExprNode& right);
   LatticeExprNode pow  (const LatticeExprNode& left,
			 const LatticeExprNode& right);
   LatticeExprNode fmod (const LatticeExprNode& left,
			 const LatticeExprNode& right);
   LatticeExprNode min  (const LatticeExprNode& left,
			 const LatticeExprNode& right);
   LatticeExprNode max  (const LatticeExprNode& left,
			 const LatticeExprNode& right);
  // </group>

  // Form a complex number from two real numbers.
   LatticeExprNode formComplex (const LatticeExprNode& left,
				const LatticeExprNode& right);

  // Numerical 1-argument functions which result in a real number
  // regardless of input expression type
  // <group>
   LatticeExprNode abs  (const LatticeExprNode& expr);
   LatticeExprNode arg  (const LatticeExprNode& expr);
   LatticeExprNode real (const LatticeExprNode& expr);
   LatticeExprNode imag (const LatticeExprNode& expr);
  // </group>

  // 1-argument functions operating on a numeric expression resulting 
  // in a scalar
  // <group>
   LatticeExprNode min      (const LatticeExprNode& expr);
   LatticeExprNode max      (const LatticeExprNode& expr);
   LatticeExprNode sum      (const LatticeExprNode& expr);
   LatticeExprNode median   (const LatticeExprNode& expr);
   LatticeExprNode mean     (const LatticeExprNode& expr);
   LatticeExprNode variance (const LatticeExprNode& expr);
   LatticeExprNode stddev   (const LatticeExprNode& expr);
   LatticeExprNode avdev    (const LatticeExprNode& expr);
  // </group>

  // Determine the value of the element at the part <src>fraction</src>
  // from the beginning of the given lattice.
  // Thus <src>fraction=0.5</src> is equal to the median.
   LatticeExprNode fractile (const LatticeExprNode& expr,
			     const LatticeExprNode& fraction);

  // Determine the value range of the elements at the part <src>fraction1</src>
  // and fraction2 from the beginning of the given lattice. Both fractions
  // must be >=0 and <=1 and fraction1 must be <= fraction2.
  // By default <src>fraction2</src> is equal to <src>1-fraction1</src>.
  // Thus <src>fraction=0.25</src> gives the quartile range of the lattice.
  // <group>
   LatticeExprNode fractileRange (const LatticeExprNode& expr,
				  const LatticeExprNode& fraction1,
				  const LatticeExprNode& fraction2);
   LatticeExprNode fractileRange (const LatticeExprNode& expr,
				  const LatticeExprNode& fraction);
  // </group>

  // 1-argument function to get the number of elements in a lattice.
  // If the lattice is masked, only the True elements are counted.
  // Results in a scalar Double.
   LatticeExprNode nelements (const LatticeExprNode& expr);

  // 1-argument function to get the dimensionality of a lattice.
  // 0 is returned if it is a scalar.
  // Results in a scalar Float.
   LatticeExprNode ndim (const LatticeExprNode& expr);

  // 2-argument function to get the length of an axis.
  // Results in a scalar Float.
  // The 2nd expression (giving the axis number) has to be a real scalar.
  // <note role=caution>
  // Axes start counting at 0.
  // If the axis is a number < 0, an exception is thrown.
  // If the axis is a number exceeding the dimensionality, 1 is returned.
  // </note>
   LatticeExprNode length (const LatticeExprNode& expr,
			   const LatticeExprNode& axis);

  // 2-argument function telling per pixel if its index on the given axis
  // is contained in the 2nd argument. The 2nd argument should be a boolean
  // vector where True means that the index is contained.
  // For indices >= vector_length, the 2nd argument defaults to False.
  // Results in a Bool array.
  // <note role=caution>
  // Axes start counting at 0.
  // If the axis is a number < 0 or >= ndim, an exception is thrown.
  // </note>
   LatticeExprNode indexin (const LatticeExprNode& axis,
			    const LatticeExprNode& indexFlags);

  // 2-argument function rebinning Lattice by given factors. The 2nd argument
  // should be a vector (preferably Float - really Int but Int not well
  // supported in LEL yet).  Results in a T array.
   LatticeExprNode rebin (const LatticeExprNode& expr,
			  const LatticeExprNode& bin);

// Test if a value is a NaN.
   LatticeExprNode isNaN (const LatticeExprNode& expr);

  // Functions operating on a logical expression resulting in a scalar;
  // Functions "any" (are any pixels "True") and "all" (are all pixels
  // "True") result in a Bool; functions "ntrue" and "nfalse" result 
  // in a Double.
  // <group>
   LatticeExprNode any   (const LatticeExprNode& expr);
   LatticeExprNode all   (const LatticeExprNode& expr);
   LatticeExprNode ntrue (const LatticeExprNode& expr);
   LatticeExprNode nfalse(const LatticeExprNode& expr);
  // </group>

  // This function returns the mask of the given expression.
  // If it has no mask, the result is an array with all True values.
   LatticeExprNode mask (const LatticeExprNode& expr);

  // This function returns the value of the expression without a mask.
   LatticeExprNode value (const LatticeExprNode& expr);

  // This function finds <src>sqrt(left^2+right^2)</src>.  This
  // could be used to find the (biased) polarized intensity if
  // left and right are images of Stokes Q and U.
   LatticeExprNode amp (const LatticeExprNode& left,
			const LatticeExprNode& right);

  // This function finds <src>180/pi*atan2(left,right)/2</src>.  This could be 
  // used to find the position of linear polarization if left 
  // and right are images of Stokes U and Q, respectively.
   LatticeExprNode pa (const LatticeExprNode& left,
		       const LatticeExprNode& right);

  // This function finds the spectral index
  // <src>alpha = log(s1/s2) / log(f1/f2)</src>.
   LatticeExprNode spectralindex (const LatticeExprNode& left,
				  const LatticeExprNode& right);

  // Function resembling the ternary <src>?:</src> construct in C++.
  // The argument "condition" has to be a Bool scalar or lattice.
  // If an element in "condition" is True, the corresponding element from
  // "arg1" is taken, otherwise it is taken from "arg2".
   LatticeExprNode iif (const LatticeExprNode& condition,
			const LatticeExprNode& arg1,
			const LatticeExprNode& arg2);

  // This function replaces every masked-off element in the first argument
  // with the corresponding element from the second argument.
  // The first argument has to be a lattice (expression), the second can
  // be a scalar or lattice. The mask of the first argument is not changed.
  // If the first argument does not have a mask, this function does nothing.
   LatticeExprNode replace (const LatticeExprNode& arg1,
			    const LatticeExprNode& arg2);

  // Functions to convert to the given data type.  These are mostly 
  // meaningful for down-conversions (e.g. double to float),
  // since up-conversions are automatically done to get matching data types
  // when needed.  Note that some conversions are not supported, such
  // as Complex to Double or Float.
  // <br>The conversion to Bool is useful to convert a region to a
  // boolean lattice, which is only possible if the region is given
  // in world coordinates. Otherwise an exception is thrown.
  // <group>
   LatticeExprNode toFloat   (const LatticeExprNode& expr);
   LatticeExprNode toDouble  (const LatticeExprNode& expr);
   LatticeExprNode toComplex (const LatticeExprNode& expr);
   LatticeExprNode toDComplex(const LatticeExprNode& expr);
   LatticeExprNode toBool    (const LatticeExprNode& expr);
   LatticeExprNode convertType (const LatticeExprNode& expr, const Float*);
   LatticeExprNode convertType (const LatticeExprNode& expr, const Double*);
   LatticeExprNode convertType (const LatticeExprNode& expr, const Complex*);
   LatticeExprNode convertType (const LatticeExprNode& expr, const DComplex*);
   LatticeExprNode convertType (const LatticeExprNode& expr, const Bool*);
  // </group>
// </group>



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
//    be found in
//    <a href="../notes/216.html">Note 216</a>
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
//  object formed from summing exp1 and exp2 is automatically converted
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
// All global functions need to be declared as friends.
// <group>
   friend LatticeExprNode operator+ (const LatticeExprNode& expr);
   friend LatticeExprNode operator- (const LatticeExprNode& expr);
   friend LatticeExprNode operator! (const LatticeExprNode& expr);
   friend LatticeExprNode operator+ (const LatticeExprNode& left,
				     const LatticeExprNode& right);
   friend LatticeExprNode operator- (const LatticeExprNode& left,
				     const LatticeExprNode& right);
   friend LatticeExprNode operator* (const LatticeExprNode& left,
                                     const LatticeExprNode& right);
   friend LatticeExprNode operator/ (const LatticeExprNode& left,
                                     const LatticeExprNode& right);
   friend LatticeExprNode operator% (const LatticeExprNode& left,
                                     const LatticeExprNode& right);
   friend LatticeExprNode operator^ (const LatticeExprNode& left,
                                     const LatticeExprNode& right);
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
   friend LatticeExprNode operator&& (const LatticeExprNode& left,
				      const LatticeExprNode& right);
   friend LatticeExprNode operator|| (const LatticeExprNode& left,
				      const LatticeExprNode& right);
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
   friend LatticeExprNode sign (const LatticeExprNode& expr);
   friend LatticeExprNode round(const LatticeExprNode& expr);
   friend LatticeExprNode ceil (const LatticeExprNode& expr);
   friend LatticeExprNode floor(const LatticeExprNode& expr);
   friend LatticeExprNode conj (const LatticeExprNode& expr);
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
   friend LatticeExprNode formComplex (const LatticeExprNode& left,
				       const LatticeExprNode& right);
   friend LatticeExprNode abs  (const LatticeExprNode& expr);
   friend LatticeExprNode arg  (const LatticeExprNode& expr);
   friend LatticeExprNode real (const LatticeExprNode& expr);
   friend LatticeExprNode imag (const LatticeExprNode& expr);
   friend LatticeExprNode min      (const LatticeExprNode& expr);
   friend LatticeExprNode max      (const LatticeExprNode& expr);
   friend LatticeExprNode sum      (const LatticeExprNode& expr);
   friend LatticeExprNode median   (const LatticeExprNode& expr);
   friend LatticeExprNode mean     (const LatticeExprNode& expr);
   friend LatticeExprNode variance (const LatticeExprNode& expr);
   friend LatticeExprNode stddev   (const LatticeExprNode& expr);
   friend LatticeExprNode avdev    (const LatticeExprNode& expr);
   friend LatticeExprNode fractile (const LatticeExprNode& expr,
				    const LatticeExprNode& fraction);
   friend LatticeExprNode fractileRange (const LatticeExprNode& expr,
					 const LatticeExprNode& fraction1,
					 const LatticeExprNode& fraction2);
   friend LatticeExprNode fractileRange (const LatticeExprNode& expr,
					 const LatticeExprNode& fraction);
   friend LatticeExprNode nelements (const LatticeExprNode& expr);
   friend LatticeExprNode ndim (const LatticeExprNode& expr);
   friend LatticeExprNode length (const LatticeExprNode& expr,
				  const LatticeExprNode& axis);
   friend LatticeExprNode indexin (const LatticeExprNode& axis,
				   const LatticeExprNode& indexFlags);
   friend LatticeExprNode rebin (const LatticeExprNode& expr,
                                 const LatticeExprNode& bin);
   friend LatticeExprNode isNaN (const LatticeExprNode& expr);
   friend LatticeExprNode any   (const LatticeExprNode& expr);
   friend LatticeExprNode all   (const LatticeExprNode& expr);
   friend LatticeExprNode ntrue (const LatticeExprNode& expr);
   friend LatticeExprNode nfalse(const LatticeExprNode& expr);
   friend LatticeExprNode mask (const LatticeExprNode& expr);
   friend LatticeExprNode value (const LatticeExprNode& expr);
   friend LatticeExprNode amp (const LatticeExprNode& left,
                               const LatticeExprNode& right);
   friend LatticeExprNode pa (const LatticeExprNode& left,
                              const LatticeExprNode& right);
   friend LatticeExprNode spectralindex (const LatticeExprNode& left,
					 const LatticeExprNode& right);
   friend LatticeExprNode iif (const LatticeExprNode& condition,
			       const LatticeExprNode& arg1,
			       const LatticeExprNode& arg2);
   friend LatticeExprNode replace (const LatticeExprNode& arg1,
			           const LatticeExprNode& arg2);
   friend LatticeExprNode toFloat   (const LatticeExprNode& expr);
   friend LatticeExprNode toDouble  (const LatticeExprNode& expr);
   friend LatticeExprNode toComplex (const LatticeExprNode& expr);
   friend LatticeExprNode toDComplex(const LatticeExprNode& expr);
   friend LatticeExprNode toBool    (const LatticeExprNode& expr);
// </group>

public:

// Default constructor
   LatticeExprNode();

// Unary constant expression constructors.
// <group>
   LatticeExprNode (Int64 constant);
   LatticeExprNode (Int constant);
   LatticeExprNode (uInt constant);
   LatticeExprNode (Long constant);
   LatticeExprNode (Float constant);
   LatticeExprNode (Double constant);
   LatticeExprNode (const Complex& constant);
   LatticeExprNode (const DComplex& constant);
   LatticeExprNode (Bool constant);
// </group>

// Constructor from an IPosition (containing indices or axes).
   LatticeExprNode (const IPosition&);

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

// Create a lattice expression from a region.
// It results in a boolean expression node.
// <group>
   LatticeExprNode (const LCRegion& region);
   LatticeExprNode (const Slicer& slicer);
   LatticeExprNode (const LattRegionHolder& region);
// </group>

// Masking operator using a condition.
// The given boolean expression forms a mask/region for this expression node.
   LatticeExprNode operator[] (const LatticeExprNode& cond) const;

// Copy constructor (reference semantics)
   LatticeExprNode (const LatticeExprNode& other);

// Destructor, does nothing
   virtual ~LatticeExprNode();

// Assignment (reference semantics)
   LatticeExprNode& operator= (const LatticeExprNode& other);

// Get the IPosition.
// It throws an exception if the node does not contain an IPosition.
   const IPosition& getIPosition() const;

// Convert the expression to another data type.
// <group>
   CountedPtr<LELInterface<Float> >    makeFloat() const;
   CountedPtr<LELInterface<Double> >   makeDouble() const;
   CountedPtr<LELInterface<Complex> >  makeComplex() const;
   CountedPtr<LELInterface<DComplex> > makeDComplex() const;
   CountedPtr<LELInterface<Bool> >     makeBool() const;
// </group>

// Evaluate the expression.
// One can be sure that the result is not a reference to another array.
// This function should be used by LatticeExpr and other users.
// <group>
   void eval (LELArray<Float>& result, const Slicer& section) const;
   void eval (LELArray<Double>& result, const Slicer& section) const;
   void eval (LELArray<Complex>& result, const Slicer& section) const;
   void eval (LELArray<DComplex>& result, const Slicer& section) const;
   void eval (LELArray<Bool>& result, const Slicer& section) const;
// </group>

// Evaluate the expression.
// The result can be a reference to some internal array (in particular
// to an array in an ArrayLattice object used as a lattice).
// This function is meant for internal use by the LEL classes and
// should not be used externally.
// <group>
   void evalRef (LELArrayRef<Float>& result, const Slicer& section) const
    { pExprFloat_p->evalRef (result, section); }
   void evalRef (LELArrayRef<Double>& result, const Slicer& section) const
    { pExprDouble_p->evalRef (result, section); }
   void evalRef (LELArrayRef<Complex>& result, const Slicer& section) const
    { pExprComplex_p->evalRef (result, section); }
   void evalRef (LELArrayRef<DComplex>& result, const Slicer& section) const
    { pExprDComplex_p->evalRef (result, section); }
   void evalRef (LELArrayRef<Bool>& result, const Slicer& section) const
    { pExprBool_p->evalRef (result, section); }
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

// Evaluate the expression (in case it is a constant array).
// <group>
   Array<Float> getArrayFloat() const;
   Array<Double> getArrayDouble() const;
   Array<Complex> getArrayComplex() const;
   Array<DComplex> getArrayDComplex() const;
   Array<Bool> getArrayBool() const;
// </group>

// Get the data type of the expression.
   DataType dataType() const
      {return dtype_p;}

// Is the expression node a region?
   Bool isRegion() const
      {return pAttr_p->isRegion();}

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

// Determine the resulting data type from the given data types.
// An exception is thrown if they are incompatible.
   static DataType resultDataType (DataType left, DataType right);

// Check the arguments of a function and return the resulting attribute object.
// The matchAxes argument tells if the axes have to match exactly or
// whether it is possible that one expression is a subset of another
// (i.e. that axes may be missing).
// <br>The expectArray argument tells if the result should be an array
// which is the case if one of the arguments is an array.
   static LELAttribute checkArg (const Block<LatticeExprNode>& arg,
				 const Block<Int>& argType,
				 Bool expectArray,
				 Bool matchAxes = True);

  // Handle locking of the LatticeExpr which is delegated to all of its parts.
  // <group>
  Bool lock (FileLocker::LockType, uInt nattempts);
  void unlock();
  Bool hasLock (FileLocker::LockType) const;
  void resync();
  // </group>


private:
// Make the object from a LELInterface* pointer.
// <group>
   LatticeExprNode(LELInterface<Float>* expr);
   LatticeExprNode(LELInterface<Double>* expr);
   LatticeExprNode(LELInterface<Complex>* expr);
   LatticeExprNode(LELInterface<DComplex>* expr);
   LatticeExprNode(LELInterface<Bool>* expr);
// </group>

// Test if both operands represent a region.
// An exception is thrown if only one of them is a region.
   static Bool areRegions (const LatticeExprNode& left,
			   const LatticeExprNode& right);

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

// Create a new node for a complex numerical function with 1 argument.
// The result has the same data type as the input.
   static LatticeExprNode newComplexFunc1D (LELFunctionEnums::Function func,
                                            const LatticeExprNode& expr);

// Create a new node for a numerical function with 1 argument that 
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

// Make (if needed and if possible) the expression nodes such that
// the dimensionalities are equal. This is only possible if both
// nodes have a coordinate system.
// It is done by creating an ExtendLattice object for the node
// with the lower dimensionality.
   static Int makeEqualDim (LatticeExprNode& expr0,
			    LatticeExprNode& expr1);

// Do the preparation for the evaluation.
   void doPrepare() const;

   
// Member variables.  

   Bool                donePrepare_p;
   DataType            dtype_p;
   Bool                isInvalid_p;
   IPosition           iposition_p;
   const LELAttribute* pAttr_p;
   CountedPtr<LELInterface<Float> >    pExprFloat_p;
   CountedPtr<LELInterface<Double> >   pExprDouble_p;
   CountedPtr<LELInterface<Complex> >  pExprComplex_p;
   CountedPtr<LELInterface<DComplex> > pExprDComplex_p;
   CountedPtr<LELInterface<Bool> >     pExprBool_p;
};



inline LatticeExprNode operator% (const LatticeExprNode& left,
				  const LatticeExprNode& right)
  { return fmod (left, right); }
inline LatticeExprNode operator^ (const LatticeExprNode& left,
				  const LatticeExprNode& right)
  { return pow (left, right); }

inline LatticeExprNode convertType(const LatticeExprNode& expr, const Float*)
  { return toFloat (expr); }
inline LatticeExprNode convertType(const LatticeExprNode& expr, const Double*)
  { return toDouble (expr); }
inline LatticeExprNode convertType(const LatticeExprNode& expr, const Complex*)
  { return toComplex (expr); }
inline LatticeExprNode convertType(const LatticeExprNode& expr, const DComplex*)
  { return toDComplex (expr); }
inline LatticeExprNode convertType(const LatticeExprNode& expr, const Bool*)
  { return toBool (expr); }

} //# NAMESPACE CASACORE - END

#endif
