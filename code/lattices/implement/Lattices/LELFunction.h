//# LELFunction.h:  LELFunction.h
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

#if !defined(AIPS_LELFUNCTION_H)
#define AIPS_LELFUNCTION_H


//# Includes
#include <trial/Lattices/LELInterface.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <trial/Lattices/LELFunctionEnums.h>
#include <aips/Containers/Block.h>

//# Forward Declarations
template <class T> class Array;
class PixelRegion;


// <summary>
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// </synopsis> 

// <example>
// </example>

// <motivation>
// </motivation>

// <todo asof="1996/07/01">
// </todo>


template <class T> class LELFunction1D : public LELInterface<T>
{
public: 
   
// Constructor takes operation and expression to be operated upon
   LELFunction1D(const LELFunctionEnums::Function function,
		 const CountedPtr<LELInterface<T> >& expr);

// Destructor 
  ~LELFunction1D();

// Evaluate the expression and put the result in the array
   virtual void eval (Array<T>& result,
                      const PixelRegion& region) const;

// Get the result of a scalar subexpression.
   virtual T getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;

private:
   LELFunctionEnums::Function   function_p;
   CountedPtr<LELInterface<T> > pExpr_p;
};




// <summary>
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// </synopsis> 

// <example>
// </example>

// <motivation>
// </motivation>

// <todo asof="1996/07/01">
// </todo>


template <class T> class LELFunctionReal1D : public LELInterface<T>
{
public: 
   
// Constructor takes operation and expression to be operated upon
   LELFunctionReal1D(const LELFunctionEnums::Function function,
		     const CountedPtr<LELInterface<T> >& expr);

// Constructor takes operation and expression to be operated upon
   LELFunctionReal1D(const LELFunctionEnums::Function function,
		     const CountedPtr<LELInterface<Bool> >& expr);

// Destructor 
  ~LELFunctionReal1D();

// Evaluate the expression and put the result in the array
   virtual void eval (Array<T>& result,
                      const PixelRegion& region) const;

// Get the result of a scalar subexpression.
   virtual T getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;

private:
   LELFunctionEnums::Function function_p;
   CountedPtr<LELInterface<T> > pExpr_p;
};




// <summary>
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// </synopsis> 

// <example>
// </example>

// <motivation>
// </motivation>

// <todo asof="1996/07/01">
// </todo>


class LELFunctionFloat : public LELInterface<Float>
{
public: 
   
// Constructor takes operation and left and right expressions
// to be operated upon
   LELFunctionFloat(const LELFunctionEnums::Function function,
		    const Block<LatticeExprNode>& expr);

// Destructor 
  ~LELFunctionFloat();

// Evaluate the expression and put the result in the array
   virtual void eval (Array<Float>& result,
                      const PixelRegion& region) const;

// Get the result of a scalar subexpression.
   virtual Float getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;

private:
   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};



// <summary>
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// </synopsis> 

// <example>
// </example>

// <motivation>
// </motivation>

// <todo asof="1996/07/01">
// </todo>


class LELFunctionDouble : public LELInterface<Double>
{
public: 
   
// Constructor takes operation and left and right expressions
// to be operated upon
   LELFunctionDouble(const LELFunctionEnums::Function function,
		     const Block<LatticeExprNode>& expr);

// Destructor 
  ~LELFunctionDouble();

// Evaluate the expression and put the result in the array
   virtual void eval (Array<Double>& result,
                      const PixelRegion& region) const;

// Get the result of a scalar subexpression.
   virtual Double getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;

private:
   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};



// <summary>
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// </synopsis> 

// <example>
// </example>

// <motivation>
// </motivation>

// <todo asof="1996/07/01">
// </todo>


class LELFunctionComplex : public LELInterface<Complex>
{
public: 
   
// Constructor takes operation and left and right expressions
// to be operated upon
   LELFunctionComplex(const LELFunctionEnums::Function function,
		      const Block<LatticeExprNode>& expr);

// Destructor 
  ~LELFunctionComplex();

// Evaluate the expression and put the result in the array
   virtual void eval (Array<Complex>& result,
                      const PixelRegion& region) const;

// Get the result of a scalar subexpression.
   virtual Complex getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;

private:
   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};



// <summary>
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// </synopsis> 

// <example>
// </example>

// <motivation>
// </motivation>

// <todo asof="1996/07/01">
// </todo>


class LELFunctionDComplex : public LELInterface<DComplex>
{
public: 
   
// Constructor takes operation and left and right expressions
// to be operated upon
   LELFunctionDComplex(const LELFunctionEnums::Function function,
		       const Block<LatticeExprNode>& expr);

// Destructor 
  ~LELFunctionDComplex();

// Evaluate the expression and put the result in the array
   virtual void eval (Array<DComplex>& result,
                      const PixelRegion& region) const;

// Get the result of a scalar subexpression.
   virtual DComplex getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;

private:
   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};



// <summary>
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// </synopsis> 

// <example>
// </example>

// <motivation>
// </motivation>

// <todo asof="1996/07/01">
// </todo>


class LELFunctionBool : public LELInterface<Bool>
{
public: 
   
// Constructor takes operation and left and right expressions
// to be operated upon
   LELFunctionBool(const LELFunctionEnums::Function function,
		   const Block<LatticeExprNode>& expr);

// Destructor 
  ~LELFunctionBool();

// Evaluate the expression and put the result in the array
   virtual void eval (Array<Bool>& result,
                      const PixelRegion& region) const;

// Get the result of a scalar subexpression.
   virtual Bool getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;

private:
   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};



#endif
