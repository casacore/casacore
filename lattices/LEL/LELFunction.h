//# LELFunction.h:  LELFunction.h
//# Copyright (C) 1997,1998,1999,2000,2001
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

#ifndef LATTICES_LELFUNCTION_H
#define LATTICES_LELFUNCTION_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELInterface.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/LEL/LELFunctionEnums.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// This LEL class handles numerical (real and complex) 1-argument functions
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
//   <li> <linkto class="LELFunctionEnums"> LELFunctionEnums</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles numerical (real and complex) 
//  1-argument functions
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It is used to construct 
// LEL objects that apply numerical 1-argument functions to Lattice 
// expressions. They operate on numerical (Float,Double,Complex,DComplex) 
// Lattice expressions and return the same type. The available C++ functions are 
// <src>sin,sinh,cos,cosh,exp,log,log10,sqrt,min,max,mean,sum</src> with 
// equivalents in the enum of SIN,SINH,COS,COSH,EXP,LOG,LOG10,SQRT,MIN1D,MAX1D,
// MEAN1D, and SUM.
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Complex> x(shape); x.set(1.0);
// ArrayLattice<Complex> y(shape); 
// y.copyData(sin(x));                 // y = sin(x)
// y.copyData(min(x));                 // y = min(x)
// </srcblock>
// Note that the min function returns a scalar, and the output
// Lattice is filled with that one value.
// </example>
//
// <motivation>
// Numerical functions are a basic mathematical expression. 
// </motivation>
//
// <todo asof="1998/01/21">
// </todo>


template <class T> class LELFunction1D : public LELInterface<T>
{
  //# Make members of parent class known.
protected:
  using LELInterface<T>::setAttr;

public: 
// Constructor takes operation and expression to be operated upon
   LELFunction1D(const LELFunctionEnums::Function function,
		 const CountedPtr<LELInterface<T> >& expr);

// Destructor 
  ~LELFunction1D();

// Recursively evaluate the expression 
   virtual void eval (LELArray<T>& result,
                      const Slicer& section) const;

// Recursively evaluate the scalar expression.
   virtual LELScalar<T> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual Bool prepareScalarExpr();

// Get class name
   virtual String className() const;

  // Handle locking/syncing of a lattice in a lattice expression.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  // </group>

private:
   LELFunctionEnums::Function   function_p;
   CountedPtr<LELInterface<T> > pExpr_p;
};




// <summary>
// This LEL class handles numerical (real only) 1-argument functions
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
//   <li> <linkto class="LELFunctionEnums"> LELFunctionEnums</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles numerical (real only) 
//  1-argument functions
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It is used to construct 
// LEL objects that apply numerical (real only)  1-argument functions to 
// Lattice expressions. They operate on Float and Double numerical Lattice 
// expressions and return the same type. The available C++ functions are 
// <src>asin,acos,tan,tanh,ceil,floor</src> with 
// equivalents in the enum of ASIN, ACOS, TAN, TANH, CEIL, and FLOOR.
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Float> x(shape); x.set(0.05);
// ArrayLattice<Float> y(shape); 
// y.copyData(asin(x));                 // y = asin(x)
// y.copyData(tan(x));                  // y = tan(x)
// </srcblock>
// Note that the min function returns a scalar, and the output
// Lattice is filled with that one value.
// </example>
//
// <motivation>
// Numerical functions are a basic mathematical expression. 
// </motivation>
//
// <todo asof="1998/01/21">
// </todo>


template <class T> class LELFunctionReal1D : public LELInterface<T>
{
  //# Make members of parent class known.
protected:
  using LELInterface<T>::setAttr;

public: 
// Constructor takes operation and expression to be operated upon
   LELFunctionReal1D(const LELFunctionEnums::Function function,
		     const CountedPtr<LELInterface<T> >& expr);

// Destructor 
  ~LELFunctionReal1D();

// Recursively evaluate the expression 
   virtual void eval (LELArray<T>& result,
                      const Slicer& section) const;

// Recursively evaluate the scalar expression 
   virtual LELScalar<T> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual Bool prepareScalarExpr();

// Get class name
   virtual String className() const;

// Handle locking/syncing of a lattice in a lattice expression.
   // <group>
   virtual Bool lock (FileLocker::LockType, uInt nattempts);
   virtual void unlock();
   virtual Bool hasLock (FileLocker::LockType) const;
   virtual void resync();
   // </group>


private:
   LELFunctionEnums::Function function_p;
   CountedPtr<LELInterface<T> > pExpr_p;
};




// <summary>
// This LEL class handles functions with a variable number of arguments.
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
//   <li> <linkto class="LELFunctionEnums"> LELFunctionEnums</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles numerical functions (arbitrary
//  number of arguments) which return any data type
// </etymology>
//
// <synopsis>
// This templated LEL letter class is derived from LELInterface.
// It is used to construct LEL objects that apply functions of
// arbitrary number of arguments to Lattice expressions.
// They operate lattices with any type and return the same type.
// The available C++ function is 
// <src>iif</src> with equivalents in the enum of IIF.
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Complex> w(shape); w.set(Complex(2.0,3.0));
// ArrayLattice<Float> x(shape); x.set(0.05);
// ArrayLattice<Float> y(shape); y.set(2.0);
// ArrayLattice<Float> z(shape); y.set(2.0);
//
// z.copyData(iif(x==0, y, x));
//
// </srcblock>
// Copy x to z, but where x==0, take the correpsonding element from y.
// </example>b
//
// <motivation>
// An "if-then-else" like construction is very useful.
// </motivation>
//
// <todo asof="1998/01/21">
// </todo>


template<class T> class LELFunctionND : public LELInterface<T>
{
  //# Make members of parent class known.
protected:
  using LELInterface<T>::setAttr;

public: 
// Constructor takes operation and expressions to be operated upon
   LELFunctionND(const LELFunctionEnums::Function function,
		 const Block<LatticeExprNode>& expr);

// Destructor 
  ~LELFunctionND();

// Recursively evaluate the expression 
   virtual void eval (LELArray<T>& result,
                      const Slicer& section) const;

// Recursively evaluate the scalar expression 
   virtual LELScalar<T> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual Bool prepareScalarExpr();

// Get class name
   virtual String className() const;

  // Handle locking/syncing of a lattice in a lattice expression.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  // </group>

private:
   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};




// <summary>
// This LEL class handles numerical functions whose return type is a Float
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
//   <li> <linkto class="LELFunctionEnums"> LELFunctionEnums</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles numerical functions (arbitrary
//  number of arguments) which return a Float
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It is used to construct 
// LEL objects that apply numerical functions of arbitrary number of
// arguments (but only 1 or 2 arguments currently implemented) to Lattice 
// expressions. They operate on Float or Complex Lattices 
// and return a Float. The available C++ functions are 
// <src>min,max,pow,atan2,fmod,abs,arg,real,imag</src> with 
// equivalents in the enum of MIN,MAX,POW,ATAN2,FMOD,ABS,ARG,REAL, and IMAG.
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Complex> w(shape); w.set(Complex(2.0,3.0));
// ArrayLattice<Float> x(shape); x.set(0.05);
// ArrayLattice<Float> y(shape); y.set(2.0);
// ArrayLattice<Float> z(shape); y.set(2.0);
//
// z.copyData(min(x,y));                // z = min(x,y)
// z.copyData(imag(w));                 // z = imag(w)
//
// </srcblock>
// Note that this min function takes two arguments and returns
// the minimum of the two, pixel by pixel (i.e. it does not
// return one scalar from the whole Lattice)
// </example>b
//
// <motivation>
// Numerical functions are a basic mathematical expression. 
// </motivation>
//
// <todo asof="1998/01/21">
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

// Recursively evaluate the expression 
   virtual void eval (LELArray<Float>& result,
                      const Slicer& section) const;

// Recursively evaluate the scalar expression 
   virtual LELScalar<Float> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual Bool prepareScalarExpr();

// Get class name
   virtual String className() const;

  // Handle locking/syncing of a lattice in a lattice expression.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  // </group>

private:
   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};



// <summary>
// This LEL class handles numerical functions whose return type is a Double
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
//   <li> <linkto class="LELFunctionEnums"> LELFunctionEnums</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles numerical functions (arbitrary
//  number of arguments) which return a Double
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It is used to construct 
// LEL objects that apply numerical functions of arbitrary number of
// arguments (but only 1 or 2 arguments currently implemented) to Lattice 
// expressions. They operate on Double or DComplex Lattices 
// and return a Double. The available C++ functions are 
// <src>min,max,pow,atan2,fmod,abs,arg,real,imag</src> with 
// equivalents in the enum of MIN,MAX,POW,ATAN2,FMOD,ABS,ARG,REAL, and IMAG.
//
// There are also two other functions for which the input Lattice expression
// type must be a Bool.  These are <src>ntrue,nfalse</src> with 
// equivalents in the enum of NTRUE and NFALSE.
//
// There is a further function for which the input Lattice expression
// type can be anything.  This is <src>nelements</src> with 
// equivalent in the enum of NELEM.
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Bool> v(shape); v.set(True);
// ArrayLattice<DComplex> w(shape); w.set(DComplex(2.0,3.0));
// ArrayLattice<Double> x(shape); x.set(0.05);
// ArrayLattice<Double> y(shape); y.set(2.0);
// ArrayLattice<Double> z(shape); y.set(2.0);
//
// z.copyData(min(x,y));                // z = min(x,y)
// z.copyData(imag(w));                 // z = imag(w)
// z.copyData(nelements(v));            // z = nelements(v)
// z.copyData(ntrue(v));                // z = ntrue(v)
// </srcblock>
// </example>
//
// <motivation>
// Numerical functions are a basic mathematical expression.
// </motivation>
//
// <todo asof="1998/01/21">
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

// Recursively evaluate the expression 
   virtual void eval (LELArray<Double>& result,
                      const Slicer& section) const;

// Recursively evaluate the scalar expression 
   virtual LELScalar<Double> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual Bool prepareScalarExpr();

// Get class name
   virtual String className() const;

  // Handle locking/syncing of a lattice in a lattice expression.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  // </group>

private:
   // Count number of masked elements in a LatticeExprNode.
   // <group>
   uInt nMaskedElements (const LatticeExprNode&) const;
   uInt nMaskedOn (const Array<Bool>& mask) const;
   // </group>

   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};



// <summary>
// This LEL class handles complex numerical functions
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
//   <li> <linkto class="LELFunctionEnums"> LELFunctionEnums</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles complex numerical functions (arbitrary
//  number of arguments) 
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It is used to construct 
// LEL objects that apply complex numerical functions of arbitrary number of
// arguments (but only 1 or 2 arguments currently implemented) to Lattice 
// expressions. They operate on Complex Lattice expressions only
// and return a Complex. The available C++ functions are 
// <src>pow,conj</src> with equivalents in the enum of POW and CONJ.
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Complex> x(shape); x.set(Complex(2.0,3.0));
// ArrayLattice<Complex> y(shape); 
// y.copyData(conj(x));                // y = conj(x)
// </srcblock>
// </example>
//
// <motivation>
// Numerical functions are a basic mathematical expression. 
// </motivation>
//
// <todo asof="1998/01/21">
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

// Recursively evaluate the expression 
   virtual void eval (LELArray<Complex>& result,
                      const Slicer& section) const;

// Recursively evaluate the scalar expression 
   virtual LELScalar<Complex> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual Bool prepareScalarExpr();

// Get class name
   virtual String className() const;

  // Handle locking/syncing of a lattice in a lattice expression.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  // </group>

private:
   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};





// <summary>
// This LEL class handles double complex numerical functions
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
//   <li> <linkto class="LELFunctionEnums"> LELFunctionEnums</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles double complex numerical functions (arbitrary
//  number of arguments) 
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It is used to construct 
// LEL objects that apply double complex numerical functions of arbitrary number of
// arguments (but only 1 or 2 arguments currently implemented) to Lattice 
// expressions. They operate on DComplex Lattice expressions only
// and return a DComplex. The available C++ functions are 
// <src>pow,conj</src> with equivalents in the enum of POW and CONJ.
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<DComplex> x(shape); x.set(DComplex(2.0,3.0));
// ArrayLattice<DComplex> y(shape); 
// y.copyData(conj(x));                // y = conj(x)
// </srcblock>
// </example>
//
// <motivation>
// Numerical functions are a basic mathematical expression. 
// </motivation>
//
// <todo asof="1998/01/21">
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

// Recursively evaluate the expression 
   virtual void eval (LELArray<DComplex>& result,
                      const Slicer& section) const;

// Recursively evaluate the scalar expression 
   virtual LELScalar<DComplex> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual Bool prepareScalarExpr();

// Get class name
   virtual String className() const;

  // Handle locking/syncing of a lattice in a lattice expression.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  // </group>

private:
   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};


// <summary>
// This LEL class handles logical functions
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
//   <li> <linkto class="LELFunctionEnums"> LELFunctionEnums</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles logical functions (arbitrary
//  number of arguments) 
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It is used to construct 
// LEL objects that apply logical functions of arbitrary number of
// arguments (but only 1 or 2 arguments currently implemented) to Lattice 
// expressions. They operate on Bool Lattice expressions only
// and return a Bool. The available C++ functions are 
// <src>all,any</src> with equivalents in the enum of ALL and ANY.
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Bool> x(shape); x.set(True);
// ArrayLattice<Bool> y(shape); 
// y.copyData(any(x));                // y = any(x)
// </srcblock>
// The result of the any function (were any of the values True) is 
// a Bool scalar. So the output Lattice is filled with that one value.
// </example>
//
// <motivation>
// Logical functions are a basic mathematical expression. 
// </motivation>
//
// <todo asof="1998/01/21">
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

// Recursively evaluate the expression 
   virtual void eval (LELArray<Bool>& result,
                      const Slicer& section) const;

// Recursively evaluate the scalar expression 
   virtual LELScalar<Bool> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual Bool prepareScalarExpr();

// Get class name
   virtual String className() const;

  // Handle locking/syncing of a lattice in a lattice expression.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  // </group>

private:
   LELFunctionEnums::Function function_p;
   Block<LatticeExprNode> arg_p;
};




} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LEL/LELFunction.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
