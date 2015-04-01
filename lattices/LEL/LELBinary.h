//# LELBinary.h:  LELBinary.h
//# Copyright (C) 1997,1998,1999,2000
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

#ifndef LATTICES_LELBINARY_H
#define LATTICES_LELBINARY_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELInterface.h>
#include <casacore/lattices/LEL/LELBinaryEnums.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary> This LEL class handles numerical binary operators </summary>
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
//   <li> <linkto class="LELBinaryEnums"> LELBinaryEnums</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles numerical binary 
//  operators 
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It
// is used to construct LEL objects that apply numerical binary
// operators to Lattice expressions.  They operate on numerical
// Lattice (Float,Double,Complex,DComplex) expressions and return the 
// same numerical type. The available C++ operators  
// are  <src>+,-,*,/</src> with  equivalents in the enum 
// of ADD, SUBTRACT, MULTIPLY, and DIVIDE.
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
//
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Float> x(shape); x.set(1.0);
// ArrayLattice<Float> y(shape); y.set(2.0);
// ArrayLattice<Float> z(shape); 
// z.copyData(x+y);                 // z = x + y;
// z.copyData(x-y);                 // z = x - y;
// z.copyData(x*y);                 // z = x * y;
// z.copyData(x/y);                 // z = x / y;
// </srcblock>
// </example>
//
// <motivation>
// Numerical binary operations are a basic mathematical expression. 
// </motivation>
//
// <todo asof="1998/01/20">
// </todo>
 

template <class T> class LELBinary : public LELInterface<T>
{
  //# Make members of parent class known.
protected:
  using LELInterface<T>::setAttr;

public: 
// Constructor takes operation and left and right expressions
// to be operated upon
   LELBinary(const LELBinaryEnums::Operation op, 
	     const CountedPtr<LELInterface<T> >& pLeftExpr,
	     const CountedPtr<LELInterface<T> >& pRightExpr);

// Destructor 
  ~LELBinary();

// Recursively evaluate the expression 
   virtual void eval (LELArray<T>& result,
                      const Slicer& section) const;

// Recursively efvaluate the scalar expression 
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
   LELBinaryEnums::Operation op_p;
   CountedPtr<LELInterface<T> > pLeftExpr_p;
   CountedPtr<LELInterface<T> > pRightExpr_p;
};




// <summary> This LEL class handles relational binary numerical operators </summary>
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
//   <li> <linkto class="LELBinaryEnums"> LELBinaryEnums</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles relational numerical binary 
//  operators 
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It
// is used to construct LEL objects that apply relational numerical 
// binary operators to Lattice expressions.  They operate on numerical
// (Float,Double,Complex,DComplex) Lattice expressions and result 
// in a Bool.  The available C++ operators are  
// <src>==,!=>,>=,<,<=,</src> with equivalents in the enum of 
// EQ, NE, GT, GE, LT, and LE
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
//
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Float> x(shape); x.set(1.0);
// ArrayLattice<Float> y(shape); y.set(2.0);
// ArrayLattice<Bool> z(shape); 
// z.copyData(x==y);                // z = x == y;
// z.copyData(x!=y);                // z = x != y;
// z.copyData(x>y);                 // z = x > y;
// z.copyData(x>=y);                // z = x >= y;
// z.copyData(x<y);                 // z = x < y;
// z.copyData(x<=y);                // z = x <= y;
// </srcblock>
// </example>
//
// <motivation>
// Numerical relational binary operations are a basic mathematical expression. 
// </motivation>
//
// <todo asof="1998/01/20">
// </todo>
 

template<class T> class LELBinaryCmp : public LELInterface<Bool>
{
public: 
   
// Constructor takes operation and left and right expressions
// to be operated upon. It can only handle the comparison operators.
   LELBinaryCmp(const LELBinaryEnums::Operation op, 
		const CountedPtr<LELInterface<T> >& pLeftExpr,
		const CountedPtr<LELInterface<T> >& pRightExpr);

// Destructor 
  ~LELBinaryCmp();

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
   LELBinaryEnums::Operation op_p;
   CountedPtr<LELInterface<T> > pLeftExpr_p;
   CountedPtr<LELInterface<T> > pRightExpr_p;
};




// <summary> This LEL class handles logical binary operators </summary>
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
//   <li> <linkto class="LELBinaryEnums"> LELBinaryEnums</linkto>
// </prerequisite>

// <etymology>
//  This derived LEL letter class handles logical binary operators 
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It
// is used to construct LEL objects that apply logical 
// binary operators to Lattice expressions.  They apply only
// to Bool Lattice expressions and result in a Bool.  The 
// available C++ operators are  <src>&&,||,==,!=</src> with 
// equivalents in the enum of  AND, OR, EQ, and NE
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
//
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  Examples of how the user
// would indirectly use this class (through the envelope) are:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Bool> x(shape); x.set(False);
// ArrayLattice<Bool> y(shape); y.set(True);
// ArrayLattice<Bool> z(shape); z.set(False);
// z.copyData(x&&y);                // z = x && y;
// z.copyData(x||y);                // z = x || y;
// z.copyData(x==y);                // z = x == y;
// z.copyData(x!=y);                // z = x != y;
// </srcblock>
// </example>
//
// <motivation>
// Logical binary operations are a basic mathematical expression. 
// </motivation>
//
// <todo asof="1998/01/20">
// </todo>
 

class LELBinaryBool : public LELInterface<Bool>
{
public: 
   
// Constructor takes operation and left and right expressions
// to be operated upon.
   LELBinaryBool(const LELBinaryEnums::Operation op, 
		 const CountedPtr<LELInterface<Bool> >& pLeftExpr,
		 const CountedPtr<LELInterface<Bool> >& pRightExpr);

// Destructor 
  ~LELBinaryBool();

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
   LELBinaryEnums::Operation op_p;
   CountedPtr<LELInterface<Bool> > pLeftExpr_p;
   CountedPtr<LELInterface<Bool> > pRightExpr_p;
};




} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LEL/LELBinary.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
