//# LELBinary.h:  LELBinary.h
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

#if !defined(AIPS_LELBINARY_H)
#define AIPS_LELBINARY_H


//# Includes
#include <trial/Lattices/LELInterface.h>
#include <trial/Lattices/LELBinaryEnums.h>

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
 

template <class T> class LELBinary : public LELInterface<T>
{
public: 
   
// Constructor takes operation and left and right expressions
// to be operated upon
   LELBinary(const LELBinaryEnums::Operation op, 
	     const CountedPtr<LELInterface<T> >& pLeftExpr,
	     const CountedPtr<LELInterface<T> >& pRightExpr);

// Destructor 
  ~LELBinary();

// Evaluate the binary expression given in the constructor,
// and put the result into the array.
   virtual void eval (Array<T>& result,
                      const PixelRegion& region) const;

// Evaluate the binary scalar expression given in the constructor,
// and return the result.
   virtual T getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;    

private:
   LELBinaryEnums::Operation op_p;
   CountedPtr<LELInterface<T> > pLeftExpr_p;
   CountedPtr<LELInterface<T> > pRightExpr_p;
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

// Evaluate the binary expression given in the constructor,
// and put the result into the array.
   virtual void eval (Array<Bool>& result,
                      const PixelRegion& region) const;

// Evaluate the binary scalar expression given in the constructor,
// and return the result.
   virtual Bool getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;    

private:
   LELBinaryEnums::Operation op_p;
   CountedPtr<LELInterface<T> > pLeftExpr_p;
   CountedPtr<LELInterface<T> > pRightExpr_p;
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
 

class LELBinaryBool : public LELInterface<Bool>
{
public: 
   
// Constructor takes operation and left and right expressions
// to be operated upon. It can only handle the AND and OR operator.
   LELBinaryBool(const LELBinaryEnums::Operation op, 
		 const CountedPtr<LELInterface<Bool> >& pLeftExpr,
		 const CountedPtr<LELInterface<Bool> >& pRightExpr);

// Destructor 
  ~LELBinaryBool();

// Evaluate the binary expression given in the constructor,
// and put the result into the array.
   virtual void eval (Array<Bool>& result,
                      const PixelRegion& region) const;

// Evaluate the binary scalar expression given in the constructor,
// and return the result.
   virtual Bool getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;    

private:
   LELBinaryEnums::Operation op_p;
   CountedPtr<LELInterface<Bool> > pLeftExpr_p;
   CountedPtr<LELInterface<Bool> > pRightExpr_p;
};



#endif
