//# LELUnary.h:  LELUnary.h
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

#if !defined(AIPS_LELUNARY_H)
#define AIPS_LELUNARY_H


//# Includes
#include <trial/Lattices/LELInterface.h>
#include <trial/Lattices/LELUnaryEnums.h>

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


template <class T> class LELUnaryConst : public LELInterface<T>
{
public: 
   
// Constructor takes a scalar.  
   LELUnaryConst(const T val);

// Destructor does nothing
  ~LELUnaryConst();

// Evaluate the expression.
// This throws an exception, since only a scalar can be returned.
   virtual void eval (Array<T>& result,
                      const PixelRegion& region) const;

// Get the constant.
   virtual T getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;    

private:
   T val_p;
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


template <class T> class LELUnary : public LELInterface<T>
{
public: 
   
// Constructor takes operation and expression
// to be operated upon
   LELUnary(const LELUnaryEnums::Operation op, 
	    const CountedPtr<LELInterface<T> >& pExpr);

// Destructor does nothing
  ~LELUnary();

// Evaluate the expression.
   virtual void eval (Array<T>& result,
                      const PixelRegion& region) const;

// Get the result of a scalar subexpression.
   virtual T getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;    

private:
   LELUnaryEnums::Operation op_p;
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


class LELUnaryBool : public LELInterface<Bool>
{
public: 
   
// Constructor takes operation and expression
// to be operated upon
   LELUnaryBool(const LELUnaryEnums::Operation op, 
		const CountedPtr<LELInterface<Bool> >& pExpr);

// Destructor does nothing
  ~LELUnaryBool();

// Evaluate the expression.
   virtual void eval (Array<Bool>& result,
                      const PixelRegion& region) const;

// Get the result of a scalar subexpression.
   virtual Bool getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;    

private:
   LELUnaryEnums::Operation op_p;
   CountedPtr<LELInterface<Bool> > pExpr_p;
};



#endif
