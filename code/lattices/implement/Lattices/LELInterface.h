//# LELInterface.h:  Abstract base class for lattice expressions
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

#if !defined(AIPS_LELInterface_H)
#define AIPS_LELInterface_H


//# Includes
#include <aips/aips.h>
#include <trial/Lattices/LELAttribute.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/CountedPtr.h>
#include <aips/Utilities/DataType.h>

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


template <class T> class LELInterface
{
public:

// Virtual destructor
   virtual ~LELInterface();

// Evaluate the expression and fill the result array
   virtual void eval (Array<T>& result,
                      const PixelRegion& region) const = 0;

// Get the result of a scalar subexpression.
   virtual T getScalar() const = 0;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare() = 0;

// Get the data type.
//   DataType dataType() const
//      {return whatType((T*)0);}

// Is the result a scalar ?
   Bool isScalar() const
      {return attr_p.isScalar();}

// Get the shape of the expression result.
   const IPosition& shape() const
      {return attr_p.shape();}

// Get expression attribute
   const LELAttribute& getAttribute() const
      {return attr_p;}

// Get class name
   virtual String className() const = 0;

// If the given expression is a scalar, replace it by its result.
   static void replaceScalarExpr (CountedPtr<LELInterface<T> >& expr);

protected:
// Set the expression attributes of this object.
   void setAttr(const LELAttribute& attrib);

private:
   LELAttribute attr_p;
};


#endif



