//# LELAttribute.h:  LELAttribute.h
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

#if !defined(AIPS_LELATTRIBUTE_H)
#define AIPS_LELATTRIBUTE_H

//# Includes
#include <aips/Lattices/IPosition.h>


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


class LELAttribute
{
public:
// Default constructor
   LELAttribute();

// Constructor
   LELAttribute(const Bool isScalar,
		const IPosition& shape);

// Constructor
   LELAttribute(const LELAttribute& attr);

// Constructor
   LELAttribute(const LELAttribute& attrLeft,
		const LELAttribute& attrRight);

// Destructor
   ~LELAttribute();

// Assignment (copy semantics)
   LELAttribute& operator= (const LELAttribute& other);

// Is expression a scalar
   const Bool isScalar() const
       { return isScalar_p; }

// What is the shape of the expression
   const IPosition& shape() const
       { return shape_p; }

private:
   Bool      isScalar_p;
   IPosition shape_p;
};


#endif



