//# TableMeasures.h: Create Measure and Quantum columns Tables.
//# Copyright (C) 1996,1997,1998,1999
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

#if !defined (AIPS_TABLEMEASURES_H)
#define AIPS_TABLEMEASURES_H

#include <trial/TableMeasures/ArrayMeasColumn.h>
#include <trial/TableMeasures/ArrayQuantColumn.h>
#include <trial/TableMeasures/ScalarMeasColumn.h>
#include <trial/TableMeasures/ScalarQuantColumn.h>
#include <trial/TableMeasures/TableMeasDesc.h>
#include <trial/TableMeasures/TableMeasOffsetDesc.h>
#include <trial/TableMeasures/TableMeasRefDesc.h>
#include <trial/TableMeasures/TableMeasValueDesc.h>
#include <trial/TableMeasures/TableQuantumDesc.h>

// <module> 

// <summary> 
// Create and use scalar and array columns of Quanta and Measures in Tables.
// </summary>

// <prerequisite>
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
// </prerequisite>

// <reviewed reviewer="" date="" demos="" tests="tTableMeasures">
// </reviewed>

// <etymology>
// Table columns containing Measures and Quanta
// </etymology>

// <synopsis>

// The TableMeasures system exists to provide a way of creating (defining)
// Measure and Quantum Table columns thus enabling the direct storage of 
// Quanta and Measures in Aips++ Tables. 
//
// Defining Quantum and Measure columns is a once only operation (for each
// column).  It can be seen as an extension to the existing Column Descriptor 
// mechanism which adds a column of a specified type to a table.  The 
// <linkto><class=TableMeasDesc>TableMeasDesc</linkto> and 
// <linkto><class=TableQuantumDesc>TableQuantumDesc</linkto> class 
// hierarchies are used to define Measure and Quantum columns.
// 
// Once defined, Measure and Quantum column objects are used to access a
// column for reading and writing of Measures and Quanta.  For Quantum
// column objects see the classes 
// <linkto><class=ROScalarQuantColumn>ScalarQuantColumn</linkto> and
// <linkto><class=ROArrayQuantColumn>ArrayQuantColumn</linkto>.  For
// Measure column objects see 
// <linkto><class=ROScalarMeasColumn>ScalarMeasColumn</linkto> and
// <linkto><class=ROArrayMeasColumn>ArrayMeasColumn</linkto>.

// </synopsis>

// <motivation>
// The standard Aips++ Table system does not directly support Quantum and
// Measure columns.  These classes overcome this limitation.
// </motivation>

//# <todo asof="">
//# </todo>

// </module>

#endif
