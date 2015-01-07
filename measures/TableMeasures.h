//# TableMeasures.h: Create Measure and Quantum columns Tables.
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#ifndef MEASURES_TABLEMEASURES_H
#define MEASURES_TABLEMEASURES_H

#include <casacore/casa/aips.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/measures/TableMeasures/TableMeasDesc.h>
#include <casacore/measures/TableMeasures/TableMeasOffsetDesc.h>
#include <casacore/measures/TableMeasures/TableMeasRefDesc.h>
#include <casacore/measures/TableMeasures/TableMeasValueDesc.h>
#include <casacore/measures/TableMeasures/TableQuantumDesc.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module> 

// <summary> 
// Create and use scalar and array columns of Quanta and Measures in Tables.
// </summary>

// <prerequisite>
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
// </prerequisite>

// <reviewed reviewer="Bob Garwood" date="1999/12/23">
// </reviewed>

// <etymology>
// Table columns containing Measures and Quanta
// </etymology>

// <synopsis>

// The TableMeasures system exists to provide a way of creating (defining)
// Measure and Quantum Table columns thus enabling the direct storage of 
// Quanta and Measures in Casacore Tables. 
// <p>
// Defining Quantum and Measure columns is a once only operation (for each
// column).  It can be seen as an extension to the existing Column Descriptor 
// mechanism which adds a column of a specified type to a table.  The 
// <linkto><class=TableMeasDesc>TableMeasDesc</linkto> and 
// <linkto><class=TableQuantumDesc>TableQuantumDesc</linkto> class 
// hierarchies are used to define Measure and Quantum columns.
// <p>
// Once defined, Measure and Quantum column objects are used to access a
// column for reading and writing of Measures and Quanta.  For Quantum
// column objects see the class
// <linkto><class=ScalarQuantColumn>ScalarQuantColumn</linkto> and
// <linkto><class=ArrayQuantColumn>ArrayQuantColumn</linkto>.  For
// Measure column objects see 
// <linkto><class=ScalarMeasColumn>ScalarMeasColumn</linkto> and
// <linkto><class=ArrayMeasColumn>ArrayMeasColumn</linkto>.
//
// <h3>Conversions</h3>
// The classes accessing the data use the underlying
// <linkto module=Quanta>Quanta</linkto> or
// <linkto module=Measures>Measures</linkto> classes to convert
// the units or references of the measures or quanta.
// The TableMeasures classes do not test if a conversion is possible.
// <br>In general one can say that about every unit conversion is possible.
// The <linkto class=Unit>Unit</linkto> class adjusts units as needed.
// <br>Conversions of Measures are only possible if enough information
// is supplied for the measure's reference.
// <br>Take a look at the abovementioned modules to find out about conversions.
//
// <h3>Performance</h3>
// Using the TableMeasures classes makes it easier to deal with
// measures in tables. However, there is a performance penalty
// compared to handling the values directly in the table using
// the Tables classes <linkto class=ScalarColumn>ScalarColumn</linkto>
// and <linkto class=ArrayColumn>ArrayColumn</linkto>.
//
// The performance of the TableMeasures classes depends on how the
// measures are stored; thus if a fixed or variable offset and reference
// are used.
// Of course, it also depends on whether the measures have to be
// converted before they can be stored.
// <br>The TableMeasures classes are always slower than the Tables classes,
// but they offer more convenience.
// In general one can say that for large tables it is better to use
// the Tables classes directly to put/get the data.
// However, even when putting directly using the Tables classes, the
// column itself should be defined as a TableMeasure. In that way there
// is one standard way of defining columns as table measures.
// <br>For example, the TIME column in a MeasurementSet should be handled
// directly uisng the Tables classes (because it is so large).
// On the other hand, the FIELD table is very small and it may make life
// easier to handle its columns through the TableMeasures classes.
//
// In a test putting an array of quanta using class
// <linkto class=ArrayQuantColumn>ArrayQuantColumn</linkto> took
// about 5 times as long as doing it directly using class
// <linkto class=ArrayColumn>ArrayColumn</linkto>. The quantum column
// had variable units. so for each row the unit had to be written as well.
// Reading it back took about 3 times as long.
// <br>When using a qunatum column with fixed units, putting took about
// 2.5 times as long as using <src>ArrayColumn</src> directly.
// Each put involved a unit conversion.
// Reading it back took only 10% more than when using <src>ArrayColumn</src>.
//
// </synopsis>

// <motivation>
// The standard Casacore Table system does not directly support Quantum and
// Measure columns.  These classes overcome this limitation.
// </motivation>

//# <todo asof="">
//# </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif
