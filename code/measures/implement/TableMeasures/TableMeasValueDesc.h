//# TableMeasValueDesc.h: Definition of a MeasValue in a Table.
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

#if !defined(AIPS_TABLEMEASVALUEDESC_H)
#define AIPS_TABLEMEASVALUEDESC_H

//# Includes

//# Forward Declarations
class ColumnDesc;
class String;
class TableDesc;
class TableRecord;

// <summary>
// Definition of a Measure Value in a Table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableMeasure.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
//   <li> TableMeasDesc
// </prerequisite>

// <synopsis>
// TableMeasValueDesc is used to set up the MeasValue component of the 
// TableMeasDesc. It is used to associate the measure column name with the
// TableMeasDesc.  The column must be an array with type double and its
// descriptor must already exist in the TableDesc.
// </synopsis>

// <example>
// See class <linkto class="TableMeasDesc">TableMeasDesc</linkto>.
// </example>

// <motivation>
// Creating the required keyword for the definition of a Measure
// in a Table is somewhat complicated. This class assists in that
// process.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableMeasValueDesc
{
public:
    // Null constructor
    TableMeasValueDesc();

    // Contructor the MeasValue column descriptor for the given column.
    // The column must be an array column of type Double and should already
    // exist in the TableDesc.
    TableMeasValueDesc(const TableDesc&, const String& columnName);

    // Copy constructor.
    TableMeasValueDesc(const TableMeasValueDesc& that);

    ~TableMeasValueDesc();

    // Assignment operator.
    TableMeasValueDesc& operator= (const TableMeasValueDesc& that);

    // Write the type, unit, and MEASINFO record into the column keywords.
    void write(TableDesc& td, const TableRecord& measInfo);

    // Get the name of the underlying column.
    const String& columnName() const { return itsColumn; }

private:
    // Throws an exception if the quantum column doesn't exist or is of the
    // wrong type.
    void checkColumn(const TableDesc& td) const;

    String itsColumn;	    //# MeasValue column name.
};

#endif
