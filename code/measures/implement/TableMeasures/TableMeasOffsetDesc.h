//# TableMeasOffseDesc.h: Definition of aOffset measure in a Table.
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

#if !defined(AIPS_TABLEMEASOFFSETDESC_H)
#define AIPS_TABLEMEASOFFSETDESC_H

//# Includes

//# Forward Declarations
class Measure;
class String;
class Table;
class TableMeasDescBase;
class TableRecord;

// <summary>
// Definition of a Measure Offset in a Table.
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
// The measure offset can be static for an entire column or it can vary per
// row.
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

class TableMeasOffsetDesc
{
public:
    // Constructor which defines a constant (non-variable) offset.
    TableMeasOffsetDesc(const Measure& offset);

    // Constructor for defining a variable offset.  
    TableMeasOffsetDesc(const TableMeasDescBase& offsetColumn);

    // Copy constructor.
    TableMeasOffsetDesc(const TableMeasOffsetDesc& that);

    ~TableMeasOffsetDesc();

    // Assignment operator.
    TableMeasOffsetDesc& operator=(const TableMeasOffsetDesc& that);
    
    // Reconstructs the TableMeasOffsetDesc from the measInfo TableRecord.
    static TableMeasOffsetDesc* reconstruct(const TableRecord& measInfo,
				    	    const String& prefix,
		    	    	            const Table& tab);
    
    // Get the (non-variable) measure offset for this column.  If it doesn't
    // exist (the offset is variable) and exception is thrown.
    const Measure& getOffset() const;
    
    // Returns True is the offset varies per row.
    Bool isVariable() const { return (itsTMDesc != 0 ? True : False); }
    
    // Gets the name of the column which stores the variable offset. "" is
    // returned if the offset is not variable.
    const String& columnName() const;
    
    // Write the information into the record.
    void write(TableDesc& td, TableRecord& measInfo, const String& prefix);

private:
    // Constructor which uses the measInfo TableRecord.
    TableMeasOffsetDesc(const TableRecord& measInfo, const String& prefix,
		      	const Table&);

    TableMeasDescBase* itsTMDesc;   //# Stores variable offset if applicable
    Measure* itsMeasure;    	    //# The offset if non-variable.
    String itsVarColName;   	    //# "" if offset non-variable.
};

#endif
