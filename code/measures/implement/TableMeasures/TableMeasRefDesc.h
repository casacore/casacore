//# TableMeasRefDef.h: Definition of a Measure Reference in a Table.
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

#if !defined(AIPS_TABLEMEASREFDESC_H)
#define AIPS_TABLEMEASREFDESC_H

//# Includes
#include <trial/TableMeasures/TableMeasOffsetDesc.h>
#include <aips/Utilities/String.h>

//# Forward Declarations
class Table;
class TableRecord;
class TableDesc;
class TableMeasDescBase;
class MRBase;

// <summary>
// Definition of a Measure Reference in a Table.
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
// TableMeasRefDesc is a class for setting up the Measure Reference
// component of a TableMeasDesc for a column.   With the aid of a
// TableMeasRefDesc the following possibilities for defining a measure
// column's reference exist:
// <ul>
//   <li> a constant, non-variable reference, where all measures in a column 
//  	are to have the same reference.
//   <li> a variable reference, where each measure put into the column 
//  	has its own reference.
//   <li> for both the above options a measure offset can be specified 
//      along with the reference.
// </ul>
// </synopsis>

// <example>
// For an example TableMeasRefDesc in the context of a full TableMeasDesc 
// declaration see class <linkto class="TableMeasDesc">TableMeasDesc</linkto>.
// </example>

// <motivation>
// Creating the required keyword for the definition of a Measure
// in a Table is somewhat complicated. This class assists in that
// process.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableMeasRefDesc
{
public:
    // Define a constant MeasRef by supplying its reference code.
    // Optionally supply a measure offset descriptor.
    // <group>
    TableMeasRefDesc(uInt referenceCode);
    TableMeasRefDesc(uInt referenceCode, const TableMeasOffsetDesc&);
    // </group>
    
    // Define a variable reference by supplying the name of the column 
    // in which the reference is to be stored.  Either an <src>Int</src> or
    // <src>String</src> column can be specified.  This determines how
    // references are stored.  <src>Int</src> columns are likely to be
    // faster but storing 
    // references as <src>Strings</src> may be useful if there is a need to
    // browse tables manually.
    // <group>
    TableMeasRefDesc(const TableDesc&, const String& column);
    TableMeasRefDesc(const TableDesc&, const String& column, 
		     const TableMeasOffsetDesc&);
    // </group>

    // Reconstruct the object from the MEASINFO record.
    static TableMeasRefDesc* reconstruct(const TableRecord& measInfo,
		    	    	         const Table& tableDesc,
				         const TableMeasDescBase& measDesc);
				       
    // Copy constructor (copy semantics)
    TableMeasRefDesc(const TableMeasRefDesc& that);

    ~TableMeasRefDesc();

    // Assignment operator (copy semantics).
    TableMeasRefDesc& operator= (const TableMeasRefDesc& that);

    // Return the reference code.
    uInt getRefCode() const { return itsRefCode; }

    // Is the reference variable?
    Bool isVariable() const { return (itsColumn.empty()) ? False : True; }

    // Return the name of its variable reference code column.
    const String& columnName() const { return itsColumn; }
    
    // Returns True if the reference has an offset.    
    Bool hasOffset() const { return (itsOffset != 0  ? True : False); }

    // Returns True if the offset is variable..    
    Bool isOffsetVariable() const {
	if (hasOffset()) {
	    return itsOffset->isVariable();
	} else {
	    return False;
	}
    }
    
    // Return the non-variable measure offset.
    const Measure& getOffset() const { return itsOffset->getOffset(); }
    
    // Return the name of the measure offset column.
    const String& offsetColumnName() const { return itsOffset->columnName(); }
    
    // Make the measure value descriptor persistent.  Normally would not be
    // called by the user directly.
    void write(TableDesc& td, TableRecord& measInfo, const TableMeasDescBase&);
    
private:
    //# Contructor which uses the MEASINFO record. Not useful for the public.
    TableMeasRefDesc(const TableRecord& measInfo, 
    	    	     const Table&,
		     const TableMeasDescBase&,
		     const String& refString);

    //# Throws an exception if the column doesn't exist or is of the
    //# wrong type.
    void checkColumn(const TableDesc& td) const;

    //# MeasRef units when not variable.
    uInt itsRefCode;    
    // The name of column containing its variable references.
    String itsColumn;
    //# Its reference offset.
    TableMeasOffsetDesc* itsOffset; 	
};


#endif
