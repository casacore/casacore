//# TableMeasDescBase.h: Definition of a Measure in a Table.
//# Copyright (C) 1997,1999
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

#if !defined(AIPS_TABLEMEASDESCBASE_H)
#define AIPS_TABLEMEASDESCBASE_H

//# Includes
#include <trial/TableMeasures/TableMeasValueDesc.h>
#include <trial/TableMeasures/TableMeasRefDesc.h>

//# Forward Declarations
class String;
class Table;
class TableDesc;

// <summary>
// Definition of a Measure in a Table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableMeasures.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
//   <li> TableMeasDesc
// </prerequisite>

// <synopsis>
// Abstract base class for TableMeasDesc.
// </synopsis>

// <example>
// See class <linkto class="TableMeasDesc">TableMeasDesc</linkto>.
// </example>

// <motivation>
// Creating the required keyword for the definition of a Measure
// in a Table is somewhat complicated. This class assists in that
// process.
// </motivation>
//
// <thrown>
//    <li>AipsError during reconstruction if the column doesn't contain
//        a MEASINFO record.
//    <li>AipsError during reconstruction if the column has a MEASINFO
//        but it Measure type is invalid.
// </thrown>
//

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableMeasDescBase
{
public:
    // Null constructor.
    TableMeasDescBase();

    // Constructor with measure value descriptor.
    TableMeasDescBase(const TableMeasValueDesc&);

    // Constructor with value and reference descriptors.
    TableMeasDescBase(const TableMeasValueDesc&, const TableMeasRefDesc&);

    // Copy constructor.
    TableMeasDescBase(const TableMeasDescBase& that);
    
    virtual ~TableMeasDescBase();

    // Clone the object.
    virtual TableMeasDescBase* clone() const = 0;

    // Assignment operator.
    TableMeasDescBase& operator= (const TableMeasDescBase& that);
    
    // Reconstructs the object for the given table and column name.  This
    // should be a private member as the user of this member are the
    // Measure column object constructors, however, the Gnu compiler (2.7.2)
    // won't parse templated friend definitions, i.e., attempting the
    // make the ScalarMeasColumn class a friend (ok with egcs though).
    static TableMeasDescBase* reconstruct(const Table& tab, 
    	    	    	    	    	  const String& columnName);

    // Makes the descriptor persistent.
    void write(TableDesc& td);

    // Get the name of the underlying column.
    const String& columnName() const { return itsValue.columnName(); }
    
    // Return the reference code.
    uInt getRefCode() const { return itsRef->getRefCode(); }
    
    // Returns True if the reference varies per row.
    Bool isRefCodeVariable() const { return itsRef->isVariable(); }
    
    // Returns the name of the ref code column when the ref code is variable.
    // The null string is returned if the ref code is not variable.
    const String& refColumnName() const { return itsRef->columnName(); }
    
    // Returns a reference to its measure reference descriptor.
    const TableMeasRefDesc& getRefDesc() { return *itsRef; }
    
    // Get the name of the offset column. Empty string is returned if no
    // offset.
    const String& offsetColumnName() const { 
	return itsRef->offsetColumnName();
    }
    
    // Returns True is and offset has been defined.
    Bool hasOffset() const { return itsRef->hasOffset(); }
    
    // Returns True is the offset is variable.
    Bool isOffsetVariable() const { return itsRef->isOffsetVariable(); }
    
    // Returns True is the offset is variable and is stored as in an
    // ArrayMeasColumn, i.e., offsets are stored per element.
    Bool isOffsetArray() const { return itsRef->isOffsetArray(); }
    
    // Returns a reference to the offset.
    const Measure& getOffset() const { return itsRef->getOffset(); }

    // Returns the descriptors measure type as a String.
    virtual const String& type() const = 0;

    // Returns the reference code for this object given a string.  Throws
    // an exception if the refString is invalid for this object.
    virtual const uInt refCode(const String& refString) const = 0;

    // Translates the refCode for the descriptors measure type.    
    virtual const String& refType(const uInt refCode) const = 0;

private:
    TableMeasValueDesc itsValue;    //# The measure value column.
    TableMeasRefDesc* itsRef;	    //# The optional reference.
};

#endif
