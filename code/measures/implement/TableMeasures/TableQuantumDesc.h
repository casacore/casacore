//# TableQuantumDesc.h: Definition of a Quantum in a Table.
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

#if !defined(AIPS_TABLEQUANTUMDESC_H)
#define AIPS_TABLEQUANTUMDESC_H

//# Includes

//# Forward Declarations
class String;
class TableDesc;
class Unit;

// <summary>
// A class for defining Quantum columns in tables.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableQuantum.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Quantum>Quantum</linkto>
//   <li> <linkto class=Table>Table</linkto>
//   <li> ScalarQuantColumn
//   <li> ArrayQuantColumn
// </prerequisite>

// <synopsis>
// The TableQuantumDesc is used to define a Quantum column in a table.
// It has the same functionality as the TableMeasDesc class hierarchy
// but is much simpler reflecting the simpler nature of Quantum objects.<br>
// Normally a TableQuantumDesc object is used to declare a Quantum column by 
// associating a Table column with a specific Quantum unit.  By doing this
// you are saying that all the Quantums contained in that column
// have that same unit.  However, it is possible to declare a variable unit
// Quantum column by supplying the name of a string
// column (used to store the units) to the appropriate 
// TableQuantumDesc constructor.<br>
// After a Quantum column has been defined by the user it can
// be accessed for reading and/or writing of Quantums using the 
// (RO)ScaQuantumCol or (RO)ArrQuantumCol classes.<br>
// </synopsis>

// <example>
// <srcblock>
//     // create a table descriptor as normal
//     TableDesc td("measTD", "1", TableDesc::New);
//     td.comment() = "A table containing measures and quantums";
//     
//     // This example sets up a Quantum<Complex> column but any valid Quantum
//     // type can be specified.  However, the type of the Quantums to be
//     // stored must match the type of the underlying table column.
//     ScalarColumnDesc<Complex> tcdQCplx("Quant", "A quantum complex column");
//
//     // For a Quantum array column an ArrayColumnDesc is 1st declared
//     ArrayColumnDesc<Double> tcdQDoub("QuantArray", "A quantum array col");
//
//     // The QuantumArray column has variable units.  A string is needed
//     // for these.  This could be done in two ways depending on what is 
//     // wanted.  Units can vary per element of array per row or
//     // just per row.  In the first instance an ArrayColumn<String> would be
//     // require.  Here we want to vary units only per row.
//     ScalarColumnDesc<String> tcdUnits("VarQuantUnits", "Quantum units");
//     
//     td.addColumn(tcdQplx);
//     td.addColumn(tcdQDoub);
//     td.addColumn(tcdUnits);
//     
//     // Create the TableQuantumDesc with units "deg" and make it persistent.
//     TableQuantumDesc tqdS(td, "Quant", unit("deg"));
//     tqdS.write(td);
//     // This for the Quantum array column with variable units.
//     TableQuantumDesc tqdA(td, "QuantArray", "VarQuantUnits");
//     tqdA.write(td);
// 
//     // Describe other columns here...
//     
//     // Setup and create the new table as usual.
//     SetupNewTable newtab("mtab", td, Table::New);
//     Table qtab(newtab);
//
//     // Now ScalarQuantColumn and ArrayQuantColumn objects could be 
//     // constructed to access the columns...
// </srcblock>
// </example>

// <motivation>
// This class assists in the definition of a Quantum Table Column.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


//template<class Qtype>
class TableQuantumDesc
{
public:
    // Constructs a Quantum column descriptor with null units (Unit == "").
    // The column should have already been added to the TableDesc.  An 
    // exeception is thrown if the column doesn't exist.
    TableQuantumDesc(const TableDesc& td, const String& column);

    // Constructs a Quantum column descriptor with the specified Quantum units.
    // The column should have already been added to the TableDesc.  An 
    // exeception is thrown if the column doesn't exist.
    TableQuantumDesc(const TableDesc& td, const String& column, const Unit& u);

    // Constructs a Quantum column descriptor with variable units stored in
    // unitCol.  Both the quantum and unit column should exist in the 
    // TableDesc.
    //<group>
    TableQuantumDesc(const TableDesc& td, const String& column, 
	    	     const String& unitCol);
    TableQuantumDesc(const TableDesc& td, const String& column, 
	    	     const Char* unitCol);
    //</group>

    // Copy constructor.
    TableQuantumDesc(const TableQuantumDesc& that);

    ~TableQuantumDesc();
    
    // Reconstructs a previously constructed TableQuantumDesc.
    static TableQuantumDesc* reconstruct(const TableDesc& td, 
	    	    	    	    	 const String& column);
    
    // Assignment.
    TableQuantumDesc& operator= (const TableQuantumDesc& that);
    
    // Returns the Quantum column descriptor's units.  "" is returned if
    // units have not been specified.  This could be because the null
    // unit contructor was used or because the units are variable.
    const String& getUnits() const { return itsUnitsName; } 
    
    // Returns True if descriptor set for variable units (one per row)
    Bool isUnitVariable() const 
	{ return (itsUnitsColName != "") ? True : False; }
    
    // Returns the name of the quantum column. 
    const String& columnName() const { return itsColName; }

    // Returns the name of the units column (an empty String is returned
    // if the units are not variable).
    const String& unitColumnName() const { return itsUnitsColName; }

    // Makes the Quantum column descriptor persistent by saving its units in
    // its set of column keywords.
    void write(TableDesc& td);
    
private:
    // Throws an exception if the quantum column doesn't exist.
    void checkColumn(const TableDesc& td) const;

    // Throws an exception if the variable units column isn't a string column.
    void checkUnitsColumn(const TableDesc& td) const;
    
    // Name of column which stores the Quantum's values.
    String itsColName;

    // The Quantum's unit as a string.
    String itsUnitsName;

    // Name of units column if units are variable.
    String itsUnitsColName;
};

#endif
