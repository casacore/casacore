//# TableMeasDesc.h: Definition of a Measure in a Table.
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

#if !defined(AIPS_TABLEMEASDESC_H)
#define AIPS_TABLEMEASDESC_H


#if defined(_AIX)
#pragma implementation ("TableMeasDesc.cc")
#endif

//# Includes
#include <trial/TableMeasures/TableMeasDescBase.h>

//# Forward Declarations
class String;
class Table;
class TableMeasRefDesc;
class TableMeasValueDesc;

// <summary>
// Definition of a Measure column in a Table.  
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableMeasure.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=MeasBase.h>MeasBase</linkto>
//   <li> <linkto class=ColumnDesc>ColumnDesc</linkto>
// </prerequisite>

// <synopsis>
// The TableMeasDesc class hierarchy was created to add support for Measures
// Columns in the Table system.  Creating and using Measures Columns is a
// two step process.  A TableMeasDesc object is used to define a Measure
// column and a one of either a ScalarMeasColumn or ArrayMeasColumn objects
// is used to add and/or read Measures to/from the column.<br>
// When defining a Measure column the TableMeasDesc associates a particular
// column with a number of key/value pairs which describe certain aspects of 
// the Measures
// contained in the column such as their type (MEpoch, MDirection, etc),
// reference and offset.  The key/value pairs are created by the different
// components of the TableMeasDesc object hierarchy.  The column associated
// with the TableMeasDesc (and effectively identifies the Measure column) must
// already exist in the Table Descriptor and must be an array column with
// type Double.<br>
// After the measure column has been defined by the TableMeasDesc its write()
// member must be used to make the TableMeasDesc persistent.  The column can
// then be accessed by the Scalar(Array)MeasColumn object.
// </synopsis>

// <example>
// <srcblock>
//     // Need a table to work with.
//     TableDesc td("tTableMeasure_desc", "1", TableDesc::New);
//     td.comment() = "A test of TableMeasures class.";
//     
//     // Array<Double>s are needed to support Measure Columns.
//     // Create an underlying column for two Measure columns.
//     ArrayColumnDesc<Double> cdTime("Time1", "An MEpoch column");
//     ArrayColumnDesc<Double> cdLast("LastColumn", "A MEpoch with an offset");
//     td.addColumn(cdTime);
//     td.addColumn(cdLast);
//     
//     {
// 	// A TableMeasDesc for a simple MEpoch column "Time1" with reference
// 	// MEpoch::TAI
// 	TableMeasRefDesc tmrd(MEpoch::TAI);
// 	TableMeasValueDesc tmvd(td, "Time1");    
// 	TableMeasDesc<MEpoch> tmdMEpoch(tmvd, tmrd);
//      // write makes the Measure column persistent.
// 	tmdMEpoch.write(td);
//     }    
//     {
//     	// Another MEpoch column descriptor this one specifies a fixed offset 
// 	// with reference MEpoch::LAST.
// 	MEpoch mjdToday(MVEpoch(51234));
// 	TableMeasOffsetDesc tmodToday(mjdToday);
// 	TableMeasRefDesc tmrdLast(MEpoch::LAST, tmodToday);
// 	TableMeasValueDesc tmvdLast(td, "LastColumn");    
// 	TableMeasDesc<MEpoch> tmdLast(tmvdLast, tmrdLast);
// 	tmdLast.write(td);
//     }    
//     
//     // create the table
//     SetupNewTable newtab("TestTableMeasures", td, Table::New);
//     const uInt tabRows = 5;
//     Table tab(newtab, tabRows);
//
//     // can now create the ScalarMeasColumn object to access the column
//     // for reading and writing of measures.
//
// </srcblock>
// </example>

// <motivation>
// Creating the required keyword for the definition of a Measure
// in a Table is somewhat complicated. This class assists in that
// process.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


template<class M>
class TableMeasDesc : public TableMeasDescBase
{
public:
    // Constructor with measure value descriptor.
    TableMeasDesc(const TableMeasValueDesc&);

    // Constructor with value and reference descriptors.
    TableMeasDesc(const TableMeasValueDesc&, const TableMeasRefDesc&);
    
    // Clone the object.
    virtual TableMeasDescBase* clone() const;

    // Copy constructor (copy semantics).
    TableMeasDesc(const TableMeasDesc<M>& that);

    ~TableMeasDesc();
    
    // Assignent operator (copy semantics)
    TableMeasDesc<M>& operator=(const TableMeasDesc<M>& that);
    
    // Returns the descriptors measure type as a String.
    virtual const String& type() const;

    // Translates the refCode for the descriptors measure type.    
    virtual const String& refType(const uInt refCode) const;
    
    // Returns the reference code for this object given a string.  Throws
    // an exception if the refString is invalid for this object.
    virtual const uInt refCode(const String& refString) const;
    
private:
    // TableMeasDescBase::reconstruct() needs to access to the null
    // constructor but the null constructor for TableMEasDesc should not be
    // callable anywhere else.  Making this member a friend is one solution
    // to this.
    friend TableMeasDescBase* TableMeasDescBase::reconstruct(const Table& tab, 
    	const String& columnName);

    // Null object constructor. Only needed by TableMeasDescBase::reconstruct.
    TableMeasDesc();
};


#endif
