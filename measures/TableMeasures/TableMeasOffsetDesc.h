//# TableMeasOffseDesc.h: Definition of an Offset measure in a Table.
//# Copyright (C) 1997,1999,2000,2001
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

#ifndef MEASURES_TABLEMEASOFFSETDESC_H
#define MEASURES_TABLEMEASOFFSETDESC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/measures/Measures/MeasureHolder.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableMeasDescBase;
class Measure;
class Table;
class TableDesc;
class TableRecord;
class String;


// <summary>
// Definition of a Measure Offset in a Table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1999/12/23" tests="tTableMeasures.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
//   <li> <linkto class=TableMeasDesc>TableMeasDesc</linkto>
// </prerequisite>

// <synopsis>
// This class assists in the definition of the offset component of a
// TableMeasDesc
// in the TableMeasures system.  Four possibilities exist for specifying the
// handling of measure offsets in a Measure column.  These are:
//
// <ul>
//   <li> an offset is not used
//   <li> all measures in the column have the same offset
//   <li> a unique (and probably different) offset is stored for each row
//   <li> a unique offset is stored in each array element per (Array)column
//	row
// </ul>
//
// Note that this last option is only relevant when using ArrayMeasColumns.
//
// Examples of each of these follow.
// </synopsis>

// <example>
//<ol>
// <li>Specifying a single fixed offset.  Note that a Measure offset is itself
// a measure
// <srcblock>
//    // create an MEpoch to use as the offset in an MEpoch column
//    MEpoch offset(MVEpoch(MVTime(1996, 5, 17, (8+18./60.)/24.)), MEpoch::UTC);
//    TableMeasOffsetDesc offsetDesc(offset);
// </srcblock>
//
// <li>Storing an offset per row needs an offset column.  Measure offsets are
// Measures so a Measure column is needed:
// <srcblock>
//    // Need a column for the offsets.  This is to be a Measure column,
//    // so the rules for creating a Measure column apply.
//    ArrayColumnDesc<Double> cdOffset("OffsetCol", "Variable Offset col");
//    ...
//    // add the column to the table
//    td.addColumn(cdOffset);
//    ...
//    // Create the Measure column to be used as the measure offset column
//    TableMeasValueDesc valDesc(td, "OffsetCol");
//    TableMeasDesc<MEpoch> offset(valDesc);
//    // Create the offset descriptor
//    TableMeasOffsetDesc offsetDesc(offset);
//
// </srcblock>
//
// <li>Storing an offset per array element per row requires one change in the
// constructor used in the previous example:
// <srcblock>
//    ...
//    // set up column and TableMeasDesc as before
//    ...
//    // Setting the asArray parameter to True in the constructor specifies
//    // per element offset storage
//    TableMeasOffsetDesc offsetDesc(offset, True);
// </srcblock>
// </ol>
//
// For an example of the use of the TableMeasOffsetDesc class in the context
// of a full TableMeasDesc declaration see class
// <linkto class="TableMeasDesc">TableMeasDesc</linkto>.
// </example>

// <motivation>
// Creating the required keyword for the definition of a Measure
// in a Table is somewhat complicated. This class assists in that
// process.
// </motivation>
//
// <thrown>
//    <li>AipsError during reconstruction of non-variable offset if a
// 	  component of the offset measure is missing in the column keywords or
//	  is corrupt in some way.
//    <li>AipsError if getOffset() called on a variable offset object.
//    <li>AipsError during a reconstruct if the column doesn't have a Unit.
// </thrown>
//
//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>

class TableMeasOffsetDesc
{
public:
  // Constructor which defines a constant (non-variable) offset.  All
  // measures in the columns will have the same offset.
  TableMeasOffsetDesc (const Measure& offset);

  // Constructor for defining a variable offset.  If asArray is True then
  // the offset is stored per array element.  The default is for the
  // offset to be stored (and hence variable) per row.
  TableMeasOffsetDesc (const TableMeasDescBase& offsetColumn,
		       Bool asArray=False);

  // Copy constructor (copy semantics).
  TableMeasOffsetDesc (const TableMeasOffsetDesc& that);

  ~TableMeasOffsetDesc();

  // Assignment operator (copy semantics).
  TableMeasOffsetDesc& operator= (const TableMeasOffsetDesc& that);

  // Reconstructs the TableMeasOffsetDesc from the measInfo TableRecord.
  static TableMeasOffsetDesc* reconstruct (const TableRecord& measInfo,
					   const String& prefix,
					   const Table& tab);

  // Get the (non-variable) measure offset for this column.  If it doesn't
  // exist (thus if the offset is variable), an exception is thrown.
  const Measure& getOffset() const;

  // Returns True if the offset varies per row.
  Bool isVariable() const
    { return (itsTMDesc != 0); }

  // Returns True if the offset varies per array element.
  Bool isArray() const
    { return (isVariable() && itsVarPerArr); }

  // Gets the name of the column which stores the variable offset.
  // "" is returned if the offset is not variable.
  const String& columnName() const
    { return itsVarColName; }

  // Reset the offset.
  // It overwrites the value used when defining the TableMeasDesc.
  // It is only possible if it was defined as fixed for the entire column.
  void resetOffset (const Measure& offset);

  // Write the information into the record.
  // <group>
  void write (TableDesc&, TableRecord& measInfo, const String& prefix);
  void write (Table&, TableRecord& measInfo, const String& prefix);
  // </group>

private:
  TableMeasDescBase* itsTMDesc;      //# Stores variable offset if applicable
  MeasureHolder      itsMeasure;     //# The offset if non-variable.
  String             itsVarColName;  //# "" if offset non-variable.
  Bool               itsVarPerArr;   //# Is variable per array element.


  // Constructor which uses the measInfo TableRecord.
  TableMeasOffsetDesc (const TableRecord& measInfo, const String& prefix,
		       const Table&);

  // Write the actual keywords.
  void writeKeys (TableRecord& measInfo, const String& prefix);
};



} //# NAMESPACE CASACORE - END

#endif
