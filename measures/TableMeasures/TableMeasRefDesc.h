//# TableMeasRefDesc.h: Definition of a Measure Reference in a Table.
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

#ifndef MEASURES_TABLEMEASREFDESC_H
#define MEASURES_TABLEMEASREFDESC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/TableMeasures/TableMeasOffsetDesc.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableMeasDescBase;
class Table;
class TableDesc;
class TableRecord;


// <summary>
// Definition of a Measure Reference in a Table.
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
// TableMeasRefDesc is a class for setting up the MeasRef
// component of a TableMeasDesc in the TableMeasures system.   With the aid
// of a
// TableMeasRefDesc the following possibilities for defining a Measure
// column's reference exist:
// <ul>
//   <li> a fixed, non-variable, reference code, where all Measures in a
//	column are to have the same reference code.
//   <li> a unique (and probably different) reference code stored in each row.
//   <li> a unique reference code stored in each array element per
//	(Array)column row.
// </ul>
// For each of the above options an offset component can be specified
// along with a reference code.  When a Measure offset is required a
// <linkto class="TableMeasOffsetDesc">TableMeasOffsetDesc</linkto> is
// supplied as an argument to the TableMeasRefDesc constructor.
// With references containing an offset component either component can be set
// to be variable or fixed independently of the other.
//
//   <note role=tip>
//	It is not necessary to specify a Reference when defining a
//	Measure column. In such cases the Measures retrieved from the column
//	will have the default reference for the type of Measure stored in the
// 	column.
//   </note>
//
// A fixed reference code is trivially stored as part of the column
// keywords in the Measure column but a variable reference code requires
// its own column.  A Scalar or Array column can be used dependent on your
// needs but its type must always be either Int or String. Note that it is
// legal to specify a Scalar
// reference column for use with an ArrayMeasColumn. In such cases a single
// reference code will be stored per array (row) of Measures.  However,
// attempting to associate an Array column for references with a
// ScalarMeasColumn will generate an exception.
// <note>
//  Because the reference codes stored are the enums defined in the Measures
//  classes, it is possible that they change over time. The type strings,
//  however, wille never change. Therefore the reference codes and types
//  valid at the time of the table creation, are stored in the column keywords
//  if the reference codes are kept in an integer column.
//  <br>
//  This has only been added in March 2007, but is fully backward compatible.
//  Older tables will get the codes and types stored when accessed for
//  read/write.
// </note>
//
// <note role=caution>
//  When storing Measures into a Measure column with a fixed reference code
//  the reference code component of the Measures stored is
//  ignored.
// </note>
// </synopsis>

// <example>
//<ol>
// <li>Simplest kind of TableMeasRefDesc (apart from not specifying one at
//     all) is a fixed reference code.  All Measures subsequently
//     retrieved from the column will have the reference MEpoch::LAST.
// <srcblock>
//    // measure reference column
//    TableMeasRefDesc reference(MEpoch::LAST);
// </srcblock>
// <li>A variable reference code requires its own Int column.
// <srcblock>
//    // An int column for the variable references.
//    ScalarColumnDesc<Int> cdRefCol("refCol", "Measure reference column");
//    td.addColumn(cdRefCol);
//    ...
//    // create the Measure reference descriptor
//    TableMeasRefDesc varRef(td, "refCol");
// </srcblock>
// <li>A fix Measure reference code with a fixed Offset
// <srcblock>
//    // Create the Offset descriptor
//    MEpoch offset(MVEpoch(MVTime(1996, 5, 17, (8+18./60.)/24.))
//    TableMeasOffsetDesc offsetDesc(offset);
//    // create the Measure reference descriptor
//    TableMeasRefDesc varRef(MEpoch::LAST, offsetDesc);
// </srcblock>
//</ol>
// For an example of the use of a TableMeasRefDesc in the context of a full
// TableMeasDesc declaration see class
// <linkto class="TableMeasDesc">TableMeasDesc</linkto>.
// </example>

// <motivation>
// Creating the required keyword for the definition of a Measure
// in a Table is somewhat complicated. This class assists in that
// process.
// </motivation>
//
// <thrown>
//    <li>AipsError if the specified column doesn't exist or its type is
//	  not Int or String.
// </thrown>
//

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableMeasRefDesc
{
public:
  // Define a fixed MeasRef by supplying its reference code
  // Optionally a Measure offset can be specified.
  // The reference code and offset should not need a reference frame.
  // <group>
  explicit TableMeasRefDesc (uInt refCode = 0);
  TableMeasRefDesc (uInt refCode, const TableMeasOffsetDesc&);
  // </group>

  // Define a variable reference by supplying the name of the column
  // in which the reference is to be stored.  Either an <src>Int</src> or
  // <src>String</src> column can be specified.  This determines how
  // references are stored.  <src>Int</src> columns are likely to be
  // faster but storing
  // references as <src>Strings</src> may be useful if there is a need to
  // browse tables manually.  Optionally supply a Measure offset.
  // The reference code and offset should not need a reference frame.
  // <group>
  TableMeasRefDesc (const TableDesc&, const String& column);
  TableMeasRefDesc (const TableDesc&, const String& column,
		    const TableMeasOffsetDesc&);
  // </group>

  // Reconstruct the object from the MEASINFO record.
  // Not useful for the public.
  TableMeasRefDesc (const TableRecord& measInfo,
		    const Table&,
		    const MeasureHolder& measHolder,
		    const TableMeasDescBase&);

  // Copy constructor (copy semantics)
  TableMeasRefDesc (const TableMeasRefDesc& that);

  ~TableMeasRefDesc();

  // Assignment operator (copy semantics).
  TableMeasRefDesc& operator= (const TableMeasRefDesc& that);

  // Return the reference code.
  uInt getRefCode() const
    { return itsRefCode; }

  // Is the reference variable?
  Bool isRefCodeVariable() const
    { return (! itsColumn.empty()); }

  // Return the name of its variable reference code column.
  const String& columnName() const
    { return itsColumn; }

  // Is the reference code variable and stored in an integer column?
  Bool isRefCodeColumnInt() const
    { return itsRefCodeColInt; }

  // Do the keywords contain the reference codes and types.
  // For old tables this might not be the case.
  Bool hasRefTab() const
    { return itsHasRefTab; }

  // Returns True if the reference has an offset.
  Bool hasOffset() const
    { return (itsOffset != 0); }

  // Returns True if the offset is variable.
  Bool isOffsetVariable() const
    { return (itsOffset != 0  ?  itsOffset->isVariable() : False); }

  // Returns True is the offset is variable and it is an ArrayMeasColumn.
  Bool isOffsetArray() const
    { return (itsOffset != 0  ?  itsOffset->isArray() : False); }

  // Return the fixed Measure offset.
  // It does not test if the offset is defined; hasOffset() should be used
  // for that purpose.
  const Measure& getOffset() const
    { return itsOffset->getOffset(); }

  // Return the name of the Measure offset column.
  // An empty string is returned if no variable offset is used.
  const String& offsetColumnName() const
    { return itsOffset->columnName(); }

  // Reset the refCode or offset.
  // It overwrites the value used when defining the TableMeasDesc.
  // It is only possible if it was defined as fixed for the entire column.
  // <group>
  void resetRefCode (uInt refCode);
  void resetOffset (const Measure& offset);
  // </group>

  // Make the Measure value descriptor persistent.  Normally would not be
  // called by the user directly.
  // <group>
  void write (TableDesc&, TableRecord& measInfo, const TableMeasDescBase&);
  void write (Table&, TableRecord& measInfo, const TableMeasDescBase&);
  // </group>

  // Initialize the table reference codes and types and
  // the maps (mapping a code onto itself).
  void initTabRef (const MeasureHolder& measHolder);

  // Reference codes can be persistent in tables.
  // Because their enum values can change, a mapping of current table
  // to table value is maintained. The mapping is created using their
  // never-changing string representations.
  // These functions convert current refcode to and from table refcode.
  // <group>
  uInt tab2cur (uInt tabRefCode) const;
  uInt cur2tab (uInt curRefCode) const;
  // </group>

  // Set the function used to get all reference codes for a MeasureHolder.
  // This is not reaaly needed for normal practice, but makes it possible
  // to add extra codes when testing.
  // <br> The default function simply calls MeasureHolder.asMeasure.allTypes.
  // <group>
  typedef void TypesFunc (Vector<String>& types,
			  Vector<uInt>& codes, const MeasureHolder&);
  static void setTypesFunc (TypesFunc* func)
    { theirTypesFunc = func; }
  static void defaultTypesFunc (Vector<String>& types,
				Vector<uInt>& codes, const MeasureHolder&);
  static TypesFunc* theirTypesFunc;
  // </group>

private:
  uInt itsRefCode;
  // The name of column containing its variable references.
  String itsColumn;
  // Is the reference code column a string column?
  Bool   itsRefCodeColInt;
  // Do the keywords contain the reference codes and types?
  Bool   itsHasRefTab;
  //# Its reference offset.
  TableMeasOffsetDesc* itsOffset; 	
  //# Define the vectors holding the measref codes and types.
  //# These are the codes as used in the table, which might be different
  //# from the current values.
  Vector<String> itsTabRefTypes;
  Vector<uInt>   itsTabRefCodes;
  //# Define the mappings of table measref codes to current ones and back.
  //# There are only filled in and used if a variable reference code is used.
  Block<Int> itsTab2Cur;
  Block<Int> itsCur2Tab;

  // Fill the reference code mappings for table<->current.
  // <group>
  void initTabRefMap();
  void fillTabRefMap (const MeasureHolder& measHolder);
  uInt fillMap (Block<Int>& f2t,
		const Vector<uInt>& codesf,
		const Vector<String>& typesf,
		Vector<uInt>& codest,
		Vector<String>& typest,
		Int maxnr);
  // </group>

  // Write the actual keywords.
  void writeKeys (TableRecord& measInfo, 
		  const TableMeasDescBase& measDesc);

  // Throw an exception if the column doesn't exist or is of the
  // wrong type.
  void checkColumn (const TableDesc& td);
};



} //# NAMESPACE CASACORE - END

#endif
