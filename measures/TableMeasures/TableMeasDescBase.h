//# TableMeasDescBase.h: Definition of a Measure in a Table.
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

#ifndef MEASURES_TABLEMEASDESCBASE_H
#define MEASURES_TABLEMEASDESCBASE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/TableMeasures/TableMeasValueDesc.h>
#include <casacore/measures/TableMeasures/TableMeasRefDesc.h>
#include <casacore/measures/TableMeasures/TableMeasType.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class Table;
class TableDesc;
class TableRecord;
class TableColumn;
class Measure;
template<class T> class Quantum;


// <summary>
// Definition of a Measure in a Table.
// </summary>

// <use visibility=local>

// <reviewed reviewer="Bob Garwood" date="1999/12/23" tests="tTableMeasures.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
//   <li> <linkto class=TableMeasDesc>TableMeasDesc</linkto>
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

  // Constructor with value and reference descriptors.
  // Note that setMeasType is always called by the derived class.
  TableMeasDescBase (const TableMeasValueDesc&, const TableMeasRefDesc&);

  // Copy constructor.
  TableMeasDescBase (const TableMeasDescBase& that);

  virtual ~TableMeasDescBase();

  // Clone the object.
  virtual TableMeasDescBase* clone() const;

  // Assignment operator.
  TableMeasDescBase& operator= (const TableMeasDescBase& that);

  // Makes the descriptor persistent.
  // <group>
  void write (TableDesc&);
  void write (Table&);
  // </group>

  // Make the descriptor persistent if there was no refcode vector.
  // This is only needed for old tables without such vectors.
  void writeIfOld (const Table&);

  // Get the name of the underlying column.
  const String& columnName() const
    { return itsValue.columnName(); }

  // Return the reference code.
  uInt getRefCode() const
    { return itsRef.getRefCode(); }

  // Returns True if the reference varies per row.
  Bool isRefCodeVariable() const
    { return itsRef.isRefCodeVariable(); }

  // Returns the name of the ref code column when the ref code is variable.
  // The null string is returned if the ref code is not variable.
  const String& refColumnName() const
    { return itsRef.columnName(); }

  // Returns a reference to its measure reference descriptor.
  const TableMeasRefDesc& getRefDesc() const
    { return itsRef; }

  // Get the name of the offset column. Empty string is returned if no
  // offset.
  const String& offsetColumnName() const
    { return itsRef.offsetColumnName(); }

  // Returns True if an offset has been defined.
  Bool hasOffset() const
    { return itsRef.hasOffset(); }

  // Returns True if the offset is variable.
  Bool isOffsetVariable() const
    { return itsRef.isOffsetVariable(); }

  // Returns True if the offset is variable and is stored in an
  // ArrayMeasColumn, i.e., offsets are stored per element.
  Bool isOffsetArray() const
    { return itsRef.isOffsetArray(); }

  // Returns a reference to the offset.
  const Measure& getOffset() const
    { return itsRef.getOffset(); }

  // Returns the descriptors measure type as a String.
  const String& type() const
    { return itsMeasType.type(); }

  // Returns the reference code for this object given a string.  Throws
  // an exception if the refString is invalid for this object.
  uInt refCode (const String& refString) const
    { return itsMeasType.refCode(refString); }

  // Translates the refCode for the descriptors measure type.
  const String& refType (uInt refCode) const
    { return itsMeasType.refType(refCode); }

  // Return the Units of the Measure values
  const Vector<Unit>& getUnits() const
    { return itsUnits; }

  // Reset the refCode, offset, or units.
  // It overwrites the value used when defining the TableMeasDesc.
  // It is only possible if it was defined as fixed for the entire column.
  // <group>
  void resetRefCode (uInt refCode)
    { itsRef.resetRefCode (refCode); }
  void resetOffset (const Measure& offset)
    { itsRef.resetOffset (offset); }
  void resetUnits (const Vector<Unit>& units);
  // </group>

  // Reconstructs the object for the given table and column name.
  static TableMeasDescBase* reconstruct (const Table& tab,
					 const String& columnName);

  // Does this column contain table measures?
  static Bool hasMeasures (const TableColumn& column);

protected:
  // Set the initial reference codes and types in the table.
  void initTabRef (const MeasureHolder& measHolder);

  // Set the measure and possible units.
  void setMeasUnits (const Measure& meas,
		     const Vector<Quantum<Double> >& val,
		     const Vector<Unit>& units);

private:
  TableMeasValueDesc itsValue;    //# The measure value column.
  TableMeasRefDesc   itsRef;	  //# The reference.
  //# this gives access to the columns Measure type etc
  TableMeasType itsMeasType;
  Vector<Unit>  itsUnits;
};



} //# NAMESPACE CASACORE - END

#endif
