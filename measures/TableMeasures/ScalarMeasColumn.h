//# ScalarMeasColumn.h: Access to Scalar Measure Columns in Tables.
//# Copyright (C) 1997,1998,1999,2000
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

#ifndef MEASURES_SCALARMEASCOLUMN_H
#define MEASURES_SCALARMEASCOLUMN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/TableMeasures/TableMeasColumn.h>
#include <casacore/measures/Measures/MeasRef.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class ArrayColumn;
template <class T> class ScalarColumn;


// <summary>
// Read only access to table scalar Measure columns.
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
// ScalarMeasColumn objects can be used to access scalar Measure Columns
// in tables, both for reading and writing (if the table is writable).
//
// Before a column can be accessed it must have previously been defined as
// a Measure column by use of the
// <linkto class="TableMeasDesc">TableMeasDesc</linkto> object.
//
// The ScalarMeasColumn class is templated on Measure type.
// Typedefs exist in the various Measure classes
// (e.g. <linkto class=MEpoch>MEpoch</linkto>) to make declaration
// less long winded.
// Constructing scalar Measure column objects using these typedefs looks like
// this:
// <srcblock>
// MEpoch::ScalarMeasColumn ec(table, "ColumnName);
// </srcblock>
//
// <h3>Reading and writing Measures</h3>
//
// The reading and writing of Measures columns is very similar to reading and
// writing of "ordinary" Table columns.
// <linkto class="ScalarMeasColumn#get">get()</linkto>
// and <linkto class="ScalarMeasColumn#get">operator()</linkto>
// exist for reading Measures and the
// <linkto class="ScalarMeasColumn#put">put()</linkto> member for adding
// Measures to a column.  (put() is obviously not defined for
// ScalarMeasColumn objects.)  Each of these members accepts a row number
// as an argument.
// The get() function gets the measure with the reference and offset as
// it is stored in the column. Furthermore the convert() function is
// available to get the measure with the given reference, possible offset,
// and possible frame
//
// When a Measure is put, the reference and possible offset are converted
// if the measure column is defined with a fixed reference and/or offset.
// If the column's reference and offset are variable, the reference and
// offset of the measure as put are written into the appropriate
// reference and offset columns.
// </synopsis>

// <example>
// <srcblock>
//     // This creates a Scalar MEpoch column for read/write access.  Column
//     // "Time1" must exist in Table "tab" and must have previously been
//     // defined as a MEpoch column using a TableMeasDesc.
//     MEpoch::ScalarMeasColumn timeCol(tab, "Time1");
// 	
//     // print some details about the column
//     if (timeCol.measDesc().isRefCodeVariable()) {
//        cout << "The column has variable references." << endl;
//     } else {
//         cout << "The fixed MeasRef for the column is: "
//	        << timeCol.getMeasRef() << endl;
//     }
//
//     // Add tab.nrow() measures to the column.	
//     MEpoch tm(Quantity(MeasData::MJD2000, "d"), MEpoch::TAI);
//     for (uInt i=0; i<tab.nrow(); i++) {
//         timeCol.put(i, tm);
//     }
//
//     // We could read from the column using timeCol but instead a read
//     // only column object is created.
//     MEpoch::ScalarMeasColumn timeColRead(tab, "Time1");
//     for (i=0; i<tab.nrow(); i++) {
//         cout << timeColRead(i) << endl;
//     }
// </srcblock>
// </example>

// <motivation>
// The standard Casacore Table system does not support Measures columns.
// This class overcomes this limitation.
// </motivation>
//
// <thrown>
//    <li>AipsError during construction if the column specified variable
//        offsets which are stored in an Array- rather than a ScalarColumn.
// </thrown>
//
//# <todo asof="$DATE:$">
//# </todo>

template <class M> class ScalarMeasColumn : public TableMeasColumn
{
public:
  // The default constructor creates a null object.  Useful for creating
  // arrays of ScalarMeasColumn objects.  Attempting to use a null object
  // will produce a segmentation fault so care needs to be taken to
  // initialize the objects first by using attach().
  // An ScalarMeasColumn object can be tested if it is null by using the
  // isNull() member.
  ScalarMeasColumn();

  // Create the ScalarMeasColumn from the table and column Name.
  ScalarMeasColumn (const Table& tab, const String& columnName);

  // Copy constructor (copy semantics).
  ScalarMeasColumn (const ScalarMeasColumn<M>& that);

  virtual ~ScalarMeasColumn();

  // Change the reference to another column.
  void reference (const ScalarMeasColumn<M>& that);

  // Attach a column to the object.
  void attach (const Table& tab, const String& columnName);

  // Get the Measure contained in the specified row.
  // It returns the Measure as found in the table.
  // <group name=get>
  void get (uInt rownr, M& meas) const;
  M operator() (uInt rownr) const;
  // </group>

  // Get the Measure contained in the specified row and convert
  // it to the reference and offset found in the given measure.
  M convert (uInt rownr, const M& meas) const
    { return convert (rownr, meas.getRef()); }

  // Get the Measure contained in the specified row and convert
  // it to the given reference.
  // <group>
  M convert (uInt rownr, const MeasRef<M>& measRef) const;
  M convert (uInt rownr, uInt refCode) const;
  // </group>

  // Returns the column's fixed reference or the reference of the last
  // read Measure if references are variable.
  const MeasRef<M>& getMeasRef() const
    { return itsMeasRef; }

  // Reset the refCode, offset, or units.
  // It overwrites the value used when defining the TableMeasDesc.
  // Resetting the refCode and offset can only be done if they were
  // defined as fixed in the description.
  // <note role=tip>
  // In principle the functions can only be used if the table is empty,
  // otherwise already written values have thereafter the incorrect
  // reference, offset, or unit.
  // However, it is possible that part of the table is already
  // written and that the entire measure column is filled in later.
  // In that case the reference, offset, or units can be set by using
  // a False <src>tableMustBeEmpty</src> argument.
  // </note>
  // <group>
  void setDescRefCode (uInt refCode, Bool tableMustBeEmpty=True);
  void setDescOffset (const Measure& offset, Bool tableMustBeEmpty=True);
  void setDescUnits (const Vector<Unit>& units, Bool tableMustBeEmpty=True);
  // </group>

  // Put a Measure into the given row.
  // <group name=put>
  void put (uInt rownr, const M& meas);
  // </group>

protected:
  // Make a MeasRef for the given row.
  MeasRef<M> makeMeasRef (uInt rownr) const;

private:
  //# Whether conversion is needed during a put.  True if either
  //# the reference code or offset is fixed for the column
  Bool itsConvFlag;
  //# Column which contains the Measure's actual data. An array column
  //# is needed if the data component of the underlying Measure is
  //# represented by more than 1 value
  ArrayColumn<Double>* itsArrDataCol;
  ScalarColumn<Double>* itsScaDataCol;
  //# Its MeasRef code column when references are variable.
  ScalarColumn<Int>* itsRefIntCol;
  ScalarColumn<String>* itsRefStrCol;
  //# Column containing its variable offsets. Only applicable if the
  //# measure references have offsets and they are variable.
  ScalarMeasColumn<M>* itsOffsetCol;
  //# This is either the column's fixed Measure reference or the reference
  //# of the last Measure read.
  MeasRef<M> itsMeasRef;


  // Assignment makes no sense in a readonly class.
  // Declaring this operator private makes it unusable.
  ScalarMeasColumn& operator= (const ScalarMeasColumn<M>& that);

  // Check if refs have the same value (as opposed to being the same object).
  Bool equalRefs (const MRBase& r1, const MRBase& r2) const;

  //# Deletes allocated memory etc. Called by ~tor and any member which
  //# needs to reallocate data.
  void cleanUp();
};


} //# NAMESPACE CASACORE - END


//# Make old name ROScalarMeasColumn still available.
#define ROScalarMeasColumn ScalarMeasColumn


#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/measures/TableMeasures/ScalarMeasColumn.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
