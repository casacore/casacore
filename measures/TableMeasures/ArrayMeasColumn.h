//# ArrayMeasColumn.h: Access to array Measure columns in Tables.
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

#ifndef MEASURES_ARRAYMEASCOLUMN_H
#define MEASURES_ARRAYMEASCOLUMN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/TableMeasures/TableMeasColumn.h>
#include <casacore/measures/Measures/MeasRef.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class ArrayColumn;
template <class T> class ScalarColumn;
template <class T> class Array;
template <class M> class ScalarMeasColumn;


// <summary>
// Read only access to table array Measure columns.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1999/12/23" tests="tTableMeasures.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
//   <li> TableMeasDesc
// </prerequisite>

// <synopsis>
// ArrayMeasColumn objects can be used to access array Measure Columns
// in tables, both for reading and writing (if the table is writable).
//
// Before a column can be accessed it must have previously been defined as
// a Measure column by use of the
// <linkto class="TableMeasDesc">TableMeasDesc</linkto> object.
//
// The ArrayMeasColumn class is templated on Measure type and MeasValue
// type but typedefs exist for making declaration less long winded. The
// Measure classes (like MEpoch) have typedefs <src>ArrayColumn</src>
// to assist in creating ArrayMeasColumn objects.
//
// Constructing array Measure column objects using these typedefs looks like
// this:
// <srcblock>
// // Read/write MEpoch array column
// MEpoch::ArrayColumn ec(table, "ColumnName);
// </srcblock>
//
// <h3>Reading and writing Measures</h3>
//
// The reading and writing of Measures columns is very similar to reading and
// writing of "ordinary" Table columns.
// <linkto class="ArrayMeasColumn#get">get()</linkto>
// and <linkto class="ArrayMeasColumn#get">operator()</linkto>
// exist for reading Measures and the
// <linkto class="ArrayMeasColumn#put">put()</linkto> member for adding
// Measures to a column.  (put() is obviously not defined for
// ScalarMeasColumn objects.)  Each of these members accepts a row number
// as an argument.
//
// Care needs to be taken when adding Measures to a column.  The user needs
// to be aware that Measures are not checked for consistency, with respect to
// a Measure's reference, between the Measures being added to a column and
// the column's predefined Measure reference.  This is only an issue if the
// Measure column was defined to have a fixed reference. For such columns
// the reference component of Measures added to a column is silently
// ignored, that is, there is no warning nor is there any sort of conversion
// to the column's reference should the reference of the added Measure be
// different from the column's reference.  The functions
// <linkto class="TableMeasDescBase#isRefCodeVariable()">
// TableMeasDescBase::isRefVariable()</linkto>
// and
// <linkto class="ArrayMeasColumn#getMeasRef()">
// ArrayMeasColumn::getMeasRef()</linkto> can be
// used to discover a Measure column's Measure reference characteristics.
// </synopsis>

// <example>
// <srcblock>
//    // create an MEpoch array column object
//    ArrayMeasColumn<MEpoch> arrayCol;
//
//    // should be null.  Can test this and attach a real MEpoch column
//    // The column Time1Arr should exist in the table and would have been
//    // defined by using a TableMeasDesc
//    if (arrayCol.isNull()) {
//	    arrayCol.attach(tab, "Time1Arr");
//    }
//
//    // This would throw an Exception if the object is still null....but
//    // hopefully won't
//    arrayCol.throwIfNull();
//
//    // create a vector of MEpochs
//    MEpoch last(Quantity(13.45, "h"), MEpoch::Ref(MEpoch::TAI));
//    Vector<MEpoch> ev(10);
//    for (uInt i=0; i<10; i++) {
//        last.set(Quantity(13.45 + i, "h"));
//        ev(i) = last;
//    }
//
//    // before adding something check the isDefined() member
//    if (!arrayCol.isDefined(0)) {
//        cout << "PASS - nothing in the measure array column row yet\n";
//    } else {
//        cout << "FAIL - there shouldn't be a valid value in the row!\n";
//    }
//
//    // add the MEpoch vector to the array Measure column at row 0
//    arrayCol.put(0, ev);
//
//    // now read the array from the row.  Could use same object to do this
//    // but here we'll create a ArrayMeasColumn<MEpoch> to do this
//    ArrayMeasColumn<MEpoch> roArrCol(tab, "Time1Arr");
//
//    // need a vector to put the MEpochs into
//    Vector<MEpoch> ew;
//
//    // setting the resize parameter to True automatically sets ew to the
//    // same shape as the array contained in the row
//    arrayCol.get(0, ew, True);
// </srcblock>
// </example>

// <motivation>
// The standard Casacore Table system does not support array Measure columns.
// This class overcomes this limitation.
// </motivation>

// <thrown>
//    <li>ArrayConformanceError during get() if supplied array is the wrong
//        shape.
// </thrown>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


template<class M> class ArrayMeasColumn : public TableMeasColumn
{
public:
  // The default constructor creates a null object.  Useful for creating
  // arrays of ArrayMeasColumn objects.  Attempting to use a null object
  // will produce a segmentation fault so care needs to be taken to
  // initialise the objects by using the attach() member before any attempt
  // is made to use the object.  A ArrayMeasColumn object can be tested
  // if it is null by using the isNull() member.
  ArrayMeasColumn();

  // Create the ArrayMeasColumn from the table and column Name.
  ArrayMeasColumn (const Table& tab, const String& columnName);

  // Copy constructor (copy semantics).
  ArrayMeasColumn (const ArrayMeasColumn<M>& that);

  virtual ~ArrayMeasColumn();

  // Change the reference to another column.
  void reference (const ArrayMeasColumn<M>& that);

  // Attach a column to the object.
  void attach (const Table& tab, const String& columnName);

  // Get the Measure array in the specified row.  For get() the supplied
  // array's shape should match the shape in the row unless resize is True.
  // <group name=get>
  void get (uInt rownr, Array<M>& meas, Bool resize = False) const;
  Array<M> operator() (uInt rownr) const;
  // </group>

  // Get the Measure array contained in the specified row and convert
  // it to the reference and offset found in the given measure.
  Array<M> convert (uInt rownr, const M& meas) const
    { return convert (rownr, meas.getRef()); }

  // Get the Measure array contained in the specified row and convert
  // it to the given reference.
  // <group>
  Array<M> convert (uInt rownr, const MeasRef<M>& measRef) const;
  Array<M> convert (uInt rownr, uInt refCode) const;
  // </group>

  // Get the column's reference.
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

  // Add a Measure array to the specified row.
  // <group name=put>
  void put (uInt rownr, const Array<M>&);
  // </group>

protected:
  //# Its measure reference when the MeasRef is constant per row.
  MeasRef<M> itsMeasRef;

private:
  //# Column which contains the Measure's actual data.
  ArrayColumn<Double>* itsDataCol;
  //# Its MeasRef code column when references are variable.
  ScalarColumn<Int>* itsRefIntCol;
  ArrayColumn<Int>* itsArrRefIntCol;
  //# Its MeasRef column when references are variable and stored as Strings.
  ScalarColumn<String>* itsRefStrCol;
  ArrayColumn<String>* itsArrRefStrCol;
  //# Column containing its variable offsets.  Only applicable if the
  //# measure references have offsets and they are variable.
  ScalarMeasColumn<M>* itsOffsetCol;
  ArrayMeasColumn<M>* itsArrOffsetCol;


  // Assignment makes no sense in a read only class.
  // Declaring this operator private makes it unusable.
  ArrayMeasColumn& operator= (const ArrayMeasColumn<M>& that);

  // Deletes allocated memory etc. Called by ~tor and any member which needs
  // to reallocate data.
  void cleanUp();

  // Get the data and convert using conversion engine.
  Array<M> doConvert (uInt rownr, typename M::Convert& conv) const;
};


} //# NAMESPACE CASACORE - END


//# Make old name ROArrayMeasColumn still available.
#define ROArrayMeasColumn ArrayMeasColumn


#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/measures/TableMeasures/ArrayMeasColumn.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
