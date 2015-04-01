//# ScalarQuantColumn.h: Access to a Scalar Quantum Column in a table.
//# Copyright (C) 1997,1998,1999,2000,2001
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

#ifndef MEASURES_SCALARQUANTCOLUMN_H
#define MEASURES_SCALARQUANTCOLUMN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/Quantum.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Table;
template <class T> class ScalarColumn;
class String;
class Unit;


// <summary>
// Provides access to Scalar Quantum Columns in Tables.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1999/12/23" tests="tTableQuantum.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableQuantumDesc>TableQuantumDesc</linkto>
//   <li> <linkto class=Table>Table</linkto>
//   <li> <linkto class=ScalarColumn>ScalarColumn</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>

// <synopsis>
// The ScalarQuantColumn class provides read/write access to quanta
// stored in a scalar Quantum Table column.  The Quantum column should
// already exist in the table and would have been defined by means of a
// <linkto class=TableQuantumDesc>TableQuantumDesc object</linkto>.
// In addition,
// for a ScalarQuantColumn object to be useful the column should
// contain Quanta.
//
// A ScalarQuantColumn object is used much in the same way as a
// <linkto class=ScalarColumn>ScalarColumn</linkto> object.
//
// <h3>Quantum Units</h3></A>
// Quanta retrieved from the column will normally have the Unit that was
// specified when the Quantum column was defined.
// However, it is possible to override the default column Unit by
// supplying a Unit in the ScalarQuantColumn constructor.
// When constructed in this fashion the retrieved Quanta will by
// default be retrieved in this unit, i.e. they will by default be
// converted to this unit.
// <br>
// By giving a unit (as a Unit or Quantum object) to a get function,
// the data can be retrieved in another unit than the default.
// </synopsis>

// <example>
// <srcblock>
//     Quantum<Double> q(5.3, "keV");
//     // "QuantScalar" has previously been defined as a Quantum column
//     // by means of a TableQuantumDesc. This example assumes the column
//     // already contains quanta.
//     ScalarQuantColumn<Double> qCol(qtab, "QuantScalar");
//     // return and print quanta as stored in the column
//     for (i = 0; i < qtab.nrow(); i++) {
//         cout << qCol(i) << endl;
//     }
//     // The following retrieves and converts the quanta to GHz.  They
//     // are then divided by the Quantum constant QC::h (Planck).
//     for (i=0; i < qtab.nrow(); i++) {
//         cout << (qCol(i, "GHz"))/(QC::h);
//     }
// </srcblock>
// </example>

// <motivation>
// Add support for Quanta in the Tables system.
// </motivation>

// <thrown>
//    <li>TableInvOper if the Table column is null.
// </thrown>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// <li> Functions like getColumn
// </todo>


template<class T> class ScalarQuantColumn
{
public:
  // The default constructor creates a null object. It is useful for creating
  // arrays of ScalarQuantColumn objects. Attempting to use a null object
  // will produce a segmentation fault so care needs to be taken to
  // initialise the objects by using the attach() member before any attempt
  // is made to use the object.  The isNull() member can be used to test
  // if a ScalarQuantColumn object is null.
  ScalarQuantColumn();

  // Create the ScalarQuantColumn from the specified table and column name.
  // The default unit for data retrieved is the unit in which they were stored.
  ScalarQuantColumn (const Table& tab, const String& columnName);

  // Create the ScalarQuantColumn from the specified table and column name.
  // The default unit for data retrieved is the given unit (the data is
  // converted as needed).
  ScalarQuantColumn (const Table& tab, const String& columnName,
                     const Unit&);

  // Copy constructor (copy semantics).
  ScalarQuantColumn (const ScalarQuantColumn<T>& that);

  ~ScalarQuantColumn();

  // Change the reference to another column.
  void reference (const ScalarQuantColumn<T>& that);

  // Attach a column to the object. Optionally supply a default unit
  // which has the same meaning as the constructor unit argument.
  // <group name="attach">
  void attach (const Table& tab, const String& columnName);
  void attach (const Table& tab, const String& columnName, const Unit&);
  // </group>

  // Get the quantum stored in the specified row.
  // <group name="get">
  void get (uInt rownr, Quantum<T>& q) const;
  // Get the quantum in the specified row, converted to the given unit.
  void get (uInt rownr, Quantum<T>& q, const Unit&) const;
  // Get the quantum in the specified row, converted to the unit in other.
  void get (uInt rownr, Quantum<T>& q, const Quantum<T>& other) const;
  // </group>

  // Return the quantum stored in the specified row.
  // <group>
  Quantum<T> operator() (uInt rownr) const;
  // Return the quantum stored in the specified row, converted to the
  // given unit.
  Quantum<T> operator() (uInt rownr, const Unit&) const;
  // Return the quantum in the specified row, converted to the unit in
  // other.
  Quantum<T> operator() (uInt rownr, const Quantum<T>& other) const;
  // </group>

  // Put a quantum into the table.  If the column supports variable units
  // the q's unit is stored into the unit column defined in the
  // TableQuantumDesc object.  If units are fixed for the column, the
  // quantum is converted as needed.
  void put (uInt rownr, const Quantum<T>& q);

  // Test whether the Quantum column has variable units
  Bool isUnitVariable() const
    { return (itsUnitsCol != 0); }

  // Returns the column's value for Units as a string.
  // An empty string is returned if the column has variable units.
  const String& getUnits() const
    { return itsUnit.getName(); }

  // Test if the object is null.
  Bool isNull() const
    { return (itsDataCol == 0); }

  // Throw an exception if the object is null.
  void throwIfNull() const;

protected:
  //# Quantum column's units (if units not variable)
  Unit itsUnit;

  // Get access to itsUnitsCol.
  const ScalarColumn<String>* unitsCol() const
    { return itsUnitsCol; }

private:
  //# The underlying data column stores the quantum column's data.
  ScalarColumn<T>* itsDataCol;
  //# Variable units column if applicable.
  ScalarColumn<String>* itsUnitsCol;
  //# Unit to retrieve the data in.
  Unit itsUnitOut;
  //# Convert unit when getting data?
  Bool itsConvOut;


  // Assignment makes no sense in a read only class.
  // Declaring this operator private makes it unusable.
  ScalarQuantColumn& operator= (const ScalarQuantColumn<T>& that);

  // Comparison is not defined, since its semantics are unclear.
  Bool operator== (const ScalarQuantColumn<T>& that);

  // Initialize the ScalarQuantColumn from the specified table and column.
  void init (const Table& tab, const String& columnName);

  // Deletes allocated memory etc. Called by destructor and any member
  // which needs to reallocate data.
  void cleanUp();

  // Get the data without possible conversion.
  void getData (uInt rownr, Quantum<T>& q) const;
};

} //# NAMESPACE CASACORE - END


//# Make old name ROScalarMeasColumn still available.
#define ROScalarQuantColumn ScalarQuantColumn


#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/measures/TableMeasures/ScalarQuantColumn.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
