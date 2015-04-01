//# ArrayQuantColumn.h: Access to an Array Quantum Column in a table.
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

#ifndef MEASURES_ARRAYQUANTCOLUMN_H
#define MEASURES_ARRAYQUANTCOLUMN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/Quantum.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Table;
template <class T> class ArrayColumn;
template <class T> class ScalarColumn;
class String;


// <summary>
// Provides read/write access to Array Quantum columns in Tables.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1999/12/23" tests="tTableQuantum.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableQuantumDesc>TableQuantumDesc</linkto>
//   <li> <linkto class=Table>Table</linkto>
//   <li> <linkto class=ArrayColumn>ArrayColumn</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>

// <synopsis>
// The ArrayQuantColumn class provides read/write access to quanta
// stored in a array Quantum Table column.  The Quantum column should
// already exist in the table and would have been defined by means of a
// <linkto class=TableQuantumDesc>TableQuantumDesc object</linkto>.
// In addition,
// for a ArrayQuantColumn object to be useful the column should
// contain Quanta.
//
// The ArrayQuantColumn class is the array version
// of the <linkto class=ScalarQuantColumn>ScalarQuantColumn</linkto>
// class.
//
// <h3>Quantum Units</h3></A>
// Quanta retrieved from the column will normally have the Unit that was
// specified when the Quantum column was defined.
// However, it is possible to override the default column Unit by
// supplying a Unit in the ArrayQuantColumn constructor.
// When constructed in this fashion the retrieved Quanta will by
// default be retrieved in this unit, i.e. they will by default be
// converted to this unit.
// <br>
// By giving a unit (as a Unit or Quantum object) to a get function,
// the data can be retrieved in another unit than the default.
// </synopsis>

// <example>
// (See <linkto class=TableQuantumDesc>TableQuantumDesc</linkto> class
// for an example of how to define a Quantum column).
// <srcblock>
//    // Create the column object with default units "deg".
//    // It gets the quantum array from row 0 and prints it to stdout.
//    ArrayQuantColumn<Double> roaqCol(qtab, "ArrQuantDouble", "deg");
//    cout << roaqCol(0) << endl;
//    // This retrieves the same array with units converted to "m/s".	
//    cout << roaqCol(0, "m/s") << endl;
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
// <li> Support for fixed unit per array element (e.g. for positions)
//      In that case #units should match first array dimension.
// <li> Functions like getColumn, getSlice.
// <li> get as <src>Quantum<Array<T>></src>.
// <li> optimize when converting when units are the same for entire array.
// </todo>


template<class T> class ArrayQuantColumn
{
public:
  // The default constructor creates a null object. It is useful for creating
  // arrays of ArrayQuantColumn objects. Attempting to use a null object
  // will produce a segmentation fault so care needs to be taken to
  // initialize the objects by using the attach() member before any attempt
  // is made to use the object.  The isNull() member can be used to test
  // if a ArrayQuantColumn object is null.
  ArrayQuantColumn();

  // Create the ArrayQuantColumn from the supplied table and column name.
  // The default unit for data retrieved is the unit in which they were stored.
  ArrayQuantColumn (const Table& tab, const String& columnName);

  // Create the ArrayQuantColumn from the supplied table and column name.
  // The default unit for data retrieved is the given unit (the data is
  // converted as needed).
  // <group>
  ArrayQuantColumn (const Table& tab, const String& columnName,
                    const Unit&);
  ArrayQuantColumn (const Table& tab, const String& columnName,
                    const Vector<Unit>&);
  // </group>

  // Copy constructor (copy semantics).
  ArrayQuantColumn (const ArrayQuantColumn<T>& that);

  ~ArrayQuantColumn();

  // Make this object reference the column in "that".
  void reference (const ArrayQuantColumn<T>& that);

  // Attach a column to the object. Optionally supply a default unit.
  // which has the same meaning as the constructor unit argument.
  // <group name="attach">
  void attach (const Table& tab, const String& columnName);
  void attach (const Table& tab, const String& columnName,
	       const Unit&);
  void attach (const Table& tab, const String& columnName,
	       const Vector<Unit>&);
  // </group>

  // Get the quantum array in the specified row.
  // If resize is True the resulting array is resized if its shape
  // is not correct. Otherwise a "conformance exception" is thrown
  // if the array is not empty and its shape mismatches.
  // <group name="get">
  void get (uInt rownr, Array<Quantum<T> >& q, Bool resize = False) const;
  // Get the quantum array in the specified row. Each quantum is
  // converted to the given unit.
  void get (uInt rownr, Array<Quantum<T> >& q,
	    const Unit&, Bool resize = False) const;
  // Get the quantum array in the specified row. Each quantum is
  // converted to the given units.
  void get (uInt rownr, Array<Quantum<T> >& q,
	    const Vector<Unit>&, Bool resize = False) const;
  // Get the quantum array in the specified row. Each quantum is
  // converted to the unit in other.
  void get (uInt rownr, Array<Quantum<T> >& q,
	    const Quantum<T>& other, Bool resize = False) const;
  // </group>

  // Return the quantum array stored in the specified row.
  // <group>
  Array<Quantum<T> > operator() (uInt rownr) const;
  // Return the quantum array stored in the specified row, converted
  // to the given unit.
  Array<Quantum<T> > operator() (uInt rownr, const Unit&) const;
  // Return the quantum array stored in the specified row, converted
  // to the given units.
  Array<Quantum<T> > operator() (uInt rownr, const Vector<Unit>&) const;
  // Return the quantum array stored in the specified row, converted
  // to the unit in other.
  Array<Quantum<T> > operator() (uInt rownr, const Quantum<T>& other) const;
  // </group>

  // Put an array of quanta into the specified row of the table.
  // If the column supports variable units, the units are stored as well.
  // Otherwise the quanta are converted to the column's units.
  void put (uInt rownr, const Array<Quantum<T> >& q);

  // Test whether the Quantum column has variable units
  Bool isUnitVariable() const
    { return (itsArrUnitsCol || itsScaUnitsCol); }

  // Returns the column's units as a vector of strings.
  // An empty vector is returned if the column has no fixed units.
  Vector<String> getUnits() const;

  // Test if the object is null.
  Bool isNull() const
    { return (itsDataCol == 0); }

  // Throw an exception if the object is null.
  void throwIfNull() const;

protected:
  //# Quantum column's units (if units not variable)
  Vector<Unit> itsUnit;    	    	    	

  // Get access to itsUnitsCol.
  // <group>
  const ArrayColumn<String>* arrUnitsCol() const
    { return itsArrUnitsCol; }
  const ScalarColumn<String>* scaUnitsCol() const
    { return itsScaUnitsCol; }
  // </group>


private:
  //# The underlying data column stores the quantum column's data.
  ArrayColumn<T>* itsDataCol;
  //# Variable units array column if applicable.
  ArrayColumn<String>*  itsArrUnitsCol;
  //# Variable units scalar column if applicable.
  ScalarColumn<String>* itsScaUnitsCol;
  //# Units to retrieve the data in.
  Vector<Unit> itsUnitOut;
  //# Convert unit when getting data?
  Bool itsConvOut;


  // Initialize the ArrayQuantColumn from the specified table and column.
  void init (const Table& tab, const String& columnName);

  // Deletes allocated memory etc. Called by ~tor and any member which needs
  // to reallocate data.
  void cleanUp();

  // Get the data without possible conversion.
  void getData (uInt rownr, Array<Quantum<T> >& q, Bool resize) const;

  // Assignment makes no sense in a read only class.
  // Declaring this operator private makes it unusable.
  ArrayQuantColumn& operator= (const ArrayQuantColumn<T>& that);

  // Comparison is not defined, since its semantics are unclear.
  Bool operator== (const ArrayQuantColumn<T>& that);
};

} //# NAMESPACE CASACORE - END


//# Make old name ROArrayMeasColumn still available.
#define ROArrayQuantColumn ArrayQuantColumn


#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/measures/TableMeasures/ArrayQuantColumn.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
