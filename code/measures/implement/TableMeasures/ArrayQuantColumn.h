//# ArrayQuantColumn.h: Provide access to array quantum columns.
//# Copyright (C) 1997,1998
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

#if !defined(AIPS_ARRQUANTUMCOL_H)
#define AIPS_ARRQUANTUMCOL_H

//# Includes

//# Forward Declarations
class String;
class Table;
class TableQuantumDesc;
class Unit;
template <class T> class ArrayColumn;
template <class T> class ScalarColumn;
template <class Qtype> class Quantum;

// <summary>
// Provides read-only access retrieving array  Quantum columns in Tables.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableQuantum.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Table>Table</linkto>
//   <li> <linkto class=ROArrayColumn>ROArrayColumn</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>

// <synopsis>
// The ROArrayQuantColumn class provides read-only access to array Quantum
// table columns.  The array Quantum column should 
// exist in the table, having be defined in advance by use of a 
// <linkto class=TableQuantumDesc>TableQuantumDesc</linkto> object.  
// In addition,
// for a ROArrayQuantColumn object to be useful the column should
// contain Quanta.  Inserting Quanta into column requires the use of a
// <linkto class=ArrayQuantColumn">ArrayQuantColumn</A> 
// object.<br>
//
// The ROArrayQuantColumn class is the array version
// of the <linkto class=ROScalarQuantColumn>ROScalarQuantColumn</linkto> 
// class.
// </synopsis>

// <example>
// (See <linkto class=TableQuantumDesc>TableQuantumDesc</linkto> class
// for an example of how to define a Quantum column).
// <srcblock>
//    // Create the column object with default units "deg".
//    // It gets the quantum array from row 0 and prints it to stdout.
//    ROArrayQuantColumn<Double> roaqCol(qtab, "ArrQuantDouble", "deg");
//    cout << roaqCol(0) << endl;
//    // This retrieves the same array with units converted to "m/s".	
//    cout << roaqCol(0, "m/s") << endl;
// </srcblock>
// </example>

// <motivation>
// Add support for Quanta in the Tables system.
// </motivation>
//
// <thrown>
//    <li>TableInvOper if the Table column is null.
// </thrown>
//
//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>

template<class Qtype> class ROArrayQuantColumn
{
public:
    // The default constructor creates a null object which is useful if an
    // array of ROArrayQuantColumn() objects is needed.  attach() should be
    // used to attach a real column to the ROArraQuantColumn before any 
    // attempt is made to read from the column which would result in a
    // segmentation fault as no check is performed on the object before it
    // is used.  The isNull() member can be used to test if the
    // ROArrayQuantColumn is null.
    ROArrayQuantColumn();

    // Create the ROArrayQuantColumn from the supplied table and column name.
    ROArrayQuantColumn(const Table& tab, const String& columnName);

    // Create the ROArrayQuantColumn from the supplied table and column name.
    // Units for the Quantum are specified in u. This over-rides the default
    // Units for the column.  Retrieved quanta are not converted
    // to 'u', they are simply returned with 'u' units as if that was how 
    // they were stored. This could be useful if a Quantum column was 
    // created with an inappropriate or undefined value for Units.
    ROArrayQuantColumn(const Table& tab, const String& columnName, 
	    	       const Unit& u);

    // Copy constructor (copy semantics).
    ROArrayQuantColumn(const ROArrayQuantColumn& that);

    ~ROArrayQuantColumn();

    // Make this object reference the column in "that".
    void reference(const ROArrayQuantColumn<Qtype>& that);

    // Attach a column to the object. Optionally supply a default unit.
    // <group name="attach">
    void attach(const Table& tab, const String& columnName); 
    void attach(const Table& tab, const String& columnName, const Unit& u);
    // </group>
 
    // Get the quantum array in the specified row.
    // <group name="get">
    void get(Array<Quantum<Qtype> >& q, uInt rownr, Bool resize = False) const;
    // Get the quantum array in the specified row. Each quantum is
    // converted to Units in s.
    void get(Array<Quantum<Qtype> >& q,  uInt rownr, const Unit& s, 
	     Bool resize = False) const;
    // Get the quantum array in the specified row. Each quantum is
    // converted to Units in other.
    void get(Array<Quantum<Qtype> >& q, uInt rownr, 
	     const Quantum<Qtype> &other, Bool resize = False) const;
    // </group>
    
    // Return the quantum array stored in the specified row.
    // <group>
    Array<Quantum<Qtype> > operator()(uInt rownr) const;
    // Return the quantum array stored in the specified row. Each
    // quantum is converted to Units in s.
    Array<Quantum<Qtype> > operator()(uInt rownr, const Unit &s) const;
    // Return the quantum array stored in the specified row. Each
    // quantum is converted to Units in other.
    Array<Quantum<Qtype> > operator()(uInt rownr, 
	    			      const Quantum<Qtype> &other) const;
    // </group>
      
    // Test whether the Quantum column has variable units
    Bool isUnitVariable() const { 
	return ((itsArrUnitsCol || itsScaUnitsCol) ? True : False); }
    
    // Returns the column's units as a string.
    const String& getUnits() const { return itsUnit.getName(); }
    
    // Test if the object is null.
    Bool isNull() const { return (itsDataCol == 0 ? True : False); }
    
    // Throw an exception if the object is null.
    void throwIfNull() const;
    
protected:
    //# Quantum column's units (if units not variable)
    Unit itsUnit;    	    	    	
    //# The underlying data array column.
    ArrayColumn<Qtype>* itsDataCol;    
    
    //# A template of the stored Quanta.  Useful when getting and putting
    //# of quanta but perhaps unnecessary.
    Quantum<Qtype> itsQuantum;
        
    //# Variable units array column if applicable.
    ArrayColumn<String>* itsArrUnitsCol;
    
    //# Variable units scalar column if applicable.
    ScalarColumn<String>* itsScaUnitsCol;

private:
    // Deletes allocated memory etc. Called by ~tor and any member which needs
    // to reallocate data.
    void cleanUp();

    // Assignment makes no sense in a read only class.
    // Declaring this operator private makes it unusable.
    ROArrayQuantColumn& operator= (const ROArrayQuantColumn<Qtype>& that);    
};

// <summary>
// A class for reading and writing Quantum Array columns in Tables.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableQuantum.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Table>Table</linkto>
//   <li> <linkto class=ArrayColumn>ArrayColumn</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>

// <synopsis>
// The ArrayQuantColumn class provides read/write access to array Quantum 
// Table columns.  The column should exist in the table and would have
// been defined as a Quantum column by means of a
// <linkto class=TableQuantumDesc>TableQuantumDesc</linkto> object.
// In addition to the operations provided by its read-only partner,
// <linkto class=ROArrayQuantColumn>ROArrayQuantColumn</linkto>, use 
// of a ArrayQuantColumn object allows the insertion of arrays of Quanta 
// into a column.<br>
//
// The treatment of Quanta Units in ArrayQuantColumns varies according
// to how the ArrayQuantColumn was defined.  See 
// <linkto class=TableQuantDesc>TableQuantDesc> for the details.
// </synopsis>

// <example>
// (See <linkto class=TableQuantumDesc>TableQuantumDesc</linkto> class
// for an example of how to define a Quantum column).
// <srcblock>
//    // This create the Quantum array object.
//    ArrayQuantColumn<Double> aqCol(qtab, "ArrQuantDouble");
//
//    // Test if the column has variable of fixed units
//    if (aqCol.isUnitVariable()) 
//        cout << "Quantum column supports variable units!" << endl;
//    else 
//        cout << "Unit for the column is: ", << aqCol.getUnits() << endl;
//
//    // need an array of Quanta.
//    IPosition shape(2, 3, 2);
//    Array<Quantum<Double> > qArr(shape);
//    Bool deleteIt;
//    Quantum<Double>* q_p = qArr.getStorage(deleteIt);
//    q_p->setValue(1.41212);
//    q_p->setUnit("GHz");
//    q_p++;
//    q_p->setValue(1.4921);
//    q_p->setUnit("deg");
//    q_p++;    
//    q_p->setValue(1.4111);
//    q_p->setUnit("ms-1");
//    q_p++;    
//    q_p->setValue(1.4003);
//    q_p->setUnit("Jy");
//    q_p++;    
//    q_p->setValue(1.22);
//    q_p->setUnit("GHz");
//    q_p++;    
//    q_p->setValue(1.090909);
//    q_p->setUnit("g");	
//    qArr.putStorage(q_p, deleteIt);
// 	
//    // put the quantum array in the column at row 0.  If the column has
//    // fixed units the Unit component of each quantum will be lost.  Just
//    // their values will be stored.
//    aqCol.put(0, qArr);
// </srcblock>
// </example>

// <motivation>
// Add support for Quanta in the Tables system.
// </motivation>
//
// <thrown>
//    <li>TableInvOper if the Table column is null.
// </thrown>
//
//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// Is there a better way of dealing with fixed unit columns?  When putting a
// Quantum into a column should the unit be checked and converted or perhaps
// it should be illegal to add quanta with incorrect units?
//# </todo>

template<class Qtype> class ArrayQuantColumn : public ROArrayQuantColumn<Qtype>
{
public:
    // The default constructor creates a null object only useful if an
    // array of ArrayQuantColumn() object is needed.  attach() should be
    // used to attach a real column to the ArrayQuantColumn before any 
    // attempt is made to read from the column.  The isNull() member can 
    // be used to test if the ArrayQuantColumn is null.
    ArrayQuantColumn();

    // Create the ArrayQuantColumn from the supplied table and column name.
    ArrayQuantColumn(const Table& tab, const String& columnName);

    // Create the ROArrayQuantColumn from the supplied table and column name.
    // Units for the Quantum are specified in u and over-rides the default
    // units for the column.  Retrieved Quanta are not converted
    // to 'u', they are simply returned with 'u' units as if that was how 
    // they were stored. This could be useful if a Quantum column was created 
    // with an inappropriate or undefined value for units.
    ArrayQuantColumn(const Table& tab, const String& columnName, 
		     const Unit& u);

    // Copy constructor (copy semantics).
    ArrayQuantColumn(const ArrayQuantColumn& that);

    ~ArrayQuantColumn();

    // Make this object reference the column in "that".
    void reference(const ArrayQuantColumn<Qtype>& that);

    // Attach a column to the object. Optionally supply a default unit.
    // <group name="attach">
    void attach(const Table& tab, const String& columnName); 
    void attach(const Table& tab, const String& columnName, const Unit& u);
    // </group>
 
    // Add an array of quanta in q to the specified row to the table.
    // If the column supports variable units units are stored as well as
    // quanta value otherwise units are disregarded (i.e., they are 
    // assumed to conform).
    void put(uInt rownr, const Array<Quantum<Qtype> >& q);
    
private:
    // reference() can be used for assignment.
    // Declaring this operator private makes it unusable.
    ArrayQuantColumn& operator= (const ArrayQuantColumn<Qtype>& that);
};

#endif
