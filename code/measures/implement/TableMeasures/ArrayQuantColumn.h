//# ArrayQuantColumn.h: Provide access to array quantum columns.
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

#if !defined(AIPS_ARRQUANTUMCOL_H)
#define AIPS_ARRQUANTUMCOL_H

//# Includes

//# Forward Declarations
class TableQuantumDesc;
class Unit;
template <class T> class ArrayColumn;
template <class T> class ScalarColumn;
template <class Qtype> class Quantum;

// <summary>
// A class for retrieving arrays of Quantums in Tables.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableQuantum.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Quantum.h>Quantum</linkto>
//   <li> TableQuantumDesc
// </prerequisite>

// <synopsis>
// The ROArrayQuantColumn class is used to retrieve arrays of 
// Quantums from an Array Quantum Table column.  The column should previously 
// have been defined as a Quantum column by means of the 
// <linkto class=TableQuantumDesc>TableQuantumDesc class</linkto>.  The
// ROArrayQuantColumn class is for read only access of Quantums in the table.
// (The ArrayQuantColumn class exist for read/write access the column.)<br>
// The Quantums retreived from the ROArrayQuantColumn will have the units as
// specified when the TableQuantumDesc was used to create the Quantum column.
// If the unit is fixed, all Quantums retrieved will have this unit.
// Alternatively the units are variable in which case the unit of the quantum
// retrieve will be the same as when it was stored.
// This default unit value can be overriden by supplying a value for units 
// when constructing the ROScalarQuantColumn.<br>
// Quantums can also be converted to new units while retrieving in a way
// which mimics the conversion facilities offered by the Quantum class.
// </synopsis>

// <example>
// <srcblock>
//    // (see <linkto class=TableQuantumDesc>TableQuantumDesc class</linkto>
//    // for an example of how to create the TableQuantumDesc).
//    // This example create the column object with default units "deg".
//    // It gets the quantum array from row 0 and prints it to stdout.
//    ROArrayQuantColumn<Double> roaqCol(qtab, "ArrQuantDouble", "deg");
//    cout << roaqCol(0) << endl;
//    // This retrieves the same array with units converted to "m/s".	
//    cout << roaqCol(0, "m/s") << endl;
// </srcblock>
// </example>

// <motivation>
// Add support for Quantums in the Tables system.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>

template<class Qtype>
class ROArrayQuantColumn
{
public:
    // The default constructor creates a null object only useful if an
    // array of ROArrayQuantColumn() object is needed.  attach() should be
    // used to attach a real column to the ROArraQuantColumn before any 
    // attempt is made to read from the column.  A ROArrayQuantColumn can
    // be tested for nullness using the isNull() member.
    ROArrayQuantColumn();

    // Create the ROArrayQuantColumn from the supplied table and column name.
    ROArrayQuantColumn(const Table& tab, const String& columnName);

    // Create the ROArrayQuantColumn from the supplied table and column name.
    // Units for the Quantum are specified in u and over-rides the default
    // units for the column.  Note that retreived quantums are not converted
    // to 'u', they are simply returned with 'u' units as if that was how 
    // they were stored. This could be useful when Quantum column are created 
    // with an inappropriate or undefined value for units.
    ROArrayQuantColumn(const Table& tab, const String& columnName, 
	    	       const Unit& u);

    // Copy constructor..
    ROArrayQuantColumn(const ROArrayQuantColumn& that);

    ~ROArrayQuantColumn();

    // Change the reference to another column.
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
    void get(Array<Quantum<Qtype> >& q, 
	     uInt rownr, const Unit& s, 
	     Bool resize = False) const;
    // Get the quantum array in the specified row. Each quantum is
    // converted to Units in other.
    void get(Array<Quantum<Qtype> >& q, 
	     uInt rownr, 
	     const Quantum<Qtype> &other, 
	     Bool resize = False) const;
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
    
    //# A template of the stored quantums.  Useful when getting and putting
    //# of quantums but perhaps unnecessary.
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
//   <li> <linkto class=Quantum.h>Quantum</linkto>
//   <li> TableQuantumDesc
// </prerequisite>

// <synopsis>
// The ArrayQuantColumn class provides read/write access to Quantum 
// Array Table columns.  The column should previously 
// have been defined as a Quantum column by means of the 
// <linkto class=TableQuantumDesc>TableQuantumDesc class</linkto>.
// (The ROArrayQuantColumn class exist for read/write access the column.)<br>
// The underlying Quantum column can have fixed or variable units which was
// specified when the TableQuantumDesc was used to create the quantum column.
// If the Quantum Column's Unit is fixed then the unit component of quantums
// being added to a column is disregarded, that is, quantums are assumed to
// have the correct unit and no attempt is made to convert the quantum to the
// correct unit before it is stored.  In this case it is not an error to add
// a quantum with the wrong unit but the user should be aware that the unit
// information will be lost.  On the otherhand, if the Quantum Column has
// variable units the quantum's unit is stored as well as its value.<br>
// </synopsis>

// <example>
// <srcblock>
//    // (see <linkto class=TableQuantumDesc>TableQuantumDesc class</linkto>
//    // for an example of how to create the TableQuantumDesc).
//    // This create the Quantum array object.
//    ArrayQuantColumn<Double> aqCol(qtab, "ArrQuantDouble");
//
//    // need an array of quantums.
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
//    // put the quantum array in the column at row 0
//    aqCol.put(0, qArr);
// </srcblock>
// </example>

// <motivation>
// Add support for Quantums in the Tables system.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>

template<class Qtype>
class ArrayQuantColumn : public ROArrayQuantColumn<Qtype>
{
public:
    // The default constructor creates a null object only useful if an
    // array of ArrayQuantColumn() object is needed.  attach() should be
    // used to attach a real column to the ArrayQuantColumn before any 
    // attempt is made to read from the column.  An ArrayQuantColumn can
    // be tested for nullness using the isNull() member.
    ArrayQuantColumn();

    // Create the ArrayQuantColumn from the supplied table and column name.
    ArrayQuantColumn(const Table& tab, const String& columnName);

    // Create the ROArrayQuantColumn from the supplied table and column name.
    // Units for the Quantum are specified in u and over-rides the default
    // units for the column.  Note that retreived quantums are not converted
    // to 'u', they are simply returned with 'u' units as if that was how 
    // they were stored. This could be useful when Quantum column are created 
    // with an inappropriate or undefined value for units.
    ArrayQuantColumn(const Table& tab, const String& columnName, const Unit& u);

    // Copy constructor.
    ArrayQuantColumn(const ArrayQuantColumn& that);

    ~ArrayQuantColumn();

    // Change the reference to another column.
    void reference(const ArrayQuantColumn<Qtype>& that);

    // Attach a column to the object. Optionally supply a default unit.
    // <group name="attach">
    void attach(const Table& tab, const String& columnName); 
    void attach(const Table& tab, const String& columnName, const Unit& u);
    // </group>
 
    // Add an array of quantums in q to the specified row to the table.
    // Quantums are first converted to the columns units if necessary.
    void put(uInt rownr, const Array<Quantum<Qtype> >& q);
    
private:
    // reference() can be used for assignment.
    // Declaring this operator private makes it unusable.
    ArrayQuantColumn& operator= (const ArrayQuantColumn<Qtype>& that);
};

#endif
