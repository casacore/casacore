//# ScalarQuantColumn.h: Access to a Scalar Quantum Column in a table.
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

#if !defined(AIPS_SCALARQUANTCOLUMN_H)
#define AIPS_SCALARQUANTCOLUMN_H

//# Includes

//# Forward Declarations
class String;
class TableQuantumDesc;
class Unit;
class Table;
template <class T> class ScalarColumn;
template <class Qtype> class Quantum;

// <summary>
// Provides read-only access to Scalar Quantum Columns in Tables.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableQuantum.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Table>Table</linkto>
//   <li> <linkto class=ROScalarColumn>ROScalarColumn</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>

// <synopsis>
// The ROScalarQuantColumn class provides read-only access to Quanta
// stored in a scalar Quantum Table column.  The Quantum column should 
// already exist in the table and would have been defined by means of a 
// <linkto class=TableQuantumDesc>TableQuantumDesc object</linkto>.  
// In addition,
// for a ROScalarQuantColumn object to be useful the column should
// contain Quanta.  Inserting Quanta into column requires the use of a
// <linkto class=ScalarQuantColumn">ScalarQuantColumn</A> 
// object.<br>
//
// A ScalarQuantColumn object is used much in the same way as a 
// <linkto class=ROScalarColumn>ROScalarColumn</linkto> object.
//
// <note role=warning> This class supports a subset (i.e., not all) of the 
// operations possible with a ROScalarColumn object.
//
// <h3>Quantum Units</h3></A>
// Quanta retrieved from the column will normally have the Unit that was
// specified 
// when the Quantum column was defined.  However, it is possible to override
// the default column Unit by supplying a Unit in a ROScalarQuantColumn
// constructor which exists for this purpose.  When constructed in this
// fashion the retrieved Quanta will have the newly specified Unit but their
// values will be the same as when originally stored in the column.  That is,
// no Unit conversion is done.
// If Unit conversion is desired it can be achieved by
// supplying a Unit argument to one of retrieval members (either get() or
// operator()). (Quanta can of course be converted after they have been
// retrieved from a column.)
// </synopsis>

// <example>
// <srcblock>
//     Quantum<Double> q(5.3, "keV");
//     // "QuantScalar" has previously been defined as a Quantum column
//     // by means of a TableQuantumDesc.  This example assumes the column
//     // already contains quanta.
//     ROScalarQuantColumn<Double> qCol(qtab, "QuantScalar");
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
//
// <thrown>
//    <li>TableInvOper if the Table column is null.
// </thrown>
//
//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>

template<class Qtype> class ROScalarQuantColumn
{
public:
    // The default constructor creates a null object.  Useful for creating
    // arrays of ROScalarQuantColumn objects.  Attempting to use a null object
    // will produce a segmentation fault so care needs to be taken to
    // initialise the objects by using the attach() member before any attempt
    // is made to use the object.  The isNull() member can be used to test
    // if a ROScalarQuantColumn object is null.
    ROScalarQuantColumn();

    // Create the ROScalarQuantColumn from the specified table and column name.
    ROScalarQuantColumn(const Table& tab, const String& columnName);

    // Create the ROScalarQuantColumn from the specified table and column name.
    // Units for the Quanta are specified in u.  This over-rides the default
    // Unit for the column.  Note that specifying a Unit does not do Unit
    // conversion.  Unit conversion is possible with one of the get() 
    // members.
    ROScalarQuantColumn(const Table& tab, const String& columnName, 
	    	        const Unit& u);

    // Copy constructor (copy semantics).
    ROScalarQuantColumn(const ROScalarQuantColumn<Qtype>& that);
    
    ~ROScalarQuantColumn();

    // Change the reference to another column.
    void reference(const ROScalarQuantColumn<Qtype>& that);

    // Attach a column to the object. Optionally supply a default unit.
    // <group name="attach">
    void attach(const Table& tab, const String& columnName); 
    void attach(const Table& tab, const String& columnName, const Unit& u);
    // </group>
 
    // Get the quantum stored in the specified row.
    // <group name="get">
    void get(Quantum<Qtype>& q, uInt rownr) const;
    // Get the quantum in the specified row, converted to Units in s.
    void get(Quantum<Qtype>& q, uInt rownr, const Unit& s) const;
    // Get the quantum in the specified row, converted to units in other.
    void get(Quantum<Qtype>& q, uInt rownr, const Quantum<Qtype> &other) const;
    // </group>
    
    // Return the quantum stored in the specified row.
    // <group>
    Quantum<Qtype> operator()(uInt rownr) const;
    // Return the quantum stored in the specified row, converted to
    // Units in s.
    Quantum<Qtype> operator()(uInt rownr, const Unit &s) const;
    // Return the quantum in the specified row, converted to Units in
    // other.
    Quantum<Qtype> operator()(uInt rownr, const Quantum<Qtype> &other) const;
    // </group>
      
    // Test whether the Quantum column has variable units
    Bool isUnitVariable() const { return (itsUnitsCol != 0 ? True : False); }
    
    // Returns the column's value for Units as a string.
    const String& getUnits() const { return itsUnit.getName(); }
    
    // Test if the object is null.
    Bool isNull() const { return (itsDataCol == 0 ? True : False); }
    
    // Throw an exception if the object is null.
    void throwIfNull() const;
    
protected:
    //# The underlying data column stores the quantum column's data.
    ScalarColumn<Qtype>* itsDataCol;

    //# Quantum column's units (if units not variable)
    Unit itsUnit;

    //# Variable units column if applicable.
    ScalarColumn<String>* itsUnitsCol;
    
private:
    // Assignment makes no sense in a read only class.
    // Declaring this operator private makes it unusable.
    ROScalarQuantColumn& operator= (const ROScalarQuantColumn& that);
    
    // Deletes allocated memory etc. Called by destructor and any member 
    // which needs to reallocate data.
    void cleanUp();

};

// <summary>
// Provides read/write access to Scalar Quantum Columns in Tables.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableQuantum.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Table>Table</linkto>
//   <li> <linkto class=ROScalarColumn>ROScalarColumn</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>

// <synopsis>
// The ScalarQuantColumn class provides read/write access to Quanta
// stored in a Quantum Table column.  The column should previously have 
// been defined as a Quantum column by means of the 
// <linkto class=TableQuantumDesc>TableQuantumDesc</linkto> object.
// In addition to the operations provided by its read-only partner,
// <linkto class=ROScalarQuantColumn>ROScalarQuantColumn</linkto>, use 
// of a ScalarQuantColumn object allows the insertion of Quanta into a column.
// <br>
//
// <h3>Quantum Units</h3></A>
// The underlying Quantum column will have been defined to have either
// variable or fixed Units
// If the Quantum Column's Unit is fixed then the unit component of quanta
// being added to a column is disregarded, that is, quanta are assumed to
// have the correct unit and no attempt is made to convert the quanta to the
// correct unit before they are stored.  Therefore, it is not an error to add
// a quantum with the wrong unit but the user should be aware that the unit
// information will be lost.  On the other-hand, if the Quantum Column was
// defined with variable units the unit of every Quantum stored is stored
// along with the Quantum's value.
// </synopsis>

// <example>
// <srcblock>
//     Quantum<Double> q(5.3, "keV");
//     // "QuantScalar" has previously been defined as a Quantum column
//     // by means of a TableQuantumDesc.
//     ScalarQuantColumn<Double> qCol(qtab, "QuantScalar");
//     for (uInt i=0; i < qtab.nrow(); i++) {
//         qCol.put(i, q);
//     }
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
// <todo asof="$DATE:$">
// Is there a better way of dealing with fixed unit columns?  When putting a
// Quantum into a column should the unit be checked and converted or perhaps
// it should be illegal to add quanta with incorrect units?
// </todo>


template<class Qtype> class ScalarQuantColumn : 
    public ROScalarQuantColumn<Qtype>
{
public:
    // The default constructor creates a null object.  Useful for creating
    // arrays of ScalarQuantColumn objects.  Attempting to use a null object
    // will produce a segmentation fault so care needs to be taken to
    // initialise the objects by using the attach() member before any attempt
    // is made to use the object.  The isNull() member can be used to test
    // if a ScalarQuantColumn object is null.
    ScalarQuantColumn();

    // Create the ScalarQuantColumn from the specified table and column name.
    ScalarQuantColumn(const Table& tab, const String& columnName);

    // Create the ScalarQuantColumn from the specified table and column name.
    // Units for the Quantum are specified in u. This over-rides the default
    // units for the column.  Note that retrieved quanta are not converted
    // to 'u', they are simply returned with 'u' units as if that was how 
    // they were stored. This could be useful when Quantum column are created 
    // with an inappropriate or undefined value for units.
    ScalarQuantColumn(const Table& tab, const String& columnName, 
	    	      const Unit& u);

    // Copy constructor (copy semantics).
    ScalarQuantColumn(const ScalarQuantColumn<Qtype>& that);

    ~ScalarQuantColumn();

    // Change the reference to another column.
    void reference(const ScalarQuantColumn<Qtype>& that);

    // Attach a column to the object. Optionally supply a default unit.
    // <group name="attach">
    void attach(const Table& tab, const String& columnName); 
    void attach(const Table& tab, const String& columnName, const Unit& u);
    // </group>
 
    // Add a quantum to the table.  If the column supports variable units
    // the q's Unit is stored with q.  If Units are fixed for the column
    // q's Unit is disregarded (i.e., it is assumed to conform).
    void put(uInt rownr, const Quantum<Qtype>& q);
    
private:
    // Assignment facility offered via reference() member.
    // Declaring this operator private makes it unusable.
    ScalarQuantColumn& operator=(const ScalarQuantColumn& that);
};

#endif
