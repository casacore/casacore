//# ArrayMeasColumn.h: Access to array Measure columns in Tables.
//# Copyright (C) 1997,1998,1999
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

#if !defined(AIPS_ARRAYMEASCOLUMN_H)
#define AIPS_ARRAYMEASCOLUMN_H

//# Includes
#include <aips/Measures/MeasRef.h>
//#include <aips/Tables/ArrayColumn.h>

//# Forward Declarations
class MEpoch;
class MVEpoch;
class MPosition;
class MVPosition;
class MDirection;
class MVDirection;
class MRadialVelocity;
class MVRadialVelocity;
class MDoppler;
class MVDoppler;
class MFrequency;
class MVFrequency;
class MBaseline;
class MVBaseline;
class Muvw;
class MVuvw;
class MEarthMagnetic;
class MVEarthMagnetic;
class String;
class Table;
class TableMeasRefDesc;
template <class T> class ArrayColumn;
template <class T> class ROArrayColumn;
template <class T> class ScalarColumn;
template <class T> class ROScalarColumn;
template <class M> class TableMeasDesc;
template <class M, class MV> class ArrayMeasColumn;
template <class M, class MV> class ScalarMeasColumn;
template <class M, class MV> class ROScalarMeasColumn;

// <summary>
// Read only access to table array Measure columns.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableMeasures.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
//   <li> TableMeasDesc
// </prerequisite>

// <synopsis>
// ROArrayMeasColumn and ArrayMeasColumn objects can be used to access 
// array Measure columns in tables.  The ROArrayMeasColumn provides read
// only access whereas the ArrayMeasColumn object can be used for reading 
// and writing of Measures array columns.
//
// Before a column can be accessed it must have previously been defined as
// a Measure column by use of the
// <linkto class="TableMeasDesc">TableMeasDesc</linkto> object.
//
// The (RO)ArrayMeasColumn class is templated on Measure type and MeasValue
// type but typedefs exist for making declaration less long winded. The
// following typedefs can be used to assist in creating (RO)ArrayMeasColumn
// objects:
// <ul>
//    <li>ROMEpochArrCol and MEpochArrCol
//    <li>ROMPositionArrCol and MPositionArrCol
//    <li>ROMDirectionArrCol and MDirectionArrCol
//    <li>ROMRadialVelocityArrCol and MRadialVelocityArrCol
//    <li>ROMDopplerArrCol and MDopplerArrCol
//    <li>ROMFrequencyArrCol and MFrequencyArrCol
//    <li>ROMBaselineArrCol and MBaselineArrCol
//    <li>ROMuvwArrCol and MuvwArrCol
//    <li>ROMEarthMagneticArrCol and MEarthMagneticArrCol
// </ul>
 
// Constructing array Measure column objects using these typedefs looks like 
// this:
// <srcblock>
// MEpochArrCol ec(table, "ColumnName);      // MEpoch array column
// ROMDopplerArrCol dc(table, "DopplerCol"); // Read only MDoppler array column
// </srcblock>
//
// <h3>Reading and writing Measures</h3>
//
// The reading and writing of Measures columns is very similar to reading and
// writing of "ordinary" Table columns. 
// <linkto class="ROArrayMeasColumn#get">get()</linkto>
// and <linkto class="ROArrayMeasColumn#get">operator()</linkto>
// exist for reading Measures and the 
// <linkto class="ArrayMeasColumn#put">put()</linkto> member for adding
// Measures to a column.  (put() is obviously not defined for 
// ROScalarMeasColumn objects.)  Each of these members accepts a row number
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
// different to the column's reference.   The members 
// <linkto class="ROArrayMeasColumn#isRefVariable">isRefVariable()</linkto> 
// and
// <linkto class="ROArrayMeasColumn#getMeasRef">getMeasRef()</linkto> can be 
// used to discover a Measure column's Measure reference characteristics.
//
// </synopsis>

// <example>
// <srcblock>
//    // create an MEpoch array column object
//    MEpochArrCol arrayCol;
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
//    // but here we'll create a ROMEpochArrCol to do this
//    ROMEpochArrCol roArrCol(tab, "Time1Arr");
//
//    // need a vector to put the MEpochs into
//    Vector<MEpoch> ew;
//
//    // setting the resise parameter to True automatically set ew to the
//    // same shape as the array contain in the row
//    arrayCol.get(0,ew, True);

// </srcblock>
// </example>

// <motivation>
// The standard Aips++ Table system does not support array Measure columns.
// This class overcomes this limitation.
// </motivation>
//
// <thrown>
//    <li>ArrayConformanceError during get() if supplied array is the wrong
//        shape.
// </thrown>
//
//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>

template<class M, class MV> class ROArrayMeasColumn
{
public:
    // The default constructor creates a null object.  Useful for creating
    // arrays of ROArrayMeasColumn objects.  Attempting to use a null object
    // will produce a segmentation fault so care needs to be taken to
    // initialise the objects by using the attach() member before any attempt
    // is made to use the object.  A ROArrayMeasColumn object can be tested
    // if it is null by using the isNull() member.
    ROArrayMeasColumn();

    // Create the ROArrayMeasColumn from the table and column Name.
    ROArrayMeasColumn(const Table& tab, const String& columnName);

    // Copy constructor (copy semantics).
    ROArrayMeasColumn(const ROArrayMeasColumn<M, MV>& that);

    ~ROArrayMeasColumn();

    // Change the reference to another column.
    void reference(const ROArrayMeasColumn<M, MV>& that);

    // Attach a column to the object. 
    void attach(const Table& tab, const String& columnName); 
 
    // Get the Measure array in the specified row.  For get() the supplied
    // array's shape should match the shape in the row unless resize is True.
    // <group name=get>
    void get(uInt rownr, Array<M>& meas, Bool resize = False) const;
    Array<M> operator()(uInt rownr) const;
    // </group>
          
    // Tests if a row contains a Measure array (i.e., if the row has a defined
    // value).
    Bool isDefined(uInt rownr) const;

    // Is there per row storage of Measure references or is the Measure 
    // reference fixed for the column?
    // <group name=isRefVariable>
    Bool isRefVariable() const { return itsVarRefFlag; }
    // </group>
    
    // Get the column's reference.
    // <group name=getMeasRef>
    const MeasRef<M>& getMeasRef() const;
    // </group>
    
    // Test if the object is null.
    Bool isNull() const { return (itsDataCol == 0 ? True : False); }
    
    // Throw an exception if the object is null.
    void throwIfNull() const;
    
protected:
    //# Resets itsMeasRef. Useful when the MeasRef varies from row to row.
    void setMeasRef(uInt rownr=0);

    //# Measure reference could be constant or vary per row.
    Bool itsVarRefFlag;
    Bool itsVarOffsetFlag;
    
private:
    // Assignment makes no sense in a read only class.
    // Declaring this operator private makes it unusable.
    ROArrayMeasColumn& operator= (const ROArrayMeasColumn& that); 

    //# Column which contains the Measure's actual data
    ROArrayColumn<Double>* itsDataCol;
    
    //# Its MeasRef code column when references are variable.
    ROScalarColumn<Int>* itsRefIntCol;
    ROArrayColumn<Int>* itsArrRefIntCol;
    
    //# Its MeasRef column when references are variable and stored as Strings.
    ROScalarColumn<String>* itsRefStrCol;
    ROArrayColumn<String>* itsArrRefStrCol;
    
    //# Column containing its variable offsets.  Only applicable if the 
    //# measure references have offsets and they are variable.
    ROScalarMeasColumn<M, MV>* itsOffsetCol;
    ROArrayMeasColumn<M, MV>* itsArrOffsetCol;
        
    //# Its measure reference when the MeasRef is constant per row.
    MeasRef<M> itsMeasRef;
    
    //# Deletes allocated memory etc. Called by ~tor and any member which needs
    //# to reallocate data.
    void cleanUp();
};

typedef ROArrayMeasColumn<MEpoch, MVEpoch> ROMEpochArrCol;
typedef ROArrayMeasColumn<MDirection, MVDirection> ROMDirectionArrCol;
typedef ROArrayMeasColumn<MPosition, MVPosition> ROMPositionArrCol;
typedef ROArrayMeasColumn<MRadialVelocity, MVRadialVelocity> 
    ROMRadialVelocityArrCol;
typedef ROArrayMeasColumn<MDoppler, MVDoppler> ROMDopplerArrCol;
typedef ROArrayMeasColumn<MFrequency, MVFrequency> ROMFrequencyCol;
typedef ROArrayMeasColumn<MBaseline, MVBaseline> ROMBaselineArrCol;
typedef ROArrayMeasColumn<Muvw, MVuvw> ROMuvwArrCol;
typedef ROArrayMeasColumn<MEarthMagnetic, MVEarthMagnetic> 
    ROMEarthMagneticArrCol;


// <summary>
// Read write access to table array Measure columns.
// </summary>

// <synopsis>
// See description for 
// <linkto class="ROArrayMeasColumn">ROArrayMeasColumn</linkto>.
// </synopsis>

template<class M, class MV> class ArrayMeasColumn 
    : public ROArrayMeasColumn<M, MV>
{
public:
    // The default constructor creates a null object.  Useful for creating
    // arrays of ROArrayMeasColumn objects.  Attempting to use a null object
    // will produce a segmentation fault so care needs to be taken to
    // initialise the objects by using the attach() member before any attempt
    // is made to use the object.  A ROArrayMeasColumn object can be tested
    // if it is null by using the isNull() member.
    ArrayMeasColumn();

    // Create the ROArrayMeasColumn from the table and column Name.
    ArrayMeasColumn(const Table& tab, const String& columnName);

    // Copy constructor (copy semantics).
    ArrayMeasColumn(const ArrayMeasColumn<M, MV>& that);

    ~ArrayMeasColumn();

    // Change the reference to another column.
    void reference(const ArrayMeasColumn<M, MV>& that);

    // Attach a column to the object. 
    void attach(const Table& tab, const String& columnName); 
 
    // Add a Measure array to the specified row.
    // <group name=put>
    void put(uInt rownr, const Array<M>&);    
    // </group>

private:
    // Declaring this operator private makes it unusable.
    // See class <linkto class="ArrayColumn">ArrayColumn</linkto> for an
    // explanation as to why this operation is disallowed.  Use the reference
    // function instead.
    ArrayMeasColumn& operator= (const ArrayMeasColumn& that); 

    //# Column which contains the Measure's actual data
    ArrayColumn<Double>* itsDataCol;
    
    //# Its MeasRef column when references are variable and stored as RefCodes.
    ScalarColumn<Int>* itsRefIntCol;
    ArrayColumn<Int>* itsArrRefIntCol;
    
    //# Its MeasRef column when references are variable and stored as Strings.
    ScalarColumn<String>* itsRefStrCol;
    ArrayColumn<String>* itsArrRefStrCol;
    
    //# Column containing its variable offsets.  Only applicable if the 
    //# measure references have offsets and they are variable.
    ScalarMeasColumn<M, MV>* itsOffsetCol;
    ArrayMeasColumn<M, MV>* itsArrOffsetCol;

    //# Its measure reference when the MeasRef is constant per row.
    MeasRef<M> itsMeasRef;
        
    //# Deletes allocated memory etc. Called by ~tor and any member which needs
    //# to reallocate data.
    void cleanUp();
};

typedef ArrayMeasColumn<MEpoch, MVEpoch> MEpochArrCol;
typedef ArrayMeasColumn<MDirection, MVDirection> MDirectionArrCol;
typedef ArrayMeasColumn<MPosition, MVPosition> MPositionArrCol;
typedef ArrayMeasColumn<MRadialVelocity, MVRadialVelocity> 
    MRadialVelocityArrCol;
typedef ArrayMeasColumn<MDoppler, MVDoppler> MDopplerArrCol;
typedef ArrayMeasColumn<MFrequency, MVFrequency> MFrequencyCol;
typedef ArrayMeasColumn<MBaseline, MVBaseline> MBaselineArrCol;
typedef ArrayMeasColumn<Muvw, MVuvw> MuvwArrCol;
typedef ArrayMeasColumn<MEarthMagnetic, MVEarthMagnetic> MEarthMagneticArrCol;

#endif
