//# ScalarMeasColumn.h: Access to Scalar Measure Columns in Tables.
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

#if !defined(AIPS_SCALARMEASCOLUMN_H)
#define AIPS_SCALARMEASCOLUMN_H

//# Includes
#include <aips/Measures/MeasRef.h>

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
class Table;
class TableMeasRefDesc;
template <class T> class ROArrayColumn;
template <class T> class ArrayColumn;
template <class T> class ScalarColumn;
template <class T> class ROScalarColumn;
template <class M, class MV> class ROScalarMeasColumn;
template <class M, class MV> class ScalarMeasColumn;

// <summary>
// Read only access to table scalar Measure columns.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableMeasures.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
//   <li> TableMeasDesc
// </prerequisite>

// <synopsis>
// ROScalarMeasColumn and ScalarMeasColumn objects can be used to access 
// Scalar Measure Columns in tables.  The ROScalarMeasColumn provides read
// only access whereas the ScalarMeasColumn object can be used for writing
// and reading of Measures to and from a Table column.
//
// Before a column can be accessed it must have previously been defined as
// a Measure column by use of the
// <linkto class="TableMeasDesc">TableMeasDesc</linkto> object.
//
// The (RO)ScalarMeasColumn class is templated on Measure type and MeasValue
// type but typedefs exist for making declaration less long winded. The
// following typedefs can be used to assist in creating (RO)ScalarMeasColumn
// objects:
// <ul>
//    <li>ROMEpochScaCol and MEpochScaCol
//    <li>ROMPositionScaCol and MPositionScaCol
//    <li>ROMDirectionScaCol and MDirectionScaCol
//    <li>ROMRadialVelocityScaCol and MRadialVelocityScaCol
//    <li>ROMDopplerScaCol and MDopplerScaCol
//    <li>ROMFrequencyScaCol and MFrequencyScaCol
//    <li>ROMBaselineScaCol and MBaselineScaCol
//    <li>ROMuvwScaCol and MuvwScaCol
//    <li>ROMEarthMagneticScaCol and MEarthMagneticScaCol
// </ul>
 
// Constructing scalar Measure column objects using these typedefs looks like 
// this:
// <srcblock>
// MEpochScaCol ec(table, "ColumnName);      // MEpoch Scalar Column
// ROMDopplerScaCol dc(table, "DopplerCol"); // Read only MDoppler column
// </srcblock>
//
// <h3>Reading and writing Measures</h3>
//
// The reading and writing of Measures columns is very similar to reading and
// writing of "ordinary" Table columns. 
// <linkto class="ROScalarMeasColumn#get">get()</linkto>
// and <linkto class="ROScalarMeasColumn#get">operator()</linkto>
// exist for reading Measures and the 
// <linkto class="ScalarMeasColumn#put">put()</linkto> member for adding
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
// <linkto class="ROScalarMeasColumn#isRefVariable">isRefVariable()</linkto> 
// and
// <linkto class="ROScalarMeasColumn#getMeasRef">getMeasRef()</linkto> can be 
// used to discover a Measure column's Measure reference characteristics.
//
// </synopsis>

// <example>
// <srcblock>
//     // This creates a Scalar MEpoch column for read/write access.  Column
//     // "Time1" must exist in Table "tab" and must have previously been
//     // defined as a MEpoch column using a TableMeasDesc.
//     MEpochScaCol timeCol(tab, "Time1");
// 	
//     // print some details about the column
//     if (timeCol.isRefVariable()) {
//        cout << "The column has variable references.\n";
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
//     ROMEpochScaCol timeColRead(tab, "Time1");
//     for (i=0; i<tab.nrow(); i++) {
//         cout << timeColRead(i) << endl;
//     }
// </srcblock>
// </example>

// <motivation>
// The standard Aips++ Table system does not support Measures columns.
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

template <class M, class MV> class ROScalarMeasColumn
{
public:
    // The default constructor creates a null object.  Useful for creating
    // arrays of ROScalarMeasColumn objects.  Attempting to use a null object
    // will produce a segmentation fault so care needs to be taken to
    // initialise the objects first by using attach().  A 
    // ROScalarMeasColumn object can be tested if it is null by using the 
    // isNull() member.
    ROScalarMeasColumn();

    // Create the ScalarMeasColumn from the table and column Name.
    ROScalarMeasColumn(const Table& tab, const String& columnName);
    
    // Copy constructor (copy semantics).
    ROScalarMeasColumn(const ROScalarMeasColumn<M, MV>& that);

    ~ROScalarMeasColumn();
    
    // Change the reference to another column.
    void reference(const ROScalarMeasColumn<M, MV>& that);

    // Attach a column to the object.
    void attach(const Table& tab, const String& columnName); 
 
    // Get the Measure contained in the specified row.
    // <group name=get>
    void get(uInt rownr, M& meas) const;
    M operator()(uInt rownr) const;
    // </group>
    
    // Tests if a row contains a Measure (i.e., if the row has a defined
    // value).
    Bool isDefined(uInt rownr) const;
          
    // Is there per row storage of Measure references or is the Measure 
    // reference fixed for the column?
    // <group name=isRefVariable>
    Bool isRefVariable() const { return itsVarRefFlag; }
    // </group>
    
    // Returns the column's fixed reference or the reference of the last 
    // read Measure if references are variable.
    // <group name=getMeasRef>
    const MeasRef<M>& getMeasRef() const;
    // </group>
    
    // Test if the object is null.
    Bool isNull() const { return (itsDataCol == 0 ? True : False); }
    
    // Throw an exception if the object is null.
    void throwIfNull() const;
    
protected:
    //# Resets itsMeasRef. Needed when the MeasRef varies from row to row.
    void setMeasRef(uInt rownr=0) const;

    //# Are references variable? (If variable they're in a column too.)
    Bool itsVarRefFlag;
    
    //# This is either the column's fixed Measure reference or the reference
    //# of the last Measure read.  Needs to to be mutable because this is
    //# updated by the constant members get() and operator() if the Measure
    //# reference is not fixed for the column.
    mutable MeasRef<M> itsMeasRef;
    
private:
    // Assignment makes no sense in a read only class.
    // Declaring this operator private makes it unusable.
    ROScalarMeasColumn& operator= (const ROScalarMeasColumn& that);  

    //# Column which contains the Measure's actual data
    ROArrayColumn<Double>* itsDataCol;
    
    //# Its MeasRef code column when references are variable.
    ROScalarColumn<Int>* itsRefIntCol;
    ROScalarColumn<String>* itsRefStrCol;
    
    //# Column containing its variable offsets.  Only applicable if the 
    //# measure references have offsets and they are variable.
    ROScalarMeasColumn<M, MV>* itsOffsetCol;
    
    //# Deletes allocated memory etc. Called by ~tor and any member which needs
    //# to reallocate data.
    void cleanUp();
};

//# Typedefs to make declaring a ROScalarMeasColumn less long-winded
typedef ROScalarMeasColumn<MEpoch, MVEpoch> ROMEpochScaCol;
typedef ROScalarMeasColumn<MPosition, MVPosition> ROMPositionScaCol;
typedef ROScalarMeasColumn<MDirection, MVDirection> ROMDirectionScaCol;
typedef ROScalarMeasColumn<MRadialVelocity, MVRadialVelocity>
    ROMRadialVelocityScaCol;
typedef ROScalarMeasColumn<MDoppler, MVDoppler> ROMDopplerScaCol;
typedef ROScalarMeasColumn<MFrequency, MVFrequency> ROMFrequencyScaCol;
typedef ROScalarMeasColumn<MBaseline, MVBaseline> ROMBaselineScaCol;
typedef ROScalarMeasColumn<Muvw, MVuvw> ROMuvwScaCol;
typedef ROScalarMeasColumn<MEarthMagnetic, MVEarthMagnetic> 
    ROMEarthMagneticScaCol;
 

// <summary>
// Read write access to table scalar Measure columns.
// </summary>

// <synopsis>
// See description for 
// <linkto class="ROScalarMeasColumn">ROScalarMeasColumn</linkto>.
// </synopsis>

template <class M, class MV> class ScalarMeasColumn 
    : public ROScalarMeasColumn<M, MV>
{
public:
    // The default constructor creates a null object.  Useful for creating
    // arrays of ScalarMeasColumn objects.  Attempting to use a null object
    // will produce a segmentation fault so care needs to be taken to
    // initialise the a null object by using the attach() member before any 
    // attempt is made to use it.  A ScalarMeasColumn object can be 
    // tested if it is null by using the isNull() member.
    ScalarMeasColumn();

    // Create the ScalarMeasColumn from the table and column Name.
    ScalarMeasColumn(const Table& tab, const String& columnName);

    // Copy constructor (copy semantics).
    ScalarMeasColumn(const ScalarMeasColumn<M, MV>& that);

    ~ScalarMeasColumn();
    
    // Change the column reference to another column.
    void reference(const ScalarMeasColumn<M, MV>& that);

    // Attach a column to the object.
    void attach(const Table& tab, const String& columnName); 
 
    // Add a Measure to the column.
    // <group name=put>
    void put(uInt rownr, const M& meas);
    // </group>
        
private:
    // Declaring this operator private makes it unusable.
    // See class <linkto class="ScalarColumn">ScalarColumn</linkto> for an
    // explanation as to why this operation is disallowed.  Use the reference
    // function instead.
    ScalarMeasColumn& operator= (const ScalarMeasColumn& that);  

    //# Column which contains the Measure's actual data
    ArrayColumn<Double>* itsDataCol;
    
    //# Its MeasRef code column when references are variable.
    ScalarColumn<Int>* itsRefIntCol;
    ScalarColumn<String>* itsRefStrCol;
    
    //# Column containing its variable offsets.  Only applicable if the 
    //# measure references have offsets and they are variable.
    ScalarMeasColumn<M, MV>* itsOffsetCol; 
   
    //# Deletes allocated memory etc. Called by ~tor and any member which
    //# needs to reallocate data.
    void cleanUp();
};

//# Typedefs
typedef ScalarMeasColumn<MEpoch, MVEpoch> MEpochScaCol;
typedef ScalarMeasColumn<MPosition, MVPosition> MPositionScaCol;
typedef ScalarMeasColumn<MDirection, MVDirection> MDirectionScaCol;
typedef ScalarMeasColumn<MRadialVelocity, MVRadialVelocity>
    MRadialVelocityScaCol;
typedef ScalarMeasColumn<MDoppler, MVDoppler> MDopplerScaCol;
typedef ScalarMeasColumn<MFrequency, MVFrequency> MFrequencyScaCol;
typedef ScalarMeasColumn<MBaseline, MVBaseline> MBaselineScaCol;
typedef ScalarMeasColumn<Muvw, MVuvw> MuvwScaCol;
typedef ScalarMeasColumn<MEarthMagnetic, MVEarthMagnetic> MEarthMagneticScaCol;

#endif
