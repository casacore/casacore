//# ScalarMeasColumn.h: Access to Scalar Measure Columns in Tables.
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

#if !defined(AIPS_SCALARMEASCOLUMN_H)
#define AIPS_SCALARMEASCOLUMN_H

//# Includes
#include <aips/Tables/ArrayColumn.h>
#include <trial/TableMeasures/TableMeasDesc.h>

//# Forward Declarations
class MEpoch;
class MVEpoch;
class MPosition;
class MVPosition;
class MCPosition;
class MDirection;
class MVDirection;
class MRadialVelocity;
class MVRadialVelocity;
class MDoppler;
class MVDoppler;
class MFrequency;
class MVFrequency;
class Table;
class TableMeasRefDesc;
template <class Ms> class MeasRef;
template <class T> class ScalarColumn;
template <class M, class MV> class ROScalarMeasColumn;
template <class M, class MV> class ScalarMeasColumn;

// <summary>
// Definition of a Scalar Measure Table Column.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableMeasure.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
//   <li> TableMeasDesc
// </prerequisite>

// <synopsis>
// ROScalarMeasColumn and ScalarMeasColumn objects can be used to access 
// scalar measure columns in tables.  The ROScalarMeasColumn provides read
// only access whereas the ScalarMeasColumn object can be used for writing
// and reading of measures to/from a Table column.<br>
// Before a column can be accessed it must have previously been defined as
// a measure column by use of the
// <linkto class=TableMeasDesc>TableMeasDesc class</linkto>.<br>
// The (RO)ScalarMeasColumn class is templated on Measure type and MeasValue
// type but typedefs exist for making declaration less long winded.
// To create a (RO)ScalarMeasColumn a reference to the table containing the
// column and the name of the column are supplied to the class's constructor.
// Using typedefed constructors this look like:
// <srcblock>
// MEpochScaCol ec(table, "ColumnName);     	// MEpoch Scalar Column
// ROMDopplerScaCol dc(table, "DopplerCol");    // Read only MDoppler column
// </srcblock>
// Measures can then be added to the column using the put() member (at 
// least for the ScalarMeasColumn) in a
// way which mimics the put() member of regular Table column objects. 
// Similarly, get() and operator() can be used to retrieve measures.
// </synopsis>

// <example>
// <srcblock>
//     // This creates a Scalar MEpoch column for read/write access.  Column
//     // "Time1" must exist in Table "tab" and must have previously been
//     // defnined as a MEpoch column using a TableMeasDesc.
//     MEpochScaCol timeCol(tab, "Time1");
// 	
//     // print some details about the column
//     if (timeCol.isRefVariable()) {
//        cout << "The column has variable references.\n";
//     } else {
//         cout << "The MeasRef for the column is: " << timeCol.getMeasRef() 
//              << endl;
//     }
// 
//     // Add tab.nrow() measures to the column.	
//     MEpoch tm(Quantity(MeasData::MJD2000, "d"), MEpoch::TAI);
//     for (uInt i=0; i<tab.nrow(); i++) {
//         timeCol.put(i, tm);
//     }
//
//     // Could read from the column using timeCol but a read only column
//     // object could be created like this. 
//     ROMEpochScaCol timeColRead(tab, "Time1");
//     for (i=0; i<tab.nrow(); i++) {
//         cout << timeColRead(i) << endl;
//     }
// </srcblock>
// </example>

// <motivation>
// The standard table system does not support storing of measures in table
// columns.  This class overcomes this limitations.
// </motivation>

//<todo asof="$DATE:$">
// Classes do not currently support internal conversion of Measures from one
// frame of reference to another.  This may be a useful feature.
//</todo>

template <class M, class MV>
class ROScalarMeasColumn
{
public:
    // The default constructor creates a null object.  Useful for creating
    // arrays of ROScalarMeasColumn objects.  Attempting to use a null object
    // will produce a segmentation fault so care needs to be taken to
    // initialise the objects by using the attach() member before any attempt
    // is made to use the object.  A ROScalarQuantColumn object can be tested
    // for nullnes by using the isNull() member.
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
 
    // Get the measure in specified row..
    // <group>
    void get(M& meas, uInt rownr);
    M operator()(uInt rownr);
    // </group>
          
    // Do the MeasRefs vary by row?
    Bool isRefVariable() const { return itsVarRefFlag; }
    
    // Returns the column's static reference or the last used reference if
    // refrences are variable.
    const MeasRef<M>& getMeasRef() const;
    
    // Test if the object is null.
    Bool isNull() const { return (itsDataCol == 0 ? True : False); }
    
    // Throw an exception if the object is null.
    void throwIfNull() const;
    
protected:
    // Resets itsMeasRef. Needed when the MeasRef varies from row to row.
    void setMeasRef(uInt rownr=0);

    //# Column which contains the Measure's actual data
    ArrayColumn<Double>* itsDataCol;
    
    //# Measure reference could be constant or vary per row.
    Bool itsVarRefFlag;
    
    //# Its MeasRef code column when references are variable.
    ScalarColumn<Int>* itsRefCodeCol;
    
    //# Column containing its variable offsets.  Only applicable if the 
    //# measure references have offsets and they are variable.
    ScalarMeasColumn<M, MV>* itsOffsetCol;
    
private:
    // Assignment makes no sense in a read only class.
    // Declaring this operator private makes it unusable.
    ROScalarMeasColumn& operator= (const ROScalarMeasColumn& that);  

    //# Its measure reference when the MeasRef is constant per row.
    MeasRef<M> itsMeasRef;
    
    // Deletes allocated memory etc. Called by ~tor and any member which needs
    // to reallocate data.
    void cleanUp();
};

//# Typedefs
typedef ROScalarMeasColumn<MEpoch, MVEpoch> ROMEpochScaCol;
typedef ROScalarMeasColumn<MPosition, MVPosition> ROMPositionScaCol;
typedef ROScalarMeasColumn<MDirection, MVDirection> ROMDirectionScaCol;
typedef ROScalarMeasColumn<MRadialVelocity, MVRadialVelocity>
    ROMRadialVelocityScaCol;
typedef ROScalarMeasColumn<MDoppler, MVDoppler> ROMDopplerScaCol;
typedef ROScalarMeasColumn<MFrequency, MVFrequency> ROMFrequency;


template <class M, class MV>
class ScalarMeasColumn : public ROScalarMeasColumn<M, MV>
{
public:
    // The default constructor creates a null object.  Useful for creating
    // arrays of ScalarMeasColumn objects.  Attempting to use a null object
    // will produce a segmentation fault so care needs to be taken to
    // initialise the objects by using the attach() member before any attempt
    // is made to use the object.  A ScalarQuantColumn object can be tested
    // for nullnes by using the isNull() member.
    ScalarMeasColumn();

    // Create the ScalarMeasColumn from the table and column Name.
    ScalarMeasColumn(const Table& tab, const String& columnName);

    // Copy constructor.
    ScalarMeasColumn(const ScalarMeasColumn<M, MV>& that);

    ~ScalarMeasColumn();
    
    // Change the reference to another column.
    void reference(const ScalarMeasColumn<M, MV>& that);

    // Attach a column to the object.
    void attach(const Table& tab, const String& columnName); 
 
    // Add a measure to the table
    void put(uInt rownr, const M& meas);
        
private:
};

//# Typedefs
typedef ScalarMeasColumn<MEpoch, MVEpoch> MEpochScaCol;
typedef ScalarMeasColumn<MPosition, MVPosition> MPositionScaCol;
typedef ScalarMeasColumn<MDirection, MVDirection> MDirectionScaCol;
typedef ScalarMeasColumn<MRadialVelocity, MVRadialVelocity>
    MRadialVelocityScaCol;
typedef ScalarMeasColumn<MDoppler, MVDoppler> MDopplerScaCol;
typedef ScalarMeasColumn<MFrequency, MVFrequency> MFrequencyScaCol;

#endif
