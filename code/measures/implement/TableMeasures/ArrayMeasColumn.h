//# ScalarMeasColumn.h: Definition of a Measure in a Table.
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

#if !defined(AIPS_ARRAYMEASCOLUMN_H)
#define AIPS_ARRAYMEASCOLUMN_H

//# Includes
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Measures/MBaseline.h>
#include <aips/Measures/Muvw.h>
#include <aips/Measures/MEarthMagnetic.h>
#include <aips/Tables/ArrayColumn.h>

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
template <class T> class ArrayColumn;
template <class T> class ScalarColumn;
template <class T> class ROScalarColumn;
template <class M> class TableMeasDesc;
template <class M, class MV> class ArrayMeasColumn;
template <class M, class MV> class ScalarMeasColumn;
template <class M, class MV> class ROScalarMeasColumn;

// <summary>
// Descinition of a Measure in a Table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableMeasure.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=MeasBase.h>MeasBase</linkto>
//   <li> <linkto class=ColumnDesc>ColumnDesc</linkto>
// </prerequisite>

// <synopsis>
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// Creating the required keyword for the definition of a Measure
// in a Table is somewhat complicated. This class assists in that
// process.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>

template<class M, class MV>
class ROArrayMeasColumn
{
public:
    // The default constructor creates a null object.  Useful for creating
    // arrays of ROArrayMeasColumn objects.  Attempting to use a null object
    // will produce a segmentation fault so care needs to be taken to
    // initialise the objects by using the attach() member before any attempt
    // is made to use the object.  A ROArrayQuantColumn object can be tested
    // for nullnes by using the isNull() member.
    ROArrayMeasColumn();

    // Create the ROArrayMeasColumn from the table and column Name.
    // Optionally specify an output reference. 
    ROArrayMeasColumn(const Table& tab, const String& columnName);

    // Copy constructor (copy semantics).
    ROArrayMeasColumn(const ROArrayMeasColumn<M, MV>& that);

    ~ROArrayMeasColumn();

    // Change the reference to another column.
    void reference(const ROArrayMeasColumn<M, MV>& that);

    // Attach a column to the object. 
    void attach(const Table& tab, const String& columnName); 
 
    // Get the measure in the default outref.
    void get(uInt rownr, Array<M>& meas, Bool resize = False) const;
    Array<M> operator()(uInt rownr) const;
          
    // Get the column's reference.
    const MeasRef<M>& getRef() const;
    
    // Test if the object is null.
    Bool isNull() const { return (itsDataCol == 0 ? True : False); }
    
    // Throw an exception if the object is null.
    void throwIfNull() const;
    
protected:
    // Resets itsMeasRef. Useful when the MeasRef varies from row to row.
    void setMeasRef(uInt rownr=0);

    // Measure reference could be constant or vary per row.
    Bool itsVarRefFlag;
    Bool itsVarOffsetFlag;
    
private:
    // Assignment makes no sense in a read only class.
    // Declaring this operator private makes it unusable.
    ROArrayMeasColumn& operator= (const ROArrayMeasColumn& that); 

    // Column which contains the Measure's actual data
    ROArrayColumn<Double>* itsDataCol;
    
    // Its MeasRef code column when references are variable.
    ROScalarColumn<Int>* itsRefIntCol;
    ROArrayColumn<Int>* itsArrRefIntCol;
    
    // Its MeasRef column when references are variable and stored as Strings.
    ROScalarColumn<String>* itsRefStrCol;
    ROArrayColumn<String>* itsArrRefStrCol;
    
    // Column containing its variable offsets.  Only applicable if the 
    // measure references have offsets and they are variable.
    ROScalarMeasColumn<M, MV>* itsOffsetCol;
    ROArrayMeasColumn<M, MV>* itsArrOffsetCol;
        
    // Its measure reference when the MeasRef is constant per row.
    MeasRef<M> itsMeasRef;
    
    // Deletes allocated memory etc. Called by ~tor and any member which needs
    // to reallocate data.
    void cleanUp();
};

typedef ROArrayMeasColumn<MEpoch, MVEpoch> ROMEpochArrCol;
typedef ROArrayMeasColumn<MDirection, MVDirection> ROMDirectionArrCol;
typedef ROArrayMeasColumn<MPosition, MVPosition> ROMPositionArrCol;
typedef ROArrayMeasColumn<MRadialVelocity, MVRadialVelocity> 
    ROMRadialVelocityArrCol;
typedef ROArrayMeasColumn<MDoppler, MVDoppler> ROMDopplerArrCol;
typedef ROArrayMeasColumn<MFrequency, MVFrequency> ROMFrequecnyCol;
typedef ROArrayMeasColumn<MBaseline, MVBaseline> ROMBaselineArrCol;
typedef ROArrayMeasColumn<Muvw, MVuvw> ROMuvwArrCol;
typedef ROArrayMeasColumn<MEarthMagnetic, MVEarthMagnetic> ROMEarthMagneticArrCol;


template<class M, class MV>
class ArrayMeasColumn : public ROArrayMeasColumn<M, MV>
{
public:
    // The default constructor creates a null object.  Useful for creating
    // arrays of ROArrayMeasColumn objects.  Attempting to use a null object
    // will produce a segmentation fault so care needs to be taken to
    // initialise the objects by using the attach() member before any attempt
    // is made to use the object.  A ROArrayQuantColumn object can be tested
    // for nullnes by using the isNull() member.
    ArrayMeasColumn();

    // Create the ROArrayMeasColumn from the table and column Name.
    // Optionally specify an output reference. 
    ArrayMeasColumn(const Table& tab, const String& columnName);

    // Copy constructor (copy semantics).
    ArrayMeasColumn(const ArrayMeasColumn<M, MV>& that);

    ~ArrayMeasColumn();

    // Change the reference to another column.
    void reference(const ArrayMeasColumn<M, MV>& that);

    // Attach a column to the object. 
    void attach(const Table& tab, const String& columnName); 
 
    // Add a measure to the table
    void put(uInt rownr, const Array<M>&);    
private:
    // See class <linkto class="ArrayColumn">ArrayColumn</linkto> for an
    // explanation as to why this operation is disallowed.  Use the reference
    // function instead.
    // Declaring this operator private makes it unusable.
    ArrayMeasColumn& operator= (const ArrayMeasColumn& that); 

    // Column which contains the Measure's actual data
    ArrayColumn<Double>* itsDataCol;
    
    // Its MeasRef column when references are variable and stored as RefCodes.
    ScalarColumn<Int>* itsRefIntCol;
    ArrayColumn<Int>* itsArrRefIntCol;
    
    // Its MeasRef column when references are variable and stored as Strings.
    ScalarColumn<String>* itsRefStrCol;
    ArrayColumn<String>* itsArrRefStrCol;
    
    // Column containing its variable offsets.  Only applicable if the 
    // measure references have offsets and they are variable.
    ScalarMeasColumn<M, MV>* itsOffsetCol;
    ArrayMeasColumn<M, MV>* itsArrOffsetCol;

    // Its measure reference when the MeasRef is constant per row.
    MeasRef<M> itsMeasRef;
        
    // Deletes allocated memory etc. Called by ~tor and any member which needs
    // to reallocate data.
    void cleanUp();
};

typedef ArrayMeasColumn<MEpoch, MVEpoch> MEpochArrCol;
typedef ArrayMeasColumn<MDirection, MVDirection> MDirectionArrCol;
typedef ArrayMeasColumn<MPosition, MVPosition> MPositionArrCol;
typedef ArrayMeasColumn<MRadialVelocity, MVRadialVelocity> 
    MRadialVelocityArrCol;
typedef ArrayMeasColumn<MDoppler, MVDoppler> MDopplerArrCol;
typedef ArrayMeasColumn<MFrequency, MVFrequency> MFrequecnyCol;
typedef ArrayMeasColumn<MBaseline, MVBaseline> MBaselineArrCol;
typedef ArrayMeasColumn<Muvw, MVuvw> MuvwArrCol;
typedef ArrayMeasColumn<MEarthMagnetic, MVEarthMagnetic> MEarthMagneticArrCol;

#endif
