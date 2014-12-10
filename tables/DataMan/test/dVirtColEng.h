//# dVirtColEng.h: Demo of a virtual column engine
//# Copyright (C) 1994,1995,1996,1997,2001
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

#ifndef TABLES_DVIRTCOLENG_H
#define TABLES_DVIRTCOLENG_H

//# Includes
#include <casacore/tables/DataMan/VirtColEng.h>
#include <casacore/tables/DataMan/VirtScaCol.h>
#include <casacore/tables/DataMan/VirtArrCol.h>

#include <casacore/casa/namespace.h>
//# Forward Declarations
class DummyVirtualEngine;
namespace casacore {
template<class T> class ScalarColumn;
template<class T> class ArrayColumn;
}


// <category lib=aips module="Tables" sect="Virtual Columns">
// <summary> Demo of a virtual scalar column </summary>
// <reviewed reviewer="GvD" date="2004/07/09" tests="">
//
// <prerequisite>
//# Classes you should understand before using this one.
// <list>
//   <item> VirtualColumnEngine
//   <item> VirtualScalarColumn
//   <item> VirtualArrayColumn
// </list>
// </prerequisite>
//
// <synopsis> 
// DummyVirtualScalar is an example of how to implement a virtual
// column class handling a scalar.
// This class scales the data in table column "DATA1" from Int to
// double and back using a scale factor given at construction time.
// This class is used by DummyVirtualEngine which is the engine for
// handling this scalar column and another column.
// </synopsis> 
//
// <motivation>
// This class is an example for writers of real virtual column classes.
// It is tested by tVirtColEng.cc.
// </motivation>

class DummyVirtualScalar : public VirtualScalarColumn<double>
{
public:

    // Construct it with the given scale factor.
    DummyVirtualScalar (DummyVirtualEngine*, double scale);

    // The copy constructor (copy semantics) is needed for
    // DummyVirtualEngine::clone().
    DummyVirtualScalar (const DummyVirtualScalar&);

    // Destructor is mandatory.
    ~DummyVirtualScalar();

    // Return the scale factor used.
    double scale() const
	{ return scale_p; }

    // Let the engine initialize the object for a new table.
    // It constructs the ScalarColumn object.
    void prepare (const Table& theTable);

    // Let the engine initialize the object for an existing table.
    // It reads the scale factor and constructs the ScalarColumn object.
    void open (AipsIO& ios);

    // Let the engine flush the object.
    // It writes the scale factor.
    void flush (AipsIO& ios);

private:
    // Assignment is not needed and therefore forbidden (so it is made private).
    DummyVirtualScalar& operator= (const DummyVirtualScalar&);

    // The column may be writable, so we must override the default
    // implementation in the base class VirtualScalarColumn.
    Bool isWritable() const;

    // Get a value.
    //+grp
    void get (uInt rownr, double& data);
    // We also implement the getdoubleV, because that saves a
    // virtual function call.
    void getdoubleV (uInt rownr, double* dataPtr);
    //-grp

    // Put a value.
    //+grp
    void put (uInt rownr, const double& data);
    // We also implement the putdoubleV, because that saves a
    // virtual function call.
    void putdoubleV (uInt rownr, const double* dataPtr);
    //-grp

    //# We could also define the get/putBlockDoubleV functions, but
    //# that is not required. The default implementation gets
    //# one value. Possible optimization can be done by
    //# implementing it here.
    //# The same is true for get/putColumn.


    //# Now define the data members.
    DummyVirtualEngine* enginePtr_p;    // pointer to engine object
    double scale_p;                     // scale factor
    Int    writable_p;                  // 1 = column is writable
    //                                    -1 = column is not writable
    //                                     0 = not known yet
    ScalarColumn<Int>*   column_p;      // the unscaled table column
};



// <category lib=aips module="Tables" sect="Virtual Columns">
// <summary> Dummy example of a virtual array column </summary>
// <reviewed reviewer="GvD" date="2004/07/09" tests="">
//
// <prerequisite>
//# Classes you should understand before using this one.
// <list>
//   <item> VirtualColumnEngine
//   <item> VirtualScalarColumn
//   <item> VirtualArrayColumn
// </list>
// </prerequisite>
//
// <synopsis> 
// DummyVirtualArray is an example of how to implement a virtual
// column class handling a array.
// This class scales the data in table column "DATA1" from Int to
// double and back using a scale factor given at construction time.
// This class is used by DummyVirtualEngine which is the engine for
// handling this array column and another column.
// </synopsis> 
//
// <motivation>
// This class is an example for writers of real virtual column classes.
// </motivation>

class DummyVirtualArray : public VirtualArrayColumn<double>
{
public:

    // Construct it with the given scale factor.
    DummyVirtualArray (DummyVirtualEngine*, double scale);

    // The copy constructor (copy semantics) is needed for
    // DummyVirtualEngine::clone().
    DummyVirtualArray (const DummyVirtualArray&);

    // Destructor is mandatory.
    ~DummyVirtualArray();

    // Return the scale factor used.
    double scale() const
	{ return scale_p; }

    // Let the engine initialize the object for an existing table.
    // It reads the scale factor.
    void open (AipsIO& ios);

    // Let the engine initialize the object for a new table.
    // It constructs the ArrayColumn object and sets the writable switch.
    void prepare (const Table& theTable);

    // Let the engine flush the object.
    // It writes the scale factor.
    void flush (AipsIO& ios);

private:
    // Assignment is not needed and therefore forbidden (so it is made private).
    DummyVirtualArray& operator= (const DummyVirtualArray&);

    // The column is writable, so we must override the default
    // implementation in the base class VirtualColumn.
    Bool isWritable() const;

    // Define the shape of the array.
    // This will define the shape of the underlying Int array.
    void setShape (uInt rownr, const IPosition& shape);

    // Test if the (underlying) array is defined.
    Bool isShapeDefined (uInt rownr);

    // Get the dimensionality of the (underlying) array.
    uInt ndim (uInt rownr);

    // Get the shape of the (underlying) array.
    IPosition shape (uInt rownr);

    // Get an array.
    void getArray (uInt rownr, Array<double>& array);

    // Put an array.
    void putArray (uInt rownr, const Array<double>& array);

    //# We could also define the get/putSlice functions, but
    //# that is not required. Possible optimization can be done by
    //# implementing it here.

    //# Now define the data members.
    DummyVirtualEngine* enginePtr_p;   // pointer to engine object
    double scale_p;                    // scale factor
    Int    writable_p;                 // 1 = column is writable
    //                                   -1 = column is not writable
    //                                    0 = not known yet
    ArrayColumn<Int>*   column_p;      // the unscaled table column (for put)
};



// <category lib=aips module="Tables" sect="Virtual Columns">
// <summary> Dummy example of a virtual column engine </summary>
// <reviewed reviewer="GvD" date="2004/07/09" tests="">
//
// <prerequisite>
//# Classes you should understand before using this one.
// <list>
//   <item> VirtualColumnEngine
//   <item> VirtualScalarColumn
//   <item> VirtualArrayColumn
// </list>
// </prerequisite>
//
// <synopsis> 
// DummyVirtualEngine is an example of how a virtual column engine
// can be implemented.
// This class scales the data in table column "DATA1" and "DATA2" from
// Int to double and back using scale factors given at construction time.
// It uses the classes DummyScalarColumn and DummyArrayColumn to handle
// the specific columns.
//
// The scale factors will be stored in the table, so they can be
// read back when the table is used again.
// The static function registerClass registers the name of this class and
// its makeObject function. It is important to call registerClass before
// a table using this virtual column engine is used, otherwise the
// engine is unknown to the table system.
// </synopsis> 
//
// <motivation>
// This class is an example for writers of real virtual column classes.
// </motivation>

class DummyVirtualEngine : public VirtualColumnEngine
{
public:

    // The default constructor is required for reconstruction of the
    // engine when a table is read back.
    // It uses scale factors 1. The correct values will be set by open.
    DummyVirtualEngine();

    // Create the object with the given scale factors.
    DummyVirtualEngine (double scaleData1, double scaleData2);

    // Destructor is mandatory.
    ~DummyVirtualEngine();

    // Clone the engine object.
    DataManager* clone() const;

    // Return the type name of the engine.
    // (i.e. its class name DummyVirtualEngine).
    String dataManagerType() const;

    // Register the class name and the static makeObject "constructor".
    // This will make the engine known to the table system.
    static void registerClass();

    //# In this example it is not needed to implement the functions
    //# canAddRow/Column and canDeleteRow/Column, because the
    //# default DataManager implementation of no suffices.
    //# Usually you do not need to implement them.

private:
    // The copy constructor is forbidden in this example (so it is private).
    // You can make it available if you want/need to.
    // The data manager system will use the clone function to make
    // a copy of the object. It does not need a copy constructor.
    DummyVirtualEngine (const DummyVirtualEngine&);

    // Assignment is forbidden in this example (so it is private).
    // You can make it available if you want/need to.
    // The data manager system will use the clone function to make
    // a copy of the object. It does not need the assignment operator.
    DummyVirtualEngine& operator= (const DummyVirtualEngine&);

    //# In this example it is not needed to implement the functions
    //# addRow/Column and deleteRow/Column, because the
    //# default DataManager implementation of no suffices.
    //# Usually you do not need to implement them.

    // Create the column object for the scalar column in this engine.
    DataManagerColumn* makeScalarColumn (const String& columnName,
					 int dataType, const String&);

    // Create the column object for the indirect array column in this engine.
    DataManagerColumn* makeIndArrColumn (const String& columnName,
					 int dataType,
					 const String& dataTypeId);

    // Flush the engine.
    // It will write a few things into the AipsIO object.
    Bool flush (AipsIO& ios, Bool fsync);

    // Initialize the object for a new table.
    // Intially the table has the given number of rows.
    // This function does not have to do anything; the real initialization
    // is done by prepare.
    void create (uInt initialNrrow);

    // Initialize the object for an existing table with the given number
    // of rows.
    // It will read back the data written by close.
    void open (uInt nrrow, AipsIO& ios);

    // Initialize the engine.
    void prepare();

    // Define the various engine column objects.
    DummyVirtualScalar data1_p;
    DummyVirtualArray  data2_p;


public:
    //*display 4
    // Define the "constructor" to construct this engine when a
    // table is read back.
    // This "constructor" has to be registered by the user of the engine.
    // If the engine is commonly used, its registration can be added
    // into the registerAllCtor function in DataManReg.cc. 
    // This function gets automatically invoked by the table system.
    static DataManager* makeObject (const String& dataManagerName,
				    const Record& spec);
};


#endif
