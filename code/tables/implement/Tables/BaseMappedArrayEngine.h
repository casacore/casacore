//# BaseMappedArrayEngine.h: Abstract virtual column engine for source->target mapping
//# Copyright (C) 1995,1996,1997,1999,2001,2002
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

#ifndef TABLES_BASEMAPPEDARRAYENGINE_H
#define TABLES_BASEMAPPEDARRAYENGINE_H

//# Includes
#include <tables/Tables/VirtColEng.h>
#include <tables/Tables/VirtArrCol.h>
#include <casa/Arrays/IPosition.h>

//# Forward Declarations
template<class T> class ROArrayColumn;
template<class T> class ArrayColumn;
class TableColumn;


// <summary>
// Templated virtual column engine for an table array of any type.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=VirtualColumnEngine>VirtualColumnEngine</linkto>
//   <li> <linkto class=VirtualArrayColumn>VirtualArrayColumn</linkto>
// </prerequisite>

// <etymology>
// BaseMappedArrayEngine contains for the 1-1 mapping of a source
// column to a target column (both containing arrays). 
// </etymology>

// <synopsis> 
// BaseMappedArrayEngine is an abstract base class for virtual column engines
// which map data from the arrays in the source column to
// the arrays in the target column.
// Examples of classes using this base class are
// <linkto class=ScaledArrayEngine>ScaledArrayEngine</linkto> and
// <linkto class=RetypedArrayEngine>RetypedArrayEngine</linkto>.
//
// The source column has to be bound to the virtual column engine used
// for it. The target column will usually be bound to a storage manager,
// but any other suitable data manager is possible. E.g. it is
// possible to use <src>MappedArrayEngine<StokesVector,float></src>
// to map a StokesVector to a float column, which in its turn uses
// <src>ScaledArrayEngine<float,Int></src> to store it as integers.
// Note that the names of the source and target column have to be different,
// otherwise the table system cannot distinguish them.
//
// This base class does several tasks for the derived classes.
// The main one is to keep and handle the information about the source
// and target column. The name of the target column is written as a keyword
// in the source column. In this way the target column is known when
// a table is read back. It also creates <src>(RO)ArrayColumn<T></src>
// objects to access the target column. The function roColumn gives
// read access, while rwColumn gives write access.
//
// An engine object should be used for one column only, because the target
// column name is part of the engine. If it would be used for more than
// one column, they would all share the same target column.
// When the engine is bound to a column, it is checked if the name
// of that column matches the given source column name.
//
// The engine can be used for a column containing any kind of array
// (thus direct or indirect, fixed or variable shaped)) as long as the
// source array can be stored in the target array. Thus a fixed shaped
// source can use a variable shaped target, but not vice versa.
// A fixed shape indirect source can use a target with direct arrays.
//
// The DataManager framework contains various virtual functions.
// This class implements several, but not all of them. Furthermore
// some implementations may not be optimal or correct for derived classes.
// Hereafter follows a list of functions which may need implementation
// in derived classes. The classes mentioned in the examples below show
// implementations of these functions.
// <ul>
// <li>
// The following (virtual) functions have to be implemented:
// <dl>
// <dt><src>
//       ~... (the destructor)
// </src>
// <dt><src>
//        DataManager* clone() const;
// </src>
// <dt><src>
//        String dataManagerType() const;
// </src>
// <dt><src>
//        static void registerClass();
// </src>
// <dt><src>
//        static DataManager* makeObject (const String& dataManagerType);
// </src>
// <dt><src>
//        void getArray (uInt rownr, Array<T>& data);
// </src>
// <dt><src>
//        void putArray (uInt rownr, const Array<T>& data);
// </src>
// </dl>
// <li>
// For efficiency reasons it could be better to implement the following
// functions:
// <dl>
// <dt><src>
//        void getSlice (uInt rownr, const Slicer& slicer, Array<T>& data);
// </src>
// <dt><src>
//        void putSlice (uInt rownr, const Slicer& slicer,
//                       const Array<T>& data);
// </src>
// <dt><src>
//        void getArrayColumn (Array<T>& data);
// </src>
// <dt><src>
//        void putArrayColumn (const Array<T>& data);
// </src>
// <dt><src>
//        void getColumnSlice (const Slicer& slicer, Array<T>& data);
// </src>
// <dt><src>
//        void putColumnSlice (const Slicer& slicer, const Array<T>& data);
// </src>
// </dl>
// <li>
// The following functions have to be implemented when the shapes
// of the source and target arrays are not the same.
// <dl>
// <dt><src>
//    void setShapeColumn (const IPosition& shape);
// </src>
// <dt><src>
//    void setShape (uInt rownr, const IPosition& shape);
// </src>
// <dt><src>
//    uInt ndim (uInt rownr);
// </src>
// <dt><src>
//    IPosition shape (uInt rownr);
// </src>
// </dl>
// <li>
// The following functions deal with the initialization and persistence
// of engine specific variables. When the class has variables of its
// own, these functions may need to be implemented. Implementations of
// create and prepare have to call the similar functions in this base class.
// <dl>
// <dt><src>
//    void close (AipsIO& ios);
// </src>
// <dt><src>
//    void create (uInt nrrow);
// </src>
// <dt><src>
//    void open (uInt nrrow, AipsIO& ios);
// </src>
// <dt><src>
//    void prepare();
// </src>
// </dl>
// <li>
// The following functions do not need to be declared and implemented
// in derived classes unless it is a very special case.
// <dl>
// <dt><src>
//    String dataManagerName() const;
// </src>
// <dt><src>
//    Bool canAddRow() const;
// </src>
// <dt><src>
//    Bool canRemoveRow() const;
// </src>
// <dt><src>
//    void addRow (uInt nrrow);
// </src>
// <dt><src>
//    void removeRow (uInt rownr);
// </src>
// <dt><src>
//    DataManagerColumn* makeDirArrColumn (const String& columnName,
//						 int dataType,
//						 const String& dataTypeId);
// </src>
// <dt><src>
//    DataManagerColumn* makeIndArrColumn (const String& columnName,
//						 int dataType,
//						 const String& dataTypeId);
// </src>
// <dt><src>
//    Bool isWritable() const;
// </src>
// <dt><src>
//    Bool isShapeDefined (uInt rownr);
// </src>
// </dl>
// </ul>
// </synopsis>

// <example>
// The derived classes
// <linkto class=ScaledArrayEngine>ScaledArrayEngine</linkto> and
// <linkto class=RetypedArrayEngine>RetypedArrayEngine</linkto>
// are two examples of how to derive a class from this base class.
// Note that ScaledArrayEngine does not need to implement functions
// dealing with shapes, because it can use them from this base class.
// On the other hand they need to be implemented in RetypedArrayEngine.
// </example>

// <motivation>
// This base class implements several functions making the implementation
// of derived classes simpler. Many details are implemented here, so often
// only the basic mapping functions (get, put) need to be implemented
// in a derived class.
// </motivation>

// <templating arg=SourceType>
//  <li> default constructor
//  <li> copy constructor
//  <li> assignment operator
//  <li> <src>static String dataTypeId();   // unique name of the class</src>
// </templating>
// <templating arg=TargetType>
//  <li> Default constructor
//  <li> Copy constructor
//  <li> Assignment operator
// </templating>


template<class SourceType, class TargetType> class BaseMappedArrayEngine : public VirtualColumnEngine, public VirtualArrayColumn<SourceType>
{
public:
    // Adding rows is possible for this engine.
    virtual Bool canAddRow() const;

    // Deleting rows is possible for this engine.
    virtual Bool canRemoveRow() const;

    // Give the source name.
    const String& sourceName() const;

protected:

    // Construct an engine to convert the source column to the target column.
    // TargetColumnName is the name of the column where the converted
    // data will be put and must have data type TargetType.
    // The source column using this engine must have data type SourceType.
    BaseMappedArrayEngine (const String& sourceColumnName,
			   const String& targetColumnName);

    // Destructor is mandatory.
    ~BaseMappedArrayEngine();

    // The default constructor is required for reconstruction of the
    // engine when a table is read back.
    BaseMappedArrayEngine();

    // Copy constructor is only used by copy constructor of derived classes.
    // (so it is made protected).
    BaseMappedArrayEngine
	              (const BaseMappedArrayEngine<SourceType, TargetType>&);

    // Set the source and target column name.
    void setNames (const String& sourceName, const String& targetName);

    // Get the target name.
    const String& targetName() const;

    // Give readonly access to the target column.
    // This can be used by the derived classes to get data.
    inline ROArrayColumn<TargetType>& roColumn();

    // Give read/write access to the target column.
    // This can be used by the derived classes to put data.
    inline ArrayColumn<TargetType>& rwColumn();

    // The column is writable if the underlying target column is writable.
    virtual Bool isWritable() const;

    // Create the column object for the array column in this engine.
    // It will check if the given column name matches the source
    // column name. This assures that the engine is bound to the
    // correct column.
    virtual DataManagerColumn* makeIndArrColumn (const String& columnName,
						 int dataType,
						 const String& dataTypeId);

    // Initialize the object for a new table.
    // It defines a source column keyword telling the target column name.
    // Initially the table has the given number of rows.
    // A derived class can have its own create function, but that should
    // always call this create function.
    virtual void create (uInt initialNrrow);

    // Preparing consists of setting the writable switch and
    // adding the initial number of rows in case of create.
    // It reads the target column name from the source column keywords.
    // A derived class can have its own prepare function, but that should
    // always call this prepare function.
    virtual void prepare();

    // Do the 2 stages of the prepare (define columns and adding rows).
    // <group>
    void prepare1();
    void prepare2();
    // </group>

    // Reopen the engine for read/write access.
    // It makes the column writable if the underlying column is writable.
    virtual void reopenRW();

    // Rows are added to the end of the table.
    // If the source column has FixedShape arrays and the target not,
    // the shape in each target row will be set.
    // This assures that the arrays are properly defined in each row,
    // so putSlice can be used without problems.
    // <br>The second version is used by prepare2, because in case a column is
    // added to an already existing table, table.nrow() gives the existing
    // number of columns instead of 0.
    // <group>
    virtual void addRow (uInt nrrow);
    virtual void addRowInit (uInt startRow, uInt nrrow);
    // </group>

    // Deleting rows is possible and is a no-op for this engine.
    virtual void removeRow (uInt rownr);

    // Set the shape of the FixedShape arrays in the column.
    // This function only gets called if the column has FixedShape arrays.
    // The shape gets saved and used to set the shape of the arrays
    // in the target in case the target has non-FixedShape arrays.
    // This implementation assumes the shape of source and target arrays
    // are the same. If not, it has to be overidden in a derived class.
    virtual void setShapeColumn (const IPosition& shape);

    // Define the shape of the array in the given row.
    // It will define the shape of the (underlying) array.
    // This implementation assumes the shape of source and target arrays
    // are the same. If not, it has to be overidden in a derived class.
    virtual void setShape (uInt rownr, const IPosition& shape);

    // Test if the (underlying) array is defined in the given row.
    virtual Bool isShapeDefined (uInt rownr);

    // Get the dimensionality of the (underlying) array in the given row.
    // This implementation assumes the dimensionality of source and
    // target arrays are the same. If not, it has to be overidden in a
    // derived class.
    virtual uInt ndim (uInt rownr);

    // Get the shape of the (underlying) array in the given row.
    // This implementation assumes the shape of source and target arrays
    // are the same. If not, it has to be overidden in a derived class.
    virtual IPosition shape (uInt rownr);

    // The data manager can handle changing the shape of an existing array
    // when the underlying target column can do it.
    virtual Bool canChangeShape() const;

    // Make a table column object for the given column.
    // This has to be used in the create function, otherwise it could not
    // create a TableColumn object to store data in the column keywords.
    TableColumn makeTableColumn (const String& columnName);
    

private:
    // Assignment is not needed and therefore forbidden
    // (so it is made private and not implemented).
    BaseMappedArrayEngine<SourceType, TargetType>& operator=
	                (const BaseMappedArrayEngine<SourceType, TargetType>&);


    //# Now define the data members.
    String         sourceName_p;         //# source column name
    String         targetName_p;         //# target column name
    Bool           tempWritable_p;       //# True =  create phase, so column
    //#                                              is temporarily writable
    //#                                      False = asks target column
    uInt           initialNrrow_p;       //# initial #rows in case of create
    Bool           arrayIsFixed_p;       //# True = source is FixedShape array
    IPosition      shapeFixed_p;         //# shape in case FixedShape array
    ROArrayColumn<TargetType>*  roColumn_p;    //# the target column (for get)
    ArrayColumn<TargetType>*    column_p;      //# the target column (for put)
};



template<class SourceType, class TargetType>
inline const String&
BaseMappedArrayEngine<SourceType, TargetType>::sourceName() const
    { return sourceName_p; }

template<class SourceType, class TargetType>
inline const String&
BaseMappedArrayEngine<SourceType, TargetType>::targetName() const
    { return targetName_p; }

template<class SourceType, class TargetType>
inline void
BaseMappedArrayEngine<SourceType, TargetType>::setNames
                    (const String& sourceName, const String& targetName)
{
    sourceName_p = sourceName;
    targetName_p = targetName;
}

template<class SourceType, class TargetType>
inline ROArrayColumn<TargetType>&
BaseMappedArrayEngine<SourceType, TargetType>::roColumn()
    { return *roColumn_p; }

template<class SourceType, class TargetType>
inline ArrayColumn<TargetType>&
BaseMappedArrayEngine<SourceType, TargetType>::rwColumn()
    { return *column_p; }


#endif
