//# ScaledComplexData.h: Templated virtual column engine to scale a complex table array
//# Copyright (C) 1999,2000
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

#if !defined(AIPS_SCALEDCOMPLEXDATA_H)
#define AIPS_SCALEDCOMPLEXDATA_H


//# Includes
#include <aips/Tables/BaseMappedArrayEngine.h>

//# Forward Declarations
template<class T> class ROScalarColumn;


// <summary>
// Templated virtual column engine to scale a complex table array
// </summary>

// <use visibility=export>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> VirtualColumnEngine
//   <li> VirtualArrayColumn
// </prerequisite>

// <synopsis> 
// ScaledComplexData is a virtual column engine which scales an array
// of a complex type to 2 values of another type (to save disk storage).
// For example, <src>ScaledComplexData<Complex,Short></src> resembles the
// classic AIPS compress method which scales the data from float to short.
// The (complex) scale factor and offset value can be given in two ways:
// <ul>
//  <li> As a fixed value which is used for all arrays in the column.
//  <li> As the name of a column. In this way each array in a
//         column can have its own scale and offset value.
//         The scale and offset value in a row must be put before
//         the array is put and should not be changed anymore.
// </ul>
// It is also possible to have a variable scale factor with a fixed offset
// value.
// As in FITS the scale and offset values are used as:
// <br><src> True_value = Stored_value * scale + offset; </src>
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
// This class can also serve as an example of how to implement
// a virtual column engine.
// </synopsis> 

// <motivation>
// This class allows to store data in a smaller representation.
// It is needed to resemble the classic AIPS compress option.
// It adds the scale and offset value on a per row basis.
//
// Because the engine can serve only one column, it was possible to
// combine the engine and the column functionality in one class.
// This has been achieved using multiple inheritance.
// The advantage of this is that only one templated class is used,
// so less template instantiations are needed.
//
// Class ScaledArrayEngine could not be used, because complex integer
// types are not supported in the tabe system.
// </motivation>

// <example>
// <srcblock>
// // Create the table description and 2 columns with indirect arrays in it.
// // The Int column will be stored, while the double will be
// // used as virtual.
// TableDesc tableDesc ("", TableDesc::Scratch);
// tableDesc.addColumn (ArrayColumnDesc<Short> ("storedArray"));
// tableDesc.addColumn (ArrayColumnDesc<Complex> ("virtualArray"));
//
// // Create a new table using the table description.
// SetupNewTable newtab (tableDesc, "tab.data", Table::New);
//
// // Create the array scaling engine to scale from double to Int
// // and bind it to the double column.
// // Create the table.
// ScaledComplexData<Complex,Short> scalingEngine("virtualArray",
//                                                "storedArray", 10);
// newtab.bindColumn ("virtualArray", scalingEngine);
// Table table (newtab);
//
// // Store a 2-D array (with dim. 3,4) into each row of the column.
// // The shape of each array in the column is implicitly set by the put
// // function. This will also set the shape of the underlying Int array
// // (as a 3-D array with shape 2,3,4).
// ArrayColumn data (table, "virtualArray");
// Array<DComplex> someArray(IPosition(2,3,4));
// someArray = 0;
// for (uInt i=0, i<10; i++) {          // table will have 10 rows
//     table.addRow();
//     data.put (i, someArray)
// }
// </srcblock>
// </example>

// <templating arg=SourceType>
//  only complex data types
// </templating>
// <templating arg=TargetType>
//  only built-in numerics data types
// </templating>

template<class SourceType, class TargetType>
class ScaledComplexData : public BaseMappedArrayEngine<SourceType, TargetType>
{
public:

    // Construct an engine to scale all arrays in a column with
    // the given offset and scale factor.
    // TargetColumnName is the name of the column where the scaled
    // data will be put and must have data type TargetType.
    // The source column using this engine must have data type SourceType.
    ScaledComplexData (const String& sourceColumnName,
		       const String& targetColumnName,
		       SourceType scale,
		       SourceType offset = 0);

    // Construct an engine to scale the arrays in a column.
    // The scale and offset values are taken from a column with
    // the given names. In that way each array has its own scale factor
    // and offset value.
    // An exception is thrown if these columns do not exist.
    // SourceColumnName is the name of the source column and is used to
    // check if the engine gets bound to the correct column.
    // TargetColumnName is the name of the column where the scaled
    // data will be put and must have data type TargetType.
    // The source column using this engine must have data type SourceType.
    // <group>
    ScaledComplexData (const String& sourceColumnName,
		       const String& targetColumnName,
		       const String& scaleColumnName,
		       SourceType offset = 0);
    ScaledComplexData (const String& sourceColumnName,
		       const String& targetColumnName,
		       const String& scaleColumnName,
		       const String& offsetColumnName);
    // </group>

    // Destructor is mandatory.
    ~ScaledComplexData();

    // Return the type name of the engine (i.e. its class name).
    String dataManagerType() const;

    // Return the name of the class.
    // This includes the names of the template arguments.
    static String className();

    // The engine can access column cells.
    virtual Bool canAccessArrayColumnCells (Bool& reask) const;

    // Register the class name and the static makeObject "constructor".
    // This will make the engine known to the table system.
    // The automatically invoked registration function in DataManReg.cc
    // contains ScaledComplexData<double,Int>.
    // Any other instantiation of this class must be registered "manually"
    // (or added to DataManReg.cc).
    static void registerClass();

private:
    // The default constructor is required for reconstruction of the
    // engine when a table is read back.
    ScaledComplexData();

    // Copy constructor is only used by clone().
    // (so it is made private).
    ScaledComplexData (const ScaledComplexData<SourceType,TargetType>&);

    // Assignment is not needed and therefore forbidden
    // (so it is made private and not implemented).
    ScaledComplexData<SourceType,TargetType>& operator=
                           (const ScaledComplexData<SourceType,TargetType>&);

    // Clone the engine object.
    virtual DataManager* clone() const;

    // Initialize the object for a new table.
    // It defines the keywords containing the engine parameters.
    virtual void create (uInt initialNrrow);

    // Preparing consists of setting the writable switch and
    // adding the initial number of rows in case of create.
    // Furthermore it reads the keywords containing the engine parameters.
    virtual void prepare();

    // Set the shape of the FixedShape arrays in the column.
    // This function only gets called if the column has FixedShape arrays.
    // The shape gets saved and used to set the shape of the arrays
    // in the target in case the target has non-FixedShape arrays.
    virtual void setShapeColumn (const IPosition& shape);

    // Define the shape of the array in the given row.
    // When the shape of the (underlying) target array has already been
    // defined, it checks whether its latter dimensions match the given
    // source shape. When matching, nothing will be done.
    // When mismatching or when the target shape has not been defined
    // yet, the target shape will be defined from the source shape and
    // the source element shape.
    // E.g. in case of a StokesVector a source shape of (512,512)
    // results in a target shape of (4,512,512).
    virtual void setShape (uInt rownr, const IPosition& shape);

    // Get the dimensionality of the array in the given row.
    virtual uInt ndim (uInt rownr);

    // Get the shape of the array in the given row.
    // This is done by stripping the first dimension from the shape
    // of the underlying target array.
    virtual IPosition shape (uInt rownr);

    // Get an array in the given row.
    // This will scale and offset from the underlying array.
    virtual void getArray (uInt rownr, Array<SourceType>& array);

    // Put an array in the given row.
    // This will scale and offset to the underlying array.
    virtual void putArray (uInt rownr, const Array<SourceType>& array);

    // Get a section of the array in the given row.
    // This will scale and offset from the underlying array.
    virtual void getSlice (uInt rownr, const Slicer& slicer,
			   Array<SourceType>& array);

    // Put into a section of the array in the given row.
    // This will scale and offset to the underlying array.
    virtual void putSlice (uInt rownr, const Slicer& slicer,
			   const Array<SourceType>& array);

    // Get an entire column.
    // This will scale and offset from the underlying array.
    virtual void getArrayColumn (Array<SourceType>& array);

    // Put an entire column.
    // This will scale and offset to the underlying array.
    virtual void putArrayColumn (const Array<SourceType>& array);

    // Get some array values in the column.
    // This will scale and offset from the underlying array.
    virtual void getArrayColumnCells (const RefRows& rownrs,
				      Array<SourceType>& data);

    // Put some array values in the column.
    // This will scale and offset to the underlying array.
    virtual void putArrayColumnCells (const RefRows& rownrs,
				      const Array<SourceType>& data);

    // Get a section of all arrays in the column.
    // This will scale and offset from the underlying array.
    virtual void getColumnSlice (const Slicer& slicer,
				 Array<SourceType>& array);

    // Put a section of all arrays in the column.
    // This will scale and offset to the underlying array.
    virtual void putColumnSlice (const Slicer& slicer,
				 const Array<SourceType>& array);

    // Get a section of some arrays in the column.
    // This will scale and offset from the underlying array.
    virtual void getColumnSliceCells (const RefRows& rownrs,
				      const Slicer& slicer,
				      Array<SourceType>& data);

    // Put into a section of some arrays in the column.
    // This will scale and offset to the underlying array.
    virtual void putColumnSliceCells (const RefRows& rownrs,
				      const Slicer& slicer,
				      const Array<SourceType>& data);

    // Scale and/or offset target to array.
    // This is meant when reading an array from the target column.
    // It optimizes for scale=1 and/or offset=0.
    void scaleOnGet (SourceType scale, SourceType offset,
		     Array<SourceType>& array,
		     const Array<TargetType>& target);

    // Scale and/or offset array to target.
    // This is meant when writing an array into the target column.
    // It optimizes for scale=1 and/or offset=0.
    void scaleOnPut (SourceType scale, SourceType offset,
		     const Array<SourceType>& array,
		     Array<TargetType>& target);

    // Scale and/or offset target to array for the entire column.
    // When the scale and offset are fixed, it will do the entire array.
    // Otherwise it iterates through the array and applies the scale
    // and offset per row.
    void scaleColumnOnGet (Array<SourceType>& array,
			   const Array<TargetType>& target);

    // Scale and/or offset array to target for the entire column.
    // When the scale and offset are fixed, it will do the entire array.
    // Otherwise it iterates through the array and applies the scale
    // and offset per row.
    void scaleColumnOnPut (const Array<SourceType>& array,
			   Array<TargetType>& target);

    // Scale and/or offset target to array for some cells in the column.
    // When the scale and offset are fixed, it will do the entire array.
    // Otherwise it iterates through the array and applies the scale
    // and offset per row.
    void scaleCellsOnGet (Array<SourceType>& array,
			  const Array<TargetType>& target,
			  const RefRows& rownrs);

    // Scale and/or offset array to target for some cells in the column.
    // When the scale and offset are fixed, it will do the entire array.
    // Otherwise it iterates through the array and applies the scale
    // and offset per row.
    void scaleCellsOnPut (const Array<SourceType>& array,
			  Array<TargetType>& target,
			  const RefRows& rownrs);

    // Determine the shape of an array in the target column.
    IPosition targetShape (const IPosition& sourceShape) const
      { return IPosition(1,2).concatenate (sourceShape); }

    // Convert the Slicer for a source to a Slicer for the target.
    Slicer targetSlicer (const Slicer& sourceSlicer) const;

    //# Now define the data members.
    String         scaleName_p;          //# name of scale column
    String         offsetName_p;         //# name of offset column
    SourceType     scale_p;              //# scale factor
    SourceType     offset_p;             //# offset value
    Bool           fixedScale_p;         //# scale is a fixed column
    Bool           fixedOffset_p;        //# offset is a fixed column
    ROScalarColumn<SourceType>* scaleColumn_p;  //# column with scale value
    ROScalarColumn<SourceType>* offsetColumn_p; //# column with offset value

    // Get the scale value for this row.
    SourceType getScale (uInt rownr);

    // Get the offset value for this row.
    SourceType getOffset (uInt rownr);

public:
    // Define the "constructor" to construct this engine when a
    // table is read back.
    // This "constructor" has to be registered by the user of the engine.
    // If the engine is commonly used, its registration can be added
    // to the registerAllCtor function in DataManReg.cc. 
    // That function gets automatically invoked by the table system.
    static DataManager* makeObject (const String& dataManagerType);
};


#endif
