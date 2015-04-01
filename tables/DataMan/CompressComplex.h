//# CompressComplex.h: Virtual column engine to scale a table Complex array
//# Copyright (C) 2001,2002,2003
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

#ifndef TABLES_COMPRESSCOMPLEX_H
#define TABLES_COMPRESSCOMPLEX_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/BaseMappedArrayEngine.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/BasicSL/Complex.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Virtual column engine to scale a table Complex array
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tCompressComplex.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> VirtualColumnEngine
//   <li> VirtualArrayColumn
// </prerequisite>

// <synopsis> 
// CompressComplex is a virtual column engine which scales an array
// of one type to another type to save disk storage.
// This resembles the classic AIPS compress method which scales the
// data from Complex to int.
// The scale factor and offset values can be given in two ways:
// <ul>
//  <li> As a fixed values which is used for all arrays in the column.
//       These values have to be given when constructing of the engine.
//  <li> As the name of a column. In this way each array in the
//         column has its own scale and offset value.
//         By default it uses auto-scaling (see below).
//         Otherwise the scale and offset value in a row must be put
//         before the array is put and should not be changed anymore.
// </ul>
// Auto-scaling means that the engine will determine the scale
// and offset value itself when an array (or a slice) is put.
// It does it by mapping the values in the array to the range [-32767,32767].
// At each put the scale/offset values are changed as needed.
// Note that with auto-scaling <src>putSlice</src> can be somewhat
// slower, because the entire array might need to be rescaled.
//
// As in FITS the scale and offset values are used as:
// <br><src> True_value = Stored_value * scale + offset; </src>
//
// An engine object should be used for one column only, because the stored
// column name is part of the engine. If it would be used for more than
// one column, they would all share the same stored column.
// When the engine is bound to a column, it is checked if the name
// of that column matches the given virtual column name.
//
// The engine can be used for a column containing any kind of array
// (thus direct or indirect, fixed or variable shaped)) as long as the
// virtual array can be stored in the stored array. Thus a fixed shaped
// virtual can use a variable shaped stored, but not vice versa.
// A fixed shape indirect virtual can use a stored with direct arrays.
//
// This class can also serve as an example of how to implement
// a virtual column engine.
// </synopsis> 

// <motivation>
// This class allows to store data in a smaller representation.
// It is needed to resemble the classic AIPS compress option.
//
// Because the engine can serve only one column, it was possible to
// combine the engine and the column functionality in one class.
// </motivation>

// <example>
// <srcblock>
// // Create the table description and 2 columns with indirect arrays in it.
// // The Int column will be stored, while the double will be
// // used as virtual.
// TableDesc tableDesc ("", TableDesc::Scratch);
// tableDesc.addColumn (ArrayColumnDesc<Int> ("storedArray"));
// tableDesc.addColumn (ArrayColumnDesc<Complex> ("virtualArray"));
// tableDesc.addColumn (ScalarColumnDesc<Complex> ("scale"));
// tableDesc.addColumn (ScalarColumnDesc<Float> ("offset"));
//
// // Create a new table using the table description.
// SetupNewTable newtab (tableDesc, "tab.data", Table::New);
//
// // Create the array scaling engine (with auto-scale)
// // and bind it to the Complex column.
// CompressComplex scalingEngine("virtualArray", "storedArray",
//                               "scale", "offset");
// newtab.bindColumn ("virtualArray", scalingEngine);
// // Create the table.
// Table table (newtab);
//
// // Store a 3-D array (with dim. 2,3,4) into each row of the column.
// // The shape of each array in the column is implicitly set by the put
// // function. This will also set the shape of the underlying Int array.
// ArrayColumn data (table, "virtualArray");
// Array<double> someArray(IPosition(4,2,3,4));
// someArray = 0;
// for (uInt i=0, i<10; i++) {          // table will have 10 rows
//     table.addRow();
//     data.put (i, someArray)
// }
// </srcblock>
// </example>

class CompressComplex : public BaseMappedArrayEngine<Complex, Int>
{
public:

  // Construct an engine to scale all arrays in a column with
  // the given offset and scale factor.
  // StoredColumnName is the name of the column where the scaled
  // data will be put and must have data type Int.
  // The virtual column using this engine must have data type Complex.
  CompressComplex (const String& virtualColumnName,
		   const String& storedColumnName,
		   Float scale,
		   Float offset = 0);

  // Construct an engine to scale the arrays in a column.
  // The scale and offset values are taken from a column with
  // the given names. In that way each array has its own scale factor
  // and offset value.
  // An exception is thrown if these columns do not exist.
  // VirtualColumnName is the name of the virtual column and is used to
  // check if the engine gets bound to the correct column.
  // StoredColumnName is the name of the column where the scaled
  // data will be put and must have data type Int.
  // The virtual column using this engine must have data type Complex.
  CompressComplex (const String& virtualColumnName,
		   const String& storedColumnName,
		   const String& scaleColumnName,
		   const String& offsetColumnName,
		   Bool autoScale = True);

  // Construct from a record specification as created by getmanagerSpec().
  CompressComplex (const Record& spec);

  // Destructor is mandatory.
  ~CompressComplex();

  // Return the type name of the engine (i.e. its class name).
  virtual String dataManagerType() const;

  // Get the name given to the engine (is the virtual column name).
  virtual String dataManagerName() const;
  
  // Record a record containing data manager specifications.
  virtual Record dataManagerSpec() const;

  // Return the name of the class.
  // This includes the names of the template arguments.
  static String className();

  // Register the class name and the static makeObject "constructor".
  // This will make the engine known to the table system.
  static void registerClass();

protected:
  // Copy constructor is only used by clone() and derived class.
  // (so it is made private).
  CompressComplex (const CompressComplex&);

private:
  // Assignment is not needed and therefore forbidden
  // (so it is made private and not implemented).
  CompressComplex& operator= (const CompressComplex&);

  // Clone the engine object.
  virtual DataManager* clone() const;

protected:
  // Initialize the object for a new table.
  // It defines the keywords containing the engine parameters.
  virtual void create (uInt initialNrrow);

private:
  // Preparing consists of setting the writable switch and
  // adding the initial number of rows in case of create.
  // Furthermore it reads the keywords containing the engine parameters.
  virtual void prepare();

  // Reopen the engine for read/write access.
  // It makes the column writable if the underlying column is writable.
  virtual void reopenRW();

  // Add rows to the table.
  // If auto-scaling, it initializes the scale column with 0
  // to indicate that no data has been processed yet.
  virtual void addRowInit (uInt startRow, uInt nrrow);

  // Get an array in the given row.
  // This will scale and offset from the underlying array.
  virtual void getArray (uInt rownr, Array<Complex>& array);

  // Put an array in the given row.
  // This will scale and offset to the underlying array.
  virtual void putArray (uInt rownr, const Array<Complex>& array);

  // Get a section of the array in the given row.
  // This will scale and offset from the underlying array.
  virtual void getSlice (uInt rownr, const Slicer& slicer,
			 Array<Complex>& array);

  // Put into a section of the array in the given row.
  // This will scale and offset to the underlying array.
  virtual void putSlice (uInt rownr, const Slicer& slicer,
			 const Array<Complex>& array);

  // Get an entire column.
  // This will scale and offset from the underlying array.
  virtual void getArrayColumn (Array<Complex>& array);

  // Put an entire column.
  // This will scale and offset to the underlying array.
  virtual void putArrayColumn (const Array<Complex>& array);

  // Get some array values in the column.
  // This will scale and offset from the underlying array.
  virtual void getArrayColumnCells (const RefRows& rownrs,
                                    Array<Complex>& data);

  // Put some array values in the column.
  // This will scale and offset to the underlying array.
  virtual void putArrayColumnCells (const RefRows& rownrs,
                                    const Array<Complex>& data);

  // Get a section of all arrays in the column.
  // This will scale and offset from the underlying array.
  virtual void getColumnSlice (const Slicer& slicer, Array<Complex>& array);

  // Put a section of all arrays in the column.
  // This will scale and offset to the underlying array.
  virtual void putColumnSlice (const Slicer& slicer, 
			       const Array<Complex>& array);

  // Get a section of some arrays in the column.
  // This will scale and offset from the underlying array.
  virtual void getColumnSliceCells (const RefRows& rownrs,
                                    const Slicer& slicer,
                                    Array<Complex>& data);

  // Put into a section of some arrays in the column.
  // This will scale and offset to the underlying array.
  virtual void putColumnSliceCells (const RefRows& rownrs,
                                    const Slicer& slicer,
                                    const Array<Complex>& data);

  // Scale and/or offset target to array.
  // This is meant when reading an array from the stored column.
  // It optimizes for scale=1 and/or offset=0.
  virtual void scaleOnGet (Float scale, Float offset,
			   Array<Complex>& array,
			   const Array<Int>& target);

  // Scale and/or offset array to target.
  // This is meant when writing an array into the stored column.
  // It optimizes for scale=1 and/or offset=0.
  virtual void scaleOnPut (Float scale, Float offset,
			   const Array<Complex>& array,
			   Array<Int>& target);

  // Scale and/or offset target to array for the entire column.
  // When the scale and offset are fixed, it will do the entire array.
  // Otherwise it iterates through the array and applies the scale
  // and offset per row.
  void scaleColumnOnGet (Array<Complex>& array,
			 const Array<Int>& target);

  // Scale and/or offset array to target for the entire column.
  // When the scale and offset are fixed, it will do the entire array.
  // Otherwise it iterates through the array and applies the scale
  // and offset per row.
  void scaleColumnOnPut (const Array<Complex>& array,
			 Array<Int>& target);

protected:
  //# Now define the data members.
  String         scaleName_p;          //# name of scale column
  String         offsetName_p;         //# name of offset column
  Float          scale_p;              //# fixed scale factor
  Float          offset_p;             //# fixed offset value
  Bool           fixed_p;              //# scale/offset is fixed
  Bool           autoScale_p;          //# determine scale/offset automatically
  ScalarColumn<Float>* scaleColumn_p;  //# column with scale value
  ScalarColumn<Float>* offsetColumn_p; //# column with offset value
  Array<Int>     buffer_p;             //# buffer to avoid Array constructions
                                       //# (makes multi-threading harder)

  // Get the scale value for this row.
  Float getScale (uInt rownr);

  // Get the offset value for this row.
  Float getOffset (uInt rownr);

  // Find minimum and maximum from the array data.
  // NaN and infinite values are ignored. If no values are finite,
  // minimum and maximum are set to NaN.
  virtual void findMinMax (Float& minVal, Float& maxVal,
			   const Array<Complex>& array) const;

  // Make scale and offset from the minimum and maximum of the array data.
  // If minVal is NaN, scale is set to 0.
  void makeScaleOffset (Float& scale, Float& offset,
			Float minVal, Float maxVal) const;

  // Put a part of an array in a row using given scale/offset values.
  void putPart (uInt rownr, const Slicer& slicer,
		const Array<Complex>& array,
		Float scale, Float offset);

  // Fill the array part into the full array and put it using the
  // given min/max values.
  void putFullPart (uInt rownr, const Slicer& slicer,
		    Array<Complex>& fullArray,
		    const Array<Complex>& partArray,
		    Float minVal, Float maxVal);

public:
  // Define the "constructor" to construct this engine when a
  // table is read back.
  // This "constructor" has to be registered by the user of the engine.
  // If the engine is commonly used, its registration can be added
  // to the registerAllCtor function in DataManager.cc. 
  // That function gets automatically invoked by the table system.
  static DataManager* makeObject (const String& dataManagerType,
				  const Record& spec);
};




// <summary>
// Virtual column engine to scale a table Complex array for Single Dish data
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tCompressComplex.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> CompressComplex
// </prerequisite>

// <synopsis> 
// CompressComplexSD is similar to CompressComplex, but compresses
// in a slighty different way optimized for single dish data.
// Usually the imaginary part of single dish data is 0, so the scaling
// is optimized for it.
// <br>If the imaginary part is 0, the real part is scaled with 15 bits
// extra to get a higher precision. The least significant bit is set to 0
// indicating the imag==0.
// <br>If the imaginary part is not 0, the real part is scaled normally.
// The imaginary part is scaled with 1 bit less. The least significant bit
// is set to 1 indicating that imag!=0.
// </synopsis> 

// <motivation>
// This class is created on top of CompressComplex to cope with SD data
// in a better way. Using CompressComplex often makes the imag part non-zero
// if it is scaled as 0.
// </motivation>

// <example>
// <srcblock>
// // Create the table description and 2 columns with indirect arrays in it.
// // The Int column will be stored, while the double will be
// // used as virtual.
// TableDesc tableDesc ("", TableDesc::Scratch);
// tableDesc.addColumn (ArrayColumnDesc<Int> ("storedArray"));
// tableDesc.addColumn (ArrayColumnDesc<Complex> ("virtualArray"));
// tableDesc.addColumn (ScalarColumnDesc<Complex> ("scale"));
// tableDesc.addColumn (ScalarColumnDesc<Float> ("offset"));
//
// // Create a new table using the table description.
// SetupNewTable newtab (tableDesc, "tab.data", Table::New);
//
// // Create the array scaling engine (with auto-scale)
// // and bind it to the Complex column.
// CompressComplexSD scalingEngine("virtualArray", "storedArray",
//                                 "scale", "offset");
// newtab.bindColumn ("virtualArray", scalingEngine);
// // Create the table.
// Table table (newtab);
//
// // Store a 3-D array (with dim. 2,3,4) into each row of the column.
// // The shape of each array in the column is implicitly set by the put
// // function. This will also set the shape of the underlying Int array.
// ArrayColumn data (table, "virtualArray");
// Array<double> someArray(IPosition(4,2,3,4));
// someArray = 0;
// for (uInt i=0, i<10; i++) {          // table will have 10 rows
//     table.addRow();
//     data.put (i, someArray)
// }
// </srcblock>
// </example>

class CompressComplexSD : public CompressComplex
{
public:

  // Construct an engine to scale all arrays in a column with
  // the given offset and scale factor.
  // StoredColumnName is the name of the column where the scaled
  // data will be put and must have data type Int.
  // The virtual column using this engine must have data type Complex.
  CompressComplexSD (const String& virtualColumnName,
		     const String& storedColumnName,
		     Float scale,
		     Float offset = 0);

  // Construct an engine to scale the arrays in a column.
  // The scale and offset values are taken from a column with
  // the given names. In that way each array has its own scale factor
  // and offset value.
  // An exception is thrown if these columns do not exist.
  // VirtualColumnName is the name of the virtual column and is used to
  // check if the engine gets bound to the correct column.
  // StoredColumnName is the name of the column where the scaled
  // data will be put and must have data type Int.
  // The virtual column using this engine must have data type Complex.
  CompressComplexSD (const String& virtualColumnName,
		     const String& storedColumnName,
		     const String& scaleColumnName,
		     const String& offsetColumnName,
		     Bool autoScale = True);

  // Construct from a record specification as created by getmanagerSpec().
  CompressComplexSD (const Record& spec);

  // Destructor is mandatory.
  ~CompressComplexSD();

  // Return the type name of the engine (i.e. its class name).
  virtual String dataManagerType() const;

  // Return the name of the class.
  // This includes the names of the template arguments.
  static String className();

  // Register the class name and the static makeObject "constructor".
  // This will make the engine known to the table system.
  static void registerClass();

private:
  // Copy constructor is only used by clone().
  // (so it is made private).
  CompressComplexSD (const CompressComplexSD&);

  // Assignment is not needed and therefore forbidden
  // (so it is made private and not implemented).
  CompressComplexSD& operator= (const CompressComplexSD&);

  // Clone the engine object.
  virtual DataManager* clone() const;

  // Initialize the object for a new table.
  // It defines the keywords containing the engine parameters.
  virtual void create (uInt initialNrrow);

  // Scale and/or offset target to array.
  // This is meant when reading an array from the stored column.
  // It optimizes for scale=1 and/or offset=0.
  virtual void scaleOnGet (Float scale, Float offset,
			   Array<Complex>& array,
			   const Array<Int>& target);

  // Scale and/or offset array to target.
  // This is meant when writing an array into the stored column.
  // It optimizes for scale=1 and/or offset=0.
  virtual void scaleOnPut (Float scale, Float offset,
			   const Array<Complex>& array,
			   Array<Int>& target);

  // Find minimum and maximum from the array data.
  // NaN and infinite values and zero imaginary parts are ignored.
  // If no values are finite, minimum and maximum are set to NaN.
  virtual void findMinMax (Float& minVal, Float& maxVal,
			   const Array<Complex>& array) const;

public:
  // Define the "constructor" to construct this engine when a
  // table is read back.
  // This "constructor" has to be registered by the user of the engine.
  // If the engine is commonly used, its registration can be added
  // to the registerAllCtor function in DataManager.cc. 
  // That function gets automatically invoked by the table system.
  static DataManager* makeObject (const String& dataManagerType,
				  const Record& spec);
};




inline Float CompressComplex::getScale (uInt rownr)
{
  return (fixed_p  ?  scale_p : (*scaleColumn_p)(rownr));
}
inline Float CompressComplex::getOffset (uInt rownr)
{
  return (fixed_p  ?  offset_p : (*offsetColumn_p)(rownr));
}



} //# NAMESPACE CASACORE - END

#endif
