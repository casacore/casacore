//# VirtualTaQLColumn.h: Virtual column engine based on TaQL
//# Copyright (C) 2005
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

#ifndef TABLES_VIRTUALTAQLCOLUMN_H
#define TABLES_VIRTUALTAQLCOLUMN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/VirtColEng.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore {
//# Forward Declarations
class TableExprNode;


// <category lib=aips module="Tables" sect="Virtual Columns">
// <summary> Virtual scalar column using TaQL</summary>
// <reviewed reviewer="GvD" date="2004/07/09" tests="">
//
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> VirtualScalarColumn
// </prerequisite>
//
// <synopsis> 
// VirtualTaQLColumn is a virtual column engine to define the contents of a
// column as a TaQL CALC expression in which possibly other columns are used.
// It is (of course) only possible to get data from the column; puts cannot
// be done. See note 199 for a description of TaQL.
// The TaQL style can be specified (such as 0- or 1-based indexing).
// <br>
// The expression result can be a scalar or array of the basic TaQL data types.
// The column data type has to be conformant with that TaQL type, thus a
// column of any integer type has to be used for an integer TaQL result.
// <br>
// Constant expressions are precalculated and cached making the retrieval of
// e.g. the full column much faster (factor 4).
// <br>
// A possible use for a virtual TaQL column is a column in a MeasurementSet
// containing a constant value. It could also be used for on-the-fly calculation
// of J2000 UVW-values or HADEC using an expression such as "derivedmscal.newuvw()"
// <note role=caution> One has to be careful with deleting columns. If in an
// existing table a TaQL expression uses a deleted column, the expression
// cannot be parsed anymore and the table cannot be opened anymore.
// In the future the Table System will be made more forgiving.
// </note>
// </synopsis> 
//
// <example>
// The following example creates a table with a few columns.
// One column is virtual and has a random value if Col3 is true.
// Otherwise it has value 0.
// <srcblock>
//    // Create the table description.
//    TableDesc td;
//    td.addColumn (ScalarColumnDesc<DComplex>("Col1"));
//    td.addColumn (ScalarColumnDesc<Int>("Col2"));
//    td.addColumn (ScalarColumnDesc<Bool>("Col3"));
//    td.addColumn (ScalarColumnDesc<Double>("ColVirt"));
//  
//    // Now create a new table from the description.
//    SetupNewTable newTab("tmtest", td, Table::New);
//    // Define the expression of the virtual column and bind the column to it.
//    // The other columns are by default bound to StandardStMan.
//    VirtualTaQLColumn engine("iif(Col3,rand(),0)");
//    newTab.bindColumn("ColVirt", engine);
//    Table tab(newTab);
// </srcblock>
// </example>

class VirtualTaQLColumn : public VirtualColumnEngine, public DataManagerColumn
{
public:

  // Construct it with the given TaQL expression.
  VirtualTaQLColumn (const String& expr, const String& style=String());

  // Construct it with the given specification.
  VirtualTaQLColumn (const Record& spec);

  // Destructor is mandatory.
  virtual ~VirtualTaQLColumn();

  // Clone the engine object.
  virtual DataManager* clone() const;

  // Get the data manager specification.
  virtual Record dataManagerSpec() const;

  // Return the type name of the engine.
  // (i.e. its class name VirtualTaQLColumn).
  virtual String dataManagerType() const;

  // Return the name of the class.
  static String className();

  // Register the class name and the static makeObject "constructor".
  // This will make the engine known to the table system.
  static void registerClass();

  // Define the "constructor" to construct this engine when a
  // table is read back.
  // This "constructor" has to be registered by the user of the engine.
  // If the engine is commonly used, its registration can be added
  // into the registerAllCtor function in DataManReg.cc. 
  // This function gets automatically invoked by the table system.
  static DataManager* makeObject (const String& dataManagerName,
				  const Record& spec);

  // Return the TaQL expression used.
  const String& expression() const
    { return itsExpr; }

  // Set the shape of an array in the column.
  // It is only called (right after the constructor) if the array has
  // a fixed shape.
  virtual void setShapeColumn (const IPosition& aShape);

  // Set the maximum length of a 'fixed length' string.
  // It is only called (right after the constructor) if the string has
  // a fixed length.
  virtual void setMaxLength (uInt maxLength);

  // Functions to return column info.
  // <group>
  virtual int dataType() const;
  virtual Bool isWritable() const;
  virtual uInt ndim (rownr_t rownr);
  virtual IPosition shape (rownr_t rownr);
  virtual Bool isShapeDefined (rownr_t rownr);
  // </group>

private:
  // Copy is not needed and therefore forbidden (so it is made private).
  VirtualTaQLColumn (const VirtualTaQLColumn&);

  // Assignment is not needed and therefore forbidden (so it is made private).
  VirtualTaQLColumn& operator= (const VirtualTaQLColumn&);

  // Create the column object for the scalar column in this engine.
  virtual DataManagerColumn* makeScalarColumn (const String& columnName,
					       int dataType, const String&);

  // Create the column object for the indirect array column in this engine.
  virtual DataManagerColumn* makeIndArrColumn (const String& columnName,
					       int dataType,
					       const String& dataTypeId);

  // Let the engine initialize the object for a new table.
  // It defines a column keyword holding the expression.
  virtual void create64 (rownr_t);

  // Prepare compiles the expression.
  virtual void prepare();

  // Get the scalar value in the given row.
  // <group>
  virtual void getBool     (rownr_t rownr, Bool* dataPtr);
  virtual void getuChar    (rownr_t rownr, uChar* dataPtr);
  virtual void getShort    (rownr_t rownr, Short* dataPtr);
  virtual void getuShort   (rownr_t rownr, uShort* dataPtr);
  virtual void getInt      (rownr_t rownr, Int* dataPtr);
  virtual void getuInt     (rownr_t rownr, uInt* dataPtr);
  virtual void getInt64    (rownr_t rownr, Int64* dataPtr);
  virtual void getfloat    (rownr_t rownr, float* dataPtr);
  virtual void getdouble   (rownr_t rownr, double* dataPtr);
  virtual void getComplex  (rownr_t rownr, Complex* dataPtr);
  virtual void getDComplex (rownr_t rownr, DComplex* dataPtr);
  virtual void getString   (rownr_t rownr, String* dataPtr);
  // </group>

  // Get the array value in the given row.
  // The array given by <src>arr</src> has to have the correct shape
  // (which is guaranteed by the ArrayColumn get function).
  virtual void getArrayV (rownr_t rownr, ArrayBase& arr);

  // Get the array result into itsCurArray.
  void getResult (rownr_t rownr);

  // Make the result cache.
  void makeCurArray();

  // Get functions implemented by means of their DataManagerColumn::getXXBase
  // counterparts, but optimized for constant expressions.
  // <group>
  virtual void getScalarColumnV (ArrayBase& arr);
  virtual void getScalarColumnCellsV (const RefRows& rownrs,
                                      ArrayBase& arr);
  // </group>

  // Fill the ColumnCache object with a constant scalar value.
  void fillColumnCache();

  // Fill an array with a constant scalar value.
  void fillArray (ArrayBase& data);

  //# Now define the data members.
  int            itsDataType;
  Bool           itsIsArray;
  Bool           itsIsConst;          //# Constant expression?
  Bool           itsTempWritable;
  String         itsColumnName;
  String         itsExpr;             //# TaQL expression
  String         itsStyle;            //# TaQL style
  TableExprNode* itsNode;             //# compiled TaQL expression
  IPosition      itsShape;            //# The shape of the column.
  uInt           itsMaxLen;           //# The maximum length of a 'fixed length' string.
  union {
    Bool     itsBool;                 //# Constant scalar values
    uChar    itsuChar;
    Short    itsShort;
    uShort   itsuShort;
    Int      itsInt;
    uInt     itsuInt;
    Int64    itsInt64;
    Float    itsFloat;
    Double   itsDouble;
  };
  Complex    itsComplex;
  DComplex   itsDComplex;
  String     itsString;
  ArrayBase* itsCurArray;             //# array value (constant or in itsCurRow)
  rownr_t    itsCurRow;               //# row of current array value
};


} //end namespace casacore

#endif
