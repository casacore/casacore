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
//#
//# $Id$

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
// column as a TaQL expression in which possibly other columns are used.
// It is (of course) only possible to get data from the column; puts cannot
// be done.
// <br>
// The expression result can be a scalar or array of the basic TaQL data types.
// The column data type has to be conformant with that TaQL type, thus a
// column of any integer type has to be used for an integer TaQL result.
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
  VirtualTaQLColumn (const String& expr);

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

  // Functions to return column info.
  // <group>
  virtual int dataType() const;
  virtual Bool isWritable() const;
  virtual uInt ndim (uInt rownr);
  virtual IPosition shape (uInt rownr);
  virtual Bool isShapeDefined (uInt rownr);
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
  virtual void create (uInt);

  // Prepare compiles the expression.
  virtual void prepare();

  //# We could also define the getBlockXXV functions, but
  //# that is not required. The default implementation gets
  //# one value. Possible optimization can be done by
  //# implementing it here.
  //# The same is true for getColumn.

  // Get the scalar value in the given row.
  // The default implementation throws an "invalid operation" exception.
  // <group>
  virtual void getBoolV     (uInt rownr, Bool* dataPtr);
  virtual void getuCharV    (uInt rownr, uChar* dataPtr);
  virtual void getShortV    (uInt rownr, Short* dataPtr);
  virtual void getuShortV   (uInt rownr, uShort* dataPtr);
  virtual void getIntV      (uInt rownr, Int* dataPtr);
  virtual void getuIntV     (uInt rownr, uInt* dataPtr);
  virtual void getfloatV    (uInt rownr, float* dataPtr);
  virtual void getdoubleV   (uInt rownr, double* dataPtr);
  virtual void getComplexV  (uInt rownr, Complex* dataPtr);
  virtual void getDComplexV (uInt rownr, DComplex* dataPtr);
  virtual void getStringV   (uInt rownr, String* dataPtr);
  // </group>

  // Get the array value in the given row.
  // The argument dataPtr is in fact an Array<T>*, but a void*
  // is needed to be generic.
  // The array pointed to by dataPtr has to have the correct shape
  // (which is guaranteed by the ArrayColumn get function).
  // The default implementation throws an "invalid operation" exception.
  virtual void getArrayV (uInt rownr, void* dataPtr);

  // Get the result.
  IPosition getResult (uInt rownr, void* dataPtr);

  // Clear the result cache.
  void clearCurResult();


  //# Now define the data members.
  int            itsDataType;
  Bool           itsIsArray;
  String         itsColumnName;
  String         itsExpr;             //# TaQL expression
  TableExprNode* itsNode;             //# compiled TaQL expression
  Bool           itsTempWritable;
  Int            itsCurRow;           //# Currently evaluated row
  void*          itsCurResult;        //# result in itsCurRow
  IPosition      itsCurShape;         //# shape in itsCurRow
};


} //end namespace casacore

#endif
