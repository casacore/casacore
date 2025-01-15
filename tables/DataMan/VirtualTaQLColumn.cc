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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/DataMan/VirtualTaQLColumn.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  VirtualTaQLColumn::VirtualTaQLColumn (const String& expr, const String& style)
: itsDataType     (TpOther),
  itsIsArray      (False),
  itsIsConst      (False),
  itsTempWritable (False),
  itsExpr         (expr),
  itsStyle        (style),
  itsNode         (0),
  itsCurArray     (0),
  itsCurRow       (-1)
{}

VirtualTaQLColumn::VirtualTaQLColumn (const Record& spec)
: itsDataType     (TpOther),
  itsIsArray      (False),
  itsIsConst      (False),
  itsTempWritable (False),
  itsNode         (0),
  itsCurArray     (0),
  itsCurRow       (-1)
{
  if (spec.isDefined ("TAQLCALCEXPR")) {
    itsExpr = spec.asString ("TAQLCALCEXPR");
  }
  if (spec.isDefined ("TAQLSTYLE")) {
    itsStyle = spec.asString ("TAQLSTYLE");
  }
}

VirtualTaQLColumn::~VirtualTaQLColumn()
{
  delete itsCurArray;
  delete itsNode;
}

void VirtualTaQLColumn::makeCurArray()
{
  delete itsCurArray;
  itsCurArray = 0;
  switch (itsDataType) {
  case TpBool:
    itsCurArray = new Array<Bool>();
    break;
  case TpUChar:
    itsCurArray = new Array<uChar>();
    break;
  case TpShort:
    itsCurArray = new Array<Short>();
    break;
  case TpUShort:
    itsCurArray = new Array<uShort>();
    break;
  case TpInt:
    itsCurArray = new Array<Int>();
    break;
  case TpUInt:
    itsCurArray = new Array<uInt>();
    break;
  case TpInt64:
    itsCurArray = new Array<Int64>();
    break;
  case TpFloat:
    itsCurArray = new Array<Float>();
    break;
  case TpDouble:
    itsCurArray = new Array<Double>();
    break;
  case TpComplex:
    itsCurArray = new Array<Complex>();
    break;
  case TpDComplex:
    itsCurArray = new Array<DComplex>();
    break;
  case TpString:
    itsCurArray = new Array<String>();
    break;
  default:
    throw DataManError ("VirtualTaQLColumn::makeCurArray - unknown data type");
  }
}

DataManager* VirtualTaQLColumn::clone() const
{
  DataManager* dmPtr = new VirtualTaQLColumn (itsExpr, itsStyle);
  return dmPtr;
}

DataManagerColumn* VirtualTaQLColumn::makeScalarColumn (const String& name,
							int dataType,
							const String&)
{
  AlwaysAssert (dataType!=TpOther, AipsError);
  itsDataType   = dataType;
  itsIsArray    = False;
  itsColumnName = name;
  return this;
}

DataManagerColumn* VirtualTaQLColumn::makeIndArrColumn (const String& name,
							int dataType,
							const String&)
{
  AlwaysAssert (dataType!=TpOther, AipsError);
  itsDataType   = dataType;
  itsIsArray    = True;
  itsColumnName = name;
  return this;
}


void VirtualTaQLColumn::create64 (rownr_t)
{
  // Define a keyword in the column telling the expression.
  // The table has to be writable for this operation only; otherwise it is not writable.
  itsTempWritable = True;
  TableColumn tabcol (table(), itsColumnName);
  itsTempWritable = False;
  tabcol.rwKeywordSet().define ("_VirtualTaQLEngine_CalcExpr", itsExpr);
  tabcol.rwKeywordSet().define ("_VirtualTaQLEngine_Style", itsStyle);
}

void VirtualTaQLColumn::prepare()
{
  // Get the expression.
  TableColumn tabcol (table(), itsColumnName);
  itsExpr = tabcol.keywordSet().asString ("_VirtualTaQLEngine_CalcExpr");
  if (tabcol.keywordSet().isDefined ("_VirtualTaQLEngine_Style")) {
    itsStyle = tabcol.keywordSet().asString ("_VirtualTaQLEngine_Style");
  }
  // Compile the expression.
  String cmd;
  if (! itsStyle.empty()) {
    cmd = "using style " + itsStyle;
  }
  TaQLResult res = tableCommand (cmd + " calc from $1 calc " + itsExpr, table());
  itsNode = new TableExprNode(res.node());
  // Check if the expression type matches the column type.
  if (itsNode->isScalar() == itsIsArray) {
    throw DataManError ("VirtualTaQLColumn: "
			"expression and " + itsColumnName +
                        " column type mismatch (not both scalar or array)");
  }
  // Check if the data types match.
  int exptype = itsDataType;
  switch (itsDataType) {
  case TpComplex:
    exptype = TpDComplex;
    break;
  case TpUChar:
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
    exptype = TpInt64;
    break;
  case TpFloat:
    exptype = TpDouble;
    break;
  default:
    break;
  }
  if (itsNode->dataType() != exptype) {
    throw DataManError ("VirtualTaQLColumn: "
			"expression and column " + itsColumnName +
                        " data type mismatch");
  }
  // Create the correct array type.
  makeCurArray();
  // If a constant expression, get the constant value and set cache for scalars.
  itsIsConst = itsNode->getRep()->isConstant();
  if (itsIsConst) {
    if (itsIsArray) {
      getResult(0);
    } else {
      fillColumnCache();
    }
  }
}

DataManager* VirtualTaQLColumn::makeObject (const String&,
					    const Record& spec)
{
  DataManager* dmPtr = new VirtualTaQLColumn (spec);
  return dmPtr;
}

void VirtualTaQLColumn::registerClass()
{
  DataManager::registerCtor (className(), makeObject);
}

String VirtualTaQLColumn::className()
{
  return "VirtualTaQLColumn";
}
String VirtualTaQLColumn::dataManagerType() const
{
  return className();
}

Record VirtualTaQLColumn::dataManagerSpec() const
{
  Record spec;
  spec.define ("TAQLCALCEXPR", itsExpr);
  return spec;
}

void VirtualTaQLColumn::setShapeColumn (const IPosition& aShape)
{
  itsShape = aShape;
}

void VirtualTaQLColumn::setMaxLength (uInt maxLength)
{
  itsMaxLen = maxLength;
}

int VirtualTaQLColumn::dataType() const
{
  return itsDataType;
}

Bool VirtualTaQLColumn::isWritable() const
{
  // This is always False except temporarily in function create64 to define a keyword.
  return itsTempWritable;
}


uInt VirtualTaQLColumn::ndim (rownr_t rownr)
{
  return shape(rownr).nelements();
}

IPosition VirtualTaQLColumn::shape (rownr_t rownr)
{
  if (!itsIsArray) {
    return IPosition();
  }
  // See if the expression has a fixed shape.
  IPosition shp = itsNode->getNodeRep()->shape();
  if (shp.nelements() > 0) {
    return shp;
  }
  // Value is already available if constant or if current row.
  if (!itsIsConst  &&  rownr != itsCurRow) {
    getResult (rownr);
    itsCurRow = rownr;
  }
  return itsCurArray->shape();
}

Bool VirtualTaQLColumn::isShapeDefined (rownr_t)
{
  return True;
}


void VirtualTaQLColumn::getBool (rownr_t rownr, Bool* dataPtr)
{
  *dataPtr = itsNode->getBool (rownr);
}
void VirtualTaQLColumn::getuChar (rownr_t rownr, uChar* dataPtr)
{
  *dataPtr = uChar(itsNode->getInt (rownr));
}
void VirtualTaQLColumn::getShort (rownr_t rownr, Short* dataPtr)
{
  *dataPtr = Short(itsNode->getInt (rownr));
}
void VirtualTaQLColumn::getuShort (rownr_t rownr, uShort* dataPtr)
{
  *dataPtr = uShort(itsNode->getInt (rownr));
}
void VirtualTaQLColumn::getInt (rownr_t rownr, Int* dataPtr)
{
  *dataPtr = Int(itsNode->getInt (rownr));
}
void VirtualTaQLColumn::getuInt (rownr_t rownr, uInt* dataPtr)
{
  *dataPtr = uInt(itsNode->getInt (rownr));
}
void VirtualTaQLColumn::getInt64 (rownr_t rownr, Int64* dataPtr)
{
  *dataPtr = itsNode->getInt (rownr);
}
void VirtualTaQLColumn::getfloat (rownr_t rownr, float* dataPtr)
{
  *dataPtr = Float(itsNode->getDouble (rownr));
}
void VirtualTaQLColumn::getdouble (rownr_t rownr, double* dataPtr)
{
  *dataPtr = itsNode->getDouble (rownr);
}
void VirtualTaQLColumn::getComplex (rownr_t rownr, Complex* dataPtr)
{
  *dataPtr = Complex(itsNode->getDComplex (rownr));
}
void VirtualTaQLColumn::getDComplex (rownr_t rownr, DComplex* dataPtr)
{
  *dataPtr = itsNode->getDComplex (rownr);
}
void VirtualTaQLColumn::getString (rownr_t rownr, String* dataPtr)
{
  *dataPtr = itsNode->getString (rownr);
  if (itsMaxLen > 0  &&  dataPtr->size() > itsMaxLen) {
    *dataPtr = dataPtr->substr (0, itsMaxLen);
  }
}

void VirtualTaQLColumn::getArrayV (rownr_t rownr, ArrayBase& arr)
{
  // Usually getShape is called before getArray.
  // To avoid double calculation of the same value, the result is cached
  // by getShape in itsCurArray (by getResult).
  // Value is also available if constant.
  if (!itsIsConst  &&  rownr != itsCurRow) {
    getResult (rownr);
    itsCurRow = rownr;
  }
  arr.assignBase (*itsCurArray);
}

void VirtualTaQLColumn::getResult (rownr_t rownr)
{
  switch (itsDataType) {
  case TpBool:
    {
      Array<Bool> arr = itsNode->getArrayBool (rownr);
      Array<Bool>& out = *static_cast<Array<Bool>*>(itsCurArray);
      out.reference (arr);
      break;
    }
  case TpUChar:
    {
      Array<Int64> arr = itsNode->getArrayInt (rownr);
      Array<uChar>& out = *static_cast<Array<uChar>*>(itsCurArray);
      out.resize (arr.shape());
      convertArray (out, arr);
      break;
    }
  case TpShort:
    {
      Array<Int64> arr = itsNode->getArrayInt (rownr);
      Array<Short>& out = *static_cast<Array<Short>*>(itsCurArray);
      out.resize (arr.shape());
      convertArray (out, arr);
      break;
    }
  case TpUShort:
    {
      Array<Int64> arr = itsNode->getArrayInt (rownr);
      Array<uShort>& out = *static_cast<Array<uShort>*>(itsCurArray);
      out.resize (arr.shape());
      convertArray (out, arr);
      break;
    }
  case TpInt:
    {
      Array<Int64> arr = itsNode->getArrayInt (rownr);
      Array<Int>& out = *static_cast<Array<Int>*>(itsCurArray);
      out.resize (arr.shape());
      convertArray (out, arr);
      break;
    }
  case TpUInt:
    {
      Array<Int64> arr = itsNode->getArrayInt (rownr);
      Array<uInt>& out = *static_cast<Array<uInt>*>(itsCurArray);
      out.resize (arr.shape());
      convertArray (out, arr);
      break;
    }
  case TpInt64:
    {
      Array<Int64> arr  = itsNode->getArrayInt (rownr);
      Array<Int64>& out = *static_cast<Array<Int64>*>(itsCurArray);
      out.reference (arr);
      break;
    }
  case TpFloat:
    {
      Array<Double> arr = itsNode->getArrayDouble (rownr);
      Array<Float>& out = *static_cast<Array<Float>*>(itsCurArray);
      out.resize (arr.shape());
      convertArray (out, arr);
      break;
    }
  case TpDouble:
    {
      Array<Double> arr  = itsNode->getArrayDouble (rownr);
      Array<Double>& out = *static_cast<Array<Double>*>(itsCurArray);
      out.reference (arr);
      break;
    }
  case TpComplex:
    {
      Array<DComplex> arr = itsNode->getArrayDComplex (rownr);
      Array<Complex>& out = *static_cast<Array<Complex>*>(itsCurArray);
      out.resize (arr.shape());
      convertArray (out, arr);
      break;
    }
  case TpDComplex:
    {
      Array<DComplex> arr  = itsNode->getArrayDComplex (rownr);
      Array<DComplex>& out = *static_cast<Array<DComplex>*>(itsCurArray);
      out.reference (arr);
      break;
    }
  case TpString:
    {
      Array<String> arr  = itsNode->getArrayString (rownr);
      Array<String>& out = *static_cast<Array<String>*>(itsCurArray);
      out.reference (arr);
      break;
    }
  default:
    throw DataManError ("VirtualTaQLColumn::getResult - unknown data type");
  }
  if (! itsShape.empty()  &&  ! itsShape.isEqual(itsCurArray->shape())) {
    throw DataManError ("VirtualTaQLColumn::getResult - shape of result mismatches fixed "
                        "shape of column " + columnName());
  }
}

void VirtualTaQLColumn::getScalarColumnV (ArrayBase& arr)
{
  if (itsIsConst) {
    // Constant value, so fill the array with the same value.
    fillArray (arr);
  } else {
    getScalarColumnBase (arr);
  }
}
void VirtualTaQLColumn::getScalarColumnCellsV (const RefRows& rownrs,
                                               ArrayBase& arr)
{
  if (itsIsConst) {
    // Constant value, so fill the array with the same value.
    fillArray (arr);
  } else {
    getScalarColumnCellsBase (rownrs, arr);
  }
}
void VirtualTaQLColumn::fillColumnCache()
{
  columnCache().setIncrement (0);
  switch (itsDataType) {
  case TpBool:
    getBool (0, &itsBool);
    columnCache().set (0, table().nrow()-1, &itsBool);
    break;
  case TpUChar:
    getuChar (0, &itsuChar);
    columnCache().set (0, table().nrow()-1, &itsuChar);
    break;
  case TpShort:
    getShort (0, &itsShort);
    columnCache().set (0, table().nrow()-1, &itsShort);
    break;
  case TpUShort:
    getuShort (0, &itsuShort);
    columnCache().set (0, table().nrow()-1, &itsuShort);
    break;
  case TpInt:
    getInt (0, &itsInt);
    columnCache().set (0, table().nrow()-1, &itsInt);
    break;
  case TpUInt:
    getuInt (0, &itsuInt);
    columnCache().set (0, table().nrow()-1, &itsuInt);
    break;
  case TpInt64:
    getInt64 (0, &itsInt64);
    columnCache().set (0, table().nrow()-1, &itsInt64);
    break;
  case TpFloat:
    getfloat (0, &itsFloat);
    columnCache().set (0, table().nrow()-1, &itsFloat);
    break;
  case TpDouble:
    getdouble (0, &itsDouble);
    columnCache().set (0, table().nrow()-1, &itsDouble);
    break;
  case TpComplex:
    getComplex (0, &itsComplex);
    columnCache().set (0, table().nrow()-1, &itsComplex);
    break;
  case TpDComplex:
    getDComplex (0, &itsDComplex);
    columnCache().set (0, table().nrow()-1, &itsDComplex);
    break;
  case TpString:
    getString (0, &itsString);
    columnCache().set (0, table().nrow()-1, &itsString);
    break;
  default:
    throw DataManInvDT(itsColumnName);
  }
}

void VirtualTaQLColumn::fillArray (ArrayBase& arr)
{
  Bool deleteIt;
  void* ptr = arr.getVStorage (deleteIt);
  switch (itsDataType) {
  case TpBool:
    objset (static_cast<Bool*>(ptr), itsBool, arr.size());
    break;
  case TpUChar:
    objset (static_cast<uChar*>(ptr), itsuChar, arr.size());
    break;
  case TpShort:
    objset (static_cast<Short*>(ptr), itsShort, arr.size());
    break;
  case TpUShort:
    objset (static_cast<uShort*>(ptr), itsuShort, arr.size());
    break;
  case TpInt:
    objset (static_cast<Int*>(ptr), itsInt, arr.size());
    break;
  case TpUInt:
    objset (static_cast<uInt*>(ptr), itsuInt, arr.size());
    break;
  case TpInt64:
    objset (static_cast<Int64*>(ptr), itsInt64, arr.size());
    break;
  case TpFloat:
    objset (static_cast<Float*>(ptr), itsFloat, arr.size());
    break;
  case TpDouble:
    objset (static_cast<Double*>(ptr), itsDouble, arr.size());
    break;
  case TpComplex:
    objset (static_cast<Complex*>(ptr), itsComplex, arr.size());
    break;
  case TpDComplex:
    objset (static_cast<DComplex*>(ptr), itsDComplex, arr.size());
    break;
  case TpString:
    objset (static_cast<String*>(ptr), itsString, arr.size());
    break;
  default:
    throw DataManInvDT(itsColumnName);
  }
  arr.putVStorage (ptr, deleteIt);
}

} //# NAMESPACE CASACORE - END
