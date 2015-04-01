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

#include <casacore/tables/DataMan/VirtualTaQLColumn.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

VirtualTaQLColumn::VirtualTaQLColumn (const String& expr)
: itsDataType     (TpOther),
  itsIsArray      (False),
  itsExpr         (expr),
  itsNode         (0),
  itsTempWritable (False),
  itsCurRow       (-1),
  itsCurResult    (0)
{}

VirtualTaQLColumn::VirtualTaQLColumn (const Record& spec)
: itsDataType     (TpOther),
  itsIsArray      (False),
  itsNode         (0),
  itsTempWritable (False),
  itsCurRow       (-1),
  itsCurResult    (0)
{
  if (spec.isDefined ("TAQLCALCEXPR")) {
    itsExpr = spec.asString ("TAQLCALCEXPR");
  }
}

VirtualTaQLColumn::~VirtualTaQLColumn()
{
  if (itsCurResult != 0) {
    clearCurResult();
  }
  delete itsNode;
}

void VirtualTaQLColumn::clearCurResult()
{
  switch (itsDataType) {
  case TpBool:
    delete static_cast<Array<Bool>*>(itsCurResult);
    break;
  case TpUChar:
    delete static_cast<Array<uChar>*>(itsCurResult);
    break;
  case TpShort:
    delete static_cast<Array<Short>*>(itsCurResult);
    break;
  case TpUShort:
    delete static_cast<Array<uShort>*>(itsCurResult);
    break;
  case TpInt:
    delete static_cast<Array<Int>*>(itsCurResult);
    break;
  case TpUInt:
    delete static_cast<Array<uInt>*>(itsCurResult);
    break;
  case TpFloat:
    delete static_cast<Array<Float>*>(itsCurResult);
    break;
  case TpDouble:
    delete static_cast<Array<Double>*>(itsCurResult);
    break;
  case TpComplex:
    delete static_cast<Array<Complex>*>(itsCurResult);
    break;
  case TpDComplex:
    delete static_cast<Array<DComplex>*>(itsCurResult);
    break;
  case TpString:
    delete static_cast<Array<String>*>(itsCurResult);
    break;
  default:
    throw DataManError ("VirtualTaQLColumn::clearResult - unknown data type");
  }
  itsCurResult = 0;
  itsCurRow    = -1;
}

DataManager* VirtualTaQLColumn::clone() const
{
  DataManager* dmPtr = new VirtualTaQLColumn (itsExpr);
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


void VirtualTaQLColumn::create (uInt)
{
  // Define a keyword in the column telling the expression.
  itsTempWritable = True;
  TableColumn tabcol (table(), itsColumnName);
  itsTempWritable = False;
  tabcol.rwKeywordSet().define ("_VirtualTaQLEngine_CalcExpr", itsExpr);
}

void VirtualTaQLColumn::prepare()
{
  // Get the expression.
  TableColumn tabcol (table(), itsColumnName);
  itsExpr = tabcol.keywordSet().asString ("_VirtualTaQLEngine_CalcExpr");
  // Compile the expression.
  TaQLResult res = tableCommand ("calc from $1 calc " + itsExpr, table());
  itsNode = new TableExprNode(res.node());
  // Check if the expression type matches the column type.
  if (itsNode->isScalar() == itsIsArray) {
    throw DataManError ("VirtualTaQLColumn: "
			"expression and column type mismatch");
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
    exptype = TpInt;
    break;
  case TpFloat:
    exptype = TpDouble;
    break;
  default:
    break;
  }
  if (itsNode->dataType() != exptype) {
    throw DataManError ("VirtualTaQLColumn: "
			"expression and column data type mismatch");
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

int VirtualTaQLColumn::dataType() const
{
  return itsDataType;
}

Bool VirtualTaQLColumn::isWritable() const
{
  return itsTempWritable;
}


uInt VirtualTaQLColumn::ndim (uInt rownr)
{
  return shape(rownr).nelements();
}

IPosition VirtualTaQLColumn::shape (uInt rownr)
{
  if (!itsIsArray) {
    return IPosition();
  }
  IPosition shp = itsNode->getNodeRep()->shape();
  if (shp.nelements() > 0) {
    return shp;
  }
  if (Int(rownr) != itsCurRow) {
    itsCurShape = getResult (rownr, itsCurResult);
    itsCurRow = rownr;
  }
  return itsCurShape;
}

Bool VirtualTaQLColumn::isShapeDefined (uInt)
{
  return True;
}


void VirtualTaQLColumn::getBoolV (uInt rownr, Bool* dataPtr)
{
  *dataPtr = itsNode->getBool (rownr);
}
void VirtualTaQLColumn::getuCharV (uInt rownr, uChar* dataPtr)
{
  *dataPtr = uChar(itsNode->getInt (rownr));
}
void VirtualTaQLColumn::getShortV (uInt rownr, Short* dataPtr)
{
  *dataPtr = Short(itsNode->getInt (rownr));
}
void VirtualTaQLColumn::getuShortV (uInt rownr, uShort* dataPtr)
{
  *dataPtr = uShort(itsNode->getInt (rownr));
}
void VirtualTaQLColumn::getIntV (uInt rownr, Int* dataPtr)
{
  *dataPtr = Int(itsNode->getInt (rownr));
}
void VirtualTaQLColumn::getuIntV (uInt rownr, uInt* dataPtr)
{
  *dataPtr = uInt(itsNode->getInt (rownr));
}
void VirtualTaQLColumn::getfloatV (uInt rownr, float* dataPtr)
{
  *dataPtr = Float(itsNode->getDouble (rownr));
}
void VirtualTaQLColumn::getdoubleV (uInt rownr, double* dataPtr)
{
  *dataPtr = itsNode->getDouble (rownr);
}
void VirtualTaQLColumn::getComplexV (uInt rownr, Complex* dataPtr)
{
  *dataPtr = Complex(itsNode->getDComplex (rownr));
}
void VirtualTaQLColumn::getDComplexV (uInt rownr, DComplex* dataPtr)
{
  *dataPtr = itsNode->getDComplex (rownr);
}
void VirtualTaQLColumn::getStringV (uInt rownr, String* dataPtr)
{
  *dataPtr = itsNode->getString (rownr);
}

void VirtualTaQLColumn::getArrayV (uInt rownr, void* dataPtr)
{
  // Usually getShape is called before getArray.
  // To avoid double calculation of the same value, the result is cached
  // by getShape in itsCurResult (by getResult).
  if (Int(rownr) != itsCurRow) {
    getResult (rownr, dataPtr);
    return;
  }
  switch (itsDataType) {
  case TpBool:
    *static_cast<Array<Bool>*>(dataPtr) =
      *static_cast<Array<Bool>*>(itsCurResult);
    break;
  case TpUChar:
    *static_cast<Array<uChar>*>(dataPtr) =
      *static_cast<Array<uChar>*>(itsCurResult);
    break;
  case TpShort:
    *static_cast<Array<Short>*>(dataPtr) =
      *static_cast<Array<Short>*>(itsCurResult);
    break;
  case TpUShort:
    *static_cast<Array<uShort>*>(dataPtr) =
      *static_cast<Array<uShort>*>(itsCurResult);
    break;
  case TpInt:
    *static_cast<Array<Int>*>(dataPtr) =
      *static_cast<Array<Int>*>(itsCurResult);
    break;
  case TpUInt:
    *static_cast<Array<uInt>*>(dataPtr) =
      *static_cast<Array<uInt>*>(itsCurResult);
    break;
  case TpFloat:
    *static_cast<Array<Float>*>(dataPtr) =
      *static_cast<Array<Float>*>(itsCurResult);
    break;
  case TpDouble:
    *static_cast<Array<Double>*>(dataPtr) =
      *static_cast<Array<Double>*>(itsCurResult);
    break;
  case TpComplex:
    *static_cast<Array<Complex>*>(dataPtr) =
      *static_cast<Array<Complex>*>(itsCurResult);
    break;
  case TpDComplex:
    *static_cast<Array<DComplex>*>(dataPtr) =
      *static_cast<Array<DComplex>*>(itsCurResult);
    break;
  case TpString:
    *static_cast<Array<String>*>(dataPtr) =
      *static_cast<Array<String>*>(itsCurResult);
    break;
  default:
    throw DataManError ("VirtualTaQLColumn::getArrayV - unknown data type");
  }
  clearCurResult();
}

IPosition VirtualTaQLColumn::getResult (uInt rownr, void* dataPtr)
{
  IPosition shp;
  switch (itsDataType) {
  case TpBool:
    itsNode->get (rownr, *static_cast<Array<Bool>*>(dataPtr));
    shp = static_cast<Array<String>*>(dataPtr)->shape();
    break;
  case TpUChar:
    {
      Array<Int64> arr = itsNode->getArrayInt (rownr);
      Array<uChar>& out = *static_cast<Array<uChar>*>(dataPtr);
      out.resize (arr.shape());
      convertArray (out, arr);
      shp = out.shape();
      break;
    }
  case TpShort:
    {
      Array<Int64> arr = itsNode->getArrayInt (rownr);
      Array<Short>& out = *static_cast<Array<Short>*>(dataPtr);
      out.resize (arr.shape());
      convertArray (out, arr);
      shp = out.shape();
      break;
    }
  case TpUShort:
    {
      Array<Int64> arr = itsNode->getArrayInt (rownr);
      Array<uShort>& out = *static_cast<Array<uShort>*>(dataPtr);
      out.resize (arr.shape());
      convertArray (out, arr);
      shp = out.shape();
      break;
    }
  case TpInt:
    {
      Array<Int64> arr = itsNode->getArrayInt (rownr);
      Array<Int>& out = *static_cast<Array<Int>*>(dataPtr);
      out.resize (arr.shape());
      convertArray (out, arr);
      shp = out.shape();
      break;
    }
  case TpUInt:
    {
      Array<Int64> arr = itsNode->getArrayInt (rownr);
      Array<uInt>& out = *static_cast<Array<uInt>*>(dataPtr);
      out.resize (arr.shape());
      convertArray (out, arr);
      shp = out.shape();
      break;
    }
  case TpFloat:
    {
      Array<Double> arr = itsNode->getArrayDouble (rownr);
      Array<Float>& out = *static_cast<Array<Float>*>(dataPtr);
      out.resize (arr.shape());
      convertArray (out, arr);
      shp = out.shape();
      break;
    }
  case TpDouble:
    itsNode->get (rownr, *static_cast<Array<Double>*>(dataPtr));
    shp = static_cast<Array<String>*>(dataPtr)->shape();
    break;
  case TpComplex:
    {
      Array<Double> arr = itsNode->getArrayDouble (rownr);
      Array<Complex>& out = *static_cast<Array<Complex>*>(dataPtr);
      out.resize (arr.shape());
      convertArray (out, arr);
      shp = out.shape();
      break;
    }
  case TpDComplex:
    itsNode->get (rownr, *static_cast<Array<DComplex>*>(dataPtr));
    shp = static_cast<Array<String>*>(dataPtr)->shape();
    break;
  case TpString:
    itsNode->get (rownr, *static_cast<Array<String>*>(dataPtr));
    shp = static_cast<Array<String>*>(dataPtr)->shape();
    break;
  default:
    throw DataManError ("VirtualTaQLColumn::getResult - unknown data type");
  }
  return shp;
}


} //# NAMESPACE CASACORE - END

