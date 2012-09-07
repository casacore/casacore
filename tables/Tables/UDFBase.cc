//# UDFBase.cc: Abstract base class for a user-defined TaQL function
//# Copyright (C) 2010
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

//# Includes
#include <tables/Tables/UDFBase.h>
#include <tables/Tables/TableError.h>
#include <casa/OS/DynLib.h>

namespace casa {

  // Define the static objects.
  // Use a recursive mutex, because loading from a shared library can cause
  // a nested lock.
  map<String,UDFBase::MakeUDFObject*> UDFBase::theirRegistry;
  Mutex UDFBase::theirMutex(Mutex::Recursive);


  UDFBase::UDFBase()
    : itsDataType   (TableExprNodeRep::NTAny),
      itsNDim       (-2),
      itsIsConstant (False)
  {}

  UDFBase::~UDFBase()
  {
    for (uInt i=0; i<itsOperands.size(); ++i) {
      TableExprNodeRep::unlink (itsOperands[i]);
    }
  }

  void UDFBase::init (const PtrBlock<TableExprNodeRep*>& operands,
                      const Table& table, const TaQLStyle& style)
  {
    // Link to the operands.
    itsOperands.resize (operands.size());
    for (uInt i=0; i<operands.size(); ++i) {
      itsOperands[i] = operands[i]->link();
    }
    setup (table, style);
    if (itsDataType == TableExprNodeRep::NTAny) {
      throw TableInvExpr ("UDFBase: data type not set by derived UDF class");
    }
    if (itsNDim < -1) {
      throw TableInvExpr ("UDFBase: ndim not set by derived UDF class");
    }
  }

  void UDFBase::setDataType (TableExprNodeRep::NodeDataType dataType)
  {
    itsDataType = dataType;
  }

  void UDFBase::setNDim (Int ndim)
  {
    AlwaysAssert (ndim >= -1, AipsError);
    if (itsShape.size() > 0) {
      AlwaysAssert (ndim == Int(itsShape.size()), AipsError);
    }
    itsNDim = ndim;
  }

  void UDFBase::setShape (const IPosition& shape)
  {
    if (itsNDim >= 0) {
      AlwaysAssert (Int(shape.size()) == itsNDim, AipsError);
    }
    itsShape = shape;
    itsNDim  = itsShape.size();
  }

  void UDFBase::setUnit (const String& unit)
  {
    itsUnit = unit;
  }

  void UDFBase::setConstant (Bool isConstant)
  {
    itsIsConstant = isConstant;
  }

  Bool      UDFBase::getBool     (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getBool not implemented"); }
  Int64     UDFBase::getInt      (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getInt not implemented"); }
  Double    UDFBase::getDouble   (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getDouble not implemented"); }
  DComplex  UDFBase::getDComplex (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getDComplex not implemented"); }
  String    UDFBase::getString   (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getString not implemented"); }
  TaqlRegex UDFBase::getRegex    (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getRegex not implemented"); }
  MVTime    UDFBase::getDate     (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getDate not implemented"); }
  MArray<Bool>     UDFBase::getArrayBool     (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getArrayBool not implemented"); }
  MArray<Int64>    UDFBase::getArrayInt      (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getArrayInt not implemented"); }
  MArray<Double>  UDFBase:: getArrayDouble   (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getArrayDouble not implemented"); }
  MArray<DComplex> UDFBase::getArrayDComplex (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getArrayDComplex not implemented"); }
  MArray<String>  UDFBase:: getArrayString   (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getArrayString not implemented"); }
  MArray<MVTime>  UDFBase:: getArrayDate     (const TableExprId&)
    { throw TableInvExpr ("UDFBase::getArrayDate not implemented"); }

  void UDFBase::registerUDF (const String& name, MakeUDFObject* func)
  {
    String fname(name);
    fname.downcase();
    ScopedMutexLock lock(theirMutex);
    map<String,MakeUDFObject*>::iterator iter = theirRegistry.find (fname);
    if (iter == theirRegistry.end()) {
      theirRegistry[fname] = func;
    } else {
      // Already defined, but allow double definition of the same.
      if (iter->second != func) {
        throw TableInvExpr ("User defined TaQL function " + fname +
                            " already exists");
      }
    }
  }

  UDFBase* UDFBase::createUDF (const String& name)
  {
    String fname(name);
    fname.downcase();
    ScopedMutexLock lock(theirMutex);
    map<String,MakeUDFObject*>::iterator iter = theirRegistry.find (fname);
    if (iter != theirRegistry.end()) {
      return iter->second (fname);
    }
    // Not found. See if it can be loaded dynamically.
    // The library name should be the first part of the function name.
    Int j = fname.index('.');
    if (j > 0  &&  j < Int(fname.size())-1) {
      String libname(fname.substr(0,j));
      // derivedmscal UDFs are used often, so allow alias mscal.
      if (libname == "mscal") {
        libname = "derivedmscal";
      }
      // Try to load the dynamic library and see if registered now.
      DynLib dl(libname, string("libcasa_"), "register_"+libname, False);
      if (dl.getHandle()) {
        map<String,MakeUDFObject*>::iterator iter = theirRegistry.find (fname);
        if (iter != theirRegistry.end()) {
          return iter->second (fname);
        }
      }
    }
    throw TableInvExpr ("TaQL function " + name + " is unknown");
  }

} // end namespace
