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
//# $Id: UDFBase.cc 21262 2012-09-07 12:38:36Z gervandiepen $

//# Includes
#include <casacore/tables/TaQL/UDFBase.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/OS/DynLib.h>

namespace casacore {

  // Define the static objects.
  // Use a recursive mutex, because loading from a shared library can cause
  // a nested lock.
  map<String,UDFBase::MakeUDFObject*> UDFBase::theirRegistry;
  std::recursive_mutex UDFBase::theirMutex;


  UDFBase::UDFBase()
    : itsDataType       (TableExprNodeRep::NTAny),
      itsNDim           (-2),
      itsIsConstant     (False),
      itsIsAggregate    (False),
      itsApplySelection (True)
  {}

  UDFBase::~UDFBase()
  {}

  void UDFBase::init (const vector<TENShPtr>& operands,
                      const Table& table, const TaQLStyle& style)
  {
    // Link to the operands.
    itsOperands.resize (operands.size());
    for (uInt i=0; i<operands.size(); ++i) {
      itsOperands[i] = operands[i];
    }
    setup (table, style);
    if (itsDataType == TableExprNodeRep::NTAny) {
      throw TableInvExpr ("UDFBase: data type not set by derived UDF class");
    }
    if (itsNDim < -1) {
      throw TableInvExpr ("UDFBase: ndim not set by derived UDF class");
    }
  }

  void UDFBase::getAggrNodes (vector<TableExprNodeRep*>& aggr)
  {
    for (uInt i=0; i<itsOperands.size(); ++i) {
      itsOperands[i]->getAggrNodes (aggr);
    }
  }

  void UDFBase::getColumnNodes (vector<TableExprNodeRep*>& cols)
  {
    for (uInt i=0; i<itsOperands.size(); ++i) {
      itsOperands[i]->getColumnNodes (cols);
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

  void UDFBase::setAttributes (const Record& attributes)
  {
    itsAttributes = attributes;
  }

  void UDFBase::setConstant (Bool isConstant)
  {
    itsIsConstant = isConstant;
  }

  void UDFBase::setAggregate (Bool isAggregate)
  {
    itsIsAggregate = isAggregate;
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

  void UDFBase::recreateColumnObjects (const Vector<rownr_t>&)
  {}

  void UDFBase::applySelection (const Vector<rownr_t>& rownrs)
  {
    if (itsApplySelection) {
      recreateColumnObjects (rownrs);
      // Clear switch in case called for a second time.
      itsApplySelection = False;
    }
  }

  void UDFBase::registerUDF (const String& name, MakeUDFObject* func)
  {
    String fname(name);
    fname.downcase();
    // The library name is the first part.
    Int j = fname.index('.');
    String libname;
    if (j > 0  &&  j < Int(fname.size())-1) {
      libname = fname.substr(0,j);
    } else {
      throw TableInvExpr("UDF " + name + " has an invalid name (no dot)");
    }
    std::lock_guard<std::recursive_mutex> lock(theirMutex);
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
    // Also register the library with null pointer (if not done yet).
    // Note that a libname is different from a function name because
    // it does not contain dots.
    iter = theirRegistry.find (libname);
    if (iter == theirRegistry.end()) {
      theirRegistry[libname] = 0;
    }
  }

  UDFBase* UDFBase::createUDF (const String& name, const TaQLStyle& style)
  {
    String fname(name);
    fname.downcase();
    map<String,MakeUDFObject*>::iterator iter;
    {
      std::lock_guard<std::recursive_mutex> lock(theirMutex);
      // Try to find the function.
      iter = theirRegistry.find (fname);
      if (iter != theirRegistry.end()) {
        return iter->second (fname);
      }
    }
    String sfname(fname);
    // Split name in library and function name.
    // Require that a . is found and is not the first or last character.
    Int j = fname.index('.');
    String libname;
    if (j > 0  &&  j < Int(fname.size())-1) {
      // Replace a possible synonym for the library name.
      libname = fname.substr(0,j);
      libname = style.findSynonym (libname);
      fname   = libname + fname.substr(j);
      // Try to find the function with the synonym.
      iter = theirRegistry.find (fname);
      if (iter != theirRegistry.end()) {
        return iter->second (fname);
      }

      std::lock_guard<std::recursive_mutex> lock(theirMutex);
      // See if the library is already loaded.
      iter = theirRegistry.find (libname);
      if (iter == theirRegistry.end()) {
        // Try to load the dynamic library.
        DynLib dl(libname, string("libcasa_"), CASACORE_STRINGIFY(SOVERSION),
                  "register_"+libname, False);
        if (dl.getHandle()) {
          // Add to map to indicate library has been loaded.
          theirRegistry[libname] = 0;
        }
      }
      // Try to find the function.
      iter = theirRegistry.find (fname);
      if (iter != theirRegistry.end()) {
        return iter->second (fname);
      }
    }
    String unk;
    if (fname != sfname) {
      unk = " (=" + fname + ')';
    }
    throw TableInvExpr ("TaQL function " + sfname + unk +
                        " is unknown" +
                        "\n  Library " + libname + " was successfully loaded; "
                        "taql 'show func " + libname + "' shows its functions"
                        "\n  Maybe check if (DY)LD_LIBRARY_PATH and "
                        "CASACORE_LDPATH match the"
                        " libraries used during the build of " + libname);
  }

} // end namespace
