//# UDFMSCal.cc: TaQL UDF to calculate derived MS values
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

#include <derivedmscal/DerivedMC/UDFMSCal.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/ExprUnitNode.h>
#include <casa/Arrays/ArrayIO.h>

namespace casa {

  UDFMSCal::UDFMSCal (ColType type, Int antnr)
    : itsType  (type),
      itsAntNr (antnr)
  {}

  UDFBase* UDFMSCal::makeHA (const String&)
    { return new UDFMSCal (HA, -1); }
  UDFBase* UDFMSCal::makeHA1 (const String&)
    { return new UDFMSCal (HA, 0); }
  UDFBase* UDFMSCal::makeHA2 (const String&)
    { return new UDFMSCal (HA, 1); }
  UDFBase* UDFMSCal::makePA1 (const String&)
    { return new UDFMSCal (PA, 0); }
  UDFBase* UDFMSCal::makePA2 (const String&)
    { return new UDFMSCal (PA, 1); }
  UDFBase* UDFMSCal::makeLAST (const String&)
    { return new UDFMSCal (LAST, -1); }
  UDFBase* UDFMSCal::makeLAST1 (const String&)
    { return new UDFMSCal (LAST, 0); }
  UDFBase* UDFMSCal::makeLAST2 (const String&)
    { return new UDFMSCal (LAST, 1); }
  UDFBase* UDFMSCal::makeAZEL1 (const String&)
    { return new UDFMSCal (AZEL, 0); }
  UDFBase* UDFMSCal::makeAZEL2 (const String&)
    { return new UDFMSCal (AZEL, 1); }
  UDFBase* UDFMSCal::makeUVW (const String&)
    { return new UDFMSCal (UVW, -1); }
  UDFBase* UDFMSCal::makeStokes (const String&)
    { return new UDFMSCal (STOKES, -1); }

  void UDFMSCal::setup (const Table& table, const TaQLStyle&)
  {
    if (table.isNull()) {
      throw AipsError ("UDFMSCal can only be used on a table");
    }
    if (itsType != STOKES) {
      itsEngine.setTable (table);
      AlwaysAssert (operands().size() < 2, AipsError);
      if (operands().size() == 1) {
        setupDir (operands()[0]);
      }
    }
    setDataType (TableExprNodeRep::NTDouble);
    switch (itsType) {
    case HA:
    case PA:
    case LAST:
      setNDim (0);
      setUnit ("rad");
      break;
    case AZEL:
      setShape (IPosition(1,2));
      setUnit ("rad");
      break;
    case UVW:
      setShape (IPosition(1,3));
      setUnit ("m");
      break;
    case STOKES:
      setDataType (TableExprNodeRep::NTComplex);
      setupStokes (table, operands());
      break;
    }
  }

  void UDFMSCal::setupDir (TableExprNodeRep*& operand)
  {
    // Make sure the operand is a constant double array
    // or a single string (e.g. MOON).
    AlwaysAssert (operand->isConstant(), AipsError);
    MDirection mdir;
    if (operand->dataType() == TableExprNodeRep::NTDouble) {
      // Get direction (given in J2000).
      AlwaysAssert (operand->valueType() == TableExprNodeRep::VTArray,
                    AipsError);
      // Make sure the unit is rad.
      // Turn the array into a vector.
      TableExprNodeUnit::adaptUnit (operand, "rad");
      Array<Double> dirs(operand->getArrayDouble(0));
      AlwaysAssert (dirs.size() == 2, AipsError);
      Vector<Double> dirVec(dirs.reform(IPosition(1,dirs.size())));
      mdir = MDirection(Quantity(dirVec[0], "rad"),
                        Quantity(dirVec[1], "rad"),
                        MDirection::J2000);
    } else if (operand->dataType() == TableExprNodeRep::NTString) {
      String str = operand->getString(0);
      MDirection::Types refType;
      Bool fnd = MDirection::getType (refType, str);
      if (!fnd || refType<=MDirection::N_Types || refType>MDirection::N_Planets
          || refType==MDirection::COMET) { 
        throw AipsError ("UDFMSCal: " + str + " is an invalid (planet) name");
      }
      mdir = MDirection(refType);
    }
    itsEngine.setDirection (mdir);
  }

  void UDFMSCal::setupStokes (const Table& table,
                              PtrBlock<TableExprNodeRep*>& operands)
  {
    // There must be at leat 1 argument (data).
    AlwaysAssert (operands.size() > 0 && operands.size() < 4, AipsError);
    itsDataNode = TableExprNode(operands[0]);
    AlwaysAssert (operands[0]->valueType() == TableExprNodeRep::VTArray,
                  AipsError);
    // The optional second argument gives the output correlation types.
    // Default is iquv.
    String type = "IQUV";
    if (operands.size() > 1) {
      AlwaysAssert (operands[1]->isConstant(), AipsError);
      AlwaysAssert (operands[1]->dataType() == TableExprNodeRep::NTString,
                    AipsError);
      type = operands[1]->getString(0);
      type.upcase();
    }
    // The optional third argument tells if a factor 2 must be applied to I.
    Bool rescale = False;
    if (operands.size() > 2) {
      AlwaysAssert (operands[2]->isConstant(), AipsError);
      AlwaysAssert (operands[2]->dataType() == TableExprNodeRep::NTBool,
                    AipsError);
      rescale = operands[2]->getBool(0);
    }
    // Open the POLARIZATION subtable and get the input correlation types.
    Table polTable (table.keywordSet().asTable("POLARIZATION"));
    AlwaysAssert (polTable.nrow() > 0, AipsError);
    Vector<Int> inTypes (ROArrayColumn<Int>(polTable, "CORR_TYPE")(0));
    // Convert the output string types to ints.
    // First convert abbrevs.
    if (type == "IQUV"  ||  type == "STOKES") {
      type = "I,Q,U,V";
    } else if (type == "CIRC"  ||  type == "CIRCULAR") {
      type = "RR,RL,LR,LL";
    } else if (type == "LIN"  ||  type == "LINEAR") {
      type  = "XX,XY,YX,YY";
    }
    Vector<String> types = stringToVector(type);
    AlwaysAssert (types.size() > 0, AipsError);
    Vector<Int> outTypes(types.size());
    for (uInt i=0; i<types.size(); ++i) {
      outTypes[i] = Stokes::type (types[i]);
    }
    itsStokesConv.setConversion (outTypes, inTypes, rescale);
    // Get the input shape.
    IPosition shape = operands[0]->shape();
    if (! shape.empty()) {
      shape[0] = outTypes.size();
      setShape (shape);
    } else {
      // Shape is unknown, so only set the dimensionality.
      setNDim (operands[0]->ndim());
    }
  }

  void UDFMSCal::replaceTable (const Table& table)
  {
    if (itsType == STOKES) {
      itsDataNode.checkReplaceTable (table);
    } else {
      itsEngine.setTable (table);
    }
  }

  Double UDFMSCal::getDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case HA:
      return itsEngine.getHA (itsAntNr, id.rownr());
    case PA:
      return itsEngine.getPA (itsAntNr, id.rownr());
    case LAST:
      return itsEngine.getLAST (itsAntNr, id.rownr());
    default:
      throw AipsError ("UDFMSCal: unexpected getDouble function");
    }
  }

  Array<Bool> UDFMSCal::getArrayBool (const TableExprId& id)
  {
    if (itsType != STOKES) {
      throw AipsError ("UDFMSCal: unexpected getArrayBool function");
    }
    Array<Bool> out;
    // Combine the flags.
    itsStokesConv.convert (out, itsDataNode.getArrayBool (id));
    return out;
  }

  Array<Double> UDFMSCal::getArrayDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case AZEL:
      itsEngine.getAzEl (itsAntNr, id.rownr(), itsTmpAzEl);
      return itsTmpAzEl;
    case UVW:
      itsEngine.getUVWJ2000 (id.rownr(), itsTmpUVW);
      return itsTmpUVW;
    case STOKES:
      {
        // Unfortunately stokes weight conversion is only defined for type Float,
        // while TableExprNode only has Double.
        // So conversions are necessary for the time being.
        // In the future we can add Double support to StokesConverter
        // or Float support to TableExprNode.
        Array<Float> outf, dataf;
        Array<Double> outd, datad;
        itsDataNode.get (id, datad);
        dataf.resize (datad.shape());
        convertArray (dataf, datad);
        itsStokesConv.convert (outf, dataf);
        outd.resize (outf.shape());
        convertArray (outd, outf);
        return outd;
      }
    default:
      throw AipsError ("UDFMSCal: unexpected getArrayDouble function");
    }
  }

  Array<DComplex> UDFMSCal::getArrayDComplex (const TableExprId& id)
  {
    if (itsType != STOKES) {
      throw AipsError ("UDFMSCal: unexpected getArrayComplex function");
    }
    // Unfortunately stokes conversion is only defined for type Complex,
    // while TableExprNode only has DComplex.
    // So conversions are necessary for the time being.
    // In the future we can add DComplex support to StokesConverter
    // or Complex support to TableExprNode.
    Array<Complex> outf, dataf;
    Array<DComplex> outd, datad;
    itsDataNode.get (id, datad);
    dataf.resize (datad.shape());
    convertArray (dataf, datad);
    itsStokesConv.convert (outf, dataf);
    outd.resize (outf.shape());
    convertArray (outd, outf);
    return outd;
  }

} //end namespace
