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
  UDFBase* UDFMSCal::makeHADEC (const String&)
    { return new UDFMSCal (HADEC, -1); }
  UDFBase* UDFMSCal::makeHADEC1 (const String&)
    { return new UDFMSCal (HADEC, 0); }
  UDFBase* UDFMSCal::makeHADEC2 (const String&)
    { return new UDFMSCal (HADEC, 1); }
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
    // Function Stokes is handled by this class, all others by the engine.
    if (itsType != STOKES) {
      itsEngine.setTable (table);
      if (operands().size() > 1) {
        throw AipsError("More than 1 argument given to DERIVEDMSCAL function");
      }
      // Setup the direction if an argument is given.
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
    case HADEC:
    case AZEL:
      setShape (IPosition(1,2));
      itsTmpVector.resize (2);
      setUnit ("rad");
      break;
    case UVW:
      setShape (IPosition(1,3));
      itsTmpVector.resize (3);
      setUnit ("m");
      break;
    case STOKES:
      setupStokes (table, operands());
      setDataType (operands()[0]->dataType());
      break;
    }
  }

  void UDFMSCal::setupDir (TableExprNodeRep*& operand)
  {
    // Make sure the operand is a constant double array
    // or a single string (e.g. MOON).
    if (! operand->isConstant()) {
      throw AipsError("Only a constant value can be given as a DERIVEDMSCAL "
                      "function argument");
    }
    // In principle type NTInt could also be allowed, but that makes no sense
    // for direction values. So it's better to be a bit strict.
    if (operand->dataType() == TableExprNodeRep::NTDouble) {
      // Get direction (given in J2000).
      if (operand->valueType() != TableExprNodeRep::VTArray) { 
        throw AipsError ("Argument to DERIVEDMSCAL function is not an array "
                         "of 2 values");
      }
      // Make sure the unit is rad.
      // Turn the array into a vector.
      TableExprNodeUnit::adaptUnit (operand, "rad");
      Array<Double> dirs(operand->getArrayDouble(0).array());
      if (dirs.size() != 2) {
        throw AipsError ("Argument to DERIVEDMSCAL function is not an array "
                         "of 2 values");
      }
      Vector<Double> dirVec(dirs.reform(IPosition(1,dirs.size())));
      itsEngine.setDirection (MDirection(Quantity(dirVec[0], "rad"),
                                         Quantity(dirVec[1], "rad"),
                                         MDirection::J2000));
    } else if (operand->dataType() == TableExprNodeRep::NTString) {
      // First try the string as a planetary object.
      // In the future comets can be supported like COMET:cometname.
      String str = operand->getString(0);
      Bool fnd = True;
      try {
        itsEngine.setDirection (MDirection::makeMDirection(str));
      } catch (std::exception&) {
        fnd = False;
      }
      if (!fnd) {
        // Now do it as a FIELD column name.
        // Skip possible leading backslash (escape char).
        if (str.size() > 0  &&  str[0] == '\\') {
          str = str.from(1);
        }
        if (str.empty()) {
          throw AipsError ("An empty string given to a DERIVEDMSCAL function");
        }
        itsEngine.setDirColName (str);
      }
    } else {
      throw AipsError ("Argument to DERIVEDMSCAL function must be double or "
                       "string");
    }
  }

  void UDFMSCal::setupStokes (const Table& table,
                              PtrBlock<TableExprNodeRep*>& operands)
  {
    // There must be at least 1 argument (data).
    if (operands.size() == 0  ||  operands.size() > 3) {
      throw AipsError ("1, 2, or 3 arguments must be given to "
                       "DERIVEDMSCAL.STOKES");
    }
    itsDataNode = TableExprNode(operands[0]);
    if (operands[0]->valueType() != TableExprNodeRep::VTArray  ||
        !(operands[0]->dataType() == TableExprNodeRep::NTBool  ||
          operands[0]->dataType() == TableExprNodeRep::NTDouble  ||
          operands[0]->dataType() == TableExprNodeRep::NTComplex)) {
      throw AipsError ("First argument of DERIVEDMSCAL.STOKES must be a "
                       "Complex, Double or Bool array");
    }
    // The optional second argument gives the output correlation types.
    // Default is iquv.
    String type = "IQUV";
    if (operands.size() > 1) {
      if (! operands[1]->isConstant()  ||
          operands[1]->valueType() != TableExprNodeRep::VTScalar  ||
          operands[1]->dataType() != TableExprNodeRep::NTString) {
        throw AipsError ("Second argument of DERIVEDMSCAL.STOKES must be a "
                         "constant String scalar");
      }
      type = operands[1]->getString(0);
      type.upcase();
    }
    // The optional third argument tells if a factor 2 must be applied to I.
    Bool rescale = False;
    if (operands.size() > 2) {
      if (! operands[2]->isConstant()  ||
          operands[2]->valueType() != TableExprNodeRep::VTScalar  ||
          operands[2]->dataType() != TableExprNodeRep::NTBool) {
        throw AipsError ("Second argument of DERIVEDMSCAL.STOKES must be a "
                         "constant Bool scalar");
      }
      rescale = operands[2]->getBool(0);
    }
    // Open the POLARIZATION subtable and get the input correlation types.
    Table polTable (table.keywordSet().asTable("POLARIZATION"));
    if (polTable.nrow() == 0) {
      throw AipsError("POLARIZATION subtable of " + table.tableName() +
                      " is empty");
    }
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
    if (types.empty()) {
      throw AipsError("No polarization types given in second argument of "
                      "DERIVEDMSCAL.STOKES");
    }
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

  MArray<Bool> UDFMSCal::getArrayBool (const TableExprId& id)
  {
    if (itsType != STOKES) {
      throw AipsError ("UDFMSCal: unexpected getArrayBool function");
    }
    Array<Bool> out;
    // Combine the flags.
    MArray<Bool> data (itsDataNode.getArrayBool(id));
    itsStokesConv.convert (out, data.array());
    return MArray<Bool> (out, data.mask());
  }

  MArray<Double> UDFMSCal::getArrayDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case HADEC:
      itsEngine.getHaDec (itsAntNr, id.rownr(), itsTmpVector);
      break;
    case AZEL:
      itsEngine.getAzEl (itsAntNr, id.rownr(), itsTmpVector);
      break;
    case UVW:
      itsEngine.getUVWJ2000 (id.rownr(), itsTmpVector);
      break;
    case STOKES:
      {
        // Unfortunately stokes weight conversion is only defined for Float,
        // while TableExprNode only has Double.
        // So conversions are necessary for the time being.
        // In the future we can add Double support to StokesConverter.
        Array<Float> outf, dataf;
        Array<Double> outd;
        MArray<Double> datad;
        itsDataNode.get (id, datad);
        dataf.resize (datad.shape());
        convertArray (dataf, datad.array());
        itsStokesConv.convert (outf, dataf);
        outd.resize (outf.shape());
        convertArray (outd, outf);
        return MArray<Double> (outd, datad.mask());
      }
    default:
      throw AipsError ("UDFMSCal: unexpected getArrayDouble function");
    }
    return MArray<Double> (itsTmpVector);
  }

  MArray<DComplex> UDFMSCal::getArrayDComplex (const TableExprId& id)
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
    Array<DComplex> outd;
    MArray<DComplex> datad;
    itsDataNode.get (id, datad);
    dataf.resize (datad.shape());
    convertArray (dataf, datad.array());
    itsStokesConv.convert (outf, dataf);
    outd.resize (outf.shape());
    convertArray (outd, outf);
    return MArray<DComplex> (outd, datad.mask());
  }

} //end namespace
