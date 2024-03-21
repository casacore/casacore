//# EpochEngine.cc: Engine for TaQL UDF Epoch conversions
//# Copyright (C) 2011
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

#include <casacore/meas/MeasUDF/EpochEngine.h>
#include <casacore/meas/MeasUDF/PositionEngine.h>

namespace casacore {

  EpochEngine::EpochEngine()
    : itsPositionEngine (0)
  {}

  EpochEngine::~EpochEngine()
  {}

  void EpochEngine::handleEpoch (vector<TENShPtr>& args,
                                 uInt& argnr)
  {
    // Initialize type to unknown.
    itsRefType = MEpoch::N_Types;
    // Convert a string epoch argument to a date.
    if (args[argnr]->dataType() == TableExprNodeRep::NTString) {
      TableExprNode dNode = datetime (args[argnr]);
      args[argnr] = dNode.getRep();
    }
    // Check if the value is a date or double.
    // If double, its unit should match days.
    if (args[argnr]->isReal()) {
      if (! args[argnr]->unit().empty()  &&
          args[argnr]->unit() != Unit("d")) {  // It tests if unit type conforms!!
        throw AipsError("Invalid unit used for an epoch in a MEAS function");
      }
    } else if (args[argnr]->dataType() == TableExprNodeRep::NTDate) {
      TableExprNode dNode = mjd(args[argnr]);  // convert date to double
      args[argnr] = dNode.getRep();
    } else {
      throw AipsError ("Invalid epoch given in a MEAS function");
    }
    // Values can be given as [t1,t2,...],reftype
    uInt nargnr = argnr+1;
    // See if there is a reference type.
    if (args.size() > nargnr  &&
        args[nargnr]->dataType() == TableExprNodeRep::NTString) {
      if (handleMeasType (args[nargnr], False)) {
        nargnr++;
      }
    }
    // Handle the epoch values.
    handleMeasArray (args[argnr]);
    // Skip the arguments handled.
    argnr = nargnr;
    // Determine the output unit, shape, and ndim.
    itsOutUnit = "s";
    // Set shape, etc. for constants.
    adaptForConstant (itsConstants.shape());
  }

  String EpochEngine::stripMeasType (const String& type)
  {
    String str(type);
    itsSidFrac = False;
    // F_ (or F-) indicates a full time, thus no sidereal fraction.
    if (str.size() >= 2  &&  str[0] == 'F'  &&
        (str[1] == '-' || str[1] == '_')) {
      str = str.substr(2);
    } else if (str.size() >= 4  &&  str[2] == 'S'  &&  str[3] == 'T') {
      itsSidFrac = True;
    }
    return str;
  }

  void EpochEngine::setPositionEngine (PositionEngine& engine)
  {
    AlwaysAssert (itsPositionEngine == 0, AipsError);
    itsPositionEngine = &engine;
    extendBase (engine, True);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MPosition());
  }

  void EpochEngine::setConverter (MEpoch::Types toType, Bool sidFrac)
  {
    MEpoch::Ref ref(toType, itsFrame);
    itsConverter = MEpoch::Convert (toType, ref);
    itsSidFrac   = sidFrac;
  }

  void EpochEngine::handleValues (TableExprNode& operand,
                                  const TableExprId& id,
                                  Array<MEpoch>& epochs)
  {
    // Get unit (default seconds).
    Unit unit = operand.unit();
    if (unit.empty()) {
      unit = "s";
    }
    // Get values (as doubles or dates).
    Array<Double> values;
    if (operand.getNodeRep()->isReal()) {
      values.reference (operand.getDoubleAS(id).array());
    } else {
      unit = "s";
      Array<MVTime> dates = operand.getDateAS(id).array();
      values.resize (dates.shape());
      for (uInt i=0; i<dates.size(); ++i) {
        values.data()[i] = dates.data()[i].second();
      }
    }
    // Use default reference type UTC if not given.
    if (itsRefType == MEpoch::N_Types) {
      itsRefType = MEpoch::UTC;
    }
    epochs.resize (values.shape());
    Bool delIt;
    const Double* valVec = values.getStorage (delIt);
    MEpoch* epVec = epochs.data();
    for (uInt i=0; i<epochs.size(); ++i) {
      epVec[i] = MEpoch(Quantity(valVec[i], unit), itsRefType);
    }
    values.freeStorage (valVec, delIt);
  }
  
  Array<MEpoch> EpochEngine::getEpochs (const TableExprId& id)
  {
    if (itsConstants.size() > 0) {
      return itsConstants;
    }
    if (!itsMeasScaCol.isNull()) {
      return Vector<MEpoch>(1, itsMeasScaCol(id.rownr()));
    } else if (!itsMeasArrCol.isNull()) {
      return itsMeasArrCol(id.rownr());
    }
    Array<MEpoch> epochs;
    handleValues (itsExprNode, id, epochs);
    return epochs;
  }

  Array<Double> EpochEngine::getArrayDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    Array<MEpoch> res (getEpochs(id));
    // Get positions if given.
    Array<MPosition> pos(IPosition(1,1));
    if (itsPositionEngine) {
      pos.reference (itsPositionEngine->getPositions (id));
    }
    // Convert the epoch to the given type for all positions.
    Array<Double> out;
    if (res.size() > 0  &&  pos.size() > 0) {
      IPosition shape = res.shape();
      if (pos.size() > 1) {
        shape.append (pos.shape());
      }
      out.resize (shape);
      double* outPtr = out.data();
      for (Array<MPosition>::const_contiter posIter = pos.cbegin();
           posIter != pos.cend(); ++posIter) {
        // Convert to desired reference type.
        if (itsPositionEngine) {
          itsFrame.resetPosition (*posIter);
        }
        for (Array<MEpoch>::const_contiter resIter = res.cbegin();
             resIter != res.cend(); ++resIter) {
          itsConverter.setModel (*resIter);
          MEpoch ep = itsConverter();
          // Convert to seconds.
          // Possibly strip day from sidereal times.
          if (itsSidFrac) {
            *outPtr++ = fmod(ep.getValue().get(), 1.) * 24*3600;
          } else {
            *outPtr++ = ep.getValue().get() * 24*3600;
          }
        }
      }
    }
    return out;
  }

} //end namespace
