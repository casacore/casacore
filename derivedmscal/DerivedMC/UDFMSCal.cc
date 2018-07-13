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

#include <casacore/derivedmscal/DerivedMC/UDFMSCal.h>
#include <casacore/ms/MSSel/MSAntennaGram.h>
#include <casacore/ms/MSSel/MSCorrGram.h>
#include <casacore/ms/MSSel/MSTimeGram.h>
#include <casacore/ms/MSSel/MSUvDistGram.h>
#include <casacore/ms/MSSel/MSSpwGram.h>
#include <casacore/ms/MSSel/MSFieldGram.h>
#include <casacore/ms/MSSel/MSFeedGram.h>
#include <casacore/ms/MSSel/MSArrayGram.h>
#include <casacore/ms/MSSel/MSStateGram.h>
#include <casacore/ms/MSSel/MSStateParse.h>
#include <casacore/ms/MSSel/MSScanGram.h>
#include <casacore/ms/MSSel/MSSelectableMainColumn.h>
#include <casacore/ms/MSSel/MSObservationGram.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>

namespace casacore {

  UDFMSCal::UDFMSCal (ColType type, Int arg)
    : itsType (type),
      itsArg  (arg)
  {
    if (itsType == DELAY) {
      // Default column to use for delays.
      itsEngine.setDirColName ("DELAY_DIR");
    }
  }

  UDFMSCal::UDFMSCal (const String& funcName)
    : itsType       (GETVALUE),
      itsArg        (0),
      itsFuncName   (funcName)
  {}

  UDFMSCal::UDFMSCal (const String& funcName, const String& subtabName,
                      const String& idcolName, Int arg)
    : itsType       (GETVALUE),
      itsArg        (arg),
      itsFuncName   (funcName),
      itsSubTabName (subtabName),
      itsIdColName  (idcolName)
  {}

  UDFMSCal::UDFMSCal (const String& funcName, const String& subtabName,
                      const String& idcolName, const String& subcolName)
    : itsType       (GETVALUE),
      itsArg        (0),
      itsFuncName   (funcName),
      itsSubTabName (subtabName),
      itsIdColName  (idcolName),
      itsSubColName (subcolName)
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
  UDFBase* UDFMSCal::makeAZEL (const String&)
    { return new UDFMSCal (AZEL, -1); }
  UDFBase* UDFMSCal::makeAZEL1 (const String&)
    { return new UDFMSCal (AZEL, 0); }
  UDFBase* UDFMSCal::makeAZEL2 (const String&)
    { return new UDFMSCal (AZEL, 1); }
  UDFBase* UDFMSCal::makeITRF (const String&)
    { return new UDFMSCal (ITRF, -1); }
  UDFBase* UDFMSCal::makeUvwWvl (const String&)
    { return new UDFMSCal (UVWWVL, -1); }
  UDFBase* UDFMSCal::makeUvwWvls (const String&)
    { return new UDFMSCal (UVWWVLS, -1); }
  UDFBase* UDFMSCal::makeUvwJ2000 (const String&)
    { return new UDFMSCal (NEWUVW, 0); }
  UDFBase* UDFMSCal::makeWvlJ2000 (const String&)
    { return new UDFMSCal (NEWUVWWVL, 0); }
  UDFBase* UDFMSCal::makeWvlsJ2000 (const String&)
    { return new UDFMSCal (NEWUVWWVLS, 0); }
  UDFBase* UDFMSCal::makeUvwAPP (const String&)
    { return new UDFMSCal (NEWUVW, 1); }
  UDFBase* UDFMSCal::makeWvlAPP (const String&)
    { return new UDFMSCal (NEWUVWWVL, 1); }
  UDFBase* UDFMSCal::makeWvlsAPP (const String&)
    { return new UDFMSCal (NEWUVWWVLS, 1); }
  UDFBase* UDFMSCal::makeDelay (const String&)
    { return new UDFMSCal (DELAY, -1); }
  UDFBase* UDFMSCal::makeDelay1 (const String&)
    { return new UDFMSCal (DELAY, 0); }
  UDFBase* UDFMSCal::makeDelay2 (const String&)
    { return new UDFMSCal (DELAY, 1); }
  UDFBase* UDFMSCal::makeStokes (const String&)
    { return new UDFMSCal (STOKES, -1); }
  UDFBase* UDFMSCal::makeBaseline (const String&)
  { return new UDFMSCal (SELECTION, BASELINE); }
  UDFBase* UDFMSCal::makeCorr (const String&)
    { return new UDFMSCal (SELECTION, CORR); }
  UDFBase* UDFMSCal::makeTime (const String&)
    { return new UDFMSCal (SELECTION, TIME); }
  UDFBase* UDFMSCal::makeUVDist (const String&)
    { return new UDFMSCal (SELECTION, UVDIST); }
  UDFBase* UDFMSCal::makeSpw (const String&)
    { return new UDFMSCal (SELECTION, SPW); }
  UDFBase* UDFMSCal::makeField (const String&)
    { return new UDFMSCal (SELECTION, FIELD); }
  UDFBase* UDFMSCal::makeFeed (const String&)
    { return new UDFMSCal (SELECTION, FEED); }
  UDFBase* UDFMSCal::makeArray (const String&)
    { return new UDFMSCal (SELECTION, ARRAY); }
  UDFBase* UDFMSCal::makeScan (const String&)
    { return new UDFMSCal (SELECTION, SCAN); }
  UDFBase* UDFMSCal::makeState (const String&)
    { return new UDFMSCal (SELECTION, STATE); }
  UDFBase* UDFMSCal::makeObs (const String&)
    { return new UDFMSCal (SELECTION, OBS); }
  UDFBase* UDFMSCal::makeAnt1Name (const String& funcName)
    { return new UDFMSCal (funcName, "ANTENNA", "ANTENNA1", "NAME"); }
  UDFBase* UDFMSCal::makeAnt2Name (const String& funcName)
    { return new UDFMSCal (funcName, "ANTENNA", "ANTENNA2", "NAME"); }
  UDFBase* UDFMSCal::makeAnt1Col (const String& funcName)
    { return new UDFMSCal (funcName, "ANTENNA", "ANTENNA1"); }
  UDFBase* UDFMSCal::makeAnt2Col (const String& funcName)
    { return new UDFMSCal (funcName, "ANTENNA", "ANTENNA2"); }
  UDFBase* UDFMSCal::makeStateCol (const String& funcName)
    { return new UDFMSCal (funcName, "STATE", "STATE_ID", -1); }
  UDFBase* UDFMSCal::makeObsCol (const String& funcName)
    { return new UDFMSCal (funcName," OBSERVATION", "OBSERVATION_ID"); }
  UDFBase* UDFMSCal::makeSpwCol (const String& funcName)
    { return new UDFMSCal (funcName, "SPECTRAL_WINDOW", "SPECTRAL_WINDOW_ID", 1); }
  UDFBase* UDFMSCal::makePolCol (const String& funcName)
    { return new UDFMSCal (funcName, "POLARIZATION", "POLARIZATION_ID", 1); }
  UDFBase* UDFMSCal::makeFieldCol (const String& funcName)
    { return new UDFMSCal (funcName, "FIELD", "FIELD_ID"); }
  UDFBase* UDFMSCal::makeProcCol (const String& funcName)
    { return new UDFMSCal (funcName, "PROCESSOR", "PROCESSOR_ID", -1); }
  UDFBase* UDFMSCal::makeSubCol (const String& funcName)
    { return new UDFMSCal (funcName); }


  void UDFMSCal::setup (const Table& table, const TaQLStyle&)
  {
    if (table.isNull()) {
      throw AipsError ("MSCAL can only be used on a table");
    }
    // Most functions are handled by the engine.
    if (itsType != STOKES  &&  itsType != SELECTION  &&  itsType != GETVALUE  &&
        itsType != UVWWVL  &&  itsType != UVWWVLS) {
      itsEngine.setTable (table);
      if (operands().size() > 1) {
        throw AipsError("More than 1 argument given to MSCAL function");
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
    case ITRF:
      setShape (IPosition(1,2));
      itsTmpVector.resize (2);
      setUnit ("rad");
      break;
    case UVWWVL:
      setupWvls (table, operands(), 0);
      setShape (IPosition(1,3));
      itsTmpVector.resize (3);
      break;
    case UVWWVLS:
      setupWvls (table, operands(), 0);
      itsTmpVector.resize (3);
      setNDim(2);  // The shape can vary (each band can be different)
      break;
    case NEWUVW:
      setUnit ("m");
      setShape (IPosition(1,3));
      itsTmpVector.resize (3);
      break;
    case NEWUVWWVL:
      setupWvls (table, operands(), 1);
      setShape (IPosition(1,3));
      itsTmpVector.resize (3);
      break;
    case NEWUVWWVLS:
      setupWvls (table, operands(), 1);
      itsTmpVector.resize (3);
      setNDim (2);  // The shape can vary (each band can be different)
      break;
    case DELAY:
      setNDim (0);
      setUnit ("s");
      break;
    case STOKES:
      setupStokes (table, operands());
      setDataType (operands()[0]->dataType());
      break;
    case SELECTION:
      setupSelection (table, operands());
      setNDim (0);
      setDataType (TableExprNodeRep::NTBool);
      break;
    case GETVALUE:
      setupGetValue (table, operands());
      setNDim (itsDataNode.getNodeRep()->ndim());
      setDataType (itsDataNode.getNodeRep()->dataType());
      setUnit (itsDataNode.getNodeRep()->unit().getName());
      break;
    }
  }

  void UDFMSCal::setupDir (TENShPtr& operand)
  {
    // Make sure the operand is a constant double array
    // or a single string (e.g. MOON).
    if (! operand->isConstant()) {
      throw AipsError("Only a constant value can be given as a MSCAL "
                      "function argument");
    }
    // In principle type NTInt could also be allowed, but that makes no sense
    // for direction values. So it's better to be a bit strict.
    if (operand->dataType() == TableExprNodeRep::NTDouble) {
      // Get direction (given in J2000).
      if (operand->valueType() != TableExprNodeRep::VTArray) { 
        throw AipsError ("Argument to MSCAL function is not an array "
                         "of 2 values");
      }
      // Make sure the unit is rad.
      // Turn the array into a vector.
      TableExprNodeUnit::adaptUnit (operand, "rad");
      Array<Double> dirs(operand->getArrayDouble(0).array());
      if (dirs.size() != 2) {
        throw AipsError ("Argument to MSCAL function is not an array "
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
          throw AipsError ("An empty string given to a MSCAL function");
        }
        itsEngine.setDirColName (str);
      }
    } else {
      throw AipsError ("Argument to MSCAL function must be double or "
                       "string");
    }
  }

  void UDFMSCal::setupWvls (const Table& table,
                            vector<TENShPtr>& operands,
                            uInt nargMax)
  {
    // There must be at least 1 argument (data).
    if (operands.size() > nargMax) {
      throw AipsError ("No arguments should be given to MSCAL.UVWWVL");
    }
    // Read the reference and channel frequencies.
    // Divide by lightspeed for conversion to wavelengths.
    // Determine the maximum nr of frequencies in a band.
    Table spwTab(table.keywordSet().asTable("SPECTRAL_WINDOW"));
    ScalarColumn<Double> refCol(spwTab, "REF_FREQUENCY");
    ArrayColumn<Double> freqCol(spwTab, "CHAN_FREQ");
    itsWavel.reserve (spwTab.nrow());
    itsWavels.reserve (spwTab.nrow());
    uInt nfreq = 0;
    for (uInt i=0; i<spwTab.nrow(); ++i) {
      itsWavel.push_back (refCol(i) / C::c);
      itsWavels.push_back (freqCol(i) / C::c);
      if (itsWavels[i].size() > nfreq) {
        nfreq = itsWavels[i].size();
      }      
    }
    itsTmpUvwWvl.resize (IPosition(2, 3, nfreq));
    if (itsType == UVWWVL  ||  itsType == UVWWVLS) {
      itsUvwCol.attach (table, "UVW");
    }
    // Set itsIdColName for recreateColumnObjects.
    itsIdColName = "DATA_DESC_ID";
    itsIdNode = table.col(itsIdColName);
    // Get the spectal window ids.
    Table ddtab(table.keywordSet().asTable("DATA_DESCRIPTION"));
    ScalarColumn<Int>(ddtab, "SPECTRAL_WINDOW_ID").getColumn (itsDDIds);
  }

  void UDFMSCal::setupStokes (const Table& table,
                              vector<TENShPtr>& operands)
  {
    // There must be at least 1 argument (data).
    if (operands.size() == 0  ||  operands.size() > 3) {
      throw AipsError ("1, 2, or 3 arguments must be given to "
                       "MSCAL.STOKES");
    }
    itsDataNode = TableExprNode(operands[0]);
    if (operands[0]->valueType() != TableExprNodeRep::VTArray  ||
        !(operands[0]->dataType() == TableExprNodeRep::NTBool  ||
          operands[0]->dataType() == TableExprNodeRep::NTDouble  ||
          operands[0]->dataType() == TableExprNodeRep::NTComplex)) {
      throw AipsError ("First argument of MSCAL.STOKES must be a "
                       "Complex, Double or Bool array");
    }
    // The optional second argument gives the output correlation types.
    // Default is iquv.
    String type = "IQUV";
    if (operands.size() > 1) {
      if (! operands[1]->isConstant()  ||
          operands[1]->valueType() != TableExprNodeRep::VTScalar  ||
          operands[1]->dataType() != TableExprNodeRep::NTString) {
        throw AipsError ("Second argument of MSCAL.STOKES must be a "
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
        throw AipsError ("Second argument of MSCAL.STOKES must be a "
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
                      "MSCAL.STOKES");
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

  void UDFMSCal::setupSelection (const Table& table,
                                 vector<TENShPtr>& operands)
  {
    // There must be 1 argument (scalar string).
    if (operands.size() != 1) {
      throw AipsError ("1 argument must be given to MSCAL selections");
    }
    if (operands[0]->valueType() != TableExprNodeRep::VTScalar  ||
        operands[0]->dataType()  != TableExprNodeRep::NTString  ||
        !operands[0]->isConstant()) {
      throw AipsError ("Argument of MSCAL selection must be a "
                       "constant string scalar");
    }
    // Get and process the selection string.
    String selStr(operands[0]->getString(0));
    switch (itsArg) {
    case BASELINE:
      {
        Table anttab(table.keywordSet().asTable("ANTENNA"));
        TableExprNode a1 (table.col("ANTENNA1"));
        TableExprNode a2 (a1);
        if (table.tableDesc().isColumn("ANTENNA2")) {
          a2 = TableExprNode (table.col("ANTENNA2"));
        }
        Vector<Int> selectedAnts1;
        Vector<Int> selectedAnts2;
        Matrix<Int> selectedBaselines;
        MSSelectionErrorHandler* curHandler = MSAntennaParse::thisMSAErrorHandler;
        UDFMSCalErrorHandler errorHandler;
        MSAntennaParse::thisMSAErrorHandler = &errorHandler;
        try {
          itsDataNode = msAntennaGramParseCommand (anttab, a1, a2, selStr, 
                                                   selectedAnts1, selectedAnts2,
                                                   selectedBaselines);
        } catch (const std::exception&) {
          MSAntennaParse::thisMSAErrorHandler = curHandler;
          throw;
        }          
        MSAntennaParse::thisMSAErrorHandler = curHandler;
      }
      break;
    case CORR:
      {
        MeasurementSet ms(table);
        if (msCorrGramParseCommand(&ms, selStr) == 0) {
          itsDataNode = *(msCorrGramParseNode());
        }
        msCorrGramParseDeleteNode();
      }
      break;
    case TIME:
      {
        MeasurementSet ms(table);
        TableExprNode node (table.col("TIME"));
        Matrix<Double> times;
	MSMainColInterface tmp;
        if (msTimeGramParseCommand (&ms,selStr, TableExprNode(),
				    tmp,
                                    node, times) == 0) {
          itsDataNode = *(msTimeGramParseNode());
        }
        msTimeGramParseDeleteNode();
      }
      break;
    case UVDIST:
      {
        MeasurementSet ms(table);
        if (msUvDistGramParseCommand(&ms, selStr) == 0) {
          itsDataNode = *(msUvDistGramParseNode());
        }
        msUvDistGramParseDeleteNode();
      }
      break;
    case SPW:
      {
        Table ddtab (table.keywordSet().asTable("DATA_DESCRIPTION"));
        Table spwtab(table.keywordSet().asTable("SPECTRAL_WINDOW"));
        TableExprNode colAsTEN = table.col("DATA_DESC_ID");
        Vector<Int> spwid, spwDDID;
        Matrix<Int> chanid;
        if (msSpwGramParseCommand(MSSpectralWindow(spwtab),
                                  MSDataDescription(ddtab),
                                  colAsTEN, selStr,
                                  spwid, chanid, spwDDID) == 0) {
          itsDataNode = *(msSpwGramParseNode());
        }
        msSpwGramParseDeleteNode();
      }
      break;
    case FIELD:
      {
        Table fieldtab(table.keywordSet().asTable("FIELD"));
        TableExprNode colAsTEN = table.col("FIELD_ID");
        Vector<Int> fldid;
        itsDataNode = msFieldGramParseCommand (fieldtab, colAsTEN, selStr,
                                               fldid);
        msFieldGramParseDeleteNode();
      }
      break;
    case FEED:
      {
        Table feedtab(table.keywordSet().asTable("FEED"));
        TableExprNode f1 (table.col("FEED1"));
        TableExprNode f2 (f1);
        if (table.tableDesc().isColumn("FEED2")) {
          f2 = TableExprNode (table.col("FEED2"));
        }
        Vector<Int> selectedFeed1;
        Vector<Int> selectedFeed2;
        Matrix<Int> selectedFeedPairs;
        MSSelectionErrorHandler* curHandler = MSFeedParse::thisMSFErrorHandler;
        UDFMSCalErrorHandler errorHandler;
        MSFeedParse::thisMSFErrorHandler = &errorHandler;
        try {
          itsDataNode = msFeedGramParseCommand (feedtab, f1, f2, selStr, 
                                                selectedFeed1, selectedFeed2,
                                                selectedFeedPairs);
        } catch (const std::exception&) {
          MSFeedParse::thisMSFErrorHandler = curHandler;
          throw;
        }          
        MSFeedParse::thisMSFErrorHandler = curHandler;
      }
      break;
    case ARRAY:
      {
        MeasurementSet ms(table);
        Vector<Int> arrid;
        Int maxArr=1000;
        itsDataNode = msArrayGramParseCommand(&ms, selStr, arrid, maxArr);
      }
      break;
    case SCAN:
      {
        MeasurementSet ms(table);
        Vector<Int> scanid;
        Int maxScan=1000;
        itsDataNode = msScanGramParseCommand(&ms, selStr, scanid, maxScan);
      }
      break;
    case STATE:
      {
        MeasurementSet ms(table);
        Vector<Int> stateid;
        MSSelectionErrorHandler* curHandler = MSStateParse::thisMSSErrorHandler;
        UDFMSCalErrorHandler errorHandler;
        MSStateParse::thisMSSErrorHandler = &errorHandler;
        try {
          if (msStateGramParseCommand(&ms, selStr, stateid) == 0) {
            itsDataNode = *(msStateGramParseNode());
          }
        } catch (const std::exception&) {
          msStateGramParseDeleteNode();
          MSStateParse::thisMSSErrorHandler = curHandler;
          throw;
        }          
        MSStateParse::thisMSSErrorHandler = curHandler;
      }
      break;
    case OBS:
      {
        MeasurementSet ms(table);
        Vector<Int> obsid;
	//        Int maxObs=1000;
        TableExprNode colAsTEN = table.col("OBSERVATION_ID");
        itsDataNode = msObservationGramParseCommand(&ms, ms.observation(),
						    colAsTEN,
                                                    selStr, obsid);
      }
      break;
    default:
      throw AipsError ("UDFMScal::setupSelection: unknown type " +
                       String::toString(itsArg));
    }
  }

  void UDFMSCal::setupGetValue (const Table& table,
                                vector<TENShPtr>& operands)
  {
    int idinx = 0;
    // See if subtable and column name have to be given explicitly as the
    // first arguments.
    if (itsSubColName.empty()) {
      idinx = 1;
      if (itsSubTabName.empty()) idinx = 2;
    }
    uInt nargReq = idinx;
    // Id column must be given if id (ANTENNA1/2) is not part of function name.
    if (itsIdColName.empty()) nargReq++;
    if (operands.size() != nargReq) {
      throw AipsError ("Function " + itsFuncName + " has " + 
                       String::toString(operands.size()) +
                       " arguments, but should have " +
                       String::toString(nargReq));
    }
    // Get subtable and column name; they must be constant strings.
    for (int i=0; i<idinx; ++i) {
      if (! operands[i]->isConstant()  ||
          operands[i]->valueType() != TableExprNodeRep::VTScalar  ||
          operands[i]->dataType() != TableExprNodeRep::NTString) {
        throw AipsError ("First " + String::toString(idinx) +
                         " argument(s) of function " + itsFuncName +
                         " must be constant strings");
      }
      String str = operands[i]->getString(0);
      if (str.empty()) {
        throw AipsError ("An empty subtable name or column name given in "
                         "function " + itsFuncName);
      }
      if (i == idinx-1) {
        itsSubColName = str;
      } else {
        itsSubTabName = str;
      }
    }
    // An id column needs to be given if not using ANTENNA1 or ANTENNA2.
    // It must be an integer column.
    if (itsIdColName.empty()) {
      if (operands[idinx]->valueType() != TableExprNodeRep::VTScalar  ||
          operands[idinx]->dataType() != TableExprNodeRep::NTInt) {
        throw AipsError ("Last argument of function " + itsFuncName +
                         " must be an integer scalar");
      }
      itsIdNode = operands[idinx];
    } else {
      if (itsArg == 1) {
        // Subtable has an indirection via the DATA_DESCRIPTION.
        // Get the ids of the required column.
        Table ddtab(table.keywordSet().asTable("DATA_DESCRIPTION"));
        ScalarColumn<Int>(ddtab, itsIdColName).getColumn (itsDDIds);
        itsIdColName = "DATA_DESC_ID";
      }
      // Create the id node.
      /// This fails for GROUPBY, because node does not use groupby rownrs.
      /// I.e. has to do applySelection!!!
      /// Maybe let ExprUDFNode map group rownr to original rownr, but that
      /// causes problems when aggregate function used in mscal.
      /// Maybe add the created nodes in TableParse to applySelNodes_p, but
      /// maybe has to take care if it is in a group.
      /// Also look at other UDF functions (in mscal and meas).
      itsIdNode = table.col(itsIdColName);
    }
    // Create the node for the data item in the subtable.
    Table subtab(table.keywordSet().asTable(itsSubTabName));
    itsDataNode = subtab.col(itsSubColName);
  }


  void UDFMSCal::recreateColumnObjects (const Vector<uInt>& rownrs)
  {
    if (! itsIdColName.empty()) {
      TableExprNodeRep* col = const_cast<TableExprNodeRep*>(itsIdNode.getNodeRep());
      col->applySelection (rownrs);
    }
    if (! itsUvwCol.isNull()) {
      Table tab(itsUvwCol.table());
      itsUvwCol.attach (tab(rownrs), "UVW");
    }
    Table tab(itsEngine.getTable());
    if (! tab.isNull()) {
      itsEngine.setTable (tab(rownrs));
    }
  }

  Int64 UDFMSCal::getRowNr (const TableExprId& id)
  {
    Int64 rownr = itsIdNode.getInt(id);
    if (itsArg == 1) {
      rownr = itsDDIds[rownr];
    }
    return rownr;
  }

  Array<Double> UDFMSCal::toWvls (const TableExprId& id)
  {
    const Vector<Double>& wvl = itsWavels[itsDDIds[itsIdNode.getInt(id)]];
    Double* ptr = itsTmpUvwWvl.data();
    for (uInt i=0; i<wvl.size(); ++i) {
      for (int j=0; j<3; ++j) {
        *ptr++ = wvl[i] * itsTmpVector[j];
      }
    }
    // Return the correct part of the array.
    if (itsTmpUvwWvl.shape()[1] == Int(wvl.size())) {
      return itsTmpUvwWvl;
    }
    return itsTmpUvwWvl(IPosition(2,0,0), IPosition(2, 2, wvl.size()-1));
  }

  Bool UDFMSCal::getBool (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case SELECTION:
      return itsDataNode.getBool (id);
    case GETVALUE:
      {
        Int64 rownr = getRowNr(id);
        if (itsArg < 0  &&  rownr >= itsDataNode.nrow()) {
          return False;
        }
        return itsDataNode.getBool (rownr);
      }
    default:
      throw AipsError ("UDFMSCal: unexpected getBool function");
    }
  }

  Int64 UDFMSCal::getInt (const TableExprId& id)
  {
    switch (itsType) {
    case GETVALUE:
      {
        Int64 rownr = getRowNr(id);
        if (itsArg < 0  &&  rownr >= itsDataNode.nrow()) {
          return 0;
        }
        return itsDataNode.getInt (rownr);
      }
    default:
      throw AipsError ("UDFMSCal: unexpected getInt function");
    }
  }

  Double UDFMSCal::getDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case HA:
      return itsEngine.getHA (itsArg, id.rownr());
    case PA:
      return itsEngine.getPA (itsArg, id.rownr());
    case LAST:
      return itsEngine.getLAST (itsArg, id.rownr());
    case DELAY:
      return itsEngine.getDelay (itsArg, id.rownr());
    case GETVALUE:
      {
        Int64 rownr = getRowNr(id);
        if (itsArg < 0  &&  rownr >= itsDataNode.nrow()) {
          return 0.;
        }
        return itsDataNode.getDouble (rownr);
      }
    default:
      throw AipsError ("UDFMSCal: unexpected getDouble function");
    }
  }

  DComplex UDFMSCal::getDComplex (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case GETVALUE:
      {
        Int64 rownr = getRowNr(id);
        if (itsArg < 0  &&  rownr >= itsDataNode.nrow()) {
          return DComplex();
        }
        return itsDataNode.getDComplex (rownr);
      }
    default:
      throw AipsError ("UDFMSCal: unexpected getDComplex function");
    }
  }

  String UDFMSCal::getString (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case GETVALUE:
      {
        Int64 rownr = getRowNr(id);
        if (itsArg < 0  &&  rownr >= itsDataNode.nrow()) {
          return String();
        }
        return itsDataNode.getString (rownr);
      }
    default:
      throw AipsError ("UDFMSCal: unexpected getString function");
    }
  }

  MArray<Bool> UDFMSCal::getArrayBool (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case STOKES:
      {
        Array<Bool> out;
        MArray<Bool> marr;
        itsDataNode.get (id, marr);
        // Combine the flags.
        itsStokesConv.convert (out, marr.array());
        if (! marr.hasMask()) {
          return MArray<Bool>(out);
        }
        // Combine the mask elements.
        Array<Bool> mask;
        itsStokesConv.convert (mask, marr.mask());
        return MArray<Bool> (out, mask);
      }
    case GETVALUE:
      return itsDataNode.getBoolAS (getRowNr(id));
    default:
      throw AipsError ("UDFMSCal: unexpected getArrayBool function");
    }
  }

  MArray<Int64> UDFMSCal::getArrayInt (const TableExprId& id)
  {
    switch (itsType) {
    case GETVALUE:
      return itsDataNode.getIntAS (getRowNr(id));
    default:
      throw AipsError ("UDFMSCal: unexpected getArrayInt function");
    }
  }

  MArray<Double> UDFMSCal::getArrayDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case HADEC:
      itsEngine.getHaDec (itsArg, id.rownr(), itsTmpVector);
      return MArray<Double>(itsTmpVector);
    case AZEL:
      itsEngine.getAzEl (itsArg, id.rownr(), itsTmpVector);
      return MArray<Double>(itsTmpVector);
    case ITRF:
      itsEngine.getItrf (itsArg, id.rownr(), itsTmpVector);
      return MArray<Double>(itsTmpVector);
    case UVWWVL:
      itsUvwCol.get (id.rownr(), itsTmpVector);
      itsTmpVector *= itsWavel[itsDDIds[itsIdNode.getInt(id)]];
      return MArray<Double>(itsTmpVector);
    case UVWWVLS:
      itsUvwCol.get (id.rownr(), itsTmpVector);
      return MArray<Double>(toWvls (id));
    case NEWUVW:
      itsEngine.getNewUVW (itsArg, id.rownr(), itsTmpVector);
      return MArray<Double>(itsTmpVector);
    case NEWUVWWVL:
      itsEngine.getNewUVW (itsArg, id.rownr(), itsTmpVector);
      itsTmpVector *= itsWavel[itsDDIds[itsIdNode.getInt(id)]];
      return MArray<Double>(itsTmpVector);
    case NEWUVWWVLS:
      itsEngine.getNewUVW (itsArg, id.rownr(), itsTmpVector);
      return MArray<Double>(toWvls (id));
    case STOKES:
      {
        // Unfortunately stokes weight conversion is only defined for Float,
        // while TableExprNode only has Double.
        // So conversions are necessary for the time being.
        // In the future we can add Double support to StokesConverter.
        MArray<Double> datad;
        Array<Float>  dataf, outf;
        Array<Double> outd;
        itsDataNode.get (id, datad);
        dataf.resize (datad.shape());
        convertArray (dataf, datad.array());
        itsStokesConv.convert (outf, dataf);
        outd.resize (outf.shape());
        convertArray (outd, outf);
        if (! datad.hasMask()) {
          return MArray<Double>(outd);
        }
        // Combine the mask elements.
        Array<Bool> mask;
        itsStokesConv.convert (mask, datad.mask());
        return MArray<Double>(outd, mask);
      }
    case GETVALUE:
      return itsDataNode.getDoubleAS (getRowNr(id));
    default:
      throw AipsError ("UDFMSCal: unexpected getArrayDouble function");
    }
  }

  MArray<DComplex> UDFMSCal::getArrayDComplex (const TableExprId& id)
  {
    switch (itsType) {
    case STOKES:
      {
        // Unfortunately stokes conversion is only defined for type Complex,
        // while TableExprNode only has DComplex.
        // So conversions are necessary for the time being.
        // In the future we can add DComplex support to StokesConverter
        // or Complex support to TableExprNode.
        MArray<DComplex> datad;
        Array<Complex> outf, dataf;
        Array<DComplex> outd;
        itsDataNode.get (id, datad);
        dataf.resize (datad.shape());
        convertArray (dataf, datad.array());
        itsStokesConv.convert (outf, dataf);
        outd.resize (outf.shape());
        convertArray (outd, outf);
        if (! datad.hasMask()) {
          return MArray<DComplex>(outd);
        }
        // Combine the mask elements.
        Array<Bool> mask;
        itsStokesConv.convert (mask, datad.mask());
        return MArray<DComplex>(outd, mask);
      }
    case GETVALUE:
      return itsDataNode.getDComplexAS (getRowNr(id));
    default:
      throw AipsError ("UDFMSCal: unexpected getArrayDComplex function");
    }
  }

  MArray<String> UDFMSCal::getArrayString (const TableExprId& id)
  {
    switch (itsType) {
    case GETVALUE:
      return itsDataNode.getStringAS (getRowNr(id));
    default:
      throw AipsError ("UDFMSCal: unexpected getArrayString function");
    }
  }

} //end namespace
