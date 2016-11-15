//# UDFMSCal.h: TaQL UDFs to calculate derived MS values
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

#ifndef DERIVEDMSCAL_UDFMSCAL_H
#define DERIVEDMSCAL_UDFMSCAL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/derivedmscal/DerivedMC/MSCalEngine.h>
#include <casacore/ms/MeasurementSets/StokesConverter.h>
#include <casacore/ms/MSSel/MSSelectionErrorHandler.h>
#include <casacore/tables/TaQL/UDFBase.h>
#include <casacore/tables/TaQL/ExprNode.h>

namespace casacore {

// <summary>
// TaQL UDFs to calculate derived MS values.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tDerivedMSCal.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> UDFBase
// </prerequisite>

// <synopsis>
// UDFMSCal defines UDFs (user defined functions) that can be used in TaQL
// to get derived MeasurementSet values hourangle, parallactic angle,
// azimuth/elevation, and local sidereal time.
// In this way such derived values appear to be ordinary TaQL functions.
//
// The following functions can be defined:
// <ul>
//  <li> HA is the hourangle of the array center (observatory position).
//  <li> HA1 is the hourangle of ANTENNA1.
//  <li> HA2 is the hourangle of ANTENNA2.
//  <li> HADEC is the hourangle/DEC of the array center (observatory position).
//  <li> HADEC1 is the hourangle/DEC of ANTENNA1.
//  <li> HADEC2 is the hourangle/DEC of ANTENNA2.
//  <li> LAST is the local sidereal time of the array center.
//  <li> LAST1 is the local sidereal time of ANTENNA1.
//  <li> LAST2 is the local sidereal time of ANTENNA2.
//  <li> PA1 is the parallactic angle of ANTENNA1.
//  <li> PA2 is the parallactic angle of ANTENNA2.
//  <li> AZEL1 is the azimuth/elevation of ANTENNA1.
//  <li> AZEL2 is the azimuth/elevation of ANTENNA2.
//  <li> ITRF is the PHASE_DIR in ITRF coordinates (depends on TIME only).
//  <li> UVW_J2000 is the UVW coordinates in J2000 (in meters)
//  <li> STOKES makes it possible to convert Stokes of data, flag, or weight.
//  <li> BASELINE is baseline selection using CASA syntax.
//  <li> CORR is correlation selection using CASA syntax.
//  <li> TIME is baseline selection using CASA syntax.
//  <li> UVDIST is UV-distance selection using CASA syntax.
//  <li> SPW is spectral window selection using CASA syntax.
//  <li> FIELD is field selection using CASA syntax.
//  <li> FEED is feed selection using CASA syntax.
//  <li> ARRAY is array selection using CASA syntax.
//  <li> SCAN is scan selection using CASA syntax.
//  <li> STATE is state selection using CASA syntax.
//  <li> OBS is observation selection using CASA syntax.
//  <li> ANTNAME is the name of the given antenna.
// </ul>
// The first functions have data type double and unit radian (except UVW).
// The HADEC, AZEL, and UVW functions return arrays while the others return
// scalars.
// <br>The STOKES function can have data type Complex, Double or Bool.
// <br>The latter functions are selection functions and return a Bool scalar.
//
// This class is meant for a MeasurementSet, but can be used for any table
// containing an ANTENNA and FIELD subtable and the relevant columns in the
// main table (ANTENNA1 and/or ANTENNA2, FIELD_ID, and TIME).
// <br>In principle the array center is the Observatory position, which is
// taken from the Measures Observatory table using the telescope name found
// in the OBSERVATION subtable. However, if the subtable is not defined or
// empty or if the telescope name is unknown, the position of the first antenna
// is used as the array position.
//
// The engine can also be used for a CASA Calibration Table. It understands
// how it references the MeasurementSets. Because calibration tables contain
// no ANTENNA2 columns, functions XX2 are the same as XX1.
// </synopsis>

// <motivation>
// It makes it possible to do queries on these values without having
// to add columns for them.
// </motivation>

  class UDFMSCal: public UDFBase
  {
  public:
    // Define the possible 'column' types.
    enum ColType {HA, HADEC, PA, LAST, AZEL, ITRF, NEWUVW,
                  UVWWVL, UVWWVLS, NEWUVWWVL, NEWUVWWVLS,
                  STOKES, SELECTION, GETVALUE};
    // Define the possible selection types.
    enum SelType {BASELINE, CORR, TIME, UVDIST, SPW, FIELD,
                  FEED, ARRAY, SCAN, STATE, OBS};

    // Create object the given ColType and SelType.
    UDFMSCal (ColType, Int arg);

    // Create the object for getting a value from a column in a subtable.
    // <group>
    explicit UDFMSCal (const String& funcName);
    UDFMSCal (const String& funcName, const String& subtabName,
              const String& idColName, Int arg=0);
    UDFMSCal (const String& funcName, const String& subtabName,
              const String& idColName, const String& colName);
    // </group>

    // Function to create an object.
    static UDFBase* makeHA       (const String&);
    static UDFBase* makeHA1      (const String&);
    static UDFBase* makeHA2      (const String&);
    static UDFBase* makeHADEC    (const String&);
    static UDFBase* makeHADEC1   (const String&);
    static UDFBase* makeHADEC2   (const String&);
    static UDFBase* makePA1      (const String&);
    static UDFBase* makePA2      (const String&);
    static UDFBase* makeLAST     (const String&);
    static UDFBase* makeLAST1    (const String&);
    static UDFBase* makeLAST2    (const String&);
    static UDFBase* makeAZEL     (const String&);
    static UDFBase* makeAZEL1    (const String&);
    static UDFBase* makeAZEL2    (const String&);
    static UDFBase* makeITRF     (const String&);
    static UDFBase* makeUVW      (const String&);
    static UDFBase* makeWvl      (const String&);
    static UDFBase* makeWvls     (const String&);
    static UDFBase* makeUvwWvl   (const String&);
    static UDFBase* makeUvwWvls  (const String&);
    static UDFBase* makeStokes   (const String&);
    static UDFBase* makeBaseline (const String&);
    static UDFBase* makeCorr     (const String&);
    static UDFBase* makeTime     (const String&);
    static UDFBase* makeUVDist   (const String&);
    static UDFBase* makeSpw      (const String&);
    static UDFBase* makeField    (const String&);
    static UDFBase* makeFeed     (const String&);
    static UDFBase* makeArray    (const String&);
    static UDFBase* makeScan     (const String&);
    static UDFBase* makeState    (const String&);
    static UDFBase* makeObs      (const String&);
    static UDFBase* makeAnt1Name (const String&);
    static UDFBase* makeAnt2Name (const String&);
    static UDFBase* makeAnt1Col  (const String&);
    static UDFBase* makeAnt2Col  (const String&);
    static UDFBase* makeStateCol (const String&);
    static UDFBase* makeObsCol   (const String&);
    static UDFBase* makeSpwCol   (const String&);
    static UDFBase* makePolCol   (const String&);
    static UDFBase* makeFieldCol (const String&);
    static UDFBase* makeProcCol  (const String&);
    static UDFBase* makeSubCol   (const String&);

    // Setup the object.
    virtual void setup (const Table&, const TaQLStyle&);

    // Get the value.
    virtual Bool     getBool     (const TableExprId& id);
    virtual Int64    getInt      (const TableExprId& id);
    virtual Double   getDouble   (const TableExprId& id);
    virtual DComplex getDComplex (const TableExprId& id);
    virtual String   getString   (const TableExprId& id);
    virtual MArray<Bool>     getArrayBool     (const TableExprId& id);
    virtual MArray<Int64>    getArrayInt      (const TableExprId& id);
    virtual MArray<Double>   getArrayDouble   (const TableExprId& id);
    virtual MArray<DComplex> getArrayDComplex (const TableExprId& id);
    virtual MArray<String>   getArrayString   (const TableExprId& id);

    // Let a derived class recreate its column objects in case a selection
    // has to be applied.
    virtual void recreateColumnObjects (const Vector<uInt>& rownrs);

  private:
    // Setup the Stokes conversion.
    void setupStokes (const Table& table,
                      PtrBlock<TableExprNodeRep*>& operands);

    // Setup the baseline selection.
    void setupSelection (const Table& table,
                         PtrBlock<TableExprNodeRep*>& operands);

    // Setup direction conversion if a direction is explicitly given.
    void setupDir (TableExprNodeRep*& operand);

    // Setup getting column values from a subtable.
    void setupGetValue (const Table& table,
                        PtrBlock<TableExprNodeRep*>& operands);

    // Setup getting the wavelength information.
    void setupWvls (const Table& table,
                    PtrBlock<TableExprNodeRep*>& operands,
                    uInt nargMax);

    // Get the rownr in the subtable for GetValue.
    // If itsArg==1 it uses indirection using itsDDIds.
    Int64 getRowNr (const TableExprId& id);

    // Convert the UVW coordinates to wavelengths for the full spectrum.
    Array<Double> toWvls (const TableExprId&);

    //# Data members.
    MSCalEngine     itsEngine;
    StokesConverter itsStokesConv;
    TableExprNode   itsDataNode;   //# for stokes, selections and getvalues
    TableExprNode   itsIdNode;     //# node giving rowid for getvalues
    ArrayColumn<Double> itsUvwCol;
    ColType         itsType;
    Int             itsArg;        //# antnr or SelType or getValueType
                                   //# -1 subtable can be empty
                                   //#  0 normal subtable
                                   //#  1 indirect subtable via DATA_DESC_ID
    String          itsFuncName;
    String          itsSubTabName;
    String          itsIdColName;
    String          itsSubColName;
    //# Preallocate arrays to avoid having to construct them too often.
    //# Makes it thread-unsafe though.
    Vector<Double>  itsTmpVector;
    Array<Double>   itsTmpUvwWvl;
    Vector<Int>     itsDDIds;      //# spw or pol ids from DATA_DESCRIPTION
    vector<Double>          itsWavel;
    vector<Vector<Double> > itsWavels;
  };


  // <summary>
  // Error handler class for MSSel selection
  // </summary>
  // <synopsis>
  // This error handler ignores the errors rising from the MSSel parsers.
  // </synopsis>
  class UDFMSCalErrorHandler : public MSSelectionErrorHandler
  {
  public:
    virtual ~UDFMSCalErrorHandler()
    {}
    virtual void handleError (MSSelectionError&)
    {}
    virtual void reportError (const char*, const String)
    {}
  };


} //end namespace

#endif
