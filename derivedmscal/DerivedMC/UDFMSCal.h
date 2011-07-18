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
#include <derivedmscal/DerivedMC/MSCalEngine.h>
#include <ms/MeasurementSets/StokesConverter.h>
#include <tables/Tables/UDFBase.h>
#include <tables/Tables/ExprNode.h>

namespace casa {

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
//  <li> LAST is the local sidereal time of the array center.
//  <li> LAST1 is the local sidereal time of ANTENNA1.
//  <li> LAST2 is the local sidereal time of ANTENNA2.
//  <li> PA1 is the parallactic angle of ANTENNA1.
//  <li> PA2 is the parallactic angle of ANTENNA2.
//  <li> AZEL1 is the azimuth/elevation of ANTENNA1.
//  <li> AZEL2 is the azimuth/elevation of ANTENNA2.
//  <li> UVW_J2000 is the UVW coordinates in J2000 (in meters)
// </ul>
// All functions have data type double and unit radian (except UVW). The AZEL
// and UVW functions return arrays while the others return scalars.
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
    enum ColType {HA, PA, LAST, AZEL, UVW, STOKES};

    explicit UDFMSCal (ColType, Int antnr);

    // Function to create an object.
    static UDFBase* makeHA    (const String&);
    static UDFBase* makeHA1   (const String&);
    static UDFBase* makeHA2   (const String&);
    static UDFBase* makePA1   (const String&);
    static UDFBase* makePA2   (const String&);
    static UDFBase* makeLAST  (const String&);
    static UDFBase* makeLAST1 (const String&);
    static UDFBase* makeLAST2 (const String&);
    static UDFBase* makeAZEL1 (const String&);
    static UDFBase* makeAZEL2 (const String&);
    static UDFBase* makeUVW   (const String&);
    static UDFBase* makeStokes(const String&);

    // Setup the object.
    virtual void setup (const Table&, const TaQLStyle&);

    // Replace the Table in this node.
    virtual void replaceTable (const Table&);

    // Get the value.
    virtual Double getDouble (const TableExprId& id);
    virtual Array<Bool> getArrayBool (const TableExprId& id);
    virtual Array<Double> getArrayDouble (const TableExprId& id);
    virtual Array<DComplex> getArrayDComplex (const TableExprId& id);

  private:
    // Setup the Stokes conversion.
    void setupStokes (const Table& table,
                      PtrBlock<TableExprNodeRep*>& operands);

    // Setup direction conversion if a direction is explicitly given.
    void setupDir (TableExprNodeRep*& operand);

    //# Data members.
    MSCalEngine     itsEngine;
    StokesConverter itsStokesConv;
    TableExprNode   itsDataNode;   //# for stokes conversion
    ColType         itsType;
    Int             itsAntNr;
    //# Preallocate vectors to avoid having to construct them too often.
    //# Makes it thread-unsafe though.
    Vector<Double>  itsTmpAzEl;
    Vector<Double>  itsTmpUVW;
  };

} //end namespace

#endif
