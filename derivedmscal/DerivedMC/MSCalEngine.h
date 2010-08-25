//# MSCalEngine.h: Engine to calculate derived MS values
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

#ifndef DERIVEDMSCAL_MSCALENGINE_H
#define DERIVEDMSCAL_MSCALENGINE_H

//# Includes
#include <tables/Tables/Table.h>
#include <tables/Tables/ScalarColumn.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MBaseline.h>
#include <measures/Measures/MeasConvert.h>
#include <measures/TableMeasures/ScalarMeasColumn.h>
#include <casa/vector.h>
#include <casa/stdmap.h>

namespace casa {

// <summary>
// Engine to calculate derived MS values
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tDerivedMSCal.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> MeasurementSet
// </prerequisite>

// <synopsis>
// MSCalEngine is a class used to calculate derived MeasurementSet
// values hourangle, parallactic angle, azimuth/elevation,
// and local sidereal time.
// It is used by the DerivedMSCal virtual columns and UDFs, but can
// be used by other software as well.
//
// The following values can be obtained:
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
// All values have data type double and unit radian (except UVW). The AZEL
// and UVW cvalues are arrays while the others are scalars.
//
// This engine is meant for a MeasurementSet, but can be used for any table
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
// no ANTENNA2 columns, columns XX2 are the same as XX1.
// </synopsis>

// <motivation>
// Factor out common code.
// </motivation>

// <todo asof="$DATE:$">
//  <li> Take care of the feeds and their offsets.
//  <li> Have a conversion engine per field/antenna/feed?
// </todo>

class MSCalEngine
{
public:
  // Default constructor.
  MSCalEngine();

  // Destructor.
  ~MSCalEngine();

  // Use the given table (MS or CalTable) in the engine.
  void setTable (const Table&);

  // Get the hourangle for the given row.
  double getHA (Int antnr, uInt rownr);

  // Get the parallatic angle for the given row.
  double getPA (Int antnr, uInt rownr);

  // Get the local sidereal time for the given row.
  double getLAST (Int antnr, uInt rownr);

  // Get the azimuth/elevation for the given row.
  void getAzEl (Int antnr, uInt rownr, Array<Double>&);

  // Get the UVW in J2000 for the given row.
  void getUVWJ2000 (uInt rownr, Array<Double>&);

private:
  // Copy constructor cannot be used.
  MSCalEngine (const MSCalEngine& that);

  // Assignment cannot be used.
  MSCalEngine& operator= (const MSCalEngine& that);
  
  // Set the data in the measure converter machines.
  // It returns the mount of the antenna.
  Int setData (Int antnr, uInt rownr);

  // Initialize the column objects, etc.
  void init();

  // Fill the CalDesc info for calibration tables.
  void fillCalDesc();

  // Fill or update the antenna positions from the ANTENNA subtable at
  // row calDescId. It is stored in the calInx-th entry of itsAntPos/itsMount.
  void fillAntPos (Int calDescId, Int calInx);

  // Fill or update the field directions from the FIELD subtable at
  // row calDescId. It is stored in the calInx-th entry of itsFieldDir.
  void fillFieldDir (Int calDescId, Int calInx);

  // Get a calibration MS subtable for the given id.
  Table getSubTable (Int calDescId, const String& subTabName,
                     Bool mustExist=True);

  //# Declare member variables.
  Table                       itsTable;        //# MS or CalTable to use
  Int                         itsLastCalInx;   //# id of CAL_DESC last used
  Int                         itsLastFieldId;  //# id of the field last used
  Int                         itsLastAntId;    //# -1 is array position used
  Double                      itsLastTime;
  ROScalarColumn<Int>         itsAntCol[2];    //# ANTENNA1 and ANTENNA2
  ROScalarColumn<Int>         itsFeedCol[2];   //# FEED1 and FEED2
  ROScalarColumn<Int>         itsFieldCol;     //# FIELD_ID
  ROScalarColumn<Double>      itsTimeCol;      //# TIME
  ROScalarMeasColumn<MEpoch>  itsTimeMeasCol;  //# TIME as Measure
  ROScalarColumn<Int>         itsCalCol;       //# CAL_DESC_ID
  map<string,int>             itsCalMap;       //# map of MS name to index
  vector<Int>                 itsCalIdMap;     //# map of calId to index
  MPosition                   itsArrayPos;
  vector<vector<MPosition> >  itsAntPos;       //# ITRF antenna positions
  vector<vector<Int> >        itsMount;        //# 1=alt-az  0=else
  vector<vector<MDirection> > itsFieldDir;     //# J2000 field directions
  vector<vector<MBaseline> >  itsAntMB;        //# J2000 MBaseline per antenna
  vector<vector<Vector<double> > > itsAntUvw;  //# J2000 UVW per antenna
  vector<Block<bool> >        itsUvwFilled;    //# is UVW filled for antenna i?
  MDirection::Convert         itsRADecToAzEl;  //# converter ra/dec to az/el
  MDirection::Convert         itsPoleToAzEl;   //# converter pole to az/el
  MDirection::Convert         itsRADecToHADec; //# converter ra/dec to ha/dec
  MEpoch::Convert             itsUTCToLAST;    //# converter UTC to LAST
  MBaseline::Convert          itsBLToJ2000;    //# convert ITRF to J2000
  MeasFrame                   itsFrame;        //# frame used by the converters
};


} //# end namespace

#endif
