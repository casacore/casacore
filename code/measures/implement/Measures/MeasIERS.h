//# MeasIERS.h: Interface to IERS tables
//# Copyright (C) 1996,1997,1999,2000
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

#if !defined(AIPS_MEASIERS_H)
#define AIPS_MEASIERS_H

//# Includes
#include <aips/aips.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableRow.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Containers/RecordField.h>

//# Forward Declarations
class String;

// <summary> Interface to IERS tables </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MeasTable>MeasTable</linkto>
// </prerequisite>
//
// <etymology>
// From Measure and IERS
// </etymology>
//
// <synopsis>
// MeasIERS is the interface class to the IERS data.
// It has only static memebers.<br>
// It has a member (<src>getTable()</src>) to open and check IERS
// (and other Measures related Tables) type tables.
// Tables are found using the aipsrc 
// (using <src>measures.<table>.directory</src>)
// mechanism. If not provided they are assumed to reside in standard places
// (i.e. they are looked for in (udir in following normally given by
// program as aips/Measures) '.', './data', '~/aips++/data/udir',
// '$AIPSROOT/data/udir', '~/aips++/code/trial/apps/measures',
// '$AIPSROOT/data/udir' (last two only ad interim)).
// Tables are assumed to have the
// VS_VERSION, VS_DATE, VS_CREATE and VS_TYPE keywords, and be of type IERS,
// else an exception will be thrown.<br>
// The <src>get()</src> method will obtain data from measured and predicted
// Earth Orientation Parameters IERS tables (i.e. the <src>IERSeop97</src> and
// the <src>IERSpredict</src> tables. If not forced, the data is taken from
// the measured table if possible. Only if forced (see below), or if data is
// not (yet) available in measured the predicted values are used. A warning
// message is (once) issued if values are not available at all.
// 
// MeasIERS looks at some <linkto class=Aipsrc>Aipsrc</linkto>
// values to determine actions:
// <ul>
//  <li> measures.measiers.b_notable : Do not use IERS tables to convert measures
//  <li> measures.measiers.b_forcepredict : Use values from prediction tables
//	even if Measured table asked by program.
//  <li> measures.measiers.d_predicttime : Use values from prediction tables if
//	(now - time) less than value given (default 5) (days)
// </ul>
// These values can be set in aipsrc as well as using 
// <linkto class=AipsrcValue>AipsrcValue</linkto> set() methods.<br>
// <logged>
// 	<li> A message is Logged (once) if an IERS table cannot be found
//	<li> A message is logged (once) if a date outside the range in
//	the Tables is asked for. 
// </logged>
// <thrown>
//     <li> AipsError if table opened has wrong format or otherwise corrupted.
// </thrown>
// </synopsis>
//
// <example>
// See the <src>dUTC()</src> method in 
// <linkto class=MeasTable>MeasTable</linkto> for an example of the
// <src>getTable</src> method; and the <src>polarMotion()</src> method for
// an example of <src>get()</src>.
// 
// </example>
//
// <motivation>
// To use the IERS data for time and nutation calculations
// </motivation>
//
// <todo asof="1997/07/02">
// </todo>

class MeasIERS {	

public:
  //# Constants
  static const Double INTV;
  
  //# Enumerations
  // Types of known data
  enum Types {
    // MJD (must be first in list)
    MJD,
    // Polar motion x
    X,
    // Polar motion y
    Y,
    // UT1-UTC
    dUT1,
    // Length of Day
    LOD,
    // dPsi
    dPsi,
    // dEpsilon
    dEps,
    // Polar motion x error
    DX,
    // Polar motion y error
    DY,
    // UT1-UTC error
    DdUT1,
    // Length of Day error
    DLOD,
    // dPsi error
    DdPsi,
    // dEpsilon error
    DdEps,
    // Number of types
    N_Types};
  
  // Types of files
  enum Files {
    // Measured EOP values
    MEASURED,
    // Predicted EOP values
    PREDICTED,
    // # of known types
    N_Files,
    // Default
    DEFAULT = MEASURED };
  
  //# General Member Functions
  // Find and open table tab, using the rc variable, the dir and the name.
  // An rfn list gives the N row field names to be used
  // Returned are an open table, a row record, pointers (rfp) to row data,
  // and the table keywordset (kws).
  // <thrown>
  //  <li> AipsError if missing VS_ keywords, or type is not IERS
  // </thrown>
  static Bool getTable(Table &table, TableRecord &kws, ROTableRow &row,
		       RORecordFieldPtr<Double> rfp[],
		       String &vs, Double &dt,
		       Int N, const String rfn[],
		       const String &name,
		       const String &rc, const String &dir);
  
  // Get the value from an IERS table, interpolated for date(in MJD).
  // The file can be PREDICTED or MEASURED, the type as given in enum.
  static Bool get(Double &returnValue,
		  MeasIERS::Files file, 
		  MeasIERS::Types type,
		  Double date);
  
private:
  
  //# Constructors
  // Default constructor, NOT defined
  MeasIERS();
  
  // Copy assign, NOT defined
  MeasIERS &operator=(const MeasIERS &other);
  
  //# Destructor
  //  Destructor, NOT defined and not declared to stop warning
  // ~MeasIERS();
  
  //# General member functions
  // Initialise tables
  static Bool initMeas(MeasIERS::Files which);
  // Fill Table lines
  static Bool fillMeas(MeasIERS::Files which, Double utf);

  //# Data members
  // Measured data readable
  static Bool measFlag[N_Files];
  // Measured data present
  static Bool measured[N_Files];
  // Current date
  static Double dateNow;
  // Open tables
  static Table t[N_Files];
  // Row descriptions
  static ROTableRow row[N_Files];
  // Field pointers
  static RORecordFieldPtr<Double> rfp[N_Files][MeasIERS::N_Types];
  // First (-1) MJD in list
  static Int mjd0[N_Files];
  // Last MJD in list
  static Int mjdl[N_Files];
  // Last read data (measlow - predictlow - meashigh - predicthigh)
  static Double ldat[2*N_Files][N_Types];
  // Message given
  static Bool msgDone;
  // File names
  static const String tp[N_Files];
  // Check prediction interval
  static uInt predicttime_reg;
  // Use no table
  static uInt notable_reg;
  // Force prediction
  static uInt forcepredict_reg;

};

//# Inline Implementations

#endif
