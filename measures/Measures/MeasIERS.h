//# MeasIERS.h: Interface to IERS tables
//# Copyright (C) 1996,1997,1999,2000,2002,2007
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

#ifndef MEASURES_MEASIERS_H
#define MEASURES_MEASIERS_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRow.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;

// <summary> Interface to IERS tables </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasMath" demos="">
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
// MeasIERS is the interface class to the global IERS data.
// It has only static members.<br>
// It has a member (<src>getTable()</src>) to open and check IERS
// (and other Measures related Tables) type tables.
// Tables are found using the aipsrc 
// (using <src>measures.<table>.directory</src>, or <src>measures.directory</src>)
// mechanism. If not provided they are assumed to reside in standard places
// (i.e. they are looked for in (udir in following normally given by
// program as ephemerides or geodetic) '.', './data', '~/aips++/data/udir',
// '$AIPSROOT/data/udir', '~/aips++/code/trial/apps/measures',
// '$AIPSROOT/data/udir' (last two only ad interim)). They are also looked
// for in <src>data/{ephemerides,geodetic}</src> (root and user aips++).
//
// If an explicit Table object is given the lookup is bypassed, and the Table
// provided is used. The table should still be named.
//
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
// <linkto class=AipsrcValue>AipsrcValue</linkto> set() methods.
// <note>
// 	A message is Logged (once) if an IERS table cannot be found.
//	A message is logged (once) if a date outside the range in
//	the Tables is asked for. 
// </note>
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
  //# Typedefs
  // Define the function pointer to be called to close files
  typedef void (*CLOSEFUN) ();

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
  // Get the value from an IERS table, interpolated for date(in MJD).
  // The file can be PREDICTED or MEASURED, the type as given in enum.
  static Bool get(Double &returnValue,
		  MeasIERS::Files file, 
		  MeasIERS::Types type,
		  Double date);

  // Find and open table tab, using the rc variable, the dir and the name.
  // An rfn list gives the N row field names to be used
  // Returned are an open table, the table keywordset (kws), a row record,
  // pointers (rfp) to row data, the table version (vs), dt, and, directly,
  // whether or not it was successful.
  // Lookup for name is bypassed if the Table address tabin is provided.
  // <thrown>
  //  <li> AipsError if missing VS_ keywords, columns, or they type is not IERS.
  // </thrown>
  static Bool getTable(Table &table, TableRecord &kws, ROTableRow &row,
		       RORecordFieldPtr<Double> rfp[],
		       String &vs, Double &dt,
		       Int N, const String rfn[],
		       const String &name,
		       const String &rc, const String &dir,
		       const Table *tabin = 0);

  // Find and open table tab, using the rc variable, the dir and the name.
  // reqcols gives the names (in order) of the columns which must be present.
  // optcols gives the names of columns which should be added, in order after
  // reqcols, if they are present.
  // Returned are an open table, the table keywordset (kws), a row record,
  // pointers (rfp) to row data, the table version (vs), dt, and, directly,
  // whether or not it was successful.  optcols is set to the optional columns
  // that were found.
  // Lookup for name is bypassed if the Table address tabin is provided.
  // <thrown>
  //  <li> AipsError if missing VS_ keywords, required columns, or the type is not IERS.
  // </thrown>
  static Bool getTable(Table &table, TableRecord &kws, ROTableRow &row,
  		       Vector<RORecordFieldPtr<Double> >& rfp,
  		       String &vs, Double &dt,
  		       const Vector<String>& reqcols,
  		       Vector<String>& optcols,
  		       const String &name,
  		       const String &rc, const String &dir,
  		       const Table *tabin = 0);

  // A helper function for getTable() which is conceivably usable outside it,
  // for finding a table in the same way, but not requiring it to fit the IERS
  // mold.
  // Finds a Table for tab, by looking in tabin, rc, dir, and name.
  // Returns whether or not it was successful.
  static Bool findTab(Table& tab, const Table *tabin, const String &rc,
		      const String &dir, const String &name);

  // Notify that a table has successfully been opened with getTable()
  static void openNote(CLOSEFUN fun);

  // Make sure all static tables are closed that were opened with getTable
  // (like JPL, IERS). This is the preferred way to close the
  // Measures related data tables.
  static void closeTables();

  // Close the set of IERS tables only
  static void closeMeas();

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
  static void initMeas();

  // A helper function for getTable() which is not likely usable outside it.
  // Sets dt and vs (the table version), and checks that 
  //  ks has VS_DATE, VS_VERSION, VS_CREATE, and VS_TYPE,
  //  and that tab's type is IERS in its info.
  // Returns whether or not it was successful.
  static Bool handle_keywords(Double &dt, String &vs,
			      const TableRecord& ks, const Table& tab);

  //# Data members
  static volatile Bool needInit;
  // Current date
  static Double dateNow;
  // Read data (meas - predict)
  static Vector<Double> ldat[N_Files][N_Types];
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
  // Size of close notification list
  static uInt sizeNote;
  // Tables notifying that they should be closed
  static CLOSEFUN *toclose;
  // Number of close notifications
  static uInt nNote;
  // Mutex for thread-safety.
  static Mutex theirMutex;
};

//# Inline Implementations


} //# NAMESPACE CASACORE - END

#endif
