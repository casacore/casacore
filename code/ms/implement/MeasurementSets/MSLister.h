//# MSLister.h: Helper class for applications listing records from an MS
//# Copyright (C) 1998,1999,2000
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
//#
#if !defined(TRIAL_MSLISTER_H)
#define TRIAL_MSLISTER_H


#include <aips/aips.h>
#include <aips/Logging/LogIO.h>
#include <aips/Utilities/String.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Record.h>
#include <aips/Glish/GlishRecord.h>
#include <trial/MeasurementSets/MSSelector.h>
class MeasurementSet;

// <summary> List visibility records from a Measurement Set </summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> <linkto class=MSSelector>MSSelector</linkto>
//   <li> <linkto class=MSSummary>MSSummary</linkto> 
// </prerequisite>
//
// <etymology>
// The name comes from being a Lister for a MS.
// </etymology>
//
// <synopsis>
// MSs containing (u,v) data consist of amplitudes and phases for each
// baseline and sample time, typically sorted in TB order.  These
// visibilities sometimes need to be examined one record at a time in
// a text-based format, giving the user access to their raw data.
// This class provides that access in a choice of several formats.
// </synopsis>
//
// <example>
// <srcBlock>
//	// Define an MS
//	MeasurementSet myMS(fileName);
//	// Define an output stream
//	LogIO myLog;
//	// Construct the Lister object
//	MSLister myList(myMS,myLog);
//	// List all data
//	myList.list();
//
//	// Send the next output to a new location
//	LogIO newLog;
//	setNewOS(newLog);
//	// List only part of the data (the list() method accepts either
//	// Double or String time inputs)
//	Double timeStart=4e9, timeStop=4.0001e9;
//	// ...or...
//	String timeStart=yyyy/mm/dd[/hh[:mm[:ss]]];
//      String timeStop=yyyy/mm/dd[/hh[:mm[:ss]]];
//	myList.list(timeStart,timeStop);
// </srcBlock>
// An <src>MSLister</src> object is constructed from a <src>MS</src>
// object, and then logged to the supplied <src>LogIO</src> object.
// A new <src>LogIO</src> object is defined for a more restricted
// listing.
// </example>
//
// <note role=caution>
// Note that if the <src>MS</src> goes out of scope, this class will
// retrieve rubbish (probably giving runtime errors) as it just
// maintains a pointer to the image.
// </note>
//
// <motivation>
// The viewing of the raw data is a basic capability that is
// commonly required.
// </motivation>
//
// <todo asof="1998/12/16">
//   <li> The string->double conversion in list() 
//	needs to default to prefix the correct "yyyy/mm/dd/" string if
//	not provided, or accept it if it is.
//   <li> The (pointer to the) MS is declared and used as non-const throughout
//	MSLister, because MSSelector requires it.  MSSelector should be
//	changed to require a const MS since it claims not to change the MS
//	anyway.  Then the pointer/MS should be made const here too.
//   <li> Add more sanity checks.
//   <li> Actually do something with the nDecimal_p number.
//   <li> There are more formatting options planned.
// </todo>
 

class MSLister
{
public:
  // Null constructor
  MSLister();

  // Construct from a MeasurementSet (set pointer), set formatting string,
  // and initialise listing with os.
  MSLister (MeasurementSet& ms, LogIO& os);

  // Copy constructor, this will initialise the MSLister's MS with other's MS
  MSLister (MSLister& other);

  // Assignment, this will initialise the MSLister's MS with other's MS
  MSLister& operator=(MSLister& other);

  // Destructor
  ~MSLister();

  // Change or set the OS this MSLister uses.  Do this before setMS()
  // if doing both.  This method avoids having to reconstruct the MSLister
  // object if you change your mind about the output destination.
  Bool setNewOS (LogIO& os);

  // Change or set the MS this MSLister refers to, and reinitialise the
  // MSLister object.  Do this after setNewOS() if doing both.
  Bool setMS (MeasurementSet& ms);

  // Page size for various formats, output devices (default for landscape
  // printing).
  void setPage (const uInt width=120, const uInt height=20);

  // Format for output, ie data display precision.
  void setFormat (const uInt ndec=2);

  // User choices for list precision (sensible defaults):
  //   (time precision for user interface is fraction of sec)
  void setPrecision ( const Int precTime=1, const Int precUVDist=0,
		      const Int precAmpl=3, const int precPhase=1,
		      const Int precWeight=0 );

  // Overall listing (3 versions). Do the actual work: list MS records to
  // the logger.  Also provided are versions of list() that accept time
  // limits in either String or Double format.
  //
  // First version to list only data for times in given range, converting
  // input Strings to Double times.
  void list (const String timeStart, const String timeStop);

  // Second version to list only data for times in given range.
  void list (Double& timeStart, Double& timeStop);

  // Third version to list data for all (possibly previously selected) times.
  void list();

  // Select only within the given time range, with sanity checks.
  Bool selectTime (Double& timeStart, Double& timeStop);

private:
  // Initialise the listing.  initList() does things that need to be done
  // once per MS: initialises the pagination/formatting, lists some header
  // information, declares and initialises the private MSSelector object,
  // and gets all the attribute ranges up front.
  void initList();

  // A preamble of abbreviated MSSummary information.
  void listHeader();

  // Get the ranges of a fixed set of MS key attributes.
  void getRanges();

  // Most of the heavy lifting is in here.  Get the data records and list
  // them.
  void listData();

  // Column header line for pagination of output.
  void listColumnHeader();

  // Pointer to the MS
  MeasurementSet* pMS;

  // Output stream
  LogIO os_p;

  // A formatting string for convenience
  const String dashline_p;

  // The MSSelector object used in list() etc.
  MSSelector mss_p;

  // Channel/Pol counters
  uInt nchan_p, npols_p;

  // SpW/Pol info from subtables
  Vector<String> pols_p;
  Vector<Double> freqs_p;


  // Field width variables
  uInt wTime_p, wAnt_p, wIntrf_p, wUVDist_p;
  uInt wFld_p, wSpW_p, wChn_p;
  uInt wAmpl_p, wPhase_p, wWeight_p, wVis_p;
  uInt wTotal_p;

  // Order of magnitude control (digits to left of decimal, including sign)
  uInt oTime_p, oUVDist_p;
  uInt oAmpl_p, oPhase_p;
  uInt oWeight_p;

  // Precision control (digits to right of decimal point)
  //   (precTime_p includes hhmmss, so 7 yields hh:mm:ss.s)
  Int precTime_p, precUVDist_p;
  Int precAmpl_p, precPhase_p;
  Int precWeight_p;
  
  // Page params
  Int pageWidth_p, pageHeight_p, nDecimal_p;
  String date_p, lastdate_p;

  // for assigning desired columns from the ms
  Vector <String> items_p;

  // Bools for column showing
  Bool doFld_p, doSpW_p, doChn_p;


  // The GlishRecord object containing the MSSelector ranges
  GlishRecord ranges_p;

  // The conversion of the above to a regular Record object
  Record dataRecords_p;

  // Clear the formatting flags
  void clearFlags();
};

#endif
