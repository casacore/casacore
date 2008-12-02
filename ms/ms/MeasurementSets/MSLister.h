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
#ifndef MS_MSLISTER_H
#define MS_MSLISTER_H


#include <casa/aips.h>
#include <casa/Logging/LogIO.h>
//#include <casa/Logging/LogSink.h>
#include <casa/BasicSL/String.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/Record.h>
#include <ms/MeasurementSets/MSSelector.h>
namespace casa { //# NAMESPACE CASA - BEGIN

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
//	// List all the data, with default output options
//	myList.list();
//    // List only selected data, with specified output options
//    datacolumn = 'corrected'; spw = '3:5~10'; timerange = '<13:34:25.1';
//    scan = '5'; pagerows = 10; listfile = 'myList.list.out';
//    // ... define any other parameters, then call function ... 
//    myList.list(options, datacolumn, field, spw, antenna, timerange,
//                correlation, scan, feed, array, uvrange, average, 
//                showflags, msselect, pagerows, listfile);
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
// <todo asof="2008/02/08">
//   <li> Several of the input parameters to <src>MSLister::list</src> are not
//    funcational presently.
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
  // <todo> os is currently not used as the primary steam for log messages.
  //   This should be corrected, or os removed completely from the class.
  // </todo>
  MSLister (const MeasurementSet& ms, LogIO& os);

  // Copy constructor, this will initialise the MSLister's MS with other's MS
  MSLister (MSLister& other);

  // Assignment, this will initialise the MSLister's MS with other's MS
  MSLister& operator=(MSLister& other);

  // Destructor
  ~MSLister();

  // Change or set the OS this MSLister uses.  Do this before setMS()
  // if doing both.  This method avoids having to reconstruct the MSLister
  // object if you change your mind about the output destination.
  // <todo> os is currently not used as the primary steam for log messages.
  //   This should be corrected, or os removed completely from the class.
  // </todo>
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

  // List the visibilities, with optional data selection and output 
  // specification.
  void list (const String options="",
             const String datacolumn="",
             const String field="", 
             const String spw="", 
             const String antenna="", 
             const String timerange="", 
             const String correlation="",
             const String scan="",
             const String feed="",
             const String array="",
             const String uvrange="", 
             const String average="",
             const bool   showflags=False,
             const String msSelect="",
             const long   pagerows=50,
             const String listfile="");

  // Set uv-data selection via MSSelection
  void selectvis(const String& timerange="",
                 const String& spw="",
                 const String& scan="",
                 const String& field="",
                 const String& baseline="",
                 const String& uvrange="",
                 const String& chanmode="none",
                 const Int& nchan=1,
                 const Int& start=0,
                 const Int& step=1,
                 const MRadialVelocity& mStart=MRadialVelocity(),
                 const MRadialVelocity& mStep=MRadialVelocity(),
                 const String& correlation="",
                 const String& array="",
                 const String& msSelect="");

private:

  // Initialise the listing.  initList() does things that need to be done
  // once per MS: declares and initialises the private MSSelector object,
  // and gets all the attribute ranges up front.
  void initList();

  // A preamble of abbreviated MSSummary information.
  void listHeader();

  // Get the ranges of a fixed set of MS key attributes.
  void getRanges(const MeasurementSet &ms);

  // Most of the heavy lifting is in here.  Get the data records and list
  // them.
  void listData(const int pageRows=50, const String listfile="");

  // Column header line for pagination of output.
  void listColumnHeader();

  // Setup class polarization information for specified MS.
  // pols_p holds the polarization names contained in the MS
  // in the same order that the polarization data are listed in the 
  // main table.
  void polarizationSetup(MeasurementSet *pMS);
  
  // Parse the correlation parameter value; fill indexPols_p to output
  // selected polarizations.  If correlation is empty, all polarizations
  // are selected.
  void polarizationParse(String correlation);
  
  // Calculate column width for a Vector<String>
  Int columnWidth(const Vector<String> antNames);

  // Pointer to the MS
  MeasurementSet* pMS_p;
  MeasurementSet* pMSSel_p;

  // Output stream
  LogIO logStream_p;

  // A formatting string for convenience
  const String dashline_p;

  // The MSSelector object used in list() etc.
  MSSelector mss_p;

  // List of channels
  Matrix<Int> chanList_p;
  // True if listing multiple channels.
  Bool multiChan_p;

  // Pol counters
  uInt npols_p;

  // SpW/Pol info from subtables
  Vector<String> pols_p;
  Vector<Double> freqs_p;

  // SpWId map from DDIs:
  Vector<Int> spwins_p;
  // True if listing multiple spws
  Bool multiSpw_p;

  // Polarization indexing variables; for polarization (correlation) selection.
  Vector<Int> indexPols_p;
  uInt nIndexPols_p;

  // Field width variables
  uInt wTime_p, wAnt1_p, wAnt2_p, wIntrf_p, wUVDist_p;
  uInt wFld_p, wSpW_p, wChn_p;
  uInt wAmpl_p, wPhase_p, wWeight_p, wVis_p, wFlag_p;
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

  // Data selections
  // data --> "amplitude", "phase"
  // corrected --> "corrected_amplitude", "corrected_phase"
  // model --> "model_amplitude", "model_phase"
  // residual --> "residual_amplitude", "residual_phase"
  Vector <String> dataColSel;

  // The Record object containing the MSSelector ranges
  Record ranges_p;

  // The conversion of the above to a regular Record object
  Record dataRecords_p;

  // Clear the formatting flags
  void clearFlags();
};


} //# NAMESPACE CASA - END

#endif
