//# MSLister.cc:  Class for listing records from a MeasurementSet
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
#include <aips/Quanta/MVTime.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Measures/Stokes.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <trial/MeasurementSets/MSLister.h>
#include <trial/MeasurementSets/MSSummary.h>
#include <trial/MeasurementSets/MSRange.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/MaskedArray.h>
#include <aips/Arrays/MaskArrMath.h>
#include <iomanip.h>
#include <iostream.h>


//
// Null constructor merely sets private formatting string
//
MSLister::MSLister ()
  : dashline_p(replicate("-",80))
{}


//
// Constructor assigns pointer (if MS goes out of scope you will get rubbish),
// initialises the output, sets string to format output, and initialises the
// listing.
//
MSLister::MSLister (MeasurementSet& ms, LogIO& os)
  : pMS(&ms),
    os_p(os),
    dashline_p(replicate("-",80))
{
  // Move these into initList()?
  // default precision (in case setPrecision is not called)
  precTime_p = 7;    // hh:mm:ss.s  (0.1 s)
  precUVDist_p = 0;  // 1 wavelength
  precAmpl_p = 3;    // mJy
  precPhase_p = 1;   // 0.1 deg
  precWeight_p = 0;  // unit weight
  // initializations
  wFld_p = wSpW_p = wChn_p = 0;

  // initialize list params
  initList();
}


//
// Assignment operator
//
MSLister& MSLister::operator=(MSLister& other)
{
  if (this==&other) return *this;
  pMS = other.pMS;
  return *this;
}


//
// Destructor does nothing
//
MSLister::~MSLister()
{}


//
// Reinitialise output stream.  Do this before setMS() if doing both.
//
Bool MSLister::setNewOS (LogIO& os)
{
  os_p = os;
  return True;
}


//
// Reassign MS pointer and reinitialise MSLister object.  Do this after
// setNewOS() if doing both.
//
Bool MSLister::setMS (MeasurementSet& ms)
{
  pMS = &ms;
  initList();
  return True;
}


//
// initList() does things that need to be done once per MS: initialises
// the pagination/formatting, lists some header information, initialises
// the MSSelector object, and gets all the attribute ranges up front.
//
void MSLister::initList()
{
  // Establish the formatting
  setPage();		// page size
  setFormat();		// the # of decimal places for data

  // List some header information so the user knows what to select on
  listHeader();

  // Initialise the MSSelector object.  By default, initSelection() takes all
  // polarisations and the first spectral channel.
  mss_p.setMS(*pMS);

  cout << endl;
  cout << "Before mss_p.initSelection()" << endl;
  mss_p.initSelection();

  cout << endl;
  cout << "After mss_p.initSelection()" << endl;

  // Get the ranges (into ranges_p) of the following usefully
  // selectable attributes.  range_p can be changed later to
  // refine selection.
  // **SPW**
  items_p.resize(6,False);
  items_p(0)="time";			// the range of times
  items_p(1)="antenna1";		// the list of antenna1 id values
  items_p(2)="antenna2";		// the list of antenna2 id values
  items_p(3)="uvdist";			// the range of the UV-distance (m)
  //  items_p(4)="spectral_window_id";	// the list of spwin id values
  items_p(4)="data_desc_id";	// the list of spwin id values
  items_p(5)="field_id";		// the list of field id values
  getRanges();

  cout << "After getRanges." << endl << endl;

  // Set up for selection on channel or polarisation
  ROMSSpWindowColumns msSpWinC(pMS->spectralWindow());
  ROMSPolarizationColumns msPolC(pMS->polarization());
  nchan_p = msSpWinC.numChan()(0);
  npols_p = msPolC.corrType()(0).nelements();
  pols_p.resize(npols_p,False);
  for (uInt i=0; i<npols_p; i++) {
    pols_p(i) = Stokes::name(Stokes::type
			     (msPolC.corrType()(0)(IPosition(1,i))));
  }

  // Store spwin ref freqs for later use:
  //   (should get channel freqs for multi-channel MS's)
  freqs_p.resize(4,False);
  freqs_p=msSpWinC.refFrequency().getColumn();

  // Signal completion of initList
  os_p << "Lister initialised for this MS" << LogIO::POST;
}


void MSLister::setPage (const uInt width, const uInt height)
{
  // Set up pagination for output.  Default is for landscape printing (w=120)
  // with font size 10 (h=60).
  pageWidth_p = width;
  pageHeight_p = height;
}


void MSLister::setFormat (const uInt ndec)
{
  // Set up data display precision
  nDecimal_p = ndec;
}

void MSLister::setPrecision ( const Int precTime, const Int precUVDist,
			      const Int precAmpl, const Int precPhase, 
			      const Int precWeight )
{
  // Set private precision vars on basis of user input:
  precTime_p   = precTime+6;  // internally, time precision includes hhmmss.
  precUVDist_p = precUVDist;
  precAmpl_p   = precAmpl;
  precPhase_p  = precPhase;
  precWeight_p = precWeight;
}



void MSLister::listHeader()
{
  // Construct the MSSummary object and output the header info
  MSSummary header(*pMS);
  header.listTitle (os_p);
  header.listWhat (os_p,False);
  header.listSpectralWindow (os_p,True);
  header.listPolarization (os_p,True);
  header.listAntenna (os_p,True);
  os_p << dashline_p << endl;
  os_p.post();
}


//
// Get the ranges of a fixed set of MS key attributes
//
void MSLister::getRanges()
{
  // Get the range of values for the items specified in items_p, into
  // a GlishRecord.
  // Assumes items_p has already been set (should add check on this)
  // **SPW**
  
  cout << endl << "mss_p.dataDescId() = " <<  mss_p.dataDescId() << endl;

  MSRange msr(mss_p.selectedTable(),mss_p.dataDescId());		
  ranges_p = msr.range(items_p);		

  // Print out the retrieved ranges:
  cout << "Printing out the GlishRecord ranges_p:" << endl;
  cout << ranges_p << endl;
}


//
// Do the actual work: list ms records to the logger.  Also provided are
// versions of list() that accept time limits in either String or Double
// format.
//
// Version to list only data for times in given range, converting
// input strings to Double times
void MSLister::list (const String stimeStart, const String stimeStop)
{
  // Convert the time strings
  Quantum<Double> qtime;
  MVTime::read(qtime, stimeStart);	Double dtimeStart= qtime.getValue("s");
  MVTime::read(qtime, stimeStop);	Double dtimeStop = qtime.getValue("s");

  // Call the next function to restrict the time range selection and list data
  list(dtimeStart,dtimeStop);
}

// Version to list only data for times in given range
void MSLister::list (Double& timeStart, Double& timeStop)
{
  // Check on time selection:
  if (!selectTime (timeStart, timeStop)) return;

  // List the data
  list();
}

// Version to list data for all (possibly previously selected) times
void MSLister::list()
{
  // NOTE: several placeholders for future functionality.  Implementing
  // some of these may require some rearrangement of the interface...
  // (eg put the various select()s into more versions of list()...)
  // selectOther(?);
  // Alter the selections
  // ranges_p = ...;
  // Select on the ms with the new ranges
  // if (!mss_p.select(ranges_p)) return boo-boo1;


  cout << endl << "Before applying selection." << endl;
  // Actually apply the ranges_p selection:
  mss_p.select(ranges_p);
  cout << endl << "After applying selection." << endl;


  // Spectral and polarisation selection are handled separately from
  // the GlishRecord ranges_p
  // uInt startChan=0, widthChan=1, incrChan=1;
  // if (!mss_p.selectChannel(nchan_p,startChan,widthChan,incrChan) return bb2;
  // if (!mss_p.selectPolarization(pols_p)) return boo-boo3;
  
  // Some pol selecting for testing output format:
  //  pols_p.resize(2);
  // pols_p(0)="RL";
  //pols_p(1)="LR";
  // npols_p=2;
  //  mss_p.selectPolarization(pols_p);


  // Then make the listing
  listData();
}


Bool MSLister::selectTime (Double& inputStartTime, Double& inputStopTime)
{
  // First get the range of available times in the MS.  The private GlishRecord
  // ranges_p has previously been set in getRanges(), while dataRecords_p is a
  // private Record member.
  Vector <Double> msTimeLimits(2);
  ranges_p.toRecord(dataRecords_p);
  dataRecords_p.get("time", msTimeLimits);

  // Maybe you think there ain't no Sanity Claus, but....
  Bool ok = ToBool(inputStartTime < inputStopTime &&
		   inputStartTime < msTimeLimits(1) &&
		   inputStopTime > msTimeLimits(0));
  if (!ok) {
    os_p << LogIO::SEVERE << "Given times select no data: try again" << endl
	 << "(format needs to be yyyy/mm/dd[/time])" << endl << LogIO::POST;
    return ok;
  }

  // Modify the range used in the select() if inputs are more restrictive
  if (inputStartTime> msTimeLimits(0)) {msTimeLimits(0) = inputStartTime;}
  if (inputStopTime < msTimeLimits(1)) {msTimeLimits(1) = inputStopTime;}
  dataRecords_p.define("time", msTimeLimits);
  ranges_p.fromRecord(dataRecords_p);

  // Signal good time selection
  os_p  << "Timerange selection made:     "
	<< MVTime(msTimeLimits(0)/86400).ymd() << "/"
	<< MVTime(msTimeLimits(0)/86400).string() << "     to  "
	<< MVTime(msTimeLimits(1)/86400).ymd() << "/"
	<< MVTime(msTimeLimits(1)/86400).string() << endl << endl;
  os_p.post();

  return ok;
}


void MSLister::listData()
{
  // Now get the data for the listing.  
  // Currently we are extracting the data through a GlishRecord 
  // to a Record to Arrays using MSSelector::getData, which requires
  // a list of items, a reasonable default set of which is defined
  // here as a Vector<String> passable to MSSelector::getData 
  // (eventually, user will have some choice here, e.g., to get
  // real/imag instead of amp/ph, etc.).
  // Eventually, it would be easier if there were public RO access to the
  // MSSelector::selms_p member...
  // **SPW**

  items_p.resize(8,True);
  items_p(0)="time";
  items_p(1)="antenna1";		
  items_p(2)="antenna2";		
  items_p(3)="uvdist";			
  //  items_p(4)="spectral_window_id";	
  items_p(4)="data_desc_id";	
  items_p(5)="field_id";		
  items_p(6)="amplitude";
  items_p(7)="phase";		
  //items_p(8)="weight";		

  // Get ranges of selected data to ranges_p for use in field-width/precision setting
  cout << endl << "Before getRanges" << endl;
  getRanges();
  cout << endl << "Before getData" << endl;


  // Now extract the selected data Record.  Note that mss_p is the *selected*
  // data, and mss_p.getData() is an implicit GlishRecord object
  mss_p.getData(items_p,False).toRecord(dataRecords_p);

  cout << endl << "After getData" << endl;

  // Construct arrays for the Record items.  The V-float declaration
  // appears to be necessary (instead of V-double) despite the get()
  // function's claim to do type promotion.
  Vector <Double>	time,uvdist;
  Vector <Int>		ant1,ant2,spwinid,fieldid;
  Array <Float>		ampl,phase;
  //  Vector <Float>	weight;

  cout << endl << "After arrays instantiation" << endl;

  // Extract the values from the Record into the Arrays
  dataRecords_p.get("time", time);  // in sec
  cout << "got Time" << endl;
  dataRecords_p.get("antenna1", ant1);
  cout << "got Ant1" << endl;
  dataRecords_p.get("antenna2", ant2);
  cout << "got Ant2" << endl;
  dataRecords_p.get("uvdist", uvdist);  // in meters
  cout << "got UVD" << endl;
  //  dataRecords_p.get("spectral_window_id", spwinid);
  dataRecords_p.get("data_desc_id", spwinid);
  cout << "got dataDesc" << endl;
  dataRecords_p.get("field_id", fieldid);
  cout << "got fldid" << endl;
  dataRecords_p.get("amplitude", ampl);
  cout << "got ampl" << endl;
  dataRecords_p.get("phase", phase);  // in rad
  cout << "got phase" << endl;
  //  dataRecords_p.get("weight", weight);
  //  cout << "got weight" << endl;

  // Number of rows that will be listed
  Int nrows = time.nelements();
  Vector <Float>	weight;
  weight.resize(nrows,True);
  
  // Convert units of some params:
  time = time/C::day;        // time now in days
  phase = phase/C::degree;   // phase now in degrees
  // Change uvdist to wavelengths as function of spwinid:
  for (Int row=0;row<nrows;row++) {
    uvdist(row) = uvdist(row)/(C::c/freqs_p(spwinid(row)));  // uvdist in wavelengths
  }    


  // Add or adjust ranges_p to non-zero absolutes for non-index and/or 
  // converted values (so we can use ranges_p for field width and 
  // precision setting):

  //     TBD....


  // Make flags for showing index columns.
  doFld_p = (ranges_p.get("field_id").nelements() > 1);
  doSpW_p = (ranges_p.get("spectral_window_id").nelements() > 1);
  doChn_p = (nchan_p > 1);


  cout << "Fld id : " << ranges_p.get("field_id") << endl;
  cout << "SpW id : " << ranges_p.get("spectral_window_id") << endl << endl;


  // From this point on, don't change scaling in list arrays, since the
  // field sizes are determined directly from the data.

  cout << "Minumum uvdist = " << min(uvdist) << endl;
  cout << "Maximum uvdist = " << max(uvdist) << endl;
  cout << "Minumum non-zero amp = " << min(ampl(ampl>0.0f)) << endl;
  cout << "Maximum amp = " << max(ampl) << endl;
  cout << "Minumum non-zero abs phase = " << min(abs(phase(phase!=0.0f))) << endl;
  cout << "Maximum phase = " << max(abs(phase)) << endl;
  //  cout << "Minumum weight = " << min(weight(weight>0.0f)) << endl;
  // cout << "Maximum weight = " << max(weight) << endl;


  // Set order of magnitude and precision (for field width setting):
  // If prec*_p < 0, then enforce >=0 (detect minimum decimal places to show is NYI)
  // If prec*_p > 0, then increment o*_p to provide space for decimal

  oTime_p = 2; // this is space for 2 :'s in time  
  if ( precTime_p < 0 ) precTime_p = 7; // hh:mm:ss.s
  if ( precTime_p > 0 ) oTime_p++; // add space for decimal


  oUVDist_p = (uInt)rint(log10(max(uvdist))+0.5); // order
  if ( precUVDist_p < 0 ) precUVDist_p = 0;
  if ( precUVDist_p > 0 ) oUVDist_p++;  // add space for decimal

  oAmpl_p = (uInt)rint(log10(max(ampl))+0.5); 
  if ( precAmpl_p < 0 ) precAmpl_p = 3;  // mJy
  if ( precAmpl_p > 0 ) oAmpl_p++;  // add space for decimal


  oPhase_p = 3;  // 100s of degs 
  if ( precPhase_p < 0 ) precPhase_p = 0; 
  if ( precPhase_p > 0 ) oPhase_p++; // add space for decimal
  oPhase_p++; // add space for sign

  oWeight_p = (uInt)rint(log10(max(weight))+0.5);  // order
  if ( precWeight_p < 0 ) precWeight_p = 0;
  if ( precWeight_p > 0 ) oWeight_p++;  // add space for decimal

  // Set field widths.
  //   For index columns just use the size of the largest value:
  wAnt_p    = (uInt)rint(log10((float)max(max(ant1),max(ant2)))+0.5);
  if (doFld_p) wFld_p    = (uInt)rint(log10((float)max(fieldid))+0.5);
  if (doSpW_p) wSpW_p    = (uInt)rint(log10((float)max(spwinid))+0.5);
  if (doChn_p) wChn_p    = 3;

  //   The field width for non-index columns is given by the  
  //    sum of the order and precision:
  wTime_p   = oTime_p + precTime_p;
  wUVDist_p = oUVDist_p + precUVDist_p;  // left+dec+right
  wAmpl_p   = oAmpl_p + precAmpl_p;  // left+dec+right
  wPhase_p  = oPhase_p + precPhase_p;  // sign+left+dec+right
  wWeight_p = oWeight_p + precWeight_p; // left+dec+right

  // Enforce minimum field widths,
  // add leading space so columns nicely separated,
  // and accumulate wTotal_p:
  wTotal_p = 0;  // initialize 
  wAnt_p    = max(wAnt_p,   (uInt)2);
  wIntrf_p  = 2*wAnt_p+1;                       wIntrf_p++;  wTotal_p+=wIntrf_p;
  if (doFld_p) { wFld_p = max(wFld_p,(uInt)3);  wFld_p++;    wTotal_p+=wFld_p; }
  if (doSpW_p) { wSpW_p = max(wSpW_p,(uInt)3);  wSpW_p++;    wTotal_p+=wSpW_p;}
  if (doChn_p) { wChn_p = max(wChn_p,(uInt)3);  wChn_p++;    wTotal_p+=wChn_p;}

  wTime_p   = max(wTime_p,  (uInt)12);
  wUVDist_p = max(wUVDist_p,(uInt)6);           wUVDist_p++; wTotal_p+=wUVDist_p;
  wAmpl_p   = max(wAmpl_p,  (uInt)4);           wAmpl_p++; 
  wPhase_p  = max(wPhase_p, (uInt)4); 
  wWeight_p = max(wWeight_p,(uInt)3);           wWeight_p++;

  wVis_p = wAmpl_p+wPhase_p+wWeight_p;
  wTotal_p+=wTime_p+npols_p*wVis_p+1;

  // Make column-ated header rule according to total and field widths
  String hSeparator(replicate("-",wTotal_p));
  uInt colPos=0;
  colPos+=wTime_p;   hSeparator[colPos]='|';
  colPos+=wIntrf_p;  hSeparator[colPos]='|';
  colPos+=wUVDist_p; hSeparator[colPos]='|';
  if (doFld_p) {colPos+=wFld_p;hSeparator[colPos]='|';}
  if (doSpW_p) {colPos+=wSpW_p;hSeparator[colPos]='|';}
  if (doChn_p) {colPos+=wChn_p;hSeparator[colPos]='|';}
  colPos++;
  for (uInt ipol=0; ipol<npols_p-1; ipol++) {
    colPos+=wVis_p;
    hSeparator[colPos]='|';
  }

  // Finally print the records out, one per line.

  os_p << "Listing " << time.nelements()
       << " data records satisfying selection criteria, " << endl
       << "for each of " << npols_p << " polarisation(s) and " << nchan_p
       << " spectral channel(s)." << endl << endl << dashline_p << endl;

  //  nrows = 90;  // override to make short list for testing



  for (Int row=0; row<nrows; row++) {
    date_p = MVTime(time(row)).string(MVTime::YMD_ONLY);
    // If page lenghth reached, or new day, then paginate
    if ((row/pageHeight_p)*pageHeight_p == row ||
        date_p != lastdate_p) {
      if (row != 0) os_p << hSeparator << endl;
      listColumnHeader();
      os_p << hSeparator << endl;
    }
    lastdate_p = date_p;

    for (uInt ichan=0; ichan<nchan_p; ichan++) {

      os_p.output().setf(ios::fixed, ios::floatfield);
      os_p.output().setf(ios::right, ios::adjustfield);
      
      os_p.output().width(wTime_p);	os_p << MVTime(time(row)).string(MVTime::TIME,precTime_p);
      os_p.output().width(wAnt_p+1);	os_p << ant1(row)+1;
      os_p.output().width(1);	        os_p << "-";
      os_p.output().width(wAnt_p);	os_p << ant2(row)+1;
      os_p.output().precision(precUVDist_p);
      os_p.output().width(wUVDist_p);os_p << uvdist(row);
      if (doFld_p) {os_p.output().width(wFld_p);	os_p << fieldid(row)+1;}
      if (doSpW_p) {os_p.output().width(wSpW_p);	os_p << spwinid(row)+1;}
      if (doChn_p) {os_p.output().width(wChn_p);	os_p << ichan+1;}
      os_p << ":";
      for (uInt ipol=0; ipol<npols_p; ipol++) {
	os_p.output().precision(precAmpl_p);
	os_p.output().width(wAmpl_p);	 os_p << ampl(IPosition(3,ipol,ichan,row));
	os_p.output().precision(precPhase_p);
	os_p.output().width(wPhase_p); os_p << phase(IPosition(3,ipol,ichan,row));
	os_p.output().precision(precWeight_p);
	os_p.output().width(wWeight_p); os_p << weight(row);
      }
      os_p << endl;
    }
  }
  //  os_p << dashline_p << endl;
  os_p << hSeparator << endl;

  // Post it
  os_p.post();
}

void MSLister::listColumnHeader() {
  // Write the column headers
  os_p << endl << endl;

  // First line of column header
  os_p.output().setf(ios::left, ios::adjustfield);
  os_p.output().width(wTime_p);             os_p << "-Date/Time-";
  os_p.output().setf(ios::right, ios::adjustfield);
  os_p.output().width(wIntrf_p);            os_p << " ";
  os_p.output().width(wUVDist_p);           os_p << " ";
  if (wFld_p) {os_p.output().width(wFld_p); os_p << " ";}
  if (wSpW_p) {os_p.output().width(wSpW_p); os_p << " ";}
  if (wChn_p) {os_p.output().width(wChn_p); os_p << " ";}
  os_p << " ";
  os_p.output().setf(ios::left, ios::adjustfield);
  for (uInt ipol=0; ipol<npols_p; ipol++) {
    os_p.output().width(wVis_p); os_p << " "+pols_p(ipol)+":";
  }
  os_p << endl;
  
  // Second line of column header
  os_p.output().setf(ios::left, ios::adjustfield);
  os_p.output().width(wTime_p);             os_p << date_p+"/";
  os_p.output().setf(ios::right, ios::adjustfield);
  os_p.output().width(wIntrf_p);            os_p << "Intrf";
  os_p.output().width(wUVDist_p);           os_p << "UVDist";
  if (wFld_p) {os_p.output().width(wFld_p); os_p << "Fld";}
  if (wSpW_p) {os_p.output().width(wSpW_p); os_p << "SpW";}
  if (wChn_p) {os_p.output().width(wChn_p); os_p << "Chn";}
  os_p << " ";
  for (uInt ipol=0; ipol<npols_p; ipol++) {
    os_p.output().width(wAmpl_p);	os_p << "Amp";
    os_p.output().width(wPhase_p);	os_p << "Phs";
    os_p.output().width(wWeight_p);	os_p << "Wt";
  }
  os_p << endl;
  // << dashline_p << endl;
}

//
// Clear all the formatting flags
//
void MSLister::clearFlags()
{
  os_p.output().unsetf(ios::left);
  os_p.output().unsetf(ios::right);
  os_p.output().unsetf(ios::internal);

  os_p.output().unsetf(ios::dec);
  os_p.output().unsetf(ios::oct);
  os_p.output().unsetf(ios::hex);

  os_p.output().unsetf(ios::showbase | ios::showpos | ios::uppercase | ios::showpoint);

  os_p.output().unsetf(ios::scientific);
  os_p.output().unsetf(ios::fixed);

}
