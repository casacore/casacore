//# NewMSLister.cc:  Class for listing records from a MeasurementSet
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
#include <aips/MeasurementSets/NewMSColumns.h>
#include <aips/MeasurementSets/NewMeasurementSet.h>
#include <trial/MeasurementSets/NewMSLister.h>
#include <trial/MeasurementSets/NewMSSummary.h>
#include <trial/MeasurementSets/NewMSRange.h>

#include <iomanip.h>
#include <iostream.h>


//
// Null constructor merely sets private formatting string
//
NewMSLister::NewMSLister ()
  : dashline_p(replicate("-",80))
{}


//
// Constructor assigns pointer (if MS goes out of scope you will get rubbish),
// initialises the output, sets string to format output, and initialises the
// listing.
//
NewMSLister::NewMSLister (NewMeasurementSet& ms, LogIO& os)
  : pMS(&ms),
    os_p(os),
    dashline_p(replicate("-",80))
{
  initList();
}


//
// Assignment operator
//
NewMSLister& NewMSLister::operator=(NewMSLister& other)
{
  if (this==&other) return *this;
  pMS = other.pMS;
  return *this;
}


//
// Destructor does nothing
//
NewMSLister::~NewMSLister()
{}


//
// Reinitialise output stream.  Do this before setNewMS() if doing both.
//
Bool NewMSLister::setNewOS (LogIO& os)
{
  os_p = os;
  return True;
}


//
// Reassign MS pointer and reinitialise NewMSLister object.  Do this after
// setNewOS() if doing both.
//
Bool NewMSLister::setNewMS (NewMeasurementSet& ms)
{
  pMS = &ms;
  initList();
  return True;
}


//
// initList() does things that need to be done once per MS: initialises
// the pagination/formatting, lists some header information, initialises
// the NewMSSelector object, and gets all the attribute ranges up front.
//
void NewMSLister::initList()
{
  // Establish the formatting
  setPage();		// page size
  setFormat();		// the # of decimal places for data

  // List some header information so the user knows what to select on
  listHeader();

  // Initialise the NewMSSelector object.  By default, initSelection() takes all
  // polarisations and the first spectral channel.
  mss_p.setMS(*pMS);
  mss_p.initSelection();

  // Get the ranges of all the usefully selectable attributes
  getRanges();

  // Set up for selection on channel or polarisation
  RONewMSSpWindowColumns msSpWinC(pMS->spectralWindow());
  RONewMSPolarizationColumns msPolC(pMS->polarization());
  nchan_p = msSpWinC.numChan()(0);
  npols_p = msPolC.corrType()(0).nelements();
  pols_p.resize(npols_p,False);
  for (uInt i=0; i<npols_p; i++) {
    pols_p(i) = Stokes::name(Stokes::type
			     (msPolC.corrType()(0)(IPosition(1,i))));
  }

  // Signal completion of initList
  os_p << "Lister initialised for this MS" << LogIO::POST;
}


void NewMSLister::setPage (const uInt width, const uInt height)
{
  // Set up pagination for output.  Default is for landscape printing (w=120)
  // with font size 10 (h=60).
  pageWidth_p = width;
  pageHeight_p = height;
}


void NewMSLister::setFormat (const uInt ndec)
{
  // Set up data display precision
  nDecimal_p = ndec;
}


void NewMSLister::listHeader()
{
  // Construct the NewMSSummary object and output the header info
  NewMSSummary header(*pMS);
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
void NewMSLister::getRanges()
{
  // NewMSSelector only allows selection on some of the MS key attributes
  // (eg, **not** amplitude, despite what documentation says: that's
  // clipping).  We fix those attributes here and get their ranges.
  items_p.resize(6,False);
  items_p(0)="time";			// the range of times
  items_p(1)="antenna1";		// the list of antenna1 id values
  items_p(2)="antenna2";		// the list of antenna2 id values
  items_p(3)="uvdist";			// the range of the UV-distance (m)
  items_p(4)="spectral_window_id";	// the list of spwin id values
  items_p(5)="field_id";		// the list of field id values

  // Get the range of values for the items specified into a GlishRecord.
  NewMSRange msr(mss_p.selectedTable(),mss_p.dataDescId());		
  ranges_p = msr.range(items_p);		
}


//
// Do the actual work: list ms records to the logger.  Also provided are
// versions of list() that accept time limits in either String or Double
// format.
//
// Version to list only data for times in given range, converting
// input strings to Double times
void NewMSLister::list (const String stimeStart, const String stimeStop)
{
  // Convert the time strings
  Quantum<Double> qtime;
  MVTime::read(qtime, stimeStart);	Double dtimeStart= qtime.getValue("s");
  MVTime::read(qtime, stimeStop);	Double dtimeStop = qtime.getValue("s");

  // Call the next function to restrict the time range selection and list data
  list(dtimeStart,dtimeStop);
}

// Version to list only data for times in given range
void NewMSLister::list (Double& timeStart, Double& timeStop)
{
  // Restrict the time range, and actually do the selection
  // (combined with a success check)
  if (!selectTime (timeStart, timeStop)) return;

  // List the data
  list();
}

// Version to list data for all (possibly previously selected) times
void NewMSLister::list()
{
  // NOTE: several placeholders for future functionality.  Implementing
  // some of these may require some rearrangement of the interface...
  // (eg put the various select()s into more versions of list()...)
  // selectOther(?);
  // Alter the selections
  // ranges_p = ...;
  // Select on the ms with the new ranges
  // if (!mss_p.select(ranges_p)) return boo-boo1;

  // Spectral and polarisation selection are handled separately from
  // the GlishRecord ranges_p
  // uInt startChan=0, widthChan=1, incrChan=1;
  // if (!mss_p.selectChannel(nchan_p,startChan,widthChan,incrChan) return bb2;
  // if (!mss_p.selectPolarization(pols_p)) return boo-boo3;
  
  // Then make the listing
  listData();
}


Bool NewMSLister::selectTime (Double& inputStartTime, Double& inputStopTime)
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

  // Select on these times (NewMSSelector treats all the other Sanity Clauses,
  // we just check on their results here)
  if (!mss_p.select(ranges_p)) return False;

  // Signal good time selection
  os_p  << "Timerange selection made:     "
	<< MVTime(msTimeLimits(0)/86400).ymd() << "/"
	<< MVTime(msTimeLimits(0)/86400).string() << "     to  "
	<< MVTime(msTimeLimits(1)/86400).ymd() << "/"
	<< MVTime(msTimeLimits(1)/86400).string() << endl << endl;
  return ok;
}


void NewMSLister::listData()
{
  // Now get the data records.  Because of the way NewMSSelector works, we
  // have to extract the data through a GlishRecord to a Record to Arrays,
  // in order to conveniently print the contents.  First change the items
  // extracted, especially "range" items to "list" items.
  items_p.resize(9,True);
  items_p(0)="time";	// the list of time values: ARGH!!! this is because
			// the "time" enum in getData() acts as the "times"
			// enum in select() or range()
  items_p(3)="uvdist";	// the list of uv-distances: AGAIN!!! "uvdist" enum
			// in getData() produces a list, but only a range
			// in select() or range()
  items_p(6)="amplitude";	// these only work in
  items_p(7)="phase";		// getData(), not in
  items_p(8)="weight";		// select() or range()

  // Now do the conversion to a Record.  Note that mss_p.getData() is an
  // implicit GlishRecord object
  mss_p.getData(items_p,False).toRecord(dataRecords_p);

  // Construct arrays for the Record items.  The V-float declaration
  // appears to be necessary (instead of V-double) despite the get()
  // function's claim to do type promotion.
  Vector <Double>	time,uvdist;
  Vector <Int>		ant1,ant2,spwinid,fieldid;
  Array <Float>		ampl,phase;
  Vector <Float>	weight;

  // Extract the values from the Record into the Arrays
  dataRecords_p.get("time", time);
  dataRecords_p.get("antenna1", ant1);
  dataRecords_p.get("antenna2", ant2);
  dataRecords_p.get("uvdist", uvdist);
  dataRecords_p.get("spectral_window_id", spwinid);
  dataRecords_p.get("field_id", fieldid);
  dataRecords_p.get("amplitude", ampl);
  dataRecords_p.get("phase", phase);
  dataRecords_p.get("weight", weight);

  // Finally print the records out, one per line.  The line is:
  // Field Time AntPair UVDist SpWin Chan# Polarisations Visibs[amp,pha,wt]
  widthTime	= 12;
  widthBasel	= 10;
  widthAnt	=  2;
  widthID	=  6;
  widthVis	= 20;
  os_p << "Listing " << time.nelements()
       << " data records satisfying selection criteria, " << endl
       << "for each of " << npols_p << " polarisation(s) and " << nchan_p
       << " spectral channel(s)." << endl << endl << dashline_p << endl;

  for (uInt ipol=0; ipol<npols_p; ipol++) {
    for (uInt ichan=0; ichan<nchan_p; ichan++) {
      Int nrows = time.nelements();
      for (Int row=0; row<nrows; row++) {
	if ((row/pageHeight_p)*pageHeight_p == row) {
	  // Paginate every pageHeight_p'th row
	  listColumnHeader();
	}
	os_p.output().setf(ios::left, ios::adjustfield);
	os_p.output().width(widthID);	os_p << fieldid(row);
	os_p.output().width(widthTime);	os_p << MVTime(time(row)/86400).string();
	os_p.output().width(widthAnt);	os_p << ant1(row);
	os_p.output().width(widthAnt);	os_p << "- ";
	os_p.output().width(widthID);	os_p << ant2(row);
	os_p.output().width(widthBasel);os_p << uvdist(row);
	os_p.output().width(widthID);	os_p << spwinid(row);
	os_p.output().width(widthID);	os_p << ichan;
	os_p.output().width(widthID);	os_p << pols_p(ipol);
	os_p.output().width(widthID);	os_p << ampl(IPosition(3,ipol,ichan,row));
	os_p.output().width(widthAnt);	os_p << ",";
	os_p.output().width(widthID);	os_p << phase(IPosition(3,ipol,ichan,row));
	os_p.output().width(widthAnt);	os_p << ",";
	os_p.output().width(widthID);	os_p << weight(row);
	os_p << endl;
      }
    }
  }
  os_p << dashline_p << endl;

  // Post it
  os_p.post();
}


void NewMSLister::listColumnHeader()
{
  // Write the column headers
  os_p << endl << endl << endl;
  os_p.output().setf(ios::left, ios::adjustfield);
  os_p.output().width(widthID);		os_p << "Field";
  os_p.output().width(widthTime);	os_p << "Time";
  os_p.output().width(widthBasel);	os_p << "Ant.Pair";
  os_p.output().width(widthBasel);	os_p << "UVDist";
  os_p.output().width(widthID);		os_p << "SpWin";
  os_p.output().width(widthID);		os_p << "Chan#";
  os_p.output().width(widthID);		os_p << "Pol'n";
  os_p.output().width(widthVis);	os_p << "Visibs[amp,pha,wt]";
  os_p << endl << dashline_p << endl;
}


//
// Clear all the formatting flags
//
void NewMSLister::clearFlags()
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
