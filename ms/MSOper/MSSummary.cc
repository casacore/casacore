//# MSSummary.cc:  Helper class for applications listing a MeasurementSet
//# Copyright (C) 1998,1999,2000,2001,2002,2003
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

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/casa/Quanta.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/TableVector.h>
#include <casacore/ms/MeasurementSets.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MSOper/MSKeys.h>
#include <casacore/ms/MSOper/MSMetaData.h>
#include <casacore/ms/MSOper/MSSummary.h>
#include <casacore/ms/MeasurementSets/MSRange.h>
#include <casacore/ms/MSSel/MSSelector.h>
#include <casacore/ms/MSSel/MSSelection.h>

#include <casacore/casa/iomanip.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//
// Constructor assigns pointer.  If MS goes out of scope you
// will get rubbish.  Also sets string to separate subtable output.
//
MSSummary::MSSummary (const MeasurementSet& ms)
: pMS(&ms), _msmd(new MSMetaData(&ms, 50.0)),
  dashlin1(replicate("-",80)),
  dashlin2(replicate("=",80)),
  _listUnflaggedRowCount(False),
  _cacheSizeMB(50)
{}

MSSummary::MSSummary (const MeasurementSet* ms)
: pMS(ms), _msmd(new MSMetaData(ms, 50.0)),
  dashlin1(replicate("-",80)),
  dashlin2(replicate("=",80)),
  _listUnflaggedRowCount(False),
  _cacheSizeMB(50)
{}

MSSummary::MSSummary (const MeasurementSet* ms, const String msname)
: pMS(ms), _msmd(new MSMetaData(ms, 50)),
  dashlin1(replicate("-",80)),
  dashlin2(replicate("=",80)),
  msname_p(msname),
  _listUnflaggedRowCount(False),
  _cacheSizeMB(50)
{}
//
// Destructor does nothing
//
MSSummary::~MSSummary ()
{}


//
// Retrieve number of rows
//
Int MSSummary::nrow () const
{
	return _msmd->nRows();
}


//
// Get ms name
//
String MSSummary::name () const
{
	if (! msname_p.empty())
		return msname_p;

	return pMS->tableName();
}


//
// Reassign pointer.
//
Bool MSSummary::setMS (const MeasurementSet& ms)
{
	const MeasurementSet* pTemp;
	pTemp = &ms;
	if (pTemp == 0) {
		return False;
	} else {
		pMS = pTemp;
		_msmd.reset(new MSMetaData(&ms, _cacheSizeMB));
		return True;
	}
}


//
// List information about an ms to the logger
//
void MSSummary::list (LogIO& os, Bool verbose, Bool oneBased) const
{
	Record dummy;
	list(os, dummy, verbose, False, oneBased);
}

void MSSummary::list (LogIO& os, Record& outRec, Bool verbose,
                      Bool fillRecord, Bool oneBased) const
{
	// List a title for the Summary
	listTitle (os);

	// List the main table as well as the subtables in a useful order and format
	listWhere (os,verbose);
	listWhat (os,outRec, verbose, fillRecord);
	listHow (os,verbose, oneBased);

	// These aren't really useful (yet?)
	//  listSource (os,verbose);
	//  listSysCal (os,verbose);
	//  listWeather (os,verbose);

	// List a summary of table sizes
	//  os << dashlin1 << endl << endl;
	//  listTables (os,verbose);
	//  os << dashlin2 << endl;

	// Post it
	os.post();

}


//
// List a title for the Summary
//
void MSSummary::listTitle (LogIO& os) const
{
	// Version number of the MS definition
	Float vers = 1.0;
	if (pMS->keywordSet().isDefined("MS_VERSION")) {
		vers = pMS->keywordSet().asFloat("MS_VERSION");
	}

	// List the MS name and version as the title of the Summary
	os << LogIO::NORMAL;
	os << dashlin2 << endl << "           MeasurementSet Name:  " << this->name()
			<< "      MS Version " << vers << endl << dashlin2 << endl;
}


//
// Convenient table groupings
//
void MSSummary::listWhere (LogIO& os, Bool verbose) const
{
	listObservation (os,verbose);
}
void MSSummary::listWhat (LogIO& os, Bool verbose) const
{
	Record dummy;
	listWhat(os, dummy, verbose, False);

}
void MSSummary::listWhat (LogIO& os, Record& outRec, Bool verbose, Bool fillRecord) const
{
	listMain (os,outRec, verbose, fillRecord);
	listField (os,outRec, verbose, fillRecord);
}


void MSSummary::listHow (LogIO& os, Bool verbose, Bool oneBased) const
{
	// listSpectralWindow (os,verbose);
	// listPolarization (os,verbose);
        listSpectralAndPolInfo(os, verbose, oneBased);
	listSource (os,verbose);
	//  listFeed (os,verbose, oneBased));
	listAntenna (os,verbose);
}


//
// SUBTABLES
//
void MSSummary::listMain (LogIO& os, Bool verbose) const
{
	Record dummy;
	listMain(os, dummy, verbose, False);

}

// TESTING CAS-2751
void MSSummary::listMain (LogIO& os, Record& outRec, Bool verbose,
		Bool fillRecord) const
{
	if (nrow()<=0) {
		os << "The MAIN table is empty: there are no data!!!" << endl << LogIO::POST;
		return;
	}
	std::pair<Double, Double> timerange = _msmd->getTimeRange();
	Double startTime = timerange.first;
	Double stopTime = timerange.second;
	Double exposTime = stopTime - startTime;
	//    Double exposTime = sum(msc.exposure().getColumn());
	MVTime startMVT(startTime/86400.0), stopMVT(stopTime/86400.0);

	ROMSMainColumns msmc(*pMS);
	String timeref=msmc.time().keywordSet().subRecord("MEASINFO").asString("Ref");

	// Output info
	os << "Data records: " << nrow() << "       Total elapsed time = "
			<< exposTime << " seconds" << endl
			<< "   Observed from   " << MVTime(startTime/C::day).string(MVTime::DMY,7)  //startMVT.string()
			<< "   to   " << MVTime(stopTime/C::day).string(MVTime::DMY,7)  // stopMVT.string()
			<< " (" << timeref << ")"
			<< endl << endl;
	os << LogIO::POST;

	if(fillRecord){
		outRec.define("numrecords", nrow());
		outRec.define("IntegrationTime", exposTime);
		outRec.define("BeginTime", startTime/C::day);
		outRec.define("EndTime", stopTime/C::day);
		outRec.define("timeref", timeref);
	}

	//		if (verbose) {   // do "scan" listing

	// Set up iteration over OBSID, ARRID, and SCAN_NUMBER:
	//      Block<String> mssortcols(3);
	//      mssortcols[0] = "OBSERVATION_ID";
	//      mssortcols[1] = "ARRAY_ID";
	//      mssortcols[2] = "SCAN_NUMBER";
	//      MSIter msIter(const_cast<MeasurementSet&>(*pMS),mssortcols,0.0,False );

	//      for (msIter.origin(); msIter.more(); msIter++) {
	//      }


	// the selected MS (all of it) as a Table tool:
	//   MS is accessed as a generic table here because
	//   the ms tool hard-wires iteration over SPWID, FLDID,
	//   and TIME and this is not desired in this application.
	//   It would be easier if the ms tool did not automatically
	//   iterate over any indices, or if there were an ms.table()
	//   function.
	MSSelector mssel;
	mssel.setMS(const_cast<MeasurementSet&>(*pMS));
	mssel.initSelection(True);
	Table mstab(mssel.selectedTable());

	// Field names:
	ROMSFieldColumns field(pMS->field());
	Vector<String> fieldnames(field.name().getColumn());
	nVisPerField_.resize(fieldnames.nelements());
	nVisPerField_=0;

	// Observing Mode (State Table)
	ROMSStateColumns state(pMS->state());
	Vector<String> obsModes(state.obsMode().getColumn());

	// Spw Ids
	ROMSDataDescColumns dd(pMS->dataDescription());
	Vector<Int> specwindids(dd.spectralWindowId().getColumn());

	// Field widths for printing:
	Int widthLead  =  2;
	Int widthScan  =  4;
	Int widthbtime = 22;
	Int widthetime = 10;
	Int widthFieldId = 5;
	Int widthField = 20;
	Int widthnrow = 10;
	Int widthNUnflaggedRow = 13;
	//Int widthInttim = 7;

	// Set up iteration over OBSID and ARRID:
	Block<String> icols(2);
	icols[0] = "OBSERVATION_ID";
	icols[1] = "ARRAY_ID";
	TableIterator obsarriter(mstab,icols);
	//Limiting record length
	Int recLength=0;
	const Int maxRecLength=10000; //limiting for speed and size sake
	// Iterate:
	while (!obsarriter.pastEnd()) {

		// Table containing this iteration:
		Table obsarrtab(obsarriter.table());
		// Extract (zero-based) OBSID and ARRID for this iteration:
		ROTableVector<Int> obsidcol(obsarrtab,"OBSERVATION_ID");
		Int obsid(obsidcol(0));
		ROTableVector<Int> arridcol(obsarrtab,"ARRAY_ID");
		Int arrid(arridcol(0));

		if (verbose) {
			// Report OBSID and ARRID, and header for listing:
			os << endl << "   ObservationID = " << obsid;
			os << "         ArrayID = " << arrid << endl;
			String datetime="  Date        Timerange                ";
			datetime.replace(24,1,"(");
			datetime.replace(25,timeref.length(),timeref);
			datetime.replace(25+timeref.length(),1,")");
			os << datetime;
			os << "Scan  FldId FieldName "
                           << "            nRows     ";
                        if (_listUnflaggedRowCount) {
                          os << "nUnflRows   ";
                        }
                        os << "SpwIds   Average Interval(s)    ScanIntent" << endl;
		}
		/* CAS-2751. Sort the table by scan then field (not timestamp)
		 * so that scans are not listed for every different DDID
		 */
		// Setup iteration over scan and field within this iteration:
		Block<String> jcols(2);
		jcols[0] = "SCAN_NUMBER";
		jcols[1] = "FIELD_ID";
		TableIterator stiter(obsarrtab,jcols);

		// Vars for keeping track of time, fields, and ddis
		Int lastscan(-1);
		Vector<Int> lastfldids;
		Vector<Int> lastddids;
		Vector<Int> laststids;
		Vector<Int> fldids(1,0);
		Vector<Int> ddids(1,0);
		Vector<Int> stids(1,0); // State IDs
		//Vector<Int> spwids;
		Int nfld(1);
		Int nddi(1);
		Int nst(1);
		Double btime(0.0), etime(0.0);
		Double lastday(0.0), day(0.0);
		Bool firsttime(True);
		Int thisnrow(0);
		Double meanIntTim(0.0);

		os.output().precision(3);
		Int subsetscan=0;
		// Iterate over scans/fields:
		std::set<uInt> spw;
		ScanKey lastScanKey;
		while (!stiter.pastEnd()) {
			// ms table at this scan
			Table t(stiter.table());
			Int nrow=t.nrow();

			// relevant columns
			ROTableVector<Double> timecol(t,"TIME");
			ROTableVector<Double> inttim(t,"EXPOSURE");
			ROTableVector<Int> scncol(t,"SCAN_NUMBER");
			ROTableVector<Int> fldcol(t,"FIELD_ID");
			ROTableVector<Int> ddicol(t,"DATA_DESC_ID");
			ROTableVector<Int> stidcol(t,"STATE_ID");

			lastScanKey.obsID = obsid;
			lastScanKey.arrayID = arrid;
			lastScanKey.scan = lastscan;

			// this timestamp
			//Double thistime(timecol(0));

			// this scan_number
			Int thisscan(scncol(0));

			// First field and ddi at this timestamp:
			fldids.resize(1,False);
			fldids(0)=fldcol(0);
			nfld=1;
			ddids.resize(1,False);
			ddids(0)=ddicol(0);
			nddi=1;

			stids.resize(1, False);
			stids(0) = stidcol(0);
			nst=1;
			nVisPerField_(fldids(0))+=nrow;

			// fill field and ddi lists for this scan
			for (Int i=1; i < nrow; i++) {
				if ( !anyEQ(fldids,fldcol(i)) ) {
					nfld++;
					fldids.resize(nfld,True);
					fldids(nfld-1)=fldcol(i);
				}

				if ( !anyEQ(ddids,ddicol(i)) ) {
					nddi++;
					ddids.resize(nddi,True);
					ddids(nddi-1)=ddicol(i);
				}

				if ( !anyEQ(stids,stidcol(i)) ) {
					nst++;
					stids.resize(nst,True);
					stids(nst-1)=stidcol(i);
				}

			}

			// If not first timestamp, check if scan changed, etc.
			ScanKey thisScanKey = lastScanKey;
			thisScanKey.scan = thisscan;
			if (!firsttime) {
				// Has state changed?
				Bool samefld;
				samefld=fldids.conform(lastfldids) && !anyNE(fldids,lastfldids);
				Bool sameddi;
				sameddi=ddids.conform(lastddids) && !anyNE(ddids,lastddids);

				Bool samest;
				samest=stids.conform(laststids) && !anyNE(stids,laststids);

				Bool samescan;
				samescan=(thisscan==lastscan);

				samescan = samescan && samefld && sameddi && samest;
				//		samescan = samescan && samefld;

				// If state changed, then print out last scan's info
				if (!samescan) {
					//	      cout << "thisscan " << thisscan << "lastscan " << lastscan
					// 		   << " fldids " << fldids << " lastfldids " << lastfldids
					// 		   << " ddids " << ddids << " lastddids " << lastddids
					// 		   << " samescan " << (thisscan==lastscan) <<  " samefld " << samefld << " sameddi " << sameddi
					// 		   << " meanIntTim " << meanIntTim << " thisnrow " << thisnrow << " inttim.makeVector().size() "
					// 		   <<  inttim.makeVector().size() << " meanIntTim/thisnrow " << meanIntTim/thisnrow << endl;
					if (thisnrow>0){
						meanIntTim/=thisnrow;
					}
					else {
						meanIntTim=0.0;
					}
					// this MJD
					day=floor(MVTime(btime/C::day).day());
					spw = _msmd->getSpwsForScan(lastScanKey);
					String name=fieldnames(lastfldids(0));
					if (verbose) {
						// Print out last scan's times, fields, ddis
						os.output().setf(ios::right, ios::adjustfield);
						os.output().width(widthLead); os << "  ";
						os.output().width(widthbtime);
						if (day!=lastday) {     // print date
							os << MVTime(btime/C::day).string(MVTime::DMY,7);
						} else {                // omit date
							os << MVTime(btime/C::day).string(MVTime::TIME,7);
						}
						os.output().width(3); os << " - ";
						os.output().width(widthetime);
						os << MVTime(etime/C::day).string(MVTime::TIME,7);
						os.output().width(widthLead); os << "  ";
						os.output().setf(ios::right, ios::adjustfield);
						os.output().width(widthScan); os << lastscan;
						os.output().width(widthLead); os << "  ";
						os.output().setf(ios::right, ios::adjustfield);
						os.output().width(widthFieldId); os << lastfldids(0) << " ";
						os.output().setf(ios::left, ios::adjustfield);
						if (name.length()>20) name.replace(19,1,'*');
						os.output().width(widthField); os << name.at(0,20);
						//os.output().width(widthnrow); os << thisnrow;
						os.output().width(widthnrow);
						os.output().setf(ios::right, ios::adjustfield);
						os <<  _msmd->nRows(MSMetaData::BOTH, arrid, obsid, lastscan, lastfldids(0));
                                                if (_listUnflaggedRowCount) {
                                                  ostringstream xx;
                                                  xx << std::fixed << setprecision(2)
                                                     << _msmd->nUnflaggedRows(
								MSMetaData::BOTH, arrid, obsid, lastscan, lastfldids(0)
							);
                                                  os.output().width(widthNUnflaggedRow);
                                                  os << xx.str();
                                                }
						//os.output().width(widthInttim); os << meanIntTim;
						os.output().width(widthLead); os << "  ";

						os << spw;
						/*
						if (spw.size() <= 9) {
							os.output().width(28 - (3*spw.size())); os << " ";
						}
						*/
						os.output().width(widthLead); os << "  ";

						std::map<uInt, Double> intToScanMap = _msmd->getAverageIntervalsForScan(lastScanKey);
						os << "[";
						for (
							std::set<uInt>::const_iterator iter=spw.begin();
							iter!=spw.end(); iter++
						) {
							if (iter!=spw.begin()) {
								os << ", ";
							}
							os << intToScanMap[*iter];
						}
						os << "] ";
						// The Obsmode column can be empty only report them if it is not
						//String obsMode = "";
						//if (obsModes.size() > (unsigned int) 0) {
						//	obsMode=obsModes(laststids(0));
						//}
						//os << obsMode;

						//os << endl;
						std::set<String> intents = _msmd->getIntentsForScan(lastScanKey);
						if (intents.size() > 0) {
							os << intents;
						}
						os << endl;
					}
					if(fillRecord && (recLength < maxRecLength))  {
						Record scanRecord;
						Record subScanRecord;
						String scanrecid=String("scan_")+String::toString(lastscan);
						if(outRec.isDefined(scanrecid)){
							scanRecord=outRec.asrwRecord(scanrecid);
							outRec.removeField(scanrecid);
						}

						subScanRecord.define("BeginTime", btime/C::day);
						subScanRecord.define("EndTime", etime/C::day);
						subScanRecord.define("scanId", lastscan);
						subScanRecord.define("FieldId", lastfldids(0));
						subScanRecord.define("FieldName", name);
						subScanRecord.define("StateId", laststids(0));
						subScanRecord.define("nRow", thisnrow);
						subScanRecord.define("IntegrationTime", meanIntTim);
						subScanRecord.define("SpwIds", Vector<Int>(spw.begin(), spw.size(), 0));
						scanRecord.defineRecord(String::toString(subsetscan), subScanRecord);
						if(!outRec.isDefined(scanrecid)){
							outRec.defineRecord(scanrecid, scanRecord);
						}
						if(lastscan == thisscan){
							++subsetscan;
						}
						else{
							subsetscan=0;
						}
					}
					btime = _msmd->getTimeRangeForScan(thisScanKey).first;
					// next last day is this day
					lastday=day;

					thisnrow=0;
					meanIntTim=0.;
					++recLength;
				}
				//						etime=thistime;
				//etime=timecol(nrow-1);   //CAS-2751
				etime = _msmd->getTimeRangeForScan(thisScanKey).second;
			}
			else {
                std::pair<Double, Double> timeRange = _msmd->getTimeRangeForScan(thisScanKey);
				// initialize btime and etime
				//btime=thistime;
				btime = timeRange.first;
				//						etime=thistime;
				//etime=timecol(nrow-1);  //CAS-2751
				etime = timeRange.second;
				// no longer first time thru
				firsttime=False;
			}

			thisnrow+=nrow;

			meanIntTim+=sum(inttim.makeVector());

			// for comparison at next timestamp
			lastfldids.assign(fldids);
			lastddids.assign(ddids);
			laststids.assign(stids);
			lastscan=thisscan;

			// push iteration
			stiter.next();
		} // end of time iteration

		meanIntTim = thisnrow > 0 ? meanIntTim/thisnrow : 0;
		/*
		if
			meanIntTim/=thisnrow;
		else
			meanIntTim=0.0;
			*/
		// this MJD
		day=floor(MVTime(btime/C::day).day());
		lastScanKey.obsID = obsid;
		lastScanKey.arrayID = arrid;
		lastScanKey.scan = lastscan;
		spw = _msmd->getSpwsForScan(lastScanKey);
		String name=fieldnames(lastfldids(0));
		if (verbose) {
			// Print out final scan's times, fields, ddis
			os.output().setf(ios::right, ios::adjustfield);
			os.output().width(widthLead); os << "  ";
			os.output().width(widthbtime);
			if (day!=lastday) {
				os << MVTime(btime/C::day).string(MVTime::DMY,7);
			} else {
				os << MVTime(btime/C::day).string(MVTime::TIME,7);
			}
			os.output().width(widthLead);  os << " - ";
			os.output().width(widthetime);
			os << MVTime(etime/C::day).string(MVTime::TIME,7);
			os.output().width(widthLead); os << "  ";
			os.output().setf(ios::right, ios::adjustfield);
			os.output().width(widthScan); os << lastscan;
			os.output().width(widthLead);  os << "  ";
			os.output().setf(ios::right, ios::adjustfield);
			os.output().width(widthFieldId); os << lastfldids(0) << " ";
			os.output().setf(ios::left, ios::adjustfield);
			if (name.length()>20) name.replace(19,1,'*');
			os.output().width(widthField); os << name.at(0,20);
			os.output().width(widthnrow);
			os.output().setf(ios::right, ios::adjustfield);
			os << _msmd->nRows(MSMetaData::BOTH, arrid, obsid, lastscan, lastfldids(0));
			if (_listUnflaggedRowCount) {
                          ostringstream xx;
                          xx << std::fixed << setprecision(2)
                             << _msmd->nUnflaggedRows(MSMetaData::BOTH, arrid, obsid, lastscan, lastfldids(0));
                          os.output().width(widthNUnflaggedRow);
                          os << xx.str();
                        }
//			os.output().width(widthInttim); os << meanIntTim;
			os.output().width(widthLead);  os << "  ";
                        showContainer (os.output(), spw, ", ", "[", "]");
			os.output().width(widthLead); os << "  ";

			std::map<uInt, Double> intToScanMap = _msmd->getAverageIntervalsForScan(lastScanKey);
			os << "[";
			for (
				std::set<uInt>::const_iterator iter=spw.begin();
				iter!=spw.end(); iter++
			) {
				if (iter!=spw.begin()) {
					os << ", ";
				}
				os << intToScanMap[*iter];
			}
			os << "] ";
			// The Obsmode column can be empty only report them if it is not
			//String obsMode = "";
			//if (obsModes.size() > (unsigned int) 0) {
			//	obsMode=obsModes(laststids(0));
			//}
			//os << obsMode;
			//os << endl;
			set<String> intents = _msmd->getIntentsForScan(lastScanKey);
			if (intents.size() > 0) {
				os << intents;
			}
			os << endl;
		}
		if(fillRecord  && (recLength < maxRecLength)){
			Record scanRecord;
			Record subScanRecord;
			String scanrecid=String("scan_")+String::toString(lastscan);
			if(outRec.isDefined(scanrecid)){
				scanRecord=outRec.asrwRecord(scanrecid);
				outRec.removeField(scanrecid);
			}

			subScanRecord.define("BeginTime", btime/C::day);
			subScanRecord.define("EndTime", etime/C::day);
			subScanRecord.define("scanId", lastscan);
			subScanRecord.define("FieldId", lastfldids(0));
			subScanRecord.define("FieldName", name);
			subScanRecord.define("nRow", thisnrow);
			subScanRecord.define("IntegrationTime", meanIntTim);
			subScanRecord.define("SpwIds", Vector<Int>(spw.begin(), spw.size(), 0));
			scanRecord.defineRecord(String::toString(subsetscan), subScanRecord);
			if(!outRec.isDefined(scanrecid)){
				outRec.defineRecord(scanrecid, scanRecord);
			}
			subsetscan=0;
			++recLength;
		}

		// post to logger
		if (verbose) os << LogIO::POST;

		// push OBS/ARR iteration
		obsarriter.next();
	} // end of OBS/ARR iteration
	if (verbose){
		os << "           (nRows = Total number of rows per scan) " << endl;
		os << LogIO::POST;
	}

	//		} // end of verbose section



	os << LogIO::POST;
}


void MSSummary::getScanSummary (Record& outRec) const
{

	if (nrow()<=0) {
		return;
	}

	// Make objects
	ROMSColumns msc(*pMS);
	Double startTime, stopTime;
	minMax(startTime, stopTime, msc.time().getColumn());

	MVTime startMVT(startTime/86400.0), stopMVT(stopTime/86400.0);

	ROMSMainColumns msmc(*pMS);
	String timeref=msmc.time().keywordSet().subRecord("MEASINFO").asString("Ref");
	//outRec.define("numrecords", nrow());
	//outRec.define("IntegrationTime", exposTime);
	//outRec.define("BeginTime", startTime/C::day);
	//outRec.define("EndTime", stopTime/C::day);
	//outRec.define("timeref", timeref);


	// the selected MS (all of it) as a Table tool:
	//   MS is accessed as a generic table here because
	//   the ms tool hard-wires iteration over SPWID, FLDID,
	//   and TIME and this is not desired in this application.
	//   It would be easier if the ms tool did not automatically
	//   iterate over any indices, or if there were an ms.table()
	//   function.
	MSSelector mssel;
	mssel.setMS(const_cast<MeasurementSet&>(*pMS));
	mssel.initSelection(True);
	Table mstab(mssel.selectedTable());

	// Field names:
	ROMSFieldColumns field(pMS->field());
	Vector<String> fieldnames(field.name().getColumn());
	nVisPerField_.resize(fieldnames.nelements());
	nVisPerField_=0;

	// Spw Ids
	ROMSDataDescColumns dd(pMS->dataDescription());
	Vector<Int> specwindids(dd.spectralWindowId().getColumn());

	// Set up iteration over OBSID and ARRID:
	Block<String> icols(2);
	icols[0] = "OBSERVATION_ID";
	icols[1] = "ARRAY_ID";
	TableIterator obsarriter(mstab,icols);
	//Limiting record length
	Int recLength=0;
	// Iterate:
	while (!obsarriter.pastEnd()) {

		// Table containing this iteration:
		Table obsarrtab(obsarriter.table());

		// Extract (zero-based) OBSID and ARRID for this iteration:
		ROTableVector<Int> obsidcol(obsarrtab,"OBSERVATION_ID");
		ROTableVector<Int> arridcol(obsarrtab,"ARRAY_ID");

		// Report OBSID and ARRID, and header for listing:
		// 	os << endl << "   ObservationID = " << obsid;
		// 	os << "         ArrayID = " << arrid << endl;
		String datetime="  Date        Timerange                ";
		datetime.replace(24,1,"(");
		datetime.replace(25,timeref.length(),timeref);
		datetime.replace(25+timeref.length(),1,")");
		// 	os << datetime;
		// 	os << "Scan  FldId FieldName    nVis   Int(s)   SpwIds" << endl;

		// Setup iteration over timestamps within this iteration:
		Block<String> jcols(2);
		jcols[0] = "SCAN_NUMBER";
		jcols[1] = "TIME";
		TableIterator stiter(obsarrtab,jcols);

		// Vars for keeping track of time, fields, and ddis
		Int lastscan(-1);
		Vector<Int> lastfldids;
		Vector<Int> lastddids;
		Vector<Int> laststids;
		Vector<Int> fldids(1,0);
		Vector<Int> ddids(1,0);
		Vector<Int> stids(1,0); // State IDs
		Vector<Int> spwids;
		Int nfld(1);
		Int nddi(1);
		Int nst(1);
		Double btime(0.0), etime(0.0);
		Bool firsttime(True);
		Int thisnrow(0);
		Double meanIntTim(0.0);


		//	os.output().precision(3);
		Int subsetscan=0;
		// Iterate over timestamps:
		while (!stiter.pastEnd()) {

			// ms table at this timestamp
			Table t(stiter.table());
			Int nrow=t.nrow();

			// relevant columns
			ROTableVector<Double> timecol(t,"TIME");
			ROTableVector<Double> inttim(t,"EXPOSURE");
			ROTableVector<Int> scncol(t,"SCAN_NUMBER");
			ROTableVector<Int> fldcol(t,"FIELD_ID");
			ROTableVector<Int> ddicol(t,"DATA_DESC_ID");
			ROTableVector<Int> stidcol(t,"STATE_ID");

			// this timestamp
			Double thistime(timecol(0));

			// this scan_number
			Int thisscan(scncol(0));

			// First field and ddi at this timestamp:
			fldids.resize(1,False);
			fldids(0)=fldcol(0);
			nfld=1;
			ddids.resize(1,False);
			ddids(0)=ddicol(0);
			nddi=1;

			stids.resize(1, False);
			stids(0) = stidcol(0);
			nst=1;

			nVisPerField_(fldids(0))+=nrow;

			// fill field and ddi lists for this timestamp
			for (Int i=1; i < nrow; i++) {
				if ( !anyEQ(fldids,fldcol(i)) ) {
					nfld++;
					fldids.resize(nfld,True);
					fldids(nfld-1)=fldcol(i);
				}

				if ( !anyEQ(ddids,ddicol(i)) ) {
					nddi++;
					ddids.resize(nddi,True);
					ddids(nddi-1)=ddicol(i);
				}

				if ( !anyEQ(stids,stidcol(i)) ) {
					nst++;
					stids.resize(nst,True);
					stids(nst-1)=stidcol(i);
				}
			}

			// If not first timestamp, check if scan changed, etc.
			if (!firsttime) {

				// Has state changed?
				Bool samefld;
				samefld=fldids.conform(lastfldids) && !anyNE(fldids,lastfldids);

				Bool sameddi;
				sameddi=ddids.conform(lastddids) && !anyNE(ddids,lastddids);

				Bool samest;
				samest=stids.conform(laststids) && !anyNE(stids,laststids);

				Bool samescan;
				samescan=(thisscan==lastscan);

				samescan = samescan && samefld && sameddi && samest;

				// If state changed, then print out last scan's info
				if (!samescan) {
					if (thisnrow>0){
						meanIntTim/=thisnrow;
					}
					else {
						meanIntTim=0.0;
					}

					// Spws
					spwids.resize(lastddids.nelements());
					for (uInt iddi=0; iddi<spwids.nelements();++iddi)
						spwids(iddi)=specwindids(lastddids(iddi));

					Record scanRecord;
					Record subScanRecord;
					String scanrecid=String::toString(lastscan);
					if(outRec.isDefined(scanrecid)){
						scanRecord=outRec.asrwRecord(scanrecid);
						outRec.removeField(scanrecid);
					}

					subScanRecord.define("BeginTime", btime/C::day);
					subScanRecord.define("EndTime", etime/C::day);
					subScanRecord.define("FieldId", lastfldids(0));
					subScanRecord.define("StateId", laststids(0));
					subScanRecord.define("nRow", thisnrow);
					subScanRecord.define("IntegrationTime", meanIntTim);
					subScanRecord.define("SpwIds", spwids);
					subScanRecord.define("DDIds", lastddids);
					scanRecord.defineRecord(String::toString(subsetscan), subScanRecord);
					if(!outRec.isDefined(scanrecid)){
						outRec.defineRecord(scanrecid, scanRecord);
					}
					if(lastscan == thisscan){
						++subsetscan;
					}
					else{
						subsetscan=0;
					}
					//}

					// new btime:
					btime=thistime;

					thisnrow=0;
					meanIntTim=0.;
					++recLength;
				}

				// etime keeps pace with thistime
				etime=thistime;

			} else {
				// initialize btime and etime
				btime=thistime;
				etime=thistime;
				// no longer first time thru
				firsttime=False;
			}

			thisnrow+=nrow;

			meanIntTim+=sum(inttim.makeVector());

			// for comparison at next timestamp
			lastfldids.assign(fldids);
			lastddids.assign(ddids);
			laststids.assign(stids);
			lastscan=thisscan;

			// push iteration
			stiter.next();
		} // end of time iteration

		if (thisnrow>0)
			meanIntTim/=thisnrow;
		else
			meanIntTim=0.0;

		// Spws
		spwids.resize(lastddids.nelements());
		for (uInt iddi=0; iddi<spwids.nelements();++iddi)
			spwids(iddi)=specwindids(lastddids(iddi));

		// Print out final scan's times, fields, ddis
		Record scanRecord;
		Record subScanRecord;
		String scanrecid=String::toString(lastscan);
		if(outRec.isDefined(scanrecid)){
			scanRecord=outRec.asrwRecord(scanrecid);
			outRec.removeField(scanrecid);
		}

		subScanRecord.define("BeginTime", btime/C::day);
		subScanRecord.define("EndTime", etime/C::day);
		subScanRecord.define("FieldId", lastfldids(0));
		subScanRecord.define("StateId", laststids(0));
		subScanRecord.define("nRow", thisnrow);
		subScanRecord.define("IntegrationTime", meanIntTim);
		subScanRecord.define("SpwIds", spwids);
		subScanRecord.define("DDIds", lastddids);
		scanRecord.defineRecord(String::toString(subsetscan), subScanRecord);
		if(!outRec.isDefined(scanrecid)){
			outRec.defineRecord(scanrecid, scanRecord);
		}
		subsetscan=0;
		++recLength;

		// push OBS/ARR iteration
		obsarriter.next();
	} // end of OBS/ARR iteration
}


void MSSummary::listAntenna (LogIO& os, Bool verbose) const
{

	// Make a MS-antenna object
	MSAntenna antennaTable(pMS->antenna());
	uInt nrow(antennaTable.nrow());

	// Quit if no antennas
	if (nrow<=0) {
		os << "The ANTENNA table is empty" << endl;
		return;
	}

	// Determine antennas  present in the main table
	MSRange msr(*pMS);
	Vector<Int> ant1,ant2;
	ant1 = msr.range(MSS::ANTENNA1).asArrayInt(RecordFieldId(0));
	ant2 = msr.range(MSS::ANTENNA2).asArrayInt(RecordFieldId(0));
	//GlishArray(msr.range(MSS::ANTENNA1).get(0)).get(ant1);
	//GlishArray(msr.range(MSS::ANTENNA2).get(0)).get(ant2);
	Vector<Int> antIds(ant1.nelements()+ant2.nelements());
	antIds(Slice(0,ant1.nelements()))=ant1;
	antIds(Slice(ant1.nelements(),ant2.nelements()))=ant2;
	const Int option=Sort::HeapSort | Sort::NoDuplicates;
	const Sort::Order order=Sort::Ascending;
	Int nAnt=GenSort<Int>::sort (antIds, order, option);

	// Get Antenna table columns:
	ROMSAntennaColumns antCol(antennaTable);

	if (verbose) {
		// Detailed antenna list

		String title;
		title="Antennas: "+String::toString(nAnt)+":";
		String indent("  ");
		uInt indwidth =5;
		uInt namewidth=6;
		uInt statwidth=10;
		uInt diamwidth=5;
		Int diamprec=1;
		uInt latwidth=13;
		uInt longwidth=14;
		uInt offsetwidth = 14;
		uInt positionwidth = 16;

		os.output().setf(ios::fixed, ios::floatfield);
		os.output().setf(ios::left, ios::adjustfield);

		// Write the title:
		os << title << endl;
		// Write the column headings:
		os << indent;
		os.output().width(indwidth);    os << "ID";
		os.output().width(namewidth);   os << "Name";
		os.output().width(statwidth);   os << "Station";
		os.output().width(diamwidth+4); os << "Diam.";
		os.output().width(longwidth);   os << "Long.";
		os.output().width(latwidth);    os << "Lat.";
		os.output().width(3*offsetwidth);
		os << "       Offset from array center (m)";
		os.output().width(3*positionwidth);
		os << "         ITRF Geocentric coordinates (m)";
		os << endl;
		os << indent;
		os.output().width(
			indwidth + namewidth + statwidth + diamwidth + 4
			+ longwidth + latwidth
		);
		os << " ";
		os.output().setf(ios::right, ios::adjustfield);
		os.output().width(offsetwidth);
		os << "East";
		os.output().width(offsetwidth);
		os << "North";
		os.output().width(offsetwidth);
		os << "Elevation";
		os.output().width(positionwidth);
		os << "x";
		os.output().width(positionwidth);
		os << "y";
		os.output().width(positionwidth);
		os << "z";
		os << endl;

		vector<MPosition> antPos = _msmd->getAntennaPositions();
		Bool posIsITRF = antPos[0].type() != MPosition::ITRF;
		vector<Quantum<Vector<Double> > > offsets = _msmd->getAntennaOffsets();

		// For each ant
		for (Int i=0; i<nAnt; i++) {
			os.output().setf(ios::left, ios::adjustfield);

			Int ant=antIds(i);
			// Get diameter
			Quantity diam=antCol.dishDiameterQuant()(ant);
			Unit diamUnit="m";

			// Get position
			MPosition mLongLat=antCol.positionMeas()(ant);
			MVAngle mvLong= mLongLat.getAngle().getValue()(0);
			MVAngle mvLat= mLongLat.getAngle().getValue()(1);
			Vector<Double> antOff = offsets[ant].getValue("m");
			if (posIsITRF) {
				MeasConvert<MPosition> toItrf(antPos[ant], MPosition::ITRF);
				antPos[i] = toItrf(antPos[ant]);
			}
			Vector<Double> xyz = antPos[ant].get("m").getValue();
			// write the row
			os << indent;
			os.output().width(indwidth);  os << ant;
			os.output().width(namewidth); os << antCol.name()(ant);
			os.output().width(statwidth); os << antCol.station()(ant);
			os.output().precision(diamprec);
			os.output().width(diamwidth); os << diam.getValue(diamUnit)<<"m   ";
			os.output().width(longwidth); os << mvLong.string(MVAngle::ANGLE,7);
			os.output().width(latwidth);  os << mvLat.string(MVAngle::DIG2,7);
			os.output().setf(ios::right, ios::adjustfield);
			os.output().precision(4);
			os.output().width(offsetwidth);
			os << antOff[0];
			os.output().width(offsetwidth);
			os << antOff[1];
			os.output().width(offsetwidth);
			os << antOff[2];
			os.output().precision(6);
			os.output().width(positionwidth);
			os << xyz[0];
			os.output().width(positionwidth);
			os << xyz[1];
			os.output().width(positionwidth);
			os << xyz[2];
			os << endl;
		}

	} else {
		// Horizontal list of the stations names:
		os << "Antennas: " << nAnt << " 'name'='station' " <<  endl;
		String line, leader;
		Int last=antIds(0)-1;
		for (Int i=0; i<nAnt; i++) {
			Int ant=antIds(i);
			// Build the line
			line = line + "'" + antCol.name()(ant) + "'" + "=";
			line = line + "'" + antCol.station()(ant) + "'";
			// Add comma if not at the end
			if (ant != (nAnt-1)) line = line + ", ";
			if (line.length()>55 || ant==(nAnt-1)) {
				// This line is finished, dump it after the line leader
				leader = String::toString(last+1) +"-" +String::toString(ant) +": ";
				os << "   ID=";
				os.output().setf(ios::right, ios::adjustfield);
				os.output().width(8); os << leader;
				os << line << endl;
				line = "";
				last = ant;
			}
		}
	}
	os << LogIO::POST;
}


void MSSummary::listFeed (LogIO& os, Bool verbose, Bool oneBased) const
{
	// Do nothing in terse mode
	if (verbose) {

		// Make a MS-feed-columns object
		ROMSFeedColumns msFC(pMS->feed());

		if (msFC.antennaId().nrow()<=0) {
			os << "The FEED table is empty" << endl;
		}
		else {
			os << "Feeds: " << msFC.antennaId().nrow();
			os << ": printing first row only";
			// Line is	FeedID SpWinID NumRecept PolTypes
			Int widthLead	=  2;
			Int widthAnt	= 10;
			Int widthSpWinId	= 20;
			Int widthNumRec	= 15;
			Int widthPolType	= 10;
			os << endl;
			os.output().setf(ios::left, ios::adjustfield);
			os.output().width(widthLead);	os << "  ";
			os.output().width(widthAnt);	os << "Antenna";
			os.output().width(widthSpWinId);	os << "Spectral Window";
			os.output().width(widthNumRec);	os << "# Receptors";
			os.output().width(widthPolType);	os << "Polarizations";
			os << endl;

			// loop through rows
			// for (uInt row=0; row<msFC.antennaId().nrow(); row++) {
			for (uInt row=0; row<1; row++) {
				os.output().setf(ios::left, ios::adjustfield);
				os.output().width(widthLead);	os << "  ";
				os.output().width(widthAnt);	os << (msFC.antennaId()(row)+1);
				Int spwId = msFC.spectralWindowId()(row);
				if (oneBased  &&  spwId >= 0) spwId = spwId + 1;
				os.output().width(widthSpWinId);os << spwId;
				os.output().width(widthNumRec);	os << msFC.numReceptors()(row);
				os.output().width(widthPolType);os << msFC.polarizationType()(row);
				os << endl;
			}
		}
	}
	os << LogIO::POST;
}

void MSSummary::listField (LogIO& os, Bool verbose) const
{
	Record dummy;
	listField(os, dummy, verbose, False);

}
void MSSummary::listField (LogIO& os, Record& outrec,  Bool verbose, Bool fillRecord) const
{

	// Make a MS-field-columns object
	ROMSFieldColumns msFC(pMS->field());

	// Is source table present?
	Bool srcok=!(pMS->source().isNull() || pMS->source().nrow()<1);

	// Determine fields present in the main table
	MSRange msr(*pMS);
	Vector<Int> fieldId;
	//ant2 = msr.range(MSS::ANTENNA2).asArrayInt(RecordFieldId(0));
	fieldId = msr.range(MSS::FIELD_ID).asArrayInt(RecordFieldId(0));

	if (msFC.phaseDir().nrow()<=0) {
		os << "The FIELD table is empty" << endl;
	} else if (fieldId.nelements()==0) {
		os << "The MAIN table is empty" << endl;
	} else {
		os << "Fields: " << fieldId.nelements()<<endl;
		Int widthLead  =  2;
		Int widthField =  5;
		Int widthCode  =  5;
		Int widthName  = 20;
		Int widthRA    = 16;
		Int widthDec   = 16;
		Int widthType  =  8;
		Int widthSrc   =  6;
		Int widthnVis  =  10;
		Int widthNUnflaggedRows = 13;

		outrec.define("nfields", Int(fieldId.nelements()));
		if (verbose) {}  // null, always same output

		// Line is	ID Date Time Name RA Dec Type
		os.output().setf(ios::left, ios::adjustfield);
		os.output().width(widthLead);	os << "  ";
		os.output().width(widthField);	os << "ID";
		os.output().width(widthCode);       os << "Code";
		os.output().width(widthName);	os << "Name";
		os.output().width(widthRA);	        os << "RA";
		os.output().width(widthDec);	os << " Decl";
		os.output().width(widthType);	os << "Epoch";
		if (srcok) {os.output().width(widthSrc);	os << "SrcId";}
		if (nVisPerField_.nelements()>0) {
			os.output().setf(ios::right, ios::adjustfield);
			os.output().width(widthnVis);
			os << "nRows";
			if (_listUnflaggedRowCount) {
                          os.output().width(widthNUnflaggedRows);
                          os << "nUnflRows";
                        }
		}
		os << endl;
		// loop through fields
		for (uInt i=0; i<fieldId.nelements(); i++) {
			uInt fld=fieldId(i);
			if (fld<msFC.phaseDir().nrow()) {
				MDirection mRaDec=msFC.phaseDirMeas(fld);
				MVAngle mvRa = mRaDec.getAngle().getValue()(0);
				MVAngle mvDec= mRaDec.getAngle().getValue()(1);
				String name=msFC.name()(fld);
				if (name.length()>20) name.replace(19,1,"*");
				os.output().setf(ios::left, ios::adjustfield);
				os.output().width(widthLead);	os << "  ";
				os.output().width(widthField);	os << (fld);
				os.output().width(widthCode);   os << msFC.code()(fld);
				os.output().width(widthName);	os << name.at(0,20);
				os.output().width(widthRA);	os << mvRa(0.0).string(MVAngle::TIME,12);
				os.output().width(widthDec);	os << mvDec.string(MVAngle::DIG2,11);
				os.output().width(widthType);
				os << MDirection::showType(mRaDec.getRefPtr()->getType());
				if (srcok) {os.output().width(widthSrc);	os << msFC.sourceId()(fld);}
				if (nVisPerField_.nelements()>fld) {
					os.output().setf(ios::right, ios::adjustfield);
					os.output().width(widthnVis);
					os << _msmd->nRows(MSMetaData::BOTH, fld);
					if (_listUnflaggedRowCount) {
                                          os.output().width(widthNUnflaggedRows);
                                          ostringstream xx;
                                          xx << std::fixed << setprecision(2) << std::right
                                             << _msmd->nUnflaggedRows(MSMetaData::BOTH, fld);
                                          os << xx.str();
                                        }
				}
				os << endl;
				if(fillRecord){
					Record fieldrec;
					fieldrec.define("name", name);
					fieldrec.define("code",msFC.code()(fld));
					MeasureHolder mh(mRaDec);
					Record dirrec;
					String err;
					mh.toRecord(err, dirrec);
					fieldrec.defineRecord("direction", dirrec);
					String fieldrecid=String("field_")+String::toString(fld);
					if(!outrec.isDefined(fieldrecid)){
						outrec.defineRecord(fieldrecid, fieldrec);
					}
				}
			} else {
				os << "Field "<<fld<<" not found in FIELD table"<<endl;
			}

		}

		//		os << "   (nVis = Total number of time/baseline visibilities per field) " << endl;

	}
	os << endl << LogIO::POST;
}

void MSSummary::listObservation (LogIO& os, Bool verbose) const
{
	// Make objects
	ROMSColumns msc(*pMS);
	const ROMSObservationColumns& msOC(msc.observation());

	if (msOC.project().nrow()<=0) {
		os << "The OBSERVATION table is empty" << endl;
	}
	else {
		os << "   Observer: " << msOC.observer()(0) << "  "
				<< "   Project: " << msOC.project()(0) << "  ";
		//v2os << "   Obs Date: " << msOC.obsDate()(0) << endl
		//     << "   Tel name: " << msOC.telescopeName()(0);
		if (msc.observation().telescopeName().nrow()>0) {
			os<<endl << "Observation: " << msc.observation().telescopeName()(0);
		}
		if (!verbose) os << "(" << msc.antenna().name().nrow() << " antennas)";
		os << endl << endl;

		if (msOC.project().nrow()>1) {
			// for version 2 of the MS
			// Line is	TelName ObsDate Observer Project
			Int widthLead =  2;
			Int widthTel  = 10;
			Int widthDate = 20;
			Int widthObs  = 15;
			Int widthProj = 15;
			os.output().setf(ios::left, ios::adjustfield);
			os.output().width(widthLead);	os << "  ";
			os.output().width(widthTel);	os << "Telescope";
			os.output().width(widthDate);	os << "Observation Date";
			os.output().width(widthObs);	os << "Observer";
			os.output().width(widthProj);	os << "Project";
			os << endl;

			for (uInt row=0;row<msOC.project().nrow();row++) {
				os.output().setf(ios::left, ios::adjustfield);
				os.output().width(widthLead);	os << "  ";
				os.output().width(widthTel);	os << msOC.telescopeName()(row);
				os.output().width(widthDate);	os << msOC.timeRange()(row);
				os.output().width(widthObs);	os << msOC.observer()(row);
				os.output().width(widthProj);	os << msOC.project()(row);
				os << endl;
			}
		}
	}
	os << LogIO::POST;
}

String formatTime(const Double time) {
	MVTime mvtime(Quantity(time, "s"));
	Time t=mvtime.getTime();
	ostringstream os;
	os << t;
	return os.str();
}

void MSSummary::listHistory (LogIO& os) const
{
	// Create a MS-history object
	ROMSHistoryColumns msHis(pMS->history());

	if (msHis.nrow()<=0) {
		os << "The HISTORY table is empty" << endl;
	}
	else {
		uInt nmessages = msHis.time().nrow();
		os << "History table entries: " << nmessages << endl << LogIO::POST;
		const ROScalarColumn<Double> &theTimes((msHis.time()));
		const ROScalarColumn<String> &messOrigin((msHis.origin()));
		const ROScalarColumn<String> &messString((msHis.message()));
		const ROScalarColumn<String> &messPriority((msHis.priority()));
		for (uInt i=0 ; i < nmessages; i++) {
			Quantity tmpq(theTimes(i), "s");
			MVTime mvtime(tmpq);
			Time messTime(mvtime.getTime());
			LogMessage::Priority itsPriority(LogMessage::DEBUGGING);
			if(messPriority(i) == "DEBUGGING"){
				itsPriority = LogMessage::DEBUGGING;
			} else if(messPriority(i) == "DEBUG2"){
				itsPriority = LogMessage::DEBUG2;
			} else if(messPriority(i) == "DEBUG1"){
				itsPriority = LogMessage::DEBUG1;
			} else if(messPriority(i) == "NORMAL5" || messPriority(i) == "INFO5"){
				itsPriority = LogMessage::NORMAL5;
			} else if(messPriority(i) == "NORMAL4" || messPriority(i) == "INFO4"){
				itsPriority = LogMessage::NORMAL4;
			} else if(messPriority(i) == "NORMAL3" || messPriority(i) == "INFO3"){
				itsPriority = LogMessage::NORMAL3;
			} else if(messPriority(i) == "NORMAL2" || messPriority(i) == "INFO2"){
				itsPriority = LogMessage::NORMAL2;
			} else if(messPriority(i) == "NORMAL1" || messPriority(i) == "INFO1"){
				itsPriority = LogMessage::NORMAL1;
			} else if(messPriority(i) == "NORMAL" || messPriority(i) == "INFO"){
				itsPriority = LogMessage::NORMAL;
			} else if(messPriority(i) == "WARN"){
				itsPriority = LogMessage::WARN;
			} else if(messPriority(i) == "SEVERE"){
				itsPriority = LogMessage::SEVERE;
			}
			LogOrigin orhist(messOrigin(i));
			LogMessage histMessage(messString(i), orhist.taskName("listHistory"), itsPriority);
			histMessage.messageTime(messTime);
			os.post(histMessage);
		}
		os << LogIO::POST;
	}
}

void MSSummary::listSource (LogIO& os, Bool verbose) const
{

	// Check if optional SOURCE table is present:
	if (pMS->source().isNull()) {
		os << "The SOURCE table is absent: see the FIELD table" << endl;
		return;
	}

	// Create a MS-source-columns object
	ROMSSourceColumns msSC(pMS->source());

	// Are restFreq and sysvel present?
	Bool restFreqOK=pMS->source().tableDesc().isColumn("REST_FREQUENCY");
	Bool sysVelOK=pMS->source().tableDesc().isColumn("SYSVEL");

	if (msSC.name().nrow()<=0) {
		os << "The SOURCE table is empty: see the FIELD table" << endl;
	}
	else {

		if (verbose) {  // activated: CAS-3180
			os << "Sources: " << msSC.name().nrow() << endl;

			//  Line is	Time Name RA Dec SysVel
			Int widthLead =  2;
			Int widthSrc  =  5;
			//      Int widthTime = 15;
			Int widthName = 20;
			//      Int widthRA   = 14;
			//      Int widthDec  = 15;
			Int widthSpw  =  6;
			Int widthRF   = 15;
			Int widthVel  = 13;
			os.output().setf(ios::left, ios::adjustfield);
			os.output().width(widthLead);	os << "  ";
			//      os.output().width(widthTime);	os << "Time MidPt";
			os.output().width(widthSrc);	os << "ID";
			os.output().width(widthName);	os << "Name";
			//      os.output().width(widthRA);	os << "RA";
			//      os.output().width(widthDec);	os << "Decl";
			os.output().width(widthSpw);      os << "SpwId";
			if (restFreqOK) {
				os.output().width(widthRF);      os << "RestFreq(MHz)";
			}
			if (sysVelOK) {
				os.output().width(widthVel);     os << "SysVel(km/s)";
			}
			os << endl;

			os.output().precision(12);

			// Loop through rows
			for (uInt row=0; row<msSC.direction().nrow(); row++) {
				MDirection mRaDec=msSC.directionMeas()(row);
				MVAngle mvRa=mRaDec.getAngle().getValue()(0);
				MVAngle mvDec=mRaDec.getAngle().getValue()(1);
				String name=msSC.name()(row);
				if (name.length()>20) name.replace(19,1,"*");

				os.output().setf(ios::left, ios::adjustfield);
				os.output().width(widthLead);	os<< "  ";
				//	os.output().width(widthTime);
				//				os<< MVTime(msSC.time()(row)/86400.0).string();
				os.output().width(widthSrc);	os<< msSC.sourceId()(row);
				os.output().width(widthName);	os<< name.at(0,20);
				//	os.output().width(widthRA);	os<< mvRa(0.0).string(MVAngle::TIME,10);
				//	os.output().width(widthDec);	os<< mvDec.string(MVAngle::DIG2,10);
				os.output().width(widthSpw);
				Int spwid=msSC.spectralWindowId()(row);
				if (spwid<0) os<< "any";
				else os<<spwid;
				if (restFreqOK) {
					os.output().width(widthRF);
					if (msSC.restFrequency().isDefined(row)) {
						Vector<Double> restfreq=msSC.restFrequency()(row);
						if (restfreq.nelements()>0)
							os<< restfreq(0)/1.0e6;
						else
							os<< "-";
					}
					else
						os<<"-";
				}

				if (sysVelOK) {
					os.output().width(widthVel);
					if (msSC.sysvel().isDefined(row)) {
						Vector<Double> sysvel=msSC.sysvel()(row);
						if (sysvel.nelements()>0)
							os<< sysvel(0)/1.0e3;
						else
							os<< "-";
					}
					else
						os<<"-";
				}
				os << endl;
			}

			if (!restFreqOK)
				os << "  NB: No rest frequency information found in SOURCE table." << endl;
			if (!sysVelOK)
				os << "  NB: No systemic velocity information found in SOURCE table." << endl;
		}
	}
	os << LogIO::POST;
}


void MSSummary::listSpectralWindow (LogIO& os, Bool verbose) const
{
	// Create a MS-spwin-columns object
	ROMSSpWindowColumns msSWC(pMS->spectralWindow());

	if (verbose) {}	//null; always the same output

	if (msSWC.refFrequency().nrow()<=0) {
		os << "The SPECTRAL_WINDOW table is empty: see the FEED table" << endl;
	}
	else {
		os << "Spectral Windows: " << msSWC.refFrequency().nrow() << endl;
		// The 8 columns below are all in the SpWin subtable of Version 1 of
		// the MS definition.  For Version 2, some info will appear in other
		// subtables, as indicated.
		// Line is (V1): RefFreq RestFreq Molecule Trans'n Resol BW Numch Correls
		// V2 subtable:		SOURCE		 SOURCE		      POLARIZ'N

		Int widthLead	=  2;
		Int widthFreq	= 12;
		Int widthFrqNum	=  7;
		Int widthNumChan	=  8;

		os.output().setf(ios::left, ios::adjustfield);
		os.output().width(widthLead);	os << "  ";
		os.output().width(widthFreq);	os << "Ref.Freq";
		os.output().width(widthNumChan);	os << "#Chans";
		os.output().width(widthFreq);	os << "Resolution";
		os.output().width(widthFreq);	os << "TotalBW";
		//    os.output().width(widthCorrTypes);os << "Correlations";
		os << endl;

		// For each row of the SpWin subtable, write the info
		for (uInt row=0; row<msSWC.refFrequency().nrow(); row++) {
			os.output().setf(ios::left, ios::adjustfield);
			os.output().width(widthLead);		os << "  ";
			// 1st column: reference frequency
			os.output().width(widthFrqNum);
			os<< msSWC.refFrequency()(row)/1.0e6 <<"MHz  ";
			// 2nd column: rest frequency of a line, "continuum" otherwise
			//      if (msSWC.restFrequency()(row)<1e3) {
			//        os.output().width(widthFreq);		os << "continuum";
			//      } else {
			//        os.output().width(widthFrqNum);
			//        os<< msSWC.restFrequency()(row)/1.0e6<<"MHz  ";
			//      }
			// 3rd column: molecule formula
			//os.output().width(widthMol);		os << msSWC.molecule()(row);
			// 4th column: transition designation
			//os.output().width(widthTrans);		os << msSWC.transition()(row);
			// 5th column: number of channels in the spectral window
			os.output().width(widthNumChan);		os << msSWC.numChan()(row);
			// 6th column: channel resolution
			os.output().width(widthFrqNum);
			os << msSWC.resolution()(row)(IPosition(1,0))/1000<<"kHz  ";
			// 7th column: total bandwidth of the spectral window
			os.output().width(widthFrqNum);
			os<< msSWC.totalBandwidth()(row)/1000<<"kHz  ";
			// 8th column: the correlation type(s)
			//      for (uInt i=0; i<msSWC.corrType()(row).nelements(); i++) {
			//	os.output().width(widthCorrType);
			//	Int index = msSWC.corrType()(row)(IPosition(1,i));
			//	os << Stokes::name(Stokes::type(index));
			//      }
			os << endl;
		}
	}
	os << LogIO::POST;
}

void MSSummary::getSpectralWindowInfo(Record& outRec) const
{
	/* This method will return information about the spectral windows
     in a MS that are used */

	// Create a MS-spwin-columns object
	ROMSSpWindowColumns msSWC(pMS->spectralWindow());
	// Create a MS-data_desc-columns object
	ROMSDataDescColumns msDDC(pMS->dataDescription());
	// Create a MS-polin-columns object
	ROMSPolarizationColumns msPOLC(pMS->polarization());

	if (msDDC.nrow()<=0 or msSWC.nrow()<=0) {
		//The DATA_DESCRIPTION or SPECTRAL_WINDOW table is empty
		return;
	}

	// Determine the data_desc_ids present in the main table
	MSRange msr(*pMS);

	Vector<Int> ddId = msr.range(MSS::DATA_DESC_ID).asArrayInt(RecordFieldId(0));
	Vector<uInt> uddId(ddId.nelements());

	for (uInt i=0; i<ddId.nelements(); i++) uddId(i)=ddId(i);
	// now get the corresponding spectral windows and pol setups
	Vector<Int> spwIds = msDDC.spectralWindowId().getColumnCells(uddId);
	Vector<Int> polIds = msDDC.polarizationId().getColumnCells(uddId);
	//const Int option=Sort::HeapSort | Sort::NoDuplicates;
	//const Sort::Order order=Sort::Ascending;
	//Int nSpw=GenSort<Int>::sort (spwIds, order, option);

	if (ddId.nelements()>0) {
		// For each row of the DataDesc subtable, write the info
		for (uInt i=0; i<ddId.nelements(); i++) {
			Int dd=ddId(i);
			Int pol=polIds(i);
			Int spw = msDDC.spectralWindowId()(dd);

			Record ddRec;
			ddRec.define("SpectralWindowId",spw);
			ddRec.define("NumChan",msSWC.numChan()(spw));
			ddRec.define("Frame", msSWC.refFrequencyMeas()(spw).getRefString());
			ddRec.define("Chan1Freq", msSWC.chanFreq()(spw)(IPosition(1,0)));
			ddRec.define("ChanWidth", msSWC.chanWidth()(spw)(IPosition(1,0)));
			ddRec.define("TotalWidth", msSWC.totalBandwidth()(spw));
			ddRec.define("RefFreq", msSWC.refFrequency()(spw));
			ddRec.define("PolId", polIds(i));
			ddRec.define("NumCorr", msPOLC.numCorr()(pol));

			outRec.defineRecord(String::toString(dd), ddRec);
		}
	}
}


void MSSummary::listPolarization (LogIO& os, Bool verbose) const
{
	// Create a MS-pol-columns object
	ROMSPolarizationColumns msPolC(pMS->polarization());

	if (verbose) {}	//null; always the same output

	uInt nRow = pMS->polarization().nrow();
	if (nRow<=0) {
		os << "The POLARIZATION table is empty: see the FEED table" << endl;
	}
	else {
		os << "Polarization setups: " << nRow << endl;

		// Define the column widths
		Int widthLead	=  2;
		Int widthCorrTypes	= msPolC.corrType()(0).nelements()*4;
		Int widthCorrType	=  4;

		// Write the column headers
		os.output().setf(ios::left, ios::adjustfield);
		os.output().width(widthLead);	os << "  ";
		os.output().width(widthCorrTypes); os << "Correlations";
		os << endl;

		// For each row of the Pol subtable, write the info
		for (uInt row=0; row<nRow; row++) {
			os.output().setf(ios::left, ios::adjustfield);
			os.output().width(widthLead);		os << "  ";
			// 8th column: the correlation type(s)
			for (uInt i=0; i<msPolC.corrType()(row).nelements(); i++) {
				os.output().width(widthCorrType);
				Int index = msPolC.corrType()(row)(IPosition(1,i));
				os << Stokes::name(Stokes::type(index));
			}
			os << endl;
		}
	}
	os << LogIO::POST;
}

void MSSummary::listSpectralAndPolInfo (LogIO& os, Bool verbose,
                                        Bool /*oneBased*/) const
{
	// Create a MS-spwin-columns object
	ROMSSpWindowColumns msSWC(pMS->spectralWindow());

	// Create a MS-pol-columns object
	ROMSPolarizationColumns msPolC(pMS->polarization());
	// Create a MS-data_desc-columns object
	ROMSDataDescColumns msDDC(pMS->dataDescription());


	if (verbose) {}	//null; always the same output

	if (msDDC.nrow()<=0) {
		os << "The DATA_DESCRIPTION table is empty: see the FEED table" << endl;
	}
	if (msSWC.nrow()<=0) {
		os << "The SPECTRAL_WINDOW table is empty: see the FEED table" << endl;
	}
	if (msPolC.nrow()<=0) {
		os << "The POLARIZATION table is empty: see the FEED table" << endl;
	}

	// determine the data_desc_ids present in the main table
	MSRange msr(*pMS);
	Vector<Int> ddId = msr.range(MSS::DATA_DESC_ID).asArrayInt(RecordFieldId(0));
	Vector<uInt> uddId(ddId.nelements());
	for (uInt i=0; i<ddId.nelements(); i++) uddId(i)=ddId(i);
	// now get the corresponding spectral windows and pol setups
	Vector<Int> spwIds = msDDC.spectralWindowId().getColumnCells(uddId);
	Vector<Int> polIds = msDDC.polarizationId().getColumnCells(uddId);
	const Int option=Sort::HeapSort | Sort::NoDuplicates;
	const Sort::Order order=Sort::Ascending;
	uInt nSpw = GenSort<Int>::sort (spwIds, order, option);
	Int nPol = GenSort<Int>::sort (polIds, order, option);

	if (ddId.nelements()>0) {
		os << "Spectral Windows: ";
		os << " ("<<nSpw<<" unique spectral windows and ";
		os << nPol << " unique polarization setups)"<<endl;

		vector<String> names = _msmd->getSpwNames();
		Int widthName = 5;
		for (
			vector<String>::const_iterator iter=names.begin();
			iter!=names.end(); iter++
		) {
			widthName = max(widthName, (Int)iter->size());
		}

		// Define the column widths
		Int widthLead	=  2;
		Int widthSpwId       =  7;
		Int widthFrame      =  6;
		Int widthFreq	= 12;
		Int widthFrqNum	= 12;
		Int widthNumChan	=  6;
		Int widthCorrTypes	= msPolC.corrType()(0).nelements()*4;
		Int widthCorrType	=  4;
		uInt widthBBCNo = 8;

		// Write the column headers
		os.output().setf(ios::left, ios::adjustfield);
		os.output().width(widthLead);	os << "  ";
		os.output().width(widthSpwId);	os << "SpwID  ";
		os.output().width(widthName);	os << "Name  ";
		os.output().setf(ios::right, ios::adjustfield);
		os.output().width(widthNumChan);	os << " #Chans" << " ";
		os.output().setf(ios::left, ios::adjustfield);
		os.output().width(widthFrame);  os << "  Frame";
		os.output().width(widthFreq);   os << "   Ch0(MHz)";
		os.output().width(widthFreq);	os << " ChanWid(kHz) ";
		os.output().width(widthFreq);	os << " TotBW(kHz)";
		os.output().width(widthFreq);	os << "CtrFreq(MHz) ";
		//		os.output().width(widthFreq);	os << "Ref(MHz)";
		Bool hasBBCNo = _msmd->hasBBCNo();
		if (hasBBCNo) {
			os.output().width(widthBBCNo);
			os << "BBC Num ";
		}
		os.output().width(widthCorrTypes);  os << " Corrs";
		os << endl;

		vector<uInt> nChans = _msmd->nChans();
		vector<QVD> chanFreqs = _msmd->getChanFreqs();
		vector<QVD> chanWidths = _msmd->getChanWidths();
 		vector<Quantity> centerFreqs = _msmd->getCenterFreqs();
		vector<Double> bandwidths = _msmd->getBandWidths();
		vector<uInt> bbcNo = hasBBCNo ? _msmd->getBBCNos() : vector<uInt>();

		os.output().precision(9);
		// order by spwid, not ddid, CAS-7376
		Vector<Int>::const_iterator iter = spwIds.begin();
		Vector<Int>::const_iterator end = spwIds.end();
		std::vector<std::set<uInt> > spwToDDID = _msmd->getSpwToDataDescriptionIDMap();
		std::set<uInt> uDDIDSet(uddId.begin(), uddId.end());
		vector<uInt> ddToPolID = _msmd->getDataDescIDToPolIDMap();
		while (iter != end) {
		// for (uInt i=0; i<ddId.nelements(); i++) {
			Int spw = *iter;
			std::set<uInt> ddids = spwToDDID[spw];
			std::set<uInt>::const_iterator diter = ddids.begin();
			std::set<uInt>::const_iterator dend = ddids.end();
			while (diter != dend) {
				uInt dd = *diter;
				if (uDDIDSet.find(dd) == uDDIDSet.end()) {
					// data description ID not in main table, so not reported here
					continue;
				}
				Int pol = ddToPolID[dd];
				os.output().setf(ios::left, ios::adjustfield);
				os.output().width(widthLead);		os << "  ";
				// 1th column: Spectral Window Id
				os.output().width(widthSpwId); os << (spw);
				// 2nd column: SPW name
				os.output().width(widthName);
				os << names[spw] << " ";

				// 3rd column: number of channels in the spectral window
				os.output().setf(ios::right, ios::adjustfield);
				os.output().width(widthNumChan);
				os << nChans[spw] << " ";
				// 4th column: Reference Frame info
				// os.output().setf(ios::left, ios::adjustfield);
				os.output().width(widthFrame);
				os<< msSWC.refFrequencyMeas()(spw).getRefString();
				// 5th column: Chan 1 freq (may be at high freq end of band!)
				os.output().setf(ios::fixed);
				os.output().precision(3);
				os.output().width(widthFrqNum);
				os<< chanFreqs[spw].getValue("MHz")[0];
				// 6th column: channel resolution
				os.output().width(widthFrqNum+2);
				os << chanWidths[spw].getValue("kHz")[0];
				// 7th column: total bandwidth of the spectral window
				os.output().width(widthFrqNum);
				os.output().precision(1);
				os<< bandwidths[spw]/1000;
				os.output().width(widthFrqNum);
				os.output().precision(4);
				os << centerFreqs[spw].getValue("MHz") << " ";
				if (hasBBCNo) {
					os.output().width(widthBBCNo);
					os<< bbcNo[spw];
				}
				// 8th column: reference frequency
				//			os.output().width(widthFrqNum);
				//			os<< msSWC.refFrequency()(spw)/1.0e6;
				// 9th column: the correlation type(s)
				for (uInt j=0; j<msPolC.corrType()(pol).nelements(); j++) {
					os.output().width(widthCorrType);
					Int index = msPolC.corrType()(pol)(IPosition(1,j));
					os << Stokes::name(Stokes::type(index));
				}
				++diter;
			}
			os << endl;
			++iter;
		}
	}
	os << LogIO::POST;
}


void MSSummary::listSysCal (LogIO& os, Bool verbose) const
{
	// Check for existence of optional SYSCAL table:
	if (pMS->sysCal().isNull()) {
		os << "The SYSCAL table is absent" << endl;
		return;
	}

	// Do nothing in terse mode
	if (verbose) {

		// Create a MS-syscal-columns object
		ROMSSysCalColumns msSCC(pMS->sysCal());

		if (msSCC.tsys().nrow()<=0) {
			os << "The SYSCAL table is empty" << endl;
		}
		else {
			os << "SysCal entries: " << msSCC.tsys().nrow() << endl;
			os << "   The average Tsys for all data is "
					<< sum(msSCC.tsys()(0))/msSCC.tsys()(0).nelements() << " K" << endl;
		}
	}
	os << LogIO::POST;
}


void MSSummary::listWeather (LogIO& os, Bool verbose) const
{
	// Check for existence of optional WEATHER table:
	if (pMS->weather().isNull()) {
		os << "The WEATHER table is absent" << endl;
		return;
	}

	// Do nothing in terse mode
	if (verbose) {

		// Create a MS-weather-columns object
		ROMSWeatherColumns msWC(pMS->weather());

		if (msWC.H2O().nrow()<=0) {
			os << "The WEATHER table is empty" << endl;
		}
		else {
			os << "Weather entries: " << msWC.H2O().nrow() << endl;
			os << "   Average H2O column density = " << msWC.H2O()(0)
					<< " m**-2      Average air temperature = "
					<< msWC.temperature()(0) << " K" << endl;
		}
	}
	os<< LogIO::POST;
}


void MSSummary::listTables (LogIO& os, Bool verbose) const
{
	// Get nrows for each table (=-1 if table absent)
	Vector<Int> tableRows(18);
	tableRows(0) = nrow();
	tableRows(1) = pMS->antenna().nrow();
	tableRows(2) = pMS->dataDescription().nrow();
	tableRows(3) = (pMS->doppler().isNull() ? -1 : (Int)pMS->doppler().nrow());
	tableRows(4) = pMS->feed().nrow();
	tableRows(5) = pMS->field().nrow();
	tableRows(6) = pMS->flagCmd().nrow();
	tableRows(7) = (pMS->freqOffset().isNull() ? -1 : (Int)pMS->freqOffset().nrow());
	tableRows(8) = pMS->history().nrow();
	tableRows(9) = pMS->observation().nrow();
	tableRows(10) = pMS->pointing().nrow();
	tableRows(11) = pMS->polarization().nrow();
	tableRows(12) = pMS->processor().nrow();
	tableRows(13) = (pMS->source().isNull() ? -1 : (Int)pMS->source().nrow());
	tableRows(14) = pMS->spectralWindow().nrow();
	tableRows(15) = pMS->state().nrow();
	tableRows(16) = (pMS->sysCal().isNull() ? -1 : (Int)pMS->sysCal().nrow());
	tableRows(17) = (pMS->weather().isNull() ? -1 : (Int)pMS->weather().nrow());

	Vector<String> rowStrings(18), tableStrings(18);
	rowStrings = " rows";
	tableStrings(0) = "MAIN";
	tableStrings(1) = "ANTENNA";
	tableStrings(2) = "DATA_DESCRIPTION";
	tableStrings(3) = "DOPPLER";
	tableStrings(4) = "FEED";
	tableStrings(5) = "FIELD";
	tableStrings(6) = "FLAG_CMD";
	tableStrings(7) = "FREQ_OFFSET";
	tableStrings(8) = "HISTORY";
	tableStrings(9) = "OBSERVATION";
	tableStrings(10)= "POINTING";
	tableStrings(11)= "POLARIZATION";
	tableStrings(12)= "PROCESSOR";
	tableStrings(13)= "SOURCE";
	tableStrings(14)= "SPECTRAL_WINDOW";
	tableStrings(15)= "STATE";
	tableStrings(16)= "SYSCAL";
	tableStrings(17)= "WEATHER";

	// Just to make things read better
	for (uInt i=0; i<18; i++) {
		if (tableRows(i)==1) rowStrings(i) = " row";
		// if table exists, but empty:
		if (tableRows(i)==0) {
			rowStrings(i) = " <empty>";
			if (tableStrings(i)=="SOURCE") rowStrings(i) += " (see FIELD)";
			if (tableStrings(i)=="SPECTRAL_WINDOW") rowStrings(i) += " (see FEED)";
		}
		// if table absent:
		if (tableRows(i)==-1) {
			rowStrings(i) = "<absent>";
			if (tableStrings(i)=="SOURCE") rowStrings(i) += " (see FIELD)";
		}
	}

	// This bit is a little messy, keeping track of the verbose and terse
	// forms of the output format, as well as the zero/nonzero tables
	// Do things on this side
	// whether verbose or not
	os << "Tables";
	if (!verbose) os << "(rows)";			os << ":";
	if (!verbose) os << "   (-1 = table absent)";
	os << endl;
	for (uInt i=0; i<18; i++) {
		if (verbose) {
			os.output().setf(ios::left, ios::adjustfield);
			os.output().width(3);
		}						os << "   ";
		if (verbose) {
			os.output().width(20);
		}						os << tableStrings(i);
		if (verbose && tableRows(i)>0) {
			os.output().setf(ios::right, ios::adjustfield);
			os.output().width(8);
		}
		if (!verbose) os << "(";
		if (!verbose || tableRows(i)>0)		os << tableRows(i);
		if (!verbose) os << ")";
		if (verbose) {
			os.output().setf(ios::left, ios::adjustfield);
			os.output().width(10);	os << rowStrings(i);
			os << endl;
		}
		else {if ((i%5)==0) os << endl;}
	}
	os << LogIO::POST;
}


//
// Clear all the formatting flags
//
void MSSummary::clearFlags(LogIO& os) const
{
	os.output().unsetf(ios::left);
	os.output().unsetf(ios::right);
	os.output().unsetf(ios::internal);

	os.output().unsetf(ios::dec);
	os.output().unsetf(ios::oct);
	os.output().unsetf(ios::hex);

	os.output().unsetf(ios::showbase | ios::showpos | ios::uppercase
			| ios::showpoint);

	os.output().unsetf(ios::scientific);
	os.output().unsetf(ios::fixed);

}



} //# NAMESPACE CASACORE - END

