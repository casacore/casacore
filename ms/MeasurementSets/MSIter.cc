//# MSIter.cc: Step through MeasurementSet by table
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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

#include <casacore/ms/MeasurementSets/MSIter.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MSSel/MSSpwIndex.h>
#include <casacore/measures/Measures.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

 
int MSInterval::comp(const void * obj1, const void * obj2) const
{
  double v1 = *(const Double*)obj1;
  double v2 = *(const Double*)obj2;
  // Initialize offset_p to first timestamp.
  // Subtract a bit to avoid rounding problems.
  // Note that a time is the middle of an interval; ideally half that width
  // should be subtracted, but we don't know the width.
  if (offset_p == 0.0) {
    offset_p = v2 - 0.01;
  }
  // Shortcut if values are equal.
  if (v1 == v2) return 0;

  // Avoid dividing by interval_p if it is very small.
  // It only takes a few / DBL_MIN to get inf,
  // and inf == inf, even if "few" differs.
  // (Specifying timeInterval = 0 leads to DBL_MAX, which might be the opposite
  //  of what is wanted!  Avoiding underflow allows those who want no chunking
  //  by TIME to use an interval_p which is guaranteed to be small enough
  //  without having to read the INTERVAL column.)
  //
  // The 2.0 is a fudge factor.  The result of the comparison should probably
  // be cached.
  if(abs(interval_p) < 2.0 * DBL_MIN)
    return v1 < v2 ? -1 : 1;

  // The times are binned in bins with a width of interval_p.
  double t1 = floor((v1 - offset_p) / interval_p);
  double t2 = floor((v2 - offset_p) / interval_p);

  return (t1==t2 ? 0 : (t1<t2 ? -1 : 1));
}


MSIter::MSIter():nMS_p(0),msc_p(0),allBeamOffsetsZero_p(True),
  timeComp_p(0) {}

MSIter::MSIter(const MeasurementSet& ms,
	       const Block<Int>& sortColumns,
	       Double timeInterval,
	       Bool addDefaultSortColumns)
: msc_p(0),curMS_p(0),lastMS_p(-1),interval_p(timeInterval),
  allBeamOffsetsZero_p(True),
  timeComp_p(0)
{
  bms_p.resize(1); 
  bms_p[0]=ms;
  construct(sortColumns,addDefaultSortColumns);
}

MSIter::MSIter(const Block<MeasurementSet>& mss,
	       const Block<Int>& sortColumns,
	       Double timeInterval,
	       Bool addDefaultSortColumns)
: bms_p(mss),msc_p(0),curMS_p(0),lastMS_p(-1),interval_p(timeInterval),
  timeComp_p(0)
{
  construct(sortColumns,addDefaultSortColumns);
}

Bool MSIter::isSubSet (const Vector<uInt>& r1, const Vector<uInt>& r2) {
  Int n1 = r1.nelements();
  Int n2 = r2.nelements();
  if (n1==0) return True;
  if (n2<n1) return False;
  Bool freeR1, freeR2;
  const uInt* p1=r1.getStorage(freeR1);
  const uInt* p2=r2.getStorage(freeR2);
  Int i,j;
  for (i=0,j=0; i<n1 && j<n2; i++) {
    while (p1[i]!=p2[j++] && j<n2) {}
  }
  Bool ok=(j<n2 || (i==n1 && p1[n1-1]==p2[n2-1]));
  r1.freeStorage(p1,freeR1);
  r2.freeStorage(p2,freeR2);
  return ok;
}

void MSIter::construct(const Block<Int>& sortColumns, 
		       Bool addDefaultSortColumns)
{
  This = (MSIter*)this; 
  nMS_p=bms_p.nelements();
  if (nMS_p==0) throw(AipsError("MSIter::construct -  No input MeasurementSets"));
  for (Int i=0; i<nMS_p; i++) {
    if (bms_p[i].nrow()==0) {
      throw(AipsError("MSIter::construct - Input MeasurementSet.has zero selected rows"));
    }
  }
  tabIter_p.resize(nMS_p);
  tabIterAtStart_p.resize(nMS_p);
  // 'sort out' the sort orders
  // We normally require the table to be sorted on ARRAY_ID and FIELD_ID,
  // DATA_DESC_ID and TIME for the correct operation of the
  // VisibilityIterator (it needs to know when any of these changes to
  // be able to give the correct coordinates with the data)
  // If these columns are not explicitly sorted on, they will be added
  // BEFORE any others, unless addDefaultSortColumns=False

  Block<Int> cols; 
  // try to reuse the existing sorted table if we didn't specify
  // any sortColumns
  if (sortColumns.nelements()==0 && 
      bms_p[0].keywordSet().isDefined("SORT_COLUMNS")) {
    // note that we use the order of the first MS for all MS's
    Vector<String> colNames = bms_p[0].keywordSet().asArrayString("SORT_COLUMNS");
    uInt n=colNames.nelements();
    cols.resize(n);
    for (uInt i=0; i<n; i++) cols[i]=MS::columnType(colNames(i));
  } else {
    cols=sortColumns;
  }

  Bool timeSeen=False, arraySeen=False, ddSeen=False, fieldSeen=False;
  Int nCol=0;
  for (uInt i=0; i<cols.nelements(); i++) {
    if (cols[i]>0 && 
	cols[i]<MS::NUMBER_PREDEFINED_COLUMNS) {
      if (cols[i]==MS::ARRAY_ID && !arraySeen) { arraySeen=True; nCol++; }
      if (cols[i]==MS::FIELD_ID && !fieldSeen) { fieldSeen=True; nCol++; }
      if (cols[i]==MS::DATA_DESC_ID && !ddSeen) { ddSeen=True; nCol++; }
      if (cols[i]==MS::TIME && !timeSeen) { timeSeen=True; nCol++; }
    } else {
      throw(AipsError("MSIter() - invalid sort column"));
    }
  }
  Block<String> columns;
  
  Int iCol=0;
  if (addDefaultSortColumns) {
    columns.resize(cols.nelements()+4-nCol);
    if (!arraySeen) {
      // add array if it's not there
      columns[iCol++]=MS::columnName(MS::ARRAY_ID);
    }
    if (!fieldSeen) {
      // add field if it's not there
      columns[iCol++]=MS::columnName(MS::FIELD_ID);
    }
    if (!ddSeen) {
      // add dd if it's not there
      columns[iCol++]=MS::columnName(MS::DATA_DESC_ID);
    }
    if (!timeSeen) {
      // add time if it's not there
      columns[iCol++]=MS::columnName(MS::TIME);
      timeSeen = True;
    }
  } else {
    columns.resize(cols.nelements());
  }
  for (uInt i=0; i<cols.nelements(); i++) {
    columns[iCol++]=MS::columnName(MS::PredefinedColumns(cols[i]));
  }

  if (interval_p==0.0) {
    interval_p=DBL_MAX; // semi infinite
  } else {
    // assume that we want to sort on time if interval is set
    if (!timeSeen) {
      columns.resize(columns.nelements()+1);
      columns[iCol++]=MS::columnName(MS::TIME);
    }
  }
  
  // now find the time column and set the compare function
  Block<CountedPtr<BaseCompare> > objComp(columns.nelements());
  timeComp_p = 0;
  for (uInt i=0; i<columns.nelements(); i++) {
    if (columns[i]==MS::columnName(MS::TIME)) {
      timeComp_p = new MSInterval(interval_p);
      objComp[i] = timeComp_p;
    }
  }
  Block<Int> orders(columns.nelements(),TableIterator::Ascending);
  
  // Store the sorted table for future access if possible, 
  // reuse it if already there
  for (Int i=0; i<nMS_p; i++) {
    Bool useIn=False, store=False, useSorted=False;
    Table sorted;
    // check if we already have a sorted table consistent with the requested
    // sort order
    if (!bms_p[i].keywordSet().isDefined("SORT_COLUMNS") ||
	!bms_p[i].keywordSet().isDefined("SORTED_TABLE") ||
	bms_p[i].keywordSet().asArrayString("SORT_COLUMNS").nelements()!=
	columns.nelements() ||
	!allEQ(bms_p[i].keywordSet().asArrayString("SORT_COLUMNS"),
	       Vector<String>(columns))) {
      // if not, sort and store it (if possible)
      store=(bms_p[i].isWritable() && (bms_p[i].tableType() != Table::Memory));
    } else {
      sorted = bms_p[i].keywordSet().asTable("SORTED_TABLE");
      // if sorted table is smaller it can't be useful, remake it
      if (sorted.nrow() < bms_p[i].nrow()) store = bms_p[i].isWritable();
      else { 
	// if input is a sorted subset of the stored sorted table
	// we can use the input in the iterator
	if (isSubSet(bms_p[i].rowNumbers(),sorted.rowNumbers())) {
	  useIn=True;
	} else {
	  // check if #rows in input table is the same as the base table
	  // i.e., this is the entire table, if so, use sorted version instead
	  String anttab = bms_p[i].antenna().tableName(); // see comments below
	  Table base (anttab.erase(anttab.length()-8));
	  if (base.nrow()==bms_p[i].nrow()) {
	    useSorted = True;
	  } else {
	    store=bms_p[i].isWritable();
	  }
	}
      }
    }

    if (!useIn && !useSorted) {
      // we have to resort the input
      if (aips_debug) cout << "MSIter::construct - resorting table"<<endl;
      sorted = bms_p[i].sort(columns, Sort::Ascending, Sort::QuickSort);
    }
    
    if (store) {
	// We need to get the name of the base table to add a persistent
	// subtable (the ms used here might be a reference table)
	// There is no table function to get this, so we use the name of
	// the antenna subtable to get at it.
	String anttab = bms_p[i].antenna().tableName();
	sorted.rename(anttab.erase(anttab.length()-7)+"SORTED_TABLE",Table::New); 
	sorted.flush();
	bms_p[i].rwKeywordSet().defineTable("SORTED_TABLE",sorted);
	bms_p[i].rwKeywordSet().define("SORT_COLUMNS", Vector<String>(columns));
    }

    // create the iterator for each MS
    // at this stage either the input is sorted already or we are using
    // the sorted table, so the iterator can avoid sorting.
    if (useIn) {
      tabIter_p[i] = new TableIterator(bms_p[i],columns,objComp,orders,
				       TableIterator::NoSort);
    } else {
      tabIter_p[i] = new TableIterator(sorted,columns,objComp,orders,
				       TableIterator::NoSort);
    } 
    tabIterAtStart_p[i]=True;
  }
  setMSInfo();
  
}

MSIter::MSIter(const MSIter& other)
{
    operator=(other);
}

MSIter::~MSIter() 
{
  if (msc_p) delete msc_p;
  for (Int i=0; i<nMS_p; i++) delete tabIter_p[i];
}

MSIter& 
MSIter::operator=(const MSIter& other) 
{
  if (this==&other) return *this;
  This=(MSIter*)this;
  bms_p=other.bms_p;
  {for (Int i=0; i<nMS_p; i++) delete tabIter_p[i];}
  nMS_p=other.nMS_p;
  tabIter_p.resize(nMS_p);
  for (Int i=0; i<nMS_p; i++) {
    tabIter_p[i]=new TableIterator(*(other.tabIter_p[i]));
  }
  tabIterAtStart_p=other.tabIterAtStart_p;
  if (msc_p) delete msc_p;
  msc_p=static_cast<ROMSColumns*>(0);
  curMS_p=0;
  lastMS_p=-1;
  interval_p=other.interval_p;
  //  origin();
  return *this;
}


const MS& MSIter::ms(const uInt id) const {

  if(id < bms_p.nelements()){
    return bms_p[id];
  }
  else{
    return bms_p[curMS_p];
  }
  
}

void MSIter::setInterval(Double timeInterval)
{
  interval_p=timeInterval;
  if (timeComp_p) {
    timeComp_p->setInterval(timeInterval);
  }
}

void MSIter::origin()
{
  curMS_p=0;
  checkFeed_p=True;
  if (!tabIterAtStart_p[curMS_p]) tabIter_p[curMS_p]->reset();
  setState();
  newMS_p=newArray_p=newSpectralWindow_p=newField_p=newPolarizationId_p=
    newDataDescId_p=more_p=True;
}

MSIter & MSIter::operator++(int)
{
  if (!more_p) return *this;
  advance();
  return *this;
}

MSIter & MSIter::operator++()
{
  if (!more_p) return *this;
  advance();
  return *this;
}

void MSIter::advance()
{
  newMS_p=newArray_p=newSpectralWindow_p=newPolarizationId_p=
    newDataDescId_p=newField_p=checkFeed_p=False;
  tabIter_p[curMS_p]->next();
  tabIterAtStart_p[curMS_p]=False;
  
  if (tabIter_p[curMS_p]->pastEnd()) {
    if (++curMS_p >= nMS_p) {
      curMS_p--;
      more_p=False;
    }
  }
  if (more_p) setState();
}

void MSIter::setState()
{
  setMSInfo();
  if(newMS_p)
    checkFeed_p=True;
  curTable_p=tabIter_p[curMS_p]->table();
  colArray_p.attach(curTable_p,MS::columnName(MS::ARRAY_ID));
  colDataDesc_p.attach(curTable_p,MS::columnName(MS::DATA_DESC_ID));
  colField_p.attach(curTable_p,MS::columnName(MS::FIELD_ID));
  // msc_p is already defined here (it is set in setMSInfo)
  if(newMS_p)
    msc_p->antenna().mount().getColumn(antennaMounts_p,True);
  setDataDescInfo();
  setArrayInfo();
  setFeedInfo();
  setFieldInfo();

  // If time binning, update the MSInterval's offset to account for glitches.
  // For example, if averaging to 5s and the input is
  //   TIME  STATE_ID  INTERVAL
  //    0      0         1
  //    1      0         1
  //    2      1         1
  //    3      1         1
  //    4      1         1
  //    5      1         1
  //    6      1         1
  //    7      0         1
  //    8      0         1
  //    9      0         1
  //   10      0         1
  //   11      0         1
  //  we want the output to be
  //   TIME  STATE_ID  INTERVAL
  //    0.5    0         2
  //    4      1         5
  //    9      0         5
  //  not what we'd get without the glitch fix:
  //   TIME  STATE_ID  INTERVAL
  //    0.5    0         2
  //    3      1         3
  //    5.5    1         2
  //    8      0         3
  //   10.5    0         2
  //
  // Resetting the offset with each advance() might be too often, i.e. we might
  // need different spws to share the same offset.  But in testing resetting
  // with each advance produces results more consistent with expectations than
  // either not resetting at all or resetting only
  // if(colTime_p(0) - 0.02 > timeComp_p->getOffset()).
  //
  if(timeComp_p){
      timeComp_p->setOffset(0.0);
  }
}

const Vector<Double>& MSIter::frequency() const
{
  if (!freqCacheOK_p) {
    This->freqCacheOK_p=True;
    Int spw = curSpectralWindow_p;
    if (preselected_p) {
      msc_p->spectralWindow().chanFreq().
	getSlice(spw,Slicer(Slice(preselectedChanStart_p[spw],
				  preselectednChan_p[spw])),
		 This->frequency_p,True);
    } else {
      msc_p->spectralWindow().chanFreq().
	get(spw,This->frequency_p,True);
    }
  }
  return frequency_p;
}

const MFrequency& MSIter::frequency0() const
{
  // set the channel0 frequency measure
    This->frequency0_p=
      Vector<MFrequency>(msc_p->spectralWindow().
			 chanFreqMeas()(curSpectralWindow_p))(0);
    // get the reference frame out off the freq measure and set epoch measure.
    This->frequency0_p.getRefPtr()->getFrame().set(msc_p->timeMeas()(0));
  return frequency0_p;
}

const MFrequency& MSIter::restFrequency(Int line) const
{
  MFrequency freq;
  Int sourceId = msc_p->field().sourceId()(curField_p);
  if (!msc_p->source().restFrequency().isNull()) {
    if (line>=0 && line < msc_p->source().restFrequency()(sourceId).shape()(0))
      freq = Vector<MFrequency>(msc_p->source().
				restFrequencyMeas()(sourceId))(line);
  }
  This->restFrequency_p=freq;
  return restFrequency_p;
}

void MSIter::setMSInfo()
{
  newMS_p=(lastMS_p!=curMS_p);
  if (newMS_p) {
    lastMS_p=curMS_p;
    if (!tabIterAtStart_p[curMS_p]) tabIter_p[curMS_p]->reset();
    if (msc_p) delete msc_p;
    msc_p = new ROMSColumns(bms_p[curMS_p]);
    // check to see if we are attached to a 'reference MS' with a 
    // DATA column that is a selection of the original DATA
    const TableRecord & kws = (msc_p->data().isNull() ? 
			 msc_p->floatData().keywordSet() :
			 msc_p->data().keywordSet());
    preselected_p = kws.isDefined("CHANNEL_SELECTION");
    if (preselected_p) {
      // get the selection
      Matrix<Int> selection;
      kws.get("CHANNEL_SELECTION",selection);
      Int nSpw=selection.ncolumn();
      preselectedChanStart_p.resize(nSpw);
      preselectednChan_p.resize(nSpw);
      for (Int i=0; i<nSpw; i++) {
	preselectedChanStart_p[i]=selection(0,i);
	preselectednChan_p[i]=selection(1,i);
      }
    }

    // determine the reference frame position
    String observatory;
    if (msc_p->observation().nrow() > 0) {
      observatory = msc_p->observation().telescopeName()
	(msc_p->observationId()(0));
    }
    if (observatory.length() == 0 || 
	!MeasTable::Observatory(telescopePosition_p,observatory)) {
      // unknown observatory, use first antenna
      telescopePosition_p=msc_p->antenna().positionMeas()(0);
    }
    // get the reference frame out of the freq measure and set the
    // position measure.
    frequency0_p.getRefPtr()->getFrame().set(telescopePosition_p);

    // force updates
    lastSpectralWindow_p=-1;
    lastArray_p=-1;
    lastPolarizationId_p=-1;
    lastDataDescId_p=-1;
    lastField_p=-1;
  }
}

void MSIter::setArrayInfo()
{
  // Set the array info
  curArray_p=colArray_p(0);
  newArray_p=(lastArray_p!=curArray_p);
  if (newArray_p) {
    lastArray_p=curArray_p;
  }
}

void MSIter::setFeedInfo()
{ 
  // Setup CJones and the receptor angle
  
  // Time-dependent feed tables are not yet supported due to a lack of
  // real application. The values for the last time range will be used
  // if such a table is encountered.
  //
  // A reasonable way (plan) to implement the code for time-dependent feed
  // tables is outlined below
  //  1. Move the detection of the time dependent table into a separate
  //     method and call it each time the measurement set is changed 
  //  2. In this method build a vector of critical times when the feed
  //     information is changed
  //  3. Add a set method for MSIterval to be able to access this vector
  //     (using a pointer or reference to avoid unnecessary copying)
  //     and make sure that critical times are known to MSInterval before
  //     tabIter_p[curMS_p]->next() in MSIter::advance
  //  4. Change MSInterval::comp to break iteration (i.e. return -1 or +1)
  //     if any critical time lies in between  *obj1 and *obj2.
  //     A sorted vector of critical times can speed up the search.
  //  5. Add an additional condition to
  //      if (((!spwDepFeed_p) || spwId(i)==curSpectralWindow_p))
  //     and to
  //      if ((spwDepFeed_p && newSpectralWindow_p) || first)
  //     in the code below.
  //     A flag newTime_p similar to newSpectralWindow_p is needed for a
  //     better performance

  // Check for time dependence.
  Bool first=False;
  if (checkFeed_p) {
    Vector<Double> feedTimes=msc_p->feed().time().getColumn();
    Vector<Double> interval=msc_p->feed().interval().getColumn();
    // Assume time dependence
    timeDepFeed_p=True;
    // if all interval values are <= zero or very large, 
    // there is no time dependence
    if (allLE(interval,0.0)||allGE(interval,1.e9)) timeDepFeed_p=False;
    else { 
      // check if any antennas appear more than once
      // check for each spectral window and feed in turn..
      Block<String> cols(2); 
      cols[0]=MSFeed::columnName(MSFeed::SPECTRAL_WINDOW_ID);
      cols[1]=MSFeed::columnName(MSFeed::FEED_ID);      
      Bool unique = True;
      for (TableIterator tabIter(msc_p->feed().time().table(),cols);
	   !tabIter.pastEnd(); tabIter.next()) {
	ROMSFeedColumns msfc(MSFeed(tabIter.table()));
	// check if any antennas appear more than once
	Vector<Int> antennas=msfc.antennaId().getColumn();
	Int nRow=antennas.nelements();
	Int nUniq=GenSort<Int>::sort(antennas,Sort::Ascending,
				     Sort::HeapSort | Sort::NoDuplicates);
	if (nUniq!=nRow) unique=False;
      }
      timeDepFeed_p=!unique;
    }
    Vector<Int> spwId=msc_p->feed().spectralWindowId().getColumn();
    spwDepFeed_p = !(allEQ(spwId,-1));
    first=True;
    checkFeed_p = False;
    if (timeDepFeed_p) {
      LogIO os;
      os << LogIO::WARN << LogOrigin("MSIter","setFeedInfo")
	 <<" time dependent feed table encountered - not correctly handled "
	 <<" - continuing anyway"<< LogIO::POST;
    }
  }
  // assume there's no time dependence, if there is we'll end up using the
  // last entry.
  if ((spwDepFeed_p && newSpectralWindow_p) || first) {
    Vector<Int> antennaId=msc_p->feed().antennaId().getColumn();
    Vector<Int> feedId=msc_p->feed().feedId().getColumn();
    Int maxAntId=max(antennaId);
    Int maxFeedId=max(feedId);
    AlwaysAssert((maxAntId>=0 && maxFeedId>=0),AipsError);    
    CJones_p.resize(maxAntId+1,maxFeedId+1);
    Vector<Int> numRecept=msc_p->feed().numReceptors().getColumn();
    uInt maxNumReceptors=max(numRecept);
    if (maxNumReceptors>2)
        throw AipsError("Can't handle more than 2 receptors");    
    receptorAngles_p.resize(maxNumReceptors,maxAntId+1,maxFeedId+1);
    receptorAnglesFeed0_p.resize(maxNumReceptors,maxAntId+1);
    beamOffsets_p.resize(maxNumReceptors,maxAntId+1,maxFeedId+1);
    allBeamOffsetsZero_p=True; 
    Vector<Int> spwId=msc_p->feed().spectralWindowId().getColumn();
    const ArrayColumn<Double>& beamOffsetColumn=
	               msc_p->feed().beamOffset();
    DebugAssert(beamOffsetColumn.nrow()==spwId.nelements(),AipsError);
    
    for (uInt i=0; i<spwId.nelements(); i++) {
      if (((!spwDepFeed_p) || spwId(i)==curSpectralWindow_p)) {
	Int iAnt=antennaId(i);
	Int iFeed=feedId(i);
	if (maxNumReceptors==1) 
	  CJones_p(iAnt,iFeed)=SquareMatrix<Complex,2>
	    ((Matrix<Complex>(msc_p->feed().polResponse()(i)))(0,0));
        else 
	  CJones_p(iAnt,iFeed)=Matrix<Complex>(msc_p->feed().polResponse()(i));
	
	// Handle variable numRecept
	IPosition blc(1,0);
	IPosition trc(1,numRecept(i)-1);
	receptorAngles_p.xyPlane(iFeed).column(iAnt)(blc,trc)=
	  Vector<Double>(msc_p->feed().receptorAngle()(i))(blc,trc);

	for (uInt rcpt=0;rcpt<maxNumReceptors;++rcpt)
	     for (uInt j=0;j<2;++j) {
		  // do an explicit iteration because these matrices are
		  // small and an element by element iteration will be
		  // required anyway to check for non-zero elements
                  Double beamOffsetBuf=
			    beamOffsetColumn(i)(IPosition(2,j,rcpt));
		  if (fabs(beamOffsetBuf)>1e-10) 
	              allBeamOffsetsZero_p=False;
	          beamOffsets_p(rcpt,iAnt,iFeed)(j)=beamOffsetBuf;
             }
      }
    }

    // a bit ugly construction
    // to preserve the old interface (feed=0 only)
    receptorAnglesFeed0_p.resize();
    receptorAnglesFeed0_p=receptorAngles_p.xyPlane(0); 
    CJonesFeed0_p.resize();
    CJonesFeed0_p=CJones_p.column(0);
    //
  }
}

void MSIter::setDataDescInfo()
{
  curDataDescId_p = colDataDesc_p(0);
  curSpectralWindow_p = msc_p->dataDescription().spectralWindowId()
    (curDataDescId_p);
  curPolarizationId_p = msc_p->dataDescription().polarizationId()
    (curDataDescId_p);
  newDataDescId_p=(lastDataDescId_p!=curDataDescId_p);
  if (newDataDescId_p) lastDataDescId_p=curDataDescId_p;
  newSpectralWindow_p=(lastSpectralWindow_p!=curSpectralWindow_p);
  newPolarizationId_p=(lastPolarizationId_p!=curPolarizationId_p);
  if (newSpectralWindow_p) {
    lastSpectralWindow_p = curSpectralWindow_p;
    startChan_p= (preselected_p ? preselectedChanStart_p[curSpectralWindow_p] :
		  0);
    freqCacheOK_p=False;
  
  }
  if (newPolarizationId_p) {
    lastPolarizationId_p = curPolarizationId_p;
    polFrame_p=Circular;
    Int polType = Vector<Int>(msc_p->polarization().
			      corrType()(curPolarizationId_p))(0);
    if (polType>=Stokes::XX && polType<=Stokes::YY) polFrame_p=Linear;
  }
}

void MSIter::setFieldInfo()
{
  curField_p=colField_p(0);
  newField_p=(lastField_p!=curField_p);
  if (newField_p) {
    lastField_p = curField_p;
    This->phaseCenter_p=msc_p->field().phaseDirMeas(curField_p);

    // Retrieve field name
    curFieldName_p = msc_p->field().name()(curField_p);
    curSource_p=msc_p->field().sourceId()(curField_p);

    // Retrieve source name, if specified.
    curSourceName_p = "";
    if (curSource_p >= 0 && !msc_p->source().sourceId().isNull()) {
      Vector<Int> sourceId=msc_p->source().sourceId().getColumn();
      uInt i=0;
      Bool found=False;
      while (i < sourceId.nelements() && !found) {
	if (sourceId(i)==curSource_p) {
	  found=True;
	  curSourceName_p=msc_p->source().name()(i);
	}
	i++;
      }
    }
  }
}


void  MSIter::getSpwInFreqRange(Block<Vector<Int> >& spw, 
				Block<Vector<Int> >& start, 
				Block<Vector<Int> >& nchan, 
				Double freqStart, Double freqEnd, 
				Double freqStep){

  spw.resize(nMS_p, True, False);
  start.resize(nMS_p, True, False);
  nchan.resize(nMS_p, True, False);

  for (Int k=0; k < nMS_p; ++k){
    MSSpwIndex spwIn(bms_p[k].spectralWindow());
    
    spwIn.matchFrequencyRange(freqStart-0.5*freqStep, freqEnd+0.5*freqStep, spw[k], start[k], nchan[k]); 
    /*
    Vector<Float> freqlist(4);
    freqlist(0)=freqStart-freqStep;
    freqlist(1)=freqEnd+freqStep;
    freqlist(2)=freqStep;
    freqlist(3)=MSSpwIndex::MSSPW_UNITHZ;
    Int numSpec;
    spw[k].resize();
    spw[k]=spwIn.convertToSpwIndex(freqlist, numSpec);
    Vector<Int> retchanlist=
      spwIn.convertToChannelIndex(spw[k], freqlist, numSpec);
    cout << "retchanlist " << retchanlist << endl;
    if((retchanlist.nelements()%3) != 0){
      cout << "Error in getting channel out " << endl;
    }
    else{
      start[k].resize(spw[k].nelements());
      nchan[k].resize(spw[k].nelements());
      for (uInt j=0; j < retchanlist.nelements()/3; ++j){
	//convert to channe returns start, stop...change to start, nchan
	start[k][j]=retchanlist[j*3];
	nchan[k][j]=retchanlist[j*3+1]-retchanlist[j*3]+1;
      }

    }
    */


  }
}
} //# NAMESPACE CASACORE - END

