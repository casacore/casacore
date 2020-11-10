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


  MSIter::MSIter():nMS_p(0),storeSorted_p(False),prevFirstTimeStamp_p(-1.0), allBeamOffsetsZero_p(True)
{}

MSIter::MSIter(const MeasurementSet& ms,
               const std::vector<std::pair<String, CountedPtr<BaseCompare>>>& sortColumns) :
  timeInSort_p(false),
  arrayInSort_p(false),
  ddInSort_p(false),
  fieldInSort_p(false),
  curMS_p(0),
  lastMS_p(-1),
  newMS_p(true),
  newArrayId_p(true),
  newFieldId_p(true),
  newSpectralWindowId_p(true),
  newPolarizationId_p(true),
  storeSorted_p(false),
  interval_p(0), 
  prevFirstTimeStamp_p(-1.0),
  allBeamOffsetsZero_p(True),
  timeComp_p(0)
{
    This = (MSIter*)this;
    bms_p.resize(1);
    bms_p[0]=ms;
    construct(sortColumns);
}

MSIter::MSIter(const Block<MeasurementSet>& mss,
  const std::vector<std::pair<String, CountedPtr<BaseCompare>>>& sortColumns) :
  bms_p(mss),
  timeInSort_p(false),
  arrayInSort_p(false),
  ddInSort_p(false),
  fieldInSort_p(false),
  curMS_p(0),
  lastMS_p(-1),
  newMS_p(true),
  newArrayId_p(true),
  newFieldId_p(true),
  newSpectralWindowId_p(true),
  newPolarizationId_p(true),
  storeSorted_p(false),
  interval_p(0), 
  prevFirstTimeStamp_p(-1.0),
  allBeamOffsetsZero_p(True),
  timeComp_p(0)
{
    This = (MSIter*)this;
    construct(sortColumns);
}

void MSIter::construct(
  const std::vector<std::pair<String, CountedPtr<BaseCompare>>>& sortColumns)
{
    nMS_p=bms_p.nelements();
    if (nMS_p==0) throw(AipsError("MSIter::construct -  No input MeasurementSets"));
    for (size_t i=0; i<nMS_p; i++) {
        if (bms_p[i].nrow()==0) {
            throw(AipsError("MSIter::construct - Input MeasurementSet.has zero selected rows"));
        }
    }
    tabIter_p.resize(nMS_p);
    tabIterAtStart_p.resize(nMS_p);

    // Creating the sorting members to be pass to the TableIterator constructor
    Block<String> sortColumnNames;
    Block<CountedPtr<BaseCompare>> sortCompareFunctions;

    sortColumnNames.resize(sortColumns.size());
    sortCompareFunctions.resize(sortColumns.size());
    Block<Int> sortOrders(sortColumns.size(),TableIterator::Ascending);
    size_t iCol=0;
    for(auto element : sortColumns)
    {
        sortColumnNames[iCol] = element.first;
        sortCompareFunctions[iCol] = element.second;
        ++iCol;
    }

    // Create the table iterators
    for (size_t i=0; i<nMS_p; i++) {
        // create the iterator for each MS
        tabIter_p[i] = new TableIterator(bms_p[i],sortColumnNames,
                                         sortCompareFunctions,sortOrders);
        tabIterAtStart_p[i]=True;
    }
    setMSInfo();
}


MSIter::MSIter(const MeasurementSet& ms,
	       const Block<Int>& sortColumns,
	       Double timeInterval,
	       Bool addDefaultSortColumns,
	       Bool storeSorted)
: curMS_p(0),lastMS_p(-1),
  newMS_p(true),
  newArrayId_p(true),
  newFieldId_p(true),
  newSpectralWindowId_p(true),
  newPolarizationId_p(true),
  storeSorted_p(storeSorted),
  interval_p(timeInterval), prevFirstTimeStamp_p(-1.0),
  allBeamOffsetsZero_p(True)
{
  bms_p.resize(1);
  bms_p[0]=ms;
  construct(sortColumns,addDefaultSortColumns);
}

MSIter::MSIter(const Block<MeasurementSet>& mss,
	       const Block<Int>& sortColumns,
	       Double timeInterval,
	       Bool addDefaultSortColumns,
	       Bool storeSorted)
: bms_p(mss),curMS_p(0),lastMS_p(-1),
  newMS_p(true),
  newArrayId_p(true),
  newFieldId_p(true),
  newSpectralWindowId_p(true),
  newPolarizationId_p(true),
  storeSorted_p(storeSorted),
  interval_p(timeInterval), prevFirstTimeStamp_p(-1.0)
{
  construct(sortColumns,addDefaultSortColumns);
}

Bool MSIter::isSubSet (const Vector<rownr_t>& r1, const Vector<rownr_t>& r2) {
  size_t n1 = r1.nelements();
  size_t n2 = r2.nelements();
  if (n1==0) return True;
  if (n2<n1) return False;
  Bool freeR1, freeR2;
  const rownr_t* p1=r1.getStorage(freeR1);
  const rownr_t* p2=r2.getStorage(freeR2);
  size_t i,j;
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
  for (size_t i=0; i<nMS_p; i++) {
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
    size_t n=colNames.nelements();
    cols.resize(n);
    for (size_t i=0; i<n; i++) cols[i]=MS::columnType(colNames(i));
  } else {
    cols=sortColumns;
  }

  timeInSort_p=False, arrayInSort_p=False, ddInSort_p=False, fieldInSort_p=False;
  size_t nCol=0;
  for (size_t i=0; i<cols.nelements(); i++) {
    if (cols[i]>0 &&
	cols[i]<MS::NUMBER_PREDEFINED_COLUMNS) {
      if (cols[i]==MS::ARRAY_ID && !arrayInSort_p) { arrayInSort_p=True; nCol++; }
      if (cols[i]==MS::FIELD_ID && !fieldInSort_p) { fieldInSort_p=True; nCol++; }
      if (cols[i]==MS::DATA_DESC_ID && !ddInSort_p) { ddInSort_p=True; nCol++; }
      if (cols[i]==MS::TIME && !timeInSort_p) { timeInSort_p=True; nCol++; }
    } else {
      throw(AipsError("MSIter() - invalid sort column"));
    }
  }
  Block<String> columns;

  size_t iCol=0;
  if (addDefaultSortColumns) {
    columns.resize(cols.nelements()+4-nCol);
    if (!arrayInSort_p) {
      // add array if it's not there
      columns[iCol++]=MS::columnName(MS::ARRAY_ID);
    }
    if (!fieldInSort_p) {
      // add field if it's not there
      columns[iCol++]=MS::columnName(MS::FIELD_ID);
    }
    if (!ddInSort_p) {
      // add dd if it's not there
      columns[iCol++]=MS::columnName(MS::DATA_DESC_ID);
    }
    if (!timeInSort_p) {
      // add time if it's not there
      columns[iCol++]=MS::columnName(MS::TIME);
      timeInSort_p = True;
    }
  } else {
    columns.resize(cols.nelements());
  }
  for (size_t i=0; i<cols.nelements(); i++) {
    columns[iCol++]=MS::columnName(MS::PredefinedColumns(cols[i]));
  }

  if (interval_p==0.0) {
    interval_p=DBL_MAX; // semi infinite
  } else {
    // assume that we want to sort on time if interval is set
    if (!timeInSort_p) {
      columns.resize(columns.nelements()+1);
      columns[iCol++]=MS::columnName(MS::TIME);
    }
  }

  // now find the time column and set the compare function
  Block<CountedPtr<BaseCompare> > objComp(columns.nelements());
  for (size_t i=0; i<columns.nelements(); i++) {
    if (columns[i]==MS::columnName(MS::TIME)) {
      timeComp_p = new MSInterval(interval_p);
      objComp[i] = timeComp_p;
    }
  }
  Block<Int> orders(columns.nelements(),TableIterator::Ascending);

  // Store the sorted table for future access if possible,
  // reuse it if already there
  for (size_t i=0; i<nMS_p; i++) {
    Bool useIn=False, store=False, useSorted=False;
    Table sorted;
    // check if we already have a sorted table consistent with the requested
    // sort order
    if (!bms_p[i].keywordSet().isDefined("SORT_COLUMNS") ||
	!bms_p[i].keywordSet().isDefined("SORTED_TABLE") ||
	bms_p[i].keywordSet().asArrayString("SORT_COLUMNS").nelements()!=
	columns.nelements() ||
	!allEQ(bms_p[i].keywordSet().asArrayString("SORT_COLUMNS"),
	       Vector<String>(columns.begin(), columns.end()))) {
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

    // Only store if globally requested _and_ locally decided
    if (storeSorted_p && store) {
	// We need to get the name of the base table to add a persistent
	// subtable (the ms used here might be a reference table)
	// There is no table function to get this, so we use the name of
	// the antenna subtable to get at it.
	String anttab = bms_p[i].antenna().tableName();
	sorted.rename(anttab.erase(anttab.length()-7)+"SORTED_TABLE",Table::New);
	sorted.flush();
	bms_p[i].rwKeywordSet().defineTable("SORTED_TABLE",sorted);
	bms_p[i].rwKeywordSet().define("SORT_COLUMNS", Vector<String>(columns.begin(), columns.end()));
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
	: nMS_p(0), storeSorted_p(False), allBeamOffsetsZero_p(True)
{
  operator=(other);
}

MSIter::~MSIter()
{
  for (size_t i=0; i<nMS_p; i++) delete tabIter_p[i];
}

MSIter&
MSIter::operator=(const MSIter& other)
{
  if (this == &other) return *this;
  This = (MSIter*)this;
  bms_p = other.bms_p;
  for (size_t i =0 ; i < nMS_p; ++i) delete tabIter_p[i];
  nMS_p = other.nMS_p;
  tabIter_p.resize(nMS_p);
  for (size_t i = 0; i < nMS_p; ++i) {
    tabIter_p[i] = new TableIterator(*(other.tabIter_p[i]));
    tabIter_p[i]->copyState(*other.tabIter_p[i]);
  }
  tabIterAtStart_p = other.tabIterAtStart_p;
  curMS_p = other.curMS_p;
  lastMS_p = other.lastMS_p;
  msc_p = other.msc_p;
  curTable_p = tabIter_p[curMS_p]->table();
  curArrayIdFirst_p = other.curArrayIdFirst_p;
  lastArrayId_p = other.lastArrayId_p;
  curSourceIdFirst_p = other.curSourceIdFirst_p;
  curFieldNameFirst_p = other.curFieldNameFirst_p;
  curSourceNameFirst_p = other.curSourceNameFirst_p;
  curFieldIdFirst_p = other.curFieldIdFirst_p;
  lastFieldId_p = other.lastFieldId_p;
  curSpectralWindowIdFirst_p = other.curSpectralWindowIdFirst_p;
  lastSpectralWindowId_p = other.lastSpectralWindowId_p;
  curPolarizationId_p = other.curPolarizationId_p;
  lastPolarizationId_p = other.lastPolarizationId_p;
  curDataDescIdFirst_p = other.curDataDescIdFirst_p;
  lastDataDescId_p = other.lastDataDescId_p;
  more_p = other.more_p;
  newMS_p = other.newMS_p;
  newArrayId_p = other.newArrayId_p;
  newFieldId_p = other.newFieldId_p;
  newSpectralWindowId_p = other.newSpectralWindowId_p;
  newPolarizationId_p = other.newPolarizationId_p;
  newDataDescId_p = other.newDataDescId_p;
  timeDepFeed_p = other.timeDepFeed_p;
  spwDepFeed_p = other.spwDepFeed_p;
  checkFeed_p = other.checkFeed_p;
  storeSorted_p = other.storeSorted_p;
  interval_p = other.interval_p;
  colArray_p = other.colArray_p;
  colDataDesc_p = other.colDataDesc_p;
  colField_p = other.colField_p;
  phaseCenter_p = other.phaseCenter_p;
  receptorAnglesFeed0_p = other.receptorAnglesFeed0_p;
  receptorAngles_p = other.receptorAngles_p;
  CJonesFeed0_p = other.CJonesFeed0_p;
  CJones_p = other.CJones_p;
  antennaMounts_p = other.antennaMounts_p;
  beamOffsets_p = other.beamOffsets_p;
  allBeamOffsetsZero_p = other.allBeamOffsetsZero_p;
  polFrame_p = other.polFrame_p;
  freqCacheOK_p = other.freqCacheOK_p;
  frequency_p = other.frequency_p;
  frequency0_p = other.frequency0_p;
  restFrequency_p = other.restFrequency_p;
  telescopePosition_p = other.telescopePosition_p;
  timeComp_p.reset(new MSInterval(interval_p));
  prevFirstTimeStamp_p=other.prevFirstTimeStamp_p;
  return *this;
}

MSIter *
MSIter::clone() const {
  return new MSIter(*this);
}

const MS& MSIter::ms(const size_t id) const {

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
  newMS_p=newArrayId_p=newSpectralWindowId_p=newFieldId_p=newPolarizationId_p=
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
  newMS_p=newArrayId_p=newSpectralWindowId_p=newPolarizationId_p=
    newDataDescId_p=newFieldId_p=checkFeed_p=False;
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
    Int spw = curSpectralWindowIdFirst_p;
    msc_p->spectralWindow().chanFreq().
      get(spw,This->frequency_p,True);
  }
  return frequency_p;
}

const MFrequency& MSIter::frequency0() const
{
  // set the channel0 frequency measure
    This->frequency0_p=
      Vector<MFrequency>(msc_p->spectralWindow().
			 chanFreqMeas()(curSpectralWindowIdFirst_p))(0);
    // get the reference frame out off the freq measure and set epoch measure.
    This->frequency0_p.getRefPtr()->getFrame().set(msc_p->timeMeas()(0));
  return frequency0_p;
}

const MFrequency& MSIter::restFrequency(Int line) const
{
  MFrequency freq;
  Int sourceId = msc_p->field().sourceId()(curFieldIdFirst_p);
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
  newMS_p = (lastMS_p != (ssize_t)curMS_p);
  if (newMS_p) {
    lastMS_p = curMS_p;
    if (!tabIterAtStart_p[curMS_p]) tabIter_p[curMS_p]->reset();
    msc_p = new MSColumns(bms_p[curMS_p]);

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
    lastSpectralWindowId_p=-1;
    lastArrayId_p=-1;
    lastPolarizationId_p=-1;
    lastDataDescId_p=-1;
    lastFieldId_p=-1;
  }
}

void MSIter::setArrayInfo()
{
  // Set the array info
  curArrayIdFirst_p=colArray_p(0);

  if(arrayInSort_p)
    newArrayId_p=(lastArrayId_p!=curArrayIdFirst_p);
  //If array is not in the sorting columns, then the array id
  //can change between elements of the same iteration, so the safest
  //is to signal that it changes.
  else
    newArrayId_p = true;
  lastArrayId_p = curArrayIdFirst_p;
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
	MSFeedColumns msfc(MSFeed(tabIter.table()));
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
  if ((spwDepFeed_p && newSpectralWindowId_p) || first) {
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

    for (size_t i=0; i<spwId.nelements(); i++) {
      if (((!spwDepFeed_p) || spwId(i)==curSpectralWindowIdFirst_p)) {
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
  curDataDescIdFirst_p = colDataDesc_p(0);
  curSpectralWindowIdFirst_p = msc_p->dataDescription().spectralWindowId()
    (curDataDescIdFirst_p);
  curPolarizationId_p = msc_p->dataDescription().polarizationId()
    (curDataDescIdFirst_p);
  if(ddInSort_p)
  {
    newDataDescId_p=(lastDataDescId_p!=curDataDescIdFirst_p);
    newSpectralWindowId_p=(lastSpectralWindowId_p!=curSpectralWindowIdFirst_p);
    newPolarizationId_p=(lastPolarizationId_p!=curPolarizationId_p);
  }
  //If array is not in the sorting columns, then the DD, SPW, pol
  //can change between elements of the same iteration, so the safest
  //is to signal that it changes.
  else
  {
    newDataDescId_p = true;
    newSpectralWindowId_p = true;
    newPolarizationId_p = true;
  }
  lastDataDescId_p=curDataDescIdFirst_p;
  lastSpectralWindowId_p = curSpectralWindowIdFirst_p;
  lastPolarizationId_p = curPolarizationId_p;

  if (newSpectralWindowId_p)
    freqCacheOK_p=False;

  if (newPolarizationId_p) {
    polFrame_p=Circular;
    Int polType = Vector<Int>(msc_p->polarization().
			      corrType()(curPolarizationId_p))(0);
    if (polType>=Stokes::XX && polType<=Stokes::YY) polFrame_p=Linear;
  }
}

void MSIter::setFieldInfo()
{
  curFieldIdFirst_p=colField_p(0);
  if(fieldInSort_p)
    newFieldId_p=(lastFieldId_p!=curFieldIdFirst_p);
  //If array is not in the sorting columns, then the field id
  //can change between elements of the same iteration, so the safest
  //is to signal that it changes.
  else
    newFieldId_p = true;
  lastFieldId_p = curFieldIdFirst_p;
}

const String& MSIter::fieldName()  const {
  if(newFieldId_p)
    This->curFieldNameFirst_p = msc_p->field().name()(curFieldIdFirst_p);

  return curFieldNameFirst_p;
}

const String& MSIter::sourceName()  const {
  if(newFieldId_p){
    // Retrieve source name, if specified.
    This->curSourceNameFirst_p = "";
    if (curSourceIdFirst_p >= 0 && !msc_p->source().sourceId().isNull()) {
      Vector<Int> sourceId=msc_p->source().sourceId().getColumn();
      size_t i=0;
      Bool found=False;
      while (i < sourceId.nelements() && !found) {
        if (sourceId(i)==curSourceIdFirst_p) {
          found=True;
          This->curSourceNameFirst_p=msc_p->source().name()(i);
        }
        i++;
      }
    }
  }
  
  return curSourceNameFirst_p;
}
const MDirection& MSIter::phaseCenter() const {
  if(msc_p){
    Double firstTimeStamp=ScalarColumn<Double>(curTable_p, MS::columnName(MS::TIME)).get(0);
    if(newFieldId_p || (firstTimeStamp != prevFirstTimeStamp_p)){
      This->prevFirstTimeStamp_p=firstTimeStamp;
      This->phaseCenter_p=msc_p->field().phaseDirMeas(curFieldIdFirst_p, firstTimeStamp);
    }
  }
  return phaseCenter_p;
}
const MDirection MSIter::phaseCenter(const Int fldid, const Double timeStamp) const{
  if(msc_p)
    return msc_p->field().phaseDirMeas(fldid, timeStamp);
  return phaseCenter_p;
}
void  MSIter::getSpwInFreqRange(Block<Vector<Int> >& spw,
				Block<Vector<Int> >& start,
				Block<Vector<Int> >& nchan,
				Double freqStart, Double freqEnd,
				Double freqStep){

  spw.resize(nMS_p, True, False);
  start.resize(nMS_p, True, False);
  nchan.resize(nMS_p, True, False);

  for (size_t k=0; k < nMS_p; ++k){
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

// Report Name of slowest column that changes at end of current iteration
const String& MSIter::keyChange() const
{
  return tabIter_p[curMS_p]->keyChangeAtLastNext();
}

} //# NAMESPACE CASACORE - END

