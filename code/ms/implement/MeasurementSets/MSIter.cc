//# NewMSIter.cc: Step through NewMeasurementSet by table
//# Copyright (C) 1996,1997,1998,1999,2000
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

#include <trial/MeasurementSets/NewMSIter.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables/TableIter.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Slicer.h>
#include <aips/MeasurementSets/NewMSColumns.h>
#include <aips/Measures.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/Stokes.h>
#include <aips/Tables/TableRecord.h>

Double NewMSInterval::interval_p;
Double NewMSInterval::offset_p;
 
Int NewMSInterval::compare(const void * obj1, const void * obj2)
{
    Double t1, t2;
    t1 = *(const Double*)obj1 - offset_p;
    t2 = *(const Double*)obj2 - offset_p;
    return ((Int(t1/interval_p)==Int(t2/interval_p)) ? 0 : (t1 < t2) ? -1 : 1);
}
 

NewMSIter::NewMSIter():nMS_p(0),msc_p(0) {}

NewMSIter::NewMSIter(const NewMeasurementSet& ms,
	       const Block<Int>& sortColumns,
	       Double timeInterval)
: msc_p(0),curMS_p(0),lastMS_p(-1),interval_p(timeInterval)
{
  bms_p.resize(1); 
  bms_p[0]=ms;
  construct(sortColumns);
}

NewMSIter::NewMSIter(const Block<NewMeasurementSet>& mss,
	       const Block<Int>& sortColumns,
	       Double timeInterval)
: bms_p(mss),msc_p(0),curMS_p(0),lastMS_p(-1),interval_p(timeInterval)
{
  construct(sortColumns);
}

// Determine if the numbers in r1 are a sorted subset of those in r2
Bool isSubSet (const Vector<uInt>& r1, const Vector<uInt>& r2) {
  Int n1 = r1.nelements();
  Int n2 = r2.nelements();
  if (n1==0) return True;
  if (n2<n1) return False;
  Bool freeR1, freeR2;
  const uInt* p1=r1.getStorage(freeR1);
  const uInt* p2=r2.getStorage(freeR2);
  Int i,j;
  for (i=0,j=0; i<n1 && j<n2; i++) {
    while (p1[i]!=p2[j++] && j<n2);
  }
  Bool ok=ToBool(j<n2 || (i==n1 && p1[n1-1]==p2[n2-1]));
  r1.freeStorage(p1,freeR1);
  r2.freeStorage(p2,freeR2);
  return ok;
}

void NewMSIter::construct(const Block<Int>& sortColumns)
{
  This = (NewMSIter*)this; 
  nMS_p=bms_p.nelements();
  if (nMS_p==0) throw(AipsError("NewMSIter::construct -  No input MeasurementSets"));
  for (Int i=0; i<nMS_p; i++) {
    if (bms_p[i].nrow()==0) {
      throw(AipsError("NewMSIter::construct - Input NewMeasurementSet.has zero selected rows"));
    }
  }
  tabIter_p.resize(nMS_p);
  tabIterAtStart_p.resize(nMS_p);
  // 'sort out' the sort orders
  // We require the table to be sorted on FIELD_ID,
  // DATA_DESC_ID and TIME for the correct operation of the
  // VisibilityIterator (it needs to know when any of these changes to
  // be able to give the correct coordinates with the data)
  // If these columns are not explicitly sorted on, they will be added
  // BEFORE any others.

  Block<Int> cols; 
  // try to reuse the existing sorted table if we didn't specify
  // any sortColumns
  if (sortColumns.nelements()==0 && 
      bms_p[0].keywordSet().isDefined("SORT_COLUMNS")) {
    // note that we use the order of the first MS for all MS's
    Vector<String> colNames = bms_p[0].keywordSet().asArrayString("SORT_COLUMNS");
    uInt n=colNames.nelements();
    cols.resize(n);
    for (uInt i=0; i<n; i++) cols[i]=NewMS::columnType(colNames(i));
  } else {
    cols=sortColumns;
  }

  Bool timeSeen=False, ddSeen=False, fieldSeen=False;
  Int nCol=0;
  for (uInt i=0; i<cols.nelements(); i++) {
    if (cols[i]>0 && 
	cols[i]<NewMS::NUMBER_PREDEFINED_COLUMNS) {
      if (cols[i]==NewMS::FIELD_ID) { fieldSeen=True; nCol++; }
      if (cols[i]==NewMS::DATA_DESC_ID) { ddSeen=True; nCol++; }
      if (cols[i]==NewMS::TIME) { timeSeen=True; nCol++; }
    } else {
      throw(AipsError("NewMSIter() - invalid sort column"));
    }
  }
  Block<String> columns(cols.nelements()+3-nCol);

  Int iCol=0;
  if (!fieldSeen) {
    // add field if it's not there
    columns[iCol++]=NewMS::columnName(NewMS::FIELD_ID);
  }
  if (!ddSeen) {
    // add dd if it's not there
    columns[iCol++]=NewMS::columnName(NewMS::DATA_DESC_ID);
  }
  if (!timeSeen) {
    // add time if it's not there
    columns[iCol++]=NewMS::columnName(NewMS::TIME);
  }
  for (uInt i=0; i<cols.nelements(); i++) {
    columns[iCol++]=NewMS::columnName(NewMS::PredefinedColumns(cols[i]));
  }
  if (interval_p==0.0) {
    interval_p=DBL_MAX; // semi infinite
  }
  NewMSInterval::setInterval(interval_p);
  // do not set the offset (all intervals are zero based so we can convert
  // times to intervals easily in other classes, e.g. TimeVarVisJones)
  // Due to flagging, intervals may not have data over their full range
  // anyway, so we don't really loose anything here.
  // (We could reintroduce offsets later, at some cost in evaluating
  // the VisEquation for solve() - keep full resolution until the current
  // VisJones is reached -> only one averaging step)

  // now find the time column and set the compare function
  PtrBlock<ObjCompareFunc*> objCompFuncs(columns.nelements());
  for (uInt i=0; i<columns.nelements(); i++) {
    if (columns[i]==NewMS::columnName(NewMS::TIME)) {
      objCompFuncs[i]=NewMSInterval::compare;
    } else {
      objCompFuncs[i]=static_cast<ObjCompareFunc*>(0);
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
      store=bms_p[i].isWritable();
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
	  Table base (anttab.before("/ANTENNA"));
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
      sorted = bms_p[i].sort(columns);
    }
    
    if (store) {
	// We need to get the name of the base table to add a persistent
	// subtable (the ms used here might be a reference table)
	// There is no table function to get this, so we use the name of
	// the antenna subtable to get at it.
	String anttab = bms_p[i].antenna().tableName();
	sorted.rename(anttab.before("ANTENNA")+"SORTED_TABLE",Table::New); 
	sorted.flush();
	bms_p[i].rwKeywordSet().defineTable("SORTED_TABLE",sorted);
	bms_p[i].rwKeywordSet().define("SORT_COLUMNS", Vector<String>(columns));
    }

    // create the iterator for each MS
    // at this stage either the input is sorted already or we are using
    // the sorted table, so the iterator can avoid sorting.
    if (useIn) {
      tabIter_p[i] = new TableIterator(bms_p[i],columns,objCompFuncs,orders,
				       TableIterator::NoSort);
    } else {
      tabIter_p[i] = new TableIterator(sorted,columns,objCompFuncs,orders,
				       TableIterator::NoSort);
    } 
    tabIterAtStart_p[i]=True;
  }
  setMSInfo();
  
}

NewMSIter::NewMSIter(const NewMSIter& other)
{
    operator=(other);
}

NewMSIter::~NewMSIter() 
{
  if (msc_p) delete msc_p;
  for (Int i=0; i<nMS_p; i++) delete tabIter_p[i];
}

NewMSIter& 
NewMSIter::operator=(const NewMSIter& other) 
{
  if (this==&other) return *this;
  This=(NewMSIter*)this;
  bms_p=other.bms_p;
  {for (Int i=0; i<nMS_p; i++) delete tabIter_p[i];}
  nMS_p=other.nMS_p;
  tabIter_p.resize(nMS_p);
  for (Int i=0; i<nMS_p; i++) {
    tabIter_p[i]=new TableIterator(*(other.tabIter_p[i]));
  }
  tabIterAtStart_p=other.tabIterAtStart_p;
  if (msc_p) delete msc_p;
  msc_p=static_cast<RONewMSColumns*>(0);
  curMS_p=0;
  lastMS_p=-1;
  interval_p=other.interval_p;
  //  origin();
  return *this;
}

void NewMSIter::setInterval(Double timeInterval)
{
  interval_p=timeInterval;
  NewMSInterval::setInterval(interval_p);
}

void NewMSIter::origin()
{
  curMS_p=0;
  NewMSInterval::setInterval(interval_p);
  if (!tabIterAtStart_p[curMS_p]) tabIter_p[curMS_p]->reset();
  setState();
  newMS_p=newSpectralWindow_p=newField_p=newPolarizationId_p=
    newDataDescId_p=more_p=checkFeed_p=True;
}

NewMSIter & NewMSIter::operator++(int)
{
  if (!more_p) return *this;
  advance();
  return *this;
}

NewMSIter & NewMSIter::operator++()
{
  if (!more_p) return *this;
  advance();
  return *this;
}

void NewMSIter::advance()
{
  newMS_p=newSpectralWindow_p=newPolarizationId_p=
    newDataDescId_p=newField_p=checkFeed_p=False;
  // make sure we've still got the right interval
  NewMSInterval::setInterval(interval_p);
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

void NewMSIter::setState()
{
  checkFeed_p = newMS_p;
  setMSInfo();
  curTable_p=tabIter_p[curMS_p]->table();
  colDataDesc_p.attach(curTable_p,NewMS::columnName(NewMS::DATA_DESC_ID));
  colField_p.attach(curTable_p,NewMS::columnName(NewMS::FIELD_ID));
  setDataDescInfo();
  setFeedInfo();
  setFieldInfo();
}

const Vector<Double>& NewMSIter::frequency() const
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

const MFrequency& NewMSIter::frequency0() const
{
  // set the channel0 frequency measure
    This->frequency0_p=
      Vector<MFrequency>(msc_p->spectralWindow().
			 chanFreqMeas()(curSpectralWindow_p))(0);
    // get the reference frame out off the freq measure and set epoch measure.
    This->frequency0_p.getRefPtr()->getFrame().set(msc_p->timeMeas()(0));
  return frequency0_p;
}

const MFrequency& NewMSIter::restFrequency(Int line) const
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

void NewMSIter::setMSInfo()
{
  newMS_p=ToBool(lastMS_p!=curMS_p);
  if (newMS_p) {
    lastMS_p=curMS_p;
    if (!tabIterAtStart_p[curMS_p]) tabIter_p[curMS_p]->reset();
    if (msc_p) delete msc_p;
    msc_p = new RONewMSColumns(bms_p[curMS_p]);
    // check to see if we are attached to a 'reference MS' with a 
    // DATA column that is a selection of the original DATA
    preselected_p = msc_p->data().keywordSet().isDefined("CHANNEL_SELECTION");
    if (preselected_p) {
      // get the selection
      Matrix<Int> selection;
      msc_p->data().keywordSet().get("CHANNEL_SELECTION",selection);
      Int nSpw=selection.ncolumn();
      preselectedChanStart_p.resize(nSpw);
      preselectednChan_p.resize(nSpw);
      for (Int i=0; i<nSpw; i++) {
	preselectedChanStart_p[i]=selection(0,i);
	preselectednChan_p[i]=selection(1,i);
      }
    }

    // determine the reference frame position
    String observatory = msc_p->observation().telescopeName()
      (msc_p->observationId()(0));
    if (!MeasTable::Observatory(telescopePosition_p,observatory)) {
      // unknown observatory, use first antenna
      telescopePosition_p=msc_p->antenna().positionMeas()(0);
    }
    // get the reference frame out of the freq measure and set the
    // position measure.
    frequency0_p.getRefPtr()->getFrame().set(telescopePosition_p);

    // force updates
    lastSpectralWindow_p=-1;
    lastPolarizationId_p=-1;
    lastDataDescId_p=-1;
    lastField_p=-1;
  }
}

void NewMSIter::setFeedInfo()
{ 

  // Setup CJones and the receptor angle: feed 0 on each antenna
  // is the feed used to get the information.
  // Check for time dependence
  Bool first=False;
  if (checkFeed_p) {
    Vector<Double> feedTimes=msc_p->feed().time().getColumn();
    timeDepFeed_p= (min(feedTimes) != max(feedTimes)); 
    Vector<Int> spwId=msc_p->feed().spectralWindowId().getColumn();
    spwDepFeed_p = !(allEQ(spwId,-1));
    first=True;
    checkFeed_p = False;
  }  
  if (!timeDepFeed_p) {
    // there's no time dependence.
    if ((spwDepFeed_p && newSpectralWindow_p) || first) {
      Vector<Int> antennaId=msc_p->feed().antennaId().getColumn();
      Vector<Int> feedId=msc_p->feed().feedId().getColumn();
      Int maxAnt=max(antennaId);
      CJones_p.resize(maxAnt+1);
      Int maxNumReceptors=max(msc_p->feed().numReceptors().getColumn());
      receptorAngle_p.resize(maxNumReceptors,maxAnt+1);
      Vector<Int> spwId=msc_p->feed().spectralWindowId().getColumn();
      for (uInt i=0; i<spwId.nelements(); i++) {
	if (((!spwDepFeed_p) || spwId(i)==curSpectralWindow_p)
	    &&(feedId(i)==0)) {
	  Int iAnt=antennaId(i);
	  CJones_p(iAnt)=Matrix<Complex>(msc_p->feed().polResponse()(i));
	  receptorAngle_p.column(iAnt)=
	    Vector<Double>(msc_p->feed().receptorAngle()(i));
	}
      }
    }	
  } else {
    throw(AipsError("NewMSIter::setFeedInfo() - time dependent feed table NYI"));
  }
}


void NewMSIter::setDataDescInfo()
{
  curDataDescId_p = colDataDesc_p(0);
  curSpectralWindow_p = msc_p->dataDescription().spectralWindowId()
    (curDataDescId_p);
  curPolarizationId_p = msc_p->dataDescription().polarizationId()
    (curDataDescId_p);
  newDataDescId_p=ToBool(lastDataDescId_p!=curDataDescId_p);
  if (newDataDescId_p) lastDataDescId_p=curDataDescId_p;
  newSpectralWindow_p=ToBool(lastSpectralWindow_p!=curSpectralWindow_p);
  newPolarizationId_p=ToBool(lastPolarizationId_p!=curPolarizationId_p);
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

void NewMSIter::setFieldInfo()
{
  curField_p=colField_p(0);
  newField_p=ToBool(lastField_p!=curField_p);
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
	};
	i++;
      };
    };
  }
}
