//# VisibilityIterator.cc: Step through MeasurementEquation by visibility
//# Copyright (C) 1996,1997
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

#include <strstream.h>
#include <trial/MeasurementEquations/VisibilityIterator.h>
#include <trial/MeasurementEquations/VisBuffer.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/Measures.h>
#include <aips/Measures/Measure.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MVTime.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Containers/Block.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Tables/TableRecord.h>

#include <aips/Logging/LogMessage.h>
#include <aips/Logging/LogSink.h>

Double Interval::interval_p;
Double Interval::offset_p;

Int Interval::compare(const void * obj1, const void * obj2)
{
    Double t1, t2;
    t1 = *(const Double*)obj1 - offset_p;
    t2 = *(const Double*)obj2 - offset_p;
    return ((Int(t1/interval_p)==Int(t2/interval_p)) ? 0 : (t1 < t2) ? -1 : 1);
}

ROVisibilityIterator::ROVisibilityIterator() {}

// const of MS is cast away, but we don't actually change it.
// (just to share code between RO version and RW version of iterator)
ROVisibilityIterator::ROVisibilityIterator(const MeasurementSet &mset,
					   const Block<Int>& sortColumns,
					   Double timeInterval)
:ms((MeasurementSet&)mset), nChan_p(0),haveIter_p(False),
 first_pa_p(True),interval_p(timeInterval),
lastUT_p(0),curChanGroup_p(0),freqCacheOK_p(False),
preselected_p(False)
{
  This = (ROVisibilityIterator*)this; 
  Block<Int> sortCols(sortColumns);
  // sort on time if no sort order specified
  if (sortCols.nelements()==0) {
      sortCols.resize(1);
      sortCols[0]=MS::TIME;
  }
  Block<String> columns(sortCols.nelements());
  for (Int i=0; i<columns.nelements(); i++) {
      if (sortCols[i]>0 && 
	  sortCols[i]<MS::NUMBER_PREDEFINED_COLUMNS) {
		columns[i] = MS::columnName(
		    MS::PredefinedColumns(sortCols[i]));
	    } else {
		throw(AipsError("ROVisibilityIterator::origin() -"
				"invalid sort column"));
	    }
  }
  if (interval_p==0.0) {
    interval_p=DBL_MAX; // semi infinite
  }
//#  Double startTime;
  MSColumns msc(ms);
//#  msc.time().get(0,startTime);
  Interval::setInterval(interval_p);
  // do not set the offset (all intervals zero based so we can convert
  // times to intervals easily in other classes, e.g. TimeVarVisJones)
  // Due to flagging, intervals may not have data over their full range
  // anyway, so we don't really loose anything here.
//#  Interval::setOffset(startTime);
  // now find the time column and set the compare function
  PtrBlock<ObjCompareFunc*> objCompFuncs(sortCols.nelements());
  for (i=0; i<sortCols.nelements(); i++) {
      if (sortCols[i]==MS::TIME) {
	  objCompFuncs[i]=Interval::compare;
      } else {
	  objCompFuncs[i]=(ObjCompareFunc*)0;
      }
  }
  Block<Int> orders(sortCols.nelements()); 
  orders=TableIterator::DontCare;
  tabIter_p = TableIterator(ms,columns,objCompFuncs,orders);

  // check to see if we are attached to a 'reference MS' with a 
  // DATA column that is a selection from the original DATA
  preselected_p = msc.data().keywordSet().isDefined("CHANNEL_SELECTION");
  if (preselected_p) {
    // get the selection
    Matrix<Int> selection;
    msc.data().keywordSet().get("CHANNEL_SELECTION",selection);
    Int nSpw=selection.ncolumn();
    preselectedChanStart_p.resize(nSpw);
    preselectednChan_p.resize(nSpw);
    for (Int i=0; i<nSpw; i++) {
      preselectedChanStart_p[i]=selection(0,i);
      preselectednChan_p[i]=selection(1,i);
    }
  }

  // find out the polarization reference frame.
  // for now this is defined by the polarization of 
  // the first entry in the feed table. Circular is the default.
  Vector<String> polType=msc.feed().polarizationType()(0);
  polFrame_p=Circular;
  if (polType(0)=="X" || polType(0)=="Y") polFrame_p=Linear;
  // setup CJones and the receptor angle.
  // this assumes a single feed per antenna and an entry for each antenna.
  // also assumes there's no time dependence.
  Int arrayId=msc.arrayId()(0);
  Double time=msc.time()(0);
  Table& feed=ms.feed();
  //# following gives warnings - skip for now 
  //# Table selectedFeedTable=(feed(feed.col("ARRAY_ID")==arrayId &&
  //# feed.col("TIME")-feed.col("INTERVAL") < time)).
  //# sort("ANTENNA_ID");
  Table selectedFeedTable=feed.sort(MSFeed::columnName(MSFeed::ANTENNA_ID));
  ScalarColumn<Int> antennaId(selectedFeedTable,
			      MSFeed::columnName(MSFeed::ANTENNA_ID));
  Int nRow=selectedFeedTable.nrow();
  if (nRow>0) {
    Int maxAnt=antennaId(nRow-1);
    CJones_p.resize(maxAnt+1);
    receptorAngle_p.resize(maxAnt+1);
    ArrayColumn<Complex> polResponse(selectedFeedTable,
				     MSFeed::columnName(MSFeed::POL_RESPONSE));
    ArrayColumn<Double> receptorAngle(selectedFeedTable,
				  MSFeed::columnName(MSFeed::RECEPTOR_ANGLE));
    
    for (Int iRow=0; iRow<nRow; iRow++) {
      Int iAnt=antennaId(iRow);
      CJones_p(iAnt)=Matrix<Complex>(polResponse(iRow));
      Vector<Double> angle=receptorAngle(iRow);
      receptorAngle_p(iAnt)=angle(0);
    }
  }
}

ROVisibilityIterator::ROVisibilityIterator(const ROVisibilityIterator& other)
{
    operator=(other);
}

ROVisibilityIterator::~ROVisibilityIterator() {}

ROVisibilityIterator& 
ROVisibilityIterator::operator=(const ROVisibilityIterator& other) 
{
  This=(ROVisibilityIterator*)this;
  ms=other.ms;
  tabIter_p=other.tabIter_p;
  haveIter_p=other.haveIter_p;
  curTable_p=other.curTable_p;
  selTable_p=other.selTable_p;
  time_p.resize(other.time_p.nelements()); time_p=other.time_p;
  curFieldID_p=other.curFieldID_p;
  lastFieldID_p=other.lastFieldID_p;
  curSpectralWindow_p=other.curSpectralWindow_p;
  lastSpectralWindow_p=other.lastSpectralWindow_p;
  curChanGroup_p=other.curChanGroup_p;
  curNumChanGroup_p=other.curNumChanGroup_p;
  channelGroupSize_p=other.channelGroupSize_p;
  curNumRow_p=other.curNumRow_p;
  nChan_p=other.nChan_p;
  nPol_p=other.nPol_p;
  more_p=other.more_p;
  newFieldID_p=other.newFieldID_p;
  newSpectralWindow_p=other.newSpectralWindow_p;
  newChanGroup_p=other.newChanGroup_p;
  moreChunks_p=other.moreChunks_p;
  preselected_p=other.preselected_p;
  interval_p=other.interval_p;
  numChanGroup_p=other.numChanGroup_p;
  chanStart_p=other.chanStart_p;
  chanWidth_p=other.chanWidth_p;
  chanInc_p=other.chanInc_p;
  preselectedChanStart_p=other.preselectedChanStart_p;
  preselectednChan_p=other.preselectednChan_p;

  freqCacheOK_p=False;
  first_pa_p=True;
  lastUT_p=0;
  // copy the cached values of things we fill in the constructor
  mount_p=other.mount_p;
  CJones_p=other.CJones_p;
  receptorAngle_p=other.receptorAngle_p;
  polFrame_p=other.polFrame_p;

  // column access functions
  colSpectralWindow.reference(other.colSpectralWindow);
  colAntenna1.reference(other.colAntenna1);
  colAntenna2.reference(other.colAntenna2);
  colFieldID.reference(other.colFieldID);
  colChanFreq.reference(other.colChanFreq);
  colTime.reference(other.colTime);
  colVis.reference(other.colVis);
  colSigma.reference(other.colSigma);
  colWeight.reference(other.colWeight);
  colFlag.reference(other.colFlag);
  colFlagRow.reference(other.colFlagRow);
  colUVW.reference(other.colUVW);

  colDirection.reference(other.colDirection);
  colAntPos.reference(other.colAntPos);
  colMount.reference(other.colMount);

  return *this;
}

void ROVisibilityIterator::origin()
{
    if (!haveIter_p) originChunks();
    curChanGroup_p=0;
    newChanGroup_p=True;
    freqCacheOK_p=False;
    curStartRow_p=0;
    setSelTable();
    updateSlicer();
    more_p=ToBool(curChanGroup_p<curNumChanGroup_p);
    // invalidate any attached VisBuffer
    if (!vbStack_p.empty()) ((VisBuffer*)vbStack_p.top())->invalidate();
}

void ROVisibilityIterator::originChunks()
{
    // make sure we've still got the right interval
    Interval::setInterval(interval_p);
    if (!haveIter_p) {
	haveIter_p=True;
    } else {
	tabIter_p.reset();
    }
    lastSpectralWindow_p=-1;
    lastFieldID_p=-1;
    moreChunks_p=ToBool(!tabIter_p.pastEnd());
    setState();
    origin();
}

void ROVisibilityIterator::setInterval(Double timeInterval)
{
    interval_p=timeInterval;
    Interval::setInterval(interval_p);
}

void ROVisibilityIterator::advance()
{
  newChanGroup_p=newSpectralWindow_p=newFieldID_p=False;
  curStartRow_p=curEndRow_p+1;
  if (curStartRow_p>=curTableNumRow_p) {
    if (++curChanGroup_p >= curNumChanGroup_p) {
      curChanGroup_p--;
      more_p=False;
    } else {
      curStartRow_p=0;
      updateSlicer();
      newChanGroup_p=True;
      freqCacheOK_p=False;
    }
  }
  if (more_p) {
    setSelTable();
    // invalidate any attached VisBuffer
    if (!vbStack_p.empty()) ((VisBuffer*)vbStack_p.top())->invalidate();
  }
}

ROVisibilityIterator& ROVisibilityIterator::nextChunk()
{
  // make sure we've still got the right interval
  Interval::setInterval(interval_p);
  if (!tabIter_p.pastEnd()) tabIter_p.next();  
  moreChunks_p = ToBool(!tabIter_p.pastEnd());
  if (moreChunks_p) {
    setState();
    if (!vbStack_p.empty()) ((VisBuffer*)vbStack_p.top())->invalidate();
  }
  more_p=moreChunks_p;
  return *this;
}

void ROVisibilityIterator::setSelTable()
{
  // work out how many rows to return (all same time)
  for (curEndRow_p=curStartRow_p+1; curEndRow_p<curTableNumRow_p && 
  	 time_p(curEndRow_p)==time_p(curEndRow_p-1); 
       curEndRow_p++);
  curEndRow_p--;
  //## testing
  //##if (curStartRow_p==0) curEndRow_p=curTableNumRow_p/2; 
  //##else curEndRow_p=curTableNumRow_p-1;
  //## testing

  curNumRow_p=curEndRow_p-curStartRow_p+1;
  Vector<uInt> rows(curNumRow_p);
  indgen(rows.ac(),uInt(curStartRow_p));
  selTable_p=curTable_p(rows);
  // virtual call
  this->attachColumns();
}

void ROVisibilityIterator::setState()
{
  curTable_p = tabIter_p.table();
  curTableNumRow_p = curTable_p.nrow();
  // get the times for this iteration
  ScalarColumn<Double> colTime(curTable_p,MS::columnName(MS::TIME));
  time_p.resize(curTableNumRow_p); colTime.getColumn(time_p);
  curStartRow_p=0;
  setSelTable();
  curSpectralWindow_p = colSpectralWindow(0);
  newSpectralWindow_p = ToBool(curSpectralWindow_p != lastSpectralWindow_p);
  if ( newSpectralWindow_p ) {
    lastSpectralWindow_p = curSpectralWindow_p;
    nChan_p = colChanFreq(curSpectralWindow_p).nelements();
    nPol_p = colVis.shape(0)(0);
    if (numChanGroup_p.nelements()<= curSpectralWindow_p || 
	numChanGroup_p[curSpectralWindow_p] == 0) {
      // no selection set yet, set default = all
      selectChannel(1,0,nChan_p);
    }
    channelGroupSize_p=chanWidth_p[curSpectralWindow_p];
    curNumChanGroup_p=numChanGroup_p[curSpectralWindow_p];
    freqCacheOK_p=False;
  }
  curFieldID_p = colFieldID(0);
  newFieldID_p = ToBool( curFieldID_p != lastFieldID_p );
  if ( newFieldID_p ) { 
    lastFieldID_p = curFieldID_p;
    // update the phase center position 
    // (could go out of date if not always asked for)
    // set the Measure for the phase center
    phaseCenter_p=MS::directionMeasure(colDirection);
    phaseCenter();
  }
}

void ROVisibilityIterator::updateSlicer()
{
  // set the Slicer to get the selected part of spectrum out of the table
  Int spw=curSpectralWindow_p;
  Int start=chanStart_p[spw]+curChanGroup_p*chanInc_p[spw];
  if (preselected_p) start-=preselectedChanStart_p[spw];
  //  slicer_p=Slicer(Slice(),Slice(start,channelGroupSize_p));
  // above is slow, use IPositions instead..
  slicer_p=Slicer(IPosition(2,0,start),
		  IPosition(2,nPol_p,channelGroupSize_p));
  useSlicer_p=ToBool(channelGroupSize_p<nChan_p);
}

void ROVisibilityIterator::attachColumns()
{
  colSpectralWindow.attach(selTable_p,MS::columnName(MS::SPECTRAL_WINDOW_ID));
  colAntenna1.attach(selTable_p,MS::columnName(MS::ANTENNA1));
  colAntenna2.attach(selTable_p,MS::columnName(MS::ANTENNA2));
  colFieldID.attach(selTable_p,MS::columnName(MS::FIELD_ID));
  colChanFreq.attach(ms.spectralWindow(),
		     MSSpectralWindow::columnName(MSSpectralWindow::CHAN_FREQ));
  colTime.attach(selTable_p,MS::columnName(MS::TIME));
  colVis.attach(selTable_p,MS::columnName(MS::DATA));
  colUVW.attach(selTable_p,MS::columnName(MS::UVW));
  colFlag.attach(selTable_p,MS::columnName(MS::FLAG));
  colFlagRow.attach(selTable_p,MS::columnName(MS::FLAG_ROW));
  colSigma.attach(selTable_p,MS::columnName(MS::SIGMA));
  colWeight.attach(selTable_p,MS::columnName(MS::WEIGHT));
  colDirection.attach(ms.field(),MSField::columnName(MSField::PHASE_DIR));
  colAntPos.attach(ms.antenna(),MSAntenna::columnName(MSAntenna::POSITION));
  colMount.attach(ms.antenna(),MSAntenna::columnName(MSAntenna::MOUNT));
}

ROVisibilityIterator & ROVisibilityIterator::operator++(int)
{
  if (!more_p) return *this;
  advance();
  return *this;
}

ROVisibilityIterator & ROVisibilityIterator::operator++()
{
  if (!more_p) return *this;
  advance();
  return *this;
}

Vector<Int>& ROVisibilityIterator::antenna1(Vector<Int>& ant1) const
{
  ant1.resize(curNumRow_p);
  colAntenna1.getColumn(ant1);
  return ant1;
}

Vector<Int>& ROVisibilityIterator::antenna2(Vector<Int>& ant2) const
{
  ant2.resize(curNumRow_p);
  colAntenna2.getColumn(ant2);
  return ant2;
}

Vector<Int>& ROVisibilityIterator::channel(Vector<Int>& chan) const
{
  Int spw = curSpectralWindow_p;
  chan.resize(channelGroupSize_p);
  for (Int i=0; i<channelGroupSize_p; i++) {
    chan(i)=chanStart_p[spw]+curChanGroup_p*chanInc_p[spw]+i;
  }
  return chan;
}

Matrix<Bool>& ROVisibilityIterator::flag(Matrix<Bool>& flags) const
{
  if (useSlicer_p) colFlag.getColumn(slicer_p,This->flagCube_p,True);
  else colFlag.getColumn(This->flagCube_p,True);
  flags.resize(channelGroupSize_p,curNumRow_p);
  // need to optimize this...
  //for (Int row=0; row<curNumRow_p; row++) {
  //  for (Int chn=0; chn<channelGroupSize_p; chn++) {
  //    flags(chn,row)=flagCube(0,chn,row);
  //    for (Int pol=1; pol<nPol_p; pol++) {
  //	  flags(chn,row)|=flagCube(pol,chn,row);
  //    }
  //  }
  //}
  Bool deleteIt1;
  Bool deleteIt2;
  const Bool* pcube=This->flagCube_p.getStorage(deleteIt1);
  Bool* pflags=flags.getStorage(deleteIt2);
  for (Int row=0; row<curNumRow_p; row++) {
    for (Int chn=0; chn<channelGroupSize_p; chn++) {
      *pflags=*pcube++;
      for (Int pol=1; pol<nPol_p; pol++, pcube++) {
	*pflags = *pcube ? *pcube : *pflags;
      }
      pflags++;
    }
  }
  flagCube_p.freeStorage(pcube, deleteIt1);
  flags.putStorage(pflags, deleteIt2);
  return flags;
}

Vector<Bool>& ROVisibilityIterator::flagRow(Vector<Bool>& rowflags) const
{
  rowflags.resize(curNumRow_p);
  colFlagRow.getColumn(rowflags);
  return rowflags;
}

Vector<Double>& ROVisibilityIterator::frequency(Vector<Double>& freq) const
{
  if (!freqCacheOK_p) {
    This->freqCacheOK_p=True;
    Int spw = curSpectralWindow_p;
    This->frequency_p.resize(channelGroupSize_p);
    Vector<Double> chanFreq=colChanFreq(spw);
    for (Int i=0; i<channelGroupSize_p; i++) {
      This->frequency_p(i)=chanFreq(chanStart_p[spw]+
				    curChanGroup_p*chanInc_p[spw]+i);
    }
  }
  freq.resize(channelGroupSize_p);
  freq=frequency_p;
  return freq;
}

const MDirection& ROVisibilityIterator::phaseCenter() const
{
  if (newFieldID_p) {
    This->phaseCenter_p.set(MVDirection
			    (Vector<Double>(colDirection(curFieldID_p))));
  }
  return phaseCenter_p;
}

Vector<Double>& ROVisibilityIterator::time(Vector<Double>& t) const
{
  t.resize(curNumRow_p);
  colTime.getColumn(t); return t;
}

Matrix<CStokesVector>& 
ROVisibilityIterator::visibility(Matrix<CStokesVector>& vis) const
{
  // We do selection on the fly here and
  // in related member funtions (frequency, flag etc.)
  // get out spectrum from start to start+channelGroupSize-1
  Int spw = curSpectralWindow_p;
  if (useSlicer_p) colVis.getColumn(slicer_p,This->visCube_p,True);
  else colVis.getColumn(This->visCube_p,True);
  vis.resize(channelGroupSize_p,curNumRow_p);
  Bool deleteIt;
  Complex* pcube=This->visCube_p.getStorage(deleteIt);
  if (deleteIt) cerr<<"Problem in ROVisIter::visibility - deleteIt True"<<endl;
  // Here we cope in a limited way with cases where not all 4 
  // polarizations are present: if only 2, assume XX,YY or RR,LL
  // if only 1, assume it's an estimate of Stokes I (one of RR,LL,XX,YY)
  // The cross terms are zero filled in these cases.
  switch (nPol_p) {
  case 4: {
    for (Int row=0; row<curNumRow_p; row++) {
      for (Int chn=0; chn<channelGroupSize_p; chn++,pcube+=4) {
	vis(chn,row)=pcube;
      }
    }
    break;
  }
  case 2: {
    for (Int row=0; row<curNumRow_p; row++) {
      for (Int chn=0; chn<channelGroupSize_p; chn++,pcube+=2) {
	CStokesVector& v=vis(chn,row);
	v=Complex(0.,0.);
	v(0)=*pcube; 
	v(3)=*(pcube+1); 
      }
    }
    break;
  }
  case 1: {
    for (Int row=0; row<curNumRow_p; row++) {
      for (Int chn=0; chn<channelGroupSize_p; chn++,pcube++) {
	CStokesVector& v=vis(chn,row);
	v=Complex(0.,0.);
	v(0)=v(3)=*pcube; 
      }
    }
  } //# case 1
  } //# switch 
  return vis;
}

Vector<RigidVector<Double,3> >& 
ROVisibilityIterator::uvw(Vector<RigidVector<Double,3> >& uvwvec) const
{
    uvwvec.resize(curNumRow_p);
    colUVW.getColumn(This->uvwMat_p,True);
    // get a pointer to the raw storage for quick access
    Bool deleteIt;
    Double* pmat=This->uvwMat_p.getStorage(deleteIt);
    for (Int row=0; row<curNumRow_p; row++, pmat+=3) uvwvec(row)=pmat;
    return uvwvec;
}

// Fill in parallactic angle.
const Vector<Float>& ROVisibilityIterator::feed_pa(Double time) const
{
  LogMessage message(LogOrigin("ROVisibilityIterator","feed_pa"));
  // If this is the first call then set up the antenna locations
  if (first_pa_p) {
    This->first_pa_p=False;
    // this assumes a single (sub)array
    Table& anTab=(This->ms).antenna();
    This->nAnt_p=anTab.nrow();
    This->pa_p.resize(nAnt_p);
    This->mount_p.resize(nAnt_p);
    for (Int iant=0;iant<nAnt_p;iant++) {
      // check mount (for now we cope with alt-az and equatorial)
      This->mount_p(iant)=0; //default
      if (colMount(iant)=="alt-az") This->mount_p(iant)=0;
      if (colMount(iant)=="equatorial") {
	This->mount_p(iant)=1;
	This->pa_p(iant)=receptorAngle_p(iant);
      }		
    }
  }	

  // Now calculate Parallactic angle for this UT. To do this we find
  // the AzEl Directions of the phase center and of the (HA,Dec) pole
  // and then calculate the position angle between these two directions.

  // Absolute UT
  Double ut=time;

  if (ut!=lastUT_p) {
    This->lastUT_p=ut;

    // Set up the Epoch using the absolute MJD in seconds
    // get the Epoch reference from the column keyword
    //#    MEpoch mEpoch=MS::epochMeasure(colTime);
    //#     now set the value
    //#    mEpoch.set(MVEpoch(Quantity(ut, "s")));
    //# Note the above fails for VLA data with TAI epoch, the resulting
    //# pa's are wrong (by much more than UTC-TAI), we force UTC here 
    //# for the moment
    MEpoch mEpoch(Quantity(ut, "s"), MEpoch::Ref(MEpoch::UTC));

    // Set up the MPosition for first antenna. 
    MPosition mAntPos = MS::positionMeasure(colAntPos);
    MVPosition antPos0(colAntPos(0));
    mAntPos.set(antPos0);

    // Set up the frame for this epoch and antenna. We will
    // adjust this to effect the coordinate transformations
    MeasFrame fAntFrame(mEpoch, mAntPos);

    // Set up the reference for AzEl and HADec
    MDirection::Ref rAzEl (MDirection::AZEL, fAntFrame);
    MDirection::Ref rHADec(MDirection::HADEC, fAntFrame);

    // Set up the Direction for this phase center
    MDirection mRADec = phaseCenter_p;

    // Make the HADec pole as expressed in HADec. The pole is the default.
    MVDirection mPole;
    MDirection mHADecPole(mPole, rHADec);

    // Make holders for RADec positions and the HADec pole as converted into AzEl
    MDirection mRADecInAzEl;
    MDirection mHADecPoleInAzEl;

    // Set up the machines to convert to AzEl 
    MDirection::Convert cRADecToAzEl(mRADec,     rAzEl);
    MDirection::Convert cHADecToAzEl(mHADecPole, rAzEl);

    // Do conversion for all antennas. Can use the same conversion machine for
    // all antennas since we just change the Frame
    for (Int iant=0;iant<nAnt_p;iant++) {
      if (mount_p(iant)==0) {

	// Now reset the reference frame used in the conversion machines to be
	// for this antenna at this time
        fAntFrame.resetPosition(MVPosition(colAntPos(iant)));

	// Now we can do the conversions using the machines
        mRADecInAzEl     = cRADecToAzEl();
        mHADecPoleInAzEl = cHADecToAzEl();

        // And now we finally get the parallactic angle
        This->pa_p(iant) = mRADecInAzEl.getValue().
	  positionAngle(mHADecPoleInAzEl.getValue());
	This->pa_p(iant)+= receptorAngle_p(iant);
	//#if (iant==0) 
	//#  cout<<"Antenna "<<iant<<" at time: "<<MVTime(mEpoch.getValue())<<
	//#  " has PA = "<<This->pa_p(iant)*57.28<<endl;

      } else if (mount_p(iant)!=1) {
        LogSink logSink;
        message.message("unhandled mount type");
        message.priority(LogMessage::SEVERE);
        logSink.post(message);
      }
    }
  }
  return pa_p;
}

Vector<Float>& ROVisibilityIterator::sigma(Vector<Float>& sig) const
{
  Matrix<Float> sigmat=colSigma.getColumn();
  // Do a rough average of the parallel hand polarizations to get a single 
  // sigma. Should do this properly someday, or return all values
  sig.resize(sigmat.ncolumn());
  sig.ac()=sigmat.row(0).ac();
  sig.ac()+=sigmat.row(nPol_p-1).ac();
  sig.ac()/=2.0f;
  return sig;
}

Vector<Float>& ROVisibilityIterator::weight(Vector<Float>& wt) const
{
  wt.resize(curNumRow_p);
  colWeight.getColumn(wt);
  return wt;
}

ROVisibilityIterator& 
ROVisibilityIterator::selectChannel(Int nGroup, Int start, Int width, 
				    Int increment, Int spectralWindow)
{
  if (spectralWindow<0) spectralWindow = curSpectralWindow_p;
  Int n = numChanGroup_p.nelements();
  if (spectralWindow >= n) {
    // we need to resize the blocks
    Int newn = max(2,max(2*n,spectralWindow+1));
    numChanGroup_p.resize(newn);
    chanStart_p.resize(newn);
    chanWidth_p.resize(newn);
    chanInc_p.resize(newn);
    for (Int i = n; i<newn; i++) numChanGroup_p[i] = 0;
  }
  if (preselected_p) {
    if (start<preselectedChanStart_p[spectralWindow] ||
	(nGroup-1)*increment+width>preselectednChan_p[spectralWindow]) {
      cerr<<" illegal selection "<<endl; return *this;
    }
  }
  chanStart_p[spectralWindow] = start;
  chanWidth_p[spectralWindow] = width;
  chanInc_p[spectralWindow] = increment;
  numChanGroup_p[spectralWindow] = nGroup;
  return *this;
}

void ROVisibilityIterator::attachVisBuffer(VisBuffer& vb)
{
  vbStack_p.push((void*)&vb);
  vb.invalidate();
}

void ROVisibilityIterator::detachVisBuffer(VisBuffer& vb)
{
  if (!vbStack_p.empty()) {
    if ((void*)vbStack_p.top() == (void*)&vb) {
      vbStack_p.pop();
      if (!vbStack_p.empty()) ((VisBuffer*)vbStack_p.top())->invalidate();
    } else {
      throw(AipsError("ROVisIter::detachVisBuffer - attempt to detach "
		      "buffer that is not the last one added"));
    }
  }
}

VisibilityIterator::VisibilityIterator() {}

VisibilityIterator::VisibilityIterator(MeasurementSet &MS, 
				       const Block<Int>& sortColumns, 
				       Double timeInterval)
:ROVisibilityIterator(MS, sortColumns, timeInterval)
{
}

VisibilityIterator::VisibilityIterator(const VisibilityIterator & other)
{
    operator=(other);
}

VisibilityIterator::~VisibilityIterator() {}

VisibilityIterator& 
VisibilityIterator::operator=(const VisibilityIterator& other)
{
    if (this!=&other) {
	ROVisibilityIterator::operator=(other);
	RWcolVis.reference(other.RWcolVis);
	RWcolWeight.reference(other.RWcolWeight);
	RWcolFlag.reference(other.RWcolFlag);
    }
    return *this;
}

VisibilityIterator & VisibilityIterator::operator++(int)
{
  if (!more_p) return *this;
  advance();
  return *this;
}

VisibilityIterator & VisibilityIterator::operator++()
{
  if (!more_p) return *this;
  advance();
  return *this;
}

void VisibilityIterator::attachColumns()
{
  ROVisibilityIterator::attachColumns();
  RWcolVis.attach(selTable_p,MS::columnName(MS::DATA));
  RWcolWeight.attach(selTable_p,MS::columnName(MS::WEIGHT));
  RWcolFlag.attach(selTable_p,MS::columnName(MS::FLAG));
}

void VisibilityIterator::setVis(const Matrix<CStokesVector> & vis)
{
  // two problems: 1. channel selection -> we can only write to reference
  // MS with 'processed' channels
  //               2. polarization: there could be 1, 2 or 4 in the
  // original data, predict() always gives us 4. We save what was there
  // originally.

  if (!preselected_p) {
    throw(AipsError("VisIter::setVis(vis) - cannot change original data"));
  }
  if (vis.nrow()!=channelGroupSize_p) {
    throw(AipsError("VisIter::setVis(vis) - inconsistent number of channels"));
  }
  // we need to reform the vis matrix to a cube before we can use
  // putColumn to a Matrix column
  //# putColumn fails at present, use row by row put instead
  //#visCube_p.resize(nPol_p,channelGroupSize_p,curNumRow_p);
  Matrix<Complex> visMat(nPol_p,channelGroupSize_p);
  Bool deleteIt;
  //#Complex* p=visCube_p.getStorage(deleteIt);
  Complex* pvisMat=visMat.getStorage(deleteIt);
  for (Int row=0; row<curNumRow_p; row++) {
    Complex* p=pvisMat;
    for (Int chn=0; chn<channelGroupSize_p; chn++) {
      const CStokesVector& v=vis(chn,row);
      switch (nPol_p) {
      case 4: *p++=v(0); *p++=v(1); *p++=v(2); *p++=v(3); break;     
      case 2: *p++=v(0); *p++=v(3); break;
      case 1: *p++=(v(0)+v(3))/2; break;
      }
    }
    if (useSlicer_p) RWcolVis.putSlice(row,slicer_p,visMat);
    else RWcolVis.put(row,visMat);
  }
  //#RWcolVis.putColumn(slicer_p,visCube_p);
}

void VisibilityIterator::setWeight(const Vector<Float>& weight)
{
    RWcolWeight.putColumn(weight);
}

void VisibilityIterator::setFlag(const Matrix<Bool>& flag)
{
  // use same value for all polarizations
  //#flagCube_p.resize(nPol_p,channelGroupSize_p,curNumRow_p);
  Bool deleteIt;
  //#Bool* pflagcube=flagCube_p.getStorage(deleteIt);
  Matrix<Bool> flagmat(nPol_p,channelGroupSize_p);
  Bool* pflagmat=flagmat.getStorage(deleteIt);
  const Bool* pflag=flag.getStorage(deleteIt);
  if (flag.nrow()!=channelGroupSize_p) {
    throw(AipsError("VisIter::setFlag(flag) - inconsistent number of channels"));
  }
  Int spw = curSpectralWindow_p;
  
  for (Int row=0; row<curNumRow_p; row++) {
    Bool* p=pflagmat;
    for (Int chn=0; chn<channelGroupSize_p; chn++) {
      for (Int pol=0; pol<nPol_p; pol++) {
	*p++=*pflag;
      }
      pflag++;
    }
    if (useSlicer_p) RWcolFlag.putSlice(row,slicer_p,flagmat);
    else RWcolFlag.put(row,flagmat);
  }
  //#putColumn fails
  //#RWcolFlag.putColumn(slicer_p,flagCube_p);
}
