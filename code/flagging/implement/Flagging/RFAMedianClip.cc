//# RFAMedianClip.cc: this defines RFAMedianClip
//# Copyright (C) 2000,2001,2002
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

#include <casa/stdio.h>
#include <flagging/Flagging/RFAMedianClip.h>
#include <casa/Arrays/ArrayMath.h>
    
namespace casa { //# NAMESPACE CASA - BEGIN

// -----------------------------------------------------------------------
// RFATimeMedian
// Accumulator class for computing sliding median per channels over time.
// Internally, we store a cubic TempLattice of ntime x nifr x nchan
// medians.
// -----------------------------------------------------------------------
RFATimeMedian::RFATimeMedian( RFChunkStats &ch,const RecordInterface &parm ) :
  RFADiffMapBase(ch,parm)
{
  halfwin = (uInt)parm.asInt(RF_HW);
  msl = NULL;
  if( fieldType(parm,RF_DEBUG,TpArrayInt) )
  {
    Vector<Int> dbg;
    parm.get(RF_DEBUG,dbg);
    Int ifr=0;
    if( dbg.nelements() == 2 )
      ifr = dbg(1);
    else if( dbg.nelements() == 3 )
      ifr = chunk.antToIfr(dbg(1),dbg(2));
    else
      os<<"\""<<RF_DEBUG<<"\" parameter must be [NCH,NIFR] or [NCH,ANT1,ANT2]"<<LogIO::EXCEPTION;
    setDebug( RFDebugPlot(chunk.pgpscr(),dbg(0),ifr,-1) );
  }
}

RFATimeMedian::~RFATimeMedian ()
{
  if( msl ) delete [] msl;
}

const RecordInterface & RFATimeMedian::getDefaults ()
{
  static Record rec;
// create record description on first entry
  if( !rec.nfields() )
  {
    rec = RFADiffMapBase::getDefaults();
    rec.define(RF_NAME,"TimeMedian");
    rec.define(RF_HW,10);
    rec.define(RF_DEBUG,False);
    rec.setComment(RF_HW,"Sliding window half-width");
    rec.setComment(RF_DEBUG,"Set to [CHAN,IFR] to produce debugging plots");
  }
  return rec;
}

// resets for new calculation
Bool RFATimeMedian::newChunk (Int &maxmem)
{
  if( num(TIME) < halfwin*4 )
  {
    os<<LogIO::WARN<<name()<<": too few time slots, ignoring this chunk\n"<<LogIO::POST;
    return active=False;
  }
// if disk-based flag cube, reserve 2MB for local iterator
  if( !flag.getMaxMem() )
    maxmem -= 2; 
// reserve memory for our bunch of median sliders
  maxmem -= (num(CHAN)*num(IFR)*MedianSlider::objsize(halfwin))/(1024*1024)+1;
// call parent's newChunk  
  if( !RFADiffMapBase::newChunk(maxmem) )
    return active=False;
// create local flag iterator
  flag_iter = flag.newCustomIter();
  pflagiter = &flag_iter;
  return active=True;
}

void RFATimeMedian::endChunk ()
{
  RFADiffMapBase::endChunk();
// create local flag iterator
  flag_iter = FlagCubeIterator();
  if( msl ) delete [] msl;
  msl = NULL;
}

// startData
// create new median sliders at start of data pass
void RFATimeMedian::startData ()
{
  RFADiffMapBase::startData();
  flag_iter.reset();
  if( msl ) delete [] msl;
// this is a workaround for a compiler bug that we occasionally see
  uInt tmpnum2 = num(CHAN)*num(IFR);
// create nchan x nifr median sliders
  msl = new MedianSlider[tmpnum2];
  for(uInt i=0; i<num(CHAN)*num(IFR); i++)
     msl[i] = MedianSlider(halfwin);
}

// iterTime
// During data passes, keep the flag lattice lagging a halw-window behind
RFA::IterMode RFATimeMedian::iterTime ( uInt it )
{
// gets pointer to visibilities cube
  setupMapper();
// Advance sync flag iterator
  flag.advance(it,True);
// During a data pass, keep the diff-lattice iterator lagging a half-window behind
// and also maintain a custom flag iterator
  if( it >= halfwin )
  {
    diff.advance(it-halfwin);
    flag_iter.advance(it-halfwin);
  }
  return RFA::CONT;
}

// -----------------------------------------------------------------------
// RFATimeMedian::iterRow
// Processes one row of data for per-channel medians over time
// -----------------------------------------------------------------------
RFA::IterMode RFATimeMedian::iterRow ( uInt irow )
{
  // start filling deviations when we get to time slot HW
  uInt iifr = chunk.ifrNum(irow);
  uInt it = chunk.iTime();
  Bool fill = ( chunk.iTime() >= (Int)halfwin );
  Bool rowfl = chunk.npass() ? flag.rowFlagged(iifr,it) 
                            : flag.rowPreFlagged(iifr,it);
  if( rowfl ) 
  {
    // the whole row is flagged, so just advance all median sliders
    for( uInt i=0; i<num(CHAN); i++ ) 
      slider(i,iifr).next();
  } 
  else 
  {
    startDataRow(iifr);
    // loop over channels for this spw, ifr
    for( uInt ich=0; ich<num(CHAN); ich++ )
    {
      Bool debug_plot = ( (dbg_i = debug.index(ich,iifr,it)) >=0 );
      // get derived flags and values
      Float val = 0;
// during first pass, look at pre-flags only. During subsequent passes,
// look at all flags
      Bool fl = chunk.npass() ? flag.anyFlagged(ich,iifr) : flag.preFlagged(ich,iifr);
      if( fl )
      {
        if( debug_plot ) // plot pre-flagged point here
        {
          dbg_val(dbg_i) = mapValue(ich,irow);
          dbg_sym(dbg_i) = PGPlotEnums::CROSS;
        }
      }
      else
      {
        val = mapValue(ich,irow);
        if( debug_plot ) 
        {
          dbg_val(dbg_i) = val;
          dbg_sym(dbg_i) = PGPlotEnums::DOT;
        }
      }
      slider(ich,iifr).add( val,fl ); 
      // are we filling in the diff-median lattice already?
      if( fill )
      {
        if( debug_plot )
          dbg_med(dbg_i-halfwin) = slider(ich,iifr).median();
        Float d = slider(ich,iifr).diff(fl);
        if( !fl )  // ignore if flagged
          setDiff( ich,iifr,d );
      }
    }
    endDataRow(iifr);
  }
  return RFA::CONT;
}

// -----------------------------------------------------------------------
// RFATimeMedian::endData
// Called at end of iteration - fill remaining slots in the diff-lattice 
// -----------------------------------------------------------------------
RFA::IterMode RFATimeMedian::endData ()
{
  for( uInt it=num(TIME)-halfwin; it<num(TIME); it++ )
  {
    diff.advance(it);
    for( uInt i = 0; i<num(IFR); i++ )
    {
      startDataRow(i);
      for( uInt j = 0; j<num(CHAN); j++ )
      {
        dbg_i = debug.index(j,i,it);
        
        slider(j,i).next();
        Bool fl;
        Float diff = slider(j,i).diff(fl);
        if( !fl )
          setDiff(j,i,diff);
        
        if( dbg_i>=0 )
          dbg_med(dbg_i) = slider(j,i).median();
      }
      endDataRow(i);
    }
  }
// destroy sliders
  if( msl ) 
  {
    delete [] msl;
    msl = NULL;
  }
  return RFADiffMapBase::endData();
}


// -----------------------------------------------------------------------
// getDesc
// -----------------------------------------------------------------------
String RFATimeMedian::getDesc ()
{
  char s[128];
  sprintf(s," %s=%d",RF_HW,halfwin);
  return RFADiffMapBase::getDesc()+s;
}


// -----------------------------------------------------------------------
// RFAFreqMedian
// Accumulator class for computing sliding median per time slot over
// channels.
// -----------------------------------------------------------------------
RFAFreqMedian::RFAFreqMedian( RFChunkStats &ch,const RecordInterface &parm ) :
  RFADiffMapBase(ch,parm)
{
  halfwin = (uInt)parm.asInt(RF_HW);
  if( parm.isDefined(RF_DEBUG) && parm.dataType(RF_DEBUG) == TpArrayInt )
  {
    Vector<Int> dbg;
    parm.get(RF_DEBUG,dbg);
    Int ifr,it;
    if( dbg.nelements() == 2 )
    {
      ifr = dbg(0); 
      it = dbg(1);
    }
    else if( dbg.nelements() == 3 )
    {
      ifr = chunk.antToIfr(dbg(0),dbg(1));
      it  = dbg(2);
    }
    else
      os<<"\""<<RF_DEBUG<<"\" parameter must be [NIFR,NTIME] or [ANT1,ANT2,NTIME]"<<LogIO::EXCEPTION;
    setDebug( RFDebugPlot(chunk.pgpscr(),-1,dbg(0),dbg(1)) );
  }
}


// -----------------------------------------------------------------------
// RFAFreqMedian::newChunk
// 
// -----------------------------------------------------------------------
Bool RFAFreqMedian::newChunk (Int &maxmem)
{
  if( num(CHAN) < halfwin*4 )
  {
    os<<LogIO::WARN<<name()<<": too few channels, ignoring this chunk\n"<<LogIO::POST;
    return active=False;
  }
  return active=RFADiffMapBase::newChunk(maxmem);
}

// -----------------------------------------------------------------------
// RFAFreqMedian::iterRow
// Processes one row of data for median over frequency
// -----------------------------------------------------------------------
RFA::IterMode RFAFreqMedian::iterRow ( uInt irow )
{
// during first pass, compute diff-median. Also keep track of the AAD.
  uInt iifr = chunk.ifrNum(irow);
  uInt it = chunk.iTime();
  
  Bool rowfl = chunk.npass() ? flag.rowFlagged(iifr,it) 
                            : flag.rowPreFlagged(iifr,it);
  if( rowfl )
  {
    if( debug.type()==CHAN && debug.index(0,iifr,it) >= 0 ) 
    { 
      dprintf(os,"FreqMedian: IFR %d, time slot %d is flagged, no plot.\n",iifr,it); 
    }
  }  
  else // row not flagged
  {
    startDataRow(iifr);
    MedianSlider msl(halfwin);
// loop through all channels in this window
    for( uInt i = 0; i<num(CHAN); i++ ) 
    {
      dbg_i = debug.index(i,iifr,it);
      Float val = 0; 
// during first pass, look at pre-flags only. During subsequent passes,
// look at all flags
      Bool fl = chunk.npass() ? flag.anyFlagged(i,iifr) : flag.preFlagged(i,iifr);
      if( fl )
      {
        if( dbg_i>=0 ) // plot pre-flagged point here
        {
          dbg_val(dbg_i) = mapValue(i,irow);
          dbg_sym(dbg_i) = PGPlotEnums::CROSS;
        }
      }
      else
      {
        val = mapValue(i,irow);
        if( dbg_i>=0 ) 
        {
          dbg_val(dbg_i) = val;
          dbg_sym(dbg_i) = PGPlotEnums::DOT;
        }
      }
      msl.add( val,fl ); 
      if( i>=halfwin )
      {
        if( dbg_i>=0 )
          dbg_med(dbg_i-halfwin) = msl.median();
        Float d = msl.diff(fl);
        if( !fl )
          setDiff(i-halfwin,iifr,d);
      }
    }
    // finish sliding the medians for remaining channels
    for( uInt i=num(CHAN)-halfwin; i<num(CHAN); i++ )
    {
      dbg_i = debug.index(i,iifr,it);
      msl.next(); 
      if( dbg_i>=0 )
        dbg_med(dbg_i) = msl.median();
      Bool fl;
      Float d = msl.diff(fl);
      if( !fl ) 
        setDiff(i,iifr,d);
    }
    endDataRow(iifr);
  }
  return RFA::CONT;
}

// -----------------------------------------------------------------------
// getDesc
// -----------------------------------------------------------------------
String RFAFreqMedian::getDesc ()
{
  char s[128];
  sprintf(s," %s=%d",RF_HW,halfwin);
  return RFADiffMapBase::getDesc()+s;
}

const RecordInterface & RFAFreqMedian::getDefaults ()
{
  static Record rec;
// create record description on first entry
  if( !rec.nfields() )
  {
    rec = RFATimeMedian::getDefaults();
    rec.define(RF_NAME,"FreqMedian");
    rec.setComment(RF_DEBUG,"Set to [IFR,ITIME] to produce debugging plots");
  }
  return rec;
}

} //# NAMESPACE CASA - END

