//# RFADiffBase.cc: this defines RFADiffBase
//# Copyright (C) 2000,2001
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
#include <flagging/Flagging/RFADiffBase.h>
#include <msvis/MSVis/VisibilityIterator.h>
#include <msvis/MSVis/VisBuffer.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/MaskArrMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/Slice.h>
#include <casa/System/PGPlotterInterface.h>

#include <casa/stdio.h>

namespace casa { //# NAMESPACE CASA - BEGIN

Bool RFADiffBase::dummy_Bool;

// -----------------------------------------------------------------------
// RFADiffBase constructor
// Construct from a Record of parameters
// -----------------------------------------------------------------------
RFADiffBase::RFADiffBase (  RFChunkStats &ch,const RecordInterface &parm ) :
  RFAFlagCubeBase(ch,parm),
  clip_level(parm.asDouble(RF_THR)),
  row_clip_level(parm.asDouble(RF_ROW_THR)),
  disable_row_clip(parm.asBool(RF_ROW_DISABLE)),
  rowclipper(chunk,flag,row_clip_level,parm.asInt(RF_ROW_HW))
{
}

// -----------------------------------------------------------------------
// Destructor
// -----------------------------------------------------------------------
RFADiffBase::~RFADiffBase () 
{
}

// -----------------------------------------------------------------------
// RFADiffBase::setDebug
// Enables debugging plots for some specified axis
// -----------------------------------------------------------------------
void RFADiffBase::setDebug( const RFDebugPlot &dbg )
{
  debug = dbg;
  rowclipper.setDebug(debug);
}

// -----------------------------------------------------------------------
// RFADiffBase::getDefaults
// Returns record of default parameters
// -----------------------------------------------------------------------
const RecordInterface & RFADiffBase::getDefaults ()
{
  static Record rec;
// create record description on first entry
  if( !rec.nfields() )
  {
    rec = RFAFlagCubeBase::getDefaults();
    rec.define(RF_THR,(Double)5);
    rec.define(RF_ROW_THR,(Double)10);
    rec.define(RF_ROW_HW,(Int)6);
    rec.define(RF_ROW_DISABLE,False);
    rec.setComment(RF_THR,"Pixel flagging threshold, in AADs");
    rec.setComment(RF_ROW_THR,"Row flagging threshold, in AADs");
    rec.setComment(RF_ROW_HW,"Row flagging, half-window of sliding median");
    rec.setComment(RF_ROW_DISABLE,"Disable row-based flagging");
  }
  return rec;
}

// -----------------------------------------------------------------------
// RFADiffBase::getDesc
// Returns description of parameters
// -----------------------------------------------------------------------
String RFADiffBase::getDesc ()
{
  char s[128];
  if( disable_row_clip )
    sprintf(s,"%s=%.1f",RF_THR,clip_level);
  else
    sprintf(s,"%s=%.1f %s=%.1f",RF_THR,clip_level,RF_ROW_THR,row_clip_level);
  String str(s);
  return str+" "+RFAFlagCubeBase::getDesc();
}

uInt RFADiffBase::estimateMemoryUse ()
{
  return diff.estimateMemoryUse(num(CHAN),num(IFR),num(TIME)) + 
        RFAFlagCubeBase::estimateMemoryUse() + 2;
}
  
// -----------------------------------------------------------------------
// RFADiffBase::newChunk
// Sets up for new chunk of data
// -----------------------------------------------------------------------
Bool RFADiffBase::newChunk (Int &maxmem)
{
// compute correlations mask
  corrmask = newCorrMask();
  if( !corrmask )
  {
    os<<LogIO::WARN<<"missing selected correlations, ignoring this chunk\n"<<LogIO::POST;
    return active=False;
  }
// memory management. Estimate the max memory use for the diff 
// lattice, plus a 5% margin
  Int mmdiff = (Int)(1.05*diff.estimateMemoryUse(num(CHAN),num(IFR),num(TIME)));
  // sufficient memory? reserve it
  if( maxmem>mmdiff ) 
    maxmem -= mmdiff;  
  else // insufficient memory: use disk-based lattice
  {
    mmdiff = 0;
    maxmem -= 2; // reserve 2Mb for the iterator anyway
  }
// init flag cube
  RFAFlagCubeBase::newChunk(maxmem);
// create a temp lattice to hold nchan x num(IFR) x ntime diff-medians
  diff.init(num(CHAN),num(IFR),num(TIME),mmdiff,2);
// init the row-clipper object
  rowclipper.init(num(IFR),num(TIME));
  diffrow.resize(num(CHAN));
  
// if rows are too short, there's no point in flagging them in toto 
// based on their noise level
  clipping_rows = !disable_row_clip;
  if( num(CHAN)<10 )
    clipping_rows = False;
  
  return active=True;
}

// -----------------------------------------------------------------------
// RFADiffBase::endChunk
// Resets at end of chunk
// -----------------------------------------------------------------------
void RFADiffBase::endChunk ()
{
  RFAFlagCubeBase::endChunk();
  diff.cleanup();
  rowclipper.cleanup();
  diffrow.resize();
}

// -----------------------------------------------------------------------
// RFADiffBase::startData
// Prepares for an data pass over a VisIter chunk
// -----------------------------------------------------------------------
void RFADiffBase::startData ()
{
  RFAFlagCubeBase::startData();
  diff.reset(chunk.npass()>0,True);
  rowclipper.reset();

  pflagiter = &flag.iterator();

  if( debug.enabled() )
    resetPlot();
}

// -----------------------------------------------------------------------
// RFADiffBase::startDry
// Prepares for an dry pass 
// -----------------------------------------------------------------------
void RFADiffBase::startDry ()
{
  RFAFlagCubeBase::startDry();
  diff.reset(chunk.npass()>0,False);
  rowclipper.reset();
  pflagiter = &flag.iterator();
  
  if( debug.enabled() )
    resetPlot();
}


// -----------------------------------------------------------------------
// RFADiffBase::iterTime
// Default version of iter time just keeps the diff and flag lattices
// in sync with the time slot.
// -----------------------------------------------------------------------
RFA::IterMode RFADiffBase::iterTime (uInt it)
{
  RFAFlagCubeBase::iterTime(it);
  diff.advance(it);
  return RFA::CONT;
}

// -----------------------------------------------------------------------
// RFADiffBase::iterDry
// Dry run iterator: recomputes the AAD and does flagging
// -----------------------------------------------------------------------
RFA::IterMode RFADiffBase::iterDry ( uInt it )
{
  RFAFlagCubeBase::iterDry(it);
  diff.advance(it);
  
  for( uInt ifr=0; ifr<num(IFR); ifr++ ) // outer loop over IFRs
  {
    if( flag.rowFlagged(ifr,it) ) // skip if whole row is flagged
    {
      if( debug.type()==TIME && debug.ifr() == (Int)ifr ) 
      { // in time-axis plots, mark flagged rows on plot
        dbg_sym(it) = DAVIDSTAR;
        dbg_thr(it) = 1e+10;
      }
      continue;
    }
    Float thr = clip_level*rowclipper.sigma0(ifr,it);
    idiffrow=0;
    Bool updated=False;
    for( uInt ich=0; ich<num(CHAN); ich++ ) // loop over channels
    {
      Int di = debug.index(ich,ifr,it); // belongs in debug plot? 
      if( flag.preFlagged(ich,ifr) ) // skip pixel if pre-flagged
        continue;
      if( di>=0 )
        dbg_thr(di) = thr;
      Float d = diff(ich,ifr);
      if( d > thr )   // should be clipped?
      {
        Bool res = flag.setFlag(ich,ifr);
        updated |= res;
        if( di>=0 )
          dbg_sym(di) = res ? FSTAR5 : STAR5;
      }
      else
      {
        diffrow(idiffrow++) = d;
        Bool res = flag.clearFlag(ich,ifr);
        updated |= res;
        if( di>=0 )
          dbg_sym(di)= res ? CIRCLE4 : DOT;
      }
    } // for(ich)
    // update the noise level, if any changes in flags
    if( updated ) 
      rowclipper.setSigma(ifr,it,idiffrow ? median( diffrow(Slice(0,idiffrow)) ) : -1 );
  } // for(ifr)
  return CONT;
}
      
// -----------------------------------------------------------------------
// RFADiffBase::endData
// After a data pass, we always request one more dry pass
// -----------------------------------------------------------------------
RFA::IterMode RFADiffBase::endData ()
{
  RFAFlagCubeBase::endData();
  uInt dum;
  rowclipper.updateSigma(dum,dum);
  
  if( debug.enabled() )
    makePlot();
  
  return RFA::DRY;
}

// after a dry pass - see if AAD has managed to update itself 
// significantly
RFA::IterMode RFADiffBase::endDry ()
{
  RFAFlagCubeBase::endDry();
// update the reference AAD
  uInt ifrmax,itmax;
  Float dmax =   rowclipper.updateSigma(ifrmax,itmax);
  
  dprintf(os,"Max diff (%f) at ifr %d (%s), it %d: new sigma is %f\n",
      dmax,ifrmax,chunk.ifrString(ifrmax).chars(),itmax,rowclipper.sigma0(ifrmax,itmax));

  if( debug.enabled() )
    makePlot();
  
// no significant change this pass? Then we're really through with it.
  if( dmax <= RFA_AAD_CHANGE )
    return RFA::STOP;
// else try another dry pass
// NB: perhaps request a data pass, if too many flags?
  return RFA::DRY;
}

void RFADiffBase::startDataRow (uInt ifr)
{
  idiffrow=0;
}

void RFADiffBase::endDataRow (uInt ifr)
{
//  if( !idiffrow )
//    dprintf(os,"No data points at ifr %d\n",ifr);
  Float sigma = idiffrow ? median( diffrow( Slice(0,idiffrow) ) ) : -1;
  uInt it = diff.position();
  rowclipper.setSigma(ifr,it,sigma);
}

// -----------------------------------------------------------------------
// RFADiffBase::setDiff
// Meant to be called during a data pass. Updates the difference 
// lattice; flags things if appropriate.
// This assumes caller has already checked existing pre-flags,
// and that the current data point is NOT flagged.
// Returns the threshold used for flagging, or 0 if no threshold is
// yet available.
// -----------------------------------------------------------------------
Float RFADiffBase::setDiff ( uInt ich,uInt ifr,Float d,Bool &flagged )
{
  Float thr = 0;
  d = abs(d);
  uInt it = diff.position();
  
  // write diff to lattice
  diff(ich,ifr) = d;
  
  flagged=False; 
  if( chunk.npass() && rowclipper.sigma0(ifr,it)>0 )
  {
    thr = rowclipper.sigma0(ifr,it);
    
    if( dbg_i>=0 )
      dbg_thr(dbg_i) = thr;
    
    if( d > thr )
    {
      if( dbg_i>=0 )
        dbg_sym(dbg_i) = 18;
      if( flag.setFlag(ich,ifr,*pflagiter) )
        rowclipper.markSigma(ifr);
      flagged=True;
    }
  }
  if( !flagged )
    diffrow(idiffrow++) = d;
  
  return thr;
}

// -----------------------------------------------------------------------
// class RFADiffMapBase 
//
//  
//
// -----------------------------------------------------------------------

// -----------------------------------------------------------------------
// RFADiffMapBase constructor
// Construct from Record of parameters
// -----------------------------------------------------------------------
RFADiffMapBase::RFADiffMapBase (  RFChunkStats &ch,const RecordInterface &parm ) 
  : RFADiffBase(ch,parm),
    RFDataMapper(parm.asArrayString(RF_EXPR),parm.asString(RF_COLUMN))
{
}

// -----------------------------------------------------------------------
// RFADiffMapBase destructor
// -----------------------------------------------------------------------
RFADiffMapBase::~RFADiffMapBase () 
{ 
}

// -----------------------------------------------------------------------
// RFADiffMapBase::getDefaults
// Returns record of default paramaters
// -----------------------------------------------------------------------
const RecordInterface & RFADiffMapBase::getDefaults ()
{
  static Record rec;
// create record description on first entry
  if( !rec.nfields() )
  {
    rec = RFADiffBase::getDefaults();
    rec.define(RF_COLUMN,"DATA");
    rec.define(RF_EXPR,"+ ABS XX YY");
    rec.setComment(RF_COLUMN,"Use column: [DATA|MODEL|CORRected]");
    rec.setComment(RF_EXPR,"Expression for deriving value (e.g. \"ABS XX\", \"+ ABS XX YY\")");
  }
  return rec;
}

// -----------------------------------------------------------------------
// RFADiffMapBase::iterTime
// Sets up mapper
// -----------------------------------------------------------------------
RFA::IterMode RFADiffMapBase::iterTime (uInt it)
{
  setupMapper();
  return RFADiffBase::iterTime(it);
}

// -----------------------------------------------------------------------
// RFADiffMapBase::getDesc
// Returns short description of parameters
// -----------------------------------------------------------------------
String RFADiffMapBase::getDesc ()
{
  return RFDataMapper::description()+"; "+RFADiffBase::getDesc();
}

// -----------------------------------------------------------------------
// RFADiffBase::resetPlot
// Resets vectors in which plots are accumulated
// -----------------------------------------------------------------------
void RFADiffBase::resetPlot ()
{
  if( !debug.enabled() )
    return;
  uInt dbg_nval = num( debug.type() );
  dbg_med.resize(dbg_nval);
  dbg_val.resize(dbg_nval);
  dbg_thr.resize(dbg_nval);
  dbg_sym.resize(dbg_nval);
  dbg_med.set(0);
  dbg_val.set(0);
  dbg_thr.set(0);
  dbg_sym.set(0);
  dbg_i = -1;
}

// -----------------------------------------------------------------------
// RFADiffBase::makePlot
// Makes plot from accumulated vectors
// -----------------------------------------------------------------------
void RFADiffBase::makePlot ()
{
  PGPlotterInterface &pgp( debug.pgp() );
  Float vmin = min(dbg_val);
  Float vmax = max(dbg_val);
  Bool redraw=True;
  
  while( redraw )
  {
    pgp.ask(False);
    pgp.eras();
    pgp.sci(BLACK);
    pgp.env(0,dbg_val.nelements(),vmin,vmax,0,0);
    char s[256];
    switch( debug.type() )
    {
      case CHAN: 
          sprintf(s,"Pass %d: IFR %d (%s), time slot %d",
             chunk.npass(),debug.ifr(),chunk.ifrString(debug.ifr()).chars(),debug.time());
          pgp.lab("Channel","",s);
          break;
      case TIME:  
          sprintf(s,"Pass %d: channel %d, IFR %d (%s)",
              chunk.npass(),debug.chan(),debug.ifr(),chunk.ifrString(debug.ifr()).chars());
          pgp.lab("Time slot","",s);
          break;
      case IFR: 
          sprintf(s,"Pass %d: channel %d, time slot %d",
             chunk.npass(),debug.chan(),debug.time());
          pgp.lab("IFR","",s);
          break;
      default:
          return;
    }

    Vector<Float> x(dbg_val.nelements());
    indgen(x);

//    cout<<dbg_val-dbg_med<<dbg_sym;
    
    pgp.sci(BLACK);
    pgp.line(x,dbg_med);
    pgp.line(x,dbg_med-dbg_thr);
    pgp.line(x,dbg_med+dbg_thr);
    pgp.sci(YELLOW);
    pgp.pnts(x,dbg_val,dbg_sym);

    redraw = debug.queryPlotLimits(vmin,vmax);
  }
  pgp.ask(True);
}

} //# NAMESPACE CASA - END

