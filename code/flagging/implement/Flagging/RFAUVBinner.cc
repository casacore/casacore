//# RFAUVBinner.cc: this defines RFAUVBinner
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
#include <trial/RedFlagger/RFAUVBinner.h> 
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/MaskArrMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Slice.h>
#include <trial/MeasurementEquations/VisBuffer.h>
#include <trial/Tasking/PGPlotterInterface.h>
    
#include <aips/stdio.h>
#include <aips/stdlib.h>
    
RFAUVBinner::RFAUVBinner  ( RFChunkStats &ch,const RecordInterface &parm ) : 
    RFAFlagCubeBase(ch,parm),
    RFDataMapper(parm.asString(RF_COLUMN),parm.asArrayString(RF_EXPR)),
    threshold( parm.asDouble(RF_THR) )
//    rowclipper(chunk,flag,threshold,halfwin)
{
// get bin size arguments
  if( fieldType(parm,RF_NBINS,TpArrayInt) )
  {
    Vector<Int> binsize( parm.asArrayInt(RF_NBINS) );
    nbin_uv = binsize(0);
    nbin_y  = binsize(1);
  } 
  else if( fieldType(parm,RF_NBINS,TpInt) )
  {
    nbin_uv = nbin_y = parm.asInt(RF_NBINS);
  }
// check threshold for validity
  if( threshold >= 1 )
    throw( AipsError(String("RFAUVBinner: ")+RF_THR+" must be below 1") );
// check if a report is requested for a specific channel
  if( isFieldSet(parm,RF_PLOTCHAN) )
  {
    plot_report = True;
    if( fieldType(parm,RF_PLOTCHAN,TpInt) )
      report_chan = parm.asInt(RF_PLOTCHAN);
    else 
      report_chan = -1;
    // setup plotter
    String filename("uvbinner.ps");
    os<<"Extra plots will be dumped to file "<<filename<<endl<<LogIO::NORMAL;
    pgp = PGPlotter(filename+"/ps",80);
    // setup colormap for PS
    uInt c1=16,nc=64;
    Float scale=1.0/(nc-1);
    pgp.scir(c1,c1+nc-1);
    for( uInt c=0; c<nc; c++ )
      pgp.scr(c1+c,c*scale,c*scale,c*scale);
    // setup pane layout
    pgp.subp(2,2);
  }
  else
    plot_report = False;
}

uInt RFAUVBinner::estimateMemoryUse () 
{
  return RFAFlagCubeBase::estimateMemoryUse() +
        yvalue.estimateMemoryUse(num(CHAN),num(IFR),num(TIME)) +
        num(IFR)*num(TIME)*sizeof(Float)/(1024*1024) +
        nbin_uv*nbin_y*num(CHAN)*sizeof(Int)/(1024*1024);
}

Bool RFAUVBinner::newChunk (Int &maxmem)
{
// compute correlations mask, return False if fails
  corrmask = RFDataMapper::corrMask(chunk.visIter());
  if( !corrmask )
  {
    os<<LogIO::WARN<<"missing selected correlations, ignoring this chunk\n"<<LogIO::NORMAL;
    return active=False;
  }
// memory management. 
// bin counts are always in memory
  maxmem -= nbin_uv*nbin_y*num(CHAN)*sizeof(Int)/(1024*1024) +
            num(IFR)*num(TIME)*sizeof(Float)/(1024*1024);
// Estimate the max memory use for the lattices, plus a 5% margin  
  Int mmdiff = (Int)(1.05*yvalue.estimateMemoryUse(num(CHAN),num(IFR),num(TIME)));
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
// create temp lattice for yvalues 
  yvalue.init(num(CHAN),num(IFR),num(TIME),mmdiff,2);
// init uvdist matrix
  uvdist.resize(num(IFR),num(TIME));
  uvdist.set(-1);
// init min/max estimates
  ymin.resize(num(CHAN));
  ymax.resize(num(CHAN));
  ymin.set(C::flt_max);
  ymax.set(C::flt_min);
  uvmin.resize(num(CHAN));
  uvmax.resize(num(CHAN));
  uvmin.set(C::flt_max);
  uvmax.set(0);
  binned = False;
// setup plotting of reports
  plot_chan = -1;
  if( plot_report )
  {
    if( report_chan<0 )
      plot_chan = num(CHAN)/2;
    else if( report_chan<(Int)num(CHAN) )
      plot_chan = report_chan;
  }
// finish with init  
  RFAFlagCubeBase::newChunk(maxmem-=1);
  
  return active=True;
}

void RFAUVBinner::endChunk ()
{
  RFAFlagCubeBase::endChunk();
  yvalue.cleanup();
  uvdist.resize();
  bincounts.resize();
  ymin.resize();
  ymax.resize();
  ybinsize.resize();
  uvmin.resize();
  uvmax.resize();
  uvbinsize.resize();
  totcounts.resize();
//  rowclipper.cleanup();
}

void RFAUVBinner::startData ()
{
// reset lattices to write-only
  yvalue.reset(False,True);
  RFAFlagCubeBase::startData();
//  rowclipper.reset();
}

RFA::IterMode RFAUVBinner::iterTime (uInt it)
{
  yvalue.advance(it);
  RFAFlagCubeBase::iterTime(it);
  RFDataMapper::setVisBuffer(chunk.visBuf());
// get UVW data from VisBuffer
  puvw = & chunk.visBuf().uvw();
  return RFA::CONT;
}

RFA::IterMode RFAUVBinner::iterRow ( uInt irow )
{
  uInt ant1,ant2,ifr;
  chunk.ifrToAnt(ant1,ant2,ifr=chunk.ifrNum(irow));
// skip auto-correlations
  if( ant1==ant2 )
    return RFA::CONT;
// compute UV distance for this row
  Float uv = sqrt(square((*puvw)(irow)(0))+square((*puvw)(irow)(1)));
  uvdist(ifr,yvalue.position()) = uv;
// compute yvalues for every unflagged pixel
  for( uInt ich=0; ich<num(CHAN); ich++ )
  {
    if( flag.preFlagged(ich,ifr) )
      continue;
    // update UV range for this channel
    if( uv < uvmin(ich) )
      uvmin = uv;
    if( uv > uvmax(ich) )
      uvmax = uv;
    // compute y value and update y ranges
    Float yval = mapValue(ich,irow);
    yvalue(ich,ifr) = yval;
    if( yval < ymin(ich) )
      ymin(ich) = yval;
    if( yval > ymax(ich) )
      ymax(ich) = yval;
  }
  return RFA::CONT;
}

RFA::IterMode RFAUVBinner::endData ()
{
// compute bin sizes
  uvbinsize.resize();
  uvbinsize = (uvmax-uvmin)/nbin_uv;
  ybinsize.resize();
  ybinsize = (ymax-ymin)/nbin_y;
  
  RFAFlagCubeBase::endData();
//  uInt dum;
//  rowclipper.updateSigma(dum,dum);
  return RFA::DRY;
}


void RFAUVBinner::startDry ()
{
  RFAFlagCubeBase::startDry();
// reset lattices to read-only
  yvalue.reset(True,False);
// create bincounts cube, if necessary
  if( !binned )
  {
    bincounts.resize();
    bincounts = Cube<Int>(nbin_uv,nbin_y,num(CHAN),0);
    totcounts.resize();
    totcounts = Vector<Int>(num(CHAN),0);
// make debug plot
    if( plot_chan>=0 )
    {
      Int ich = plot_chan;
      pgp.env(uvmin(ich),uvmax(ich),ymin(ich),ymax(ich),0,0);
      pgp.bbuf();
      char s[256];
      sprintf(s,"%d by %d bins, channel %d",nbin_uv,nbin_y,plot_chan);
      pgp.lab("UV distance",RFDataMapper::descExpression(),s);
    }
  }
}

IPosition RFAUVBinner::computeBin( Float uv,Float y,uInt ich )
{
  Int i = (Int)((uv-uvmin(ich))/uvbinsize(ich));
  Int j = (Int)((y -ymin(ich))/ybinsize(ich));
  return IPosition(3,i,j,ich);
}

RFA::IterMode RFAUVBinner::iterDry (uInt it)
{
  RFAFlagCubeBase::iterDry(it);
  yvalue.advance(it);
// already binned? Do flagging
  if( binned )
  {
    for( uInt ifr=0; ifr<num(IFR); ifr++ )
    {
      Float uv = uvdist(ifr,it);
      for( uInt ich=0; ich<num(CHAN); ich++ )
      {
        if( uv>0 && bincounts(computeBin(uv,yvalue(ich,ifr),ich))<0 )
          flag.setFlag(ich,ifr);
      }
    }
  }
// else compute bins
  else
  {
    for( uInt ifr=0; ifr<num(IFR); ifr++ )
    {
      Float uv = uvdist(ifr,it);
      if( uv>0 )
        for( uInt ich=0; ich<num(CHAN); ich++ )
          if( !flag.preFlagged(ich,ifr) )
          {
            bincounts( computeBin(uv,yvalue(ich,ifr),ich) )++;
            totcounts(ich)++;
            if( plot_chan == (Int)ich )
            {
              Vector<Float> x(1,uv),y(1,yvalue(ich,ifr));
              pgp.pt(x,y,DOT);
            }
          }
    }
  }
  return RFA::CONT;
}

RFA::IterMode RFAUVBinner::endDry ()
{
// already binned? then it must have been flagged, so stop
  if( binned )
    return RFA::STOP;
// else compute bad bins
  binned = True;
  for( uInt ich=0; ich<num(CHAN); ich++ )
  {
    // make debug plot
    Bool makeplot = (plot_chan==(Int)ich);
    // bins for this channel
    Matrix<Int> bins( bincounts.xyPlane(ich) );
    Int maxcount = max(bins);
    Vector<Int> cumul(maxcount,0);
    // compute total population for each non-zero count
    // (what we compute is actually the total number of points
    // resident in a bin of size N, that is, N*numbins{count=N})
    for( uInt i=0; i<bins.ncolumn(); i++ )
      for( uInt j=0; j<bins.nrow(); j++ )
        if( bins(j,i) )
          cumul( bins(j,i)-1 ) += bins(j,i);
    // convert to cumulative
    for( Int i=1; i<maxcount; i++ )
      cumul(i) += cumul(i-1);
    // find at which bin count the cumulative probability gets higher 
    // than the threshold
    Int thr_count=0;
    Int pop_cutoff=(Int)rint(totcounts(ich)*threshold);
    while( cumul(thr_count)<=pop_cutoff && thr_count<maxcount )
      thr_count++;
    // Mark the "bad" bins by negating their bin counts
    // 0-bins are bad by definition, but they don't have 
    // anything stored in them anyway
    LogicalMatrix wh( bins<thr_count );
    bins(wh) = - bins(wh);
    // produce plots
    if( makeplot )
    {
      // plot of flagged bins
      Vector<Float> xbox(5,0.);
      Vector<Float> ybox(5,0.);
      xbox(2)=xbox(3)=ybox(1)=ybox(2)=1;
      xbox *= uvbinsize(ich);
      ybox *= ybinsize(ich);
      Int bincount=0,pixcount=0;
      for( uInt i=0; i<bins.ncolumn(); i++ )
        for( uInt j=0; j<bins.nrow(); j++ )
          if( bins(j,i)<0 )
          {
            bincount++;
            pixcount+= -bins(j,i);
            pgp.line(xbox+uvmin(ich)+j*uvbinsize(ich),ybox+ymin(ich)+i*ybinsize(ich));
          }
      pgp.ebuf();
      // plot of probability distributions
      Vector<Float> ind(maxcount);
      Vector<Float> prob(maxcount);
      indgen(ind);
      pgp.env(0,maxcount-1,0,1,0,0);

      char s1[256],s2[256],s3[256];
      sprintf(s1,"Bin population (threshold %d)",thr_count);
      sprintf(s2,"Cumulative probability (%g cut-off)",threshold);
      sprintf(s3,"%d of %d points in %d bins flagged",totcounts(ich),pixcount,bincount);
      pgp.lab(s1,s2,s3);
      
      convertArray(prob,cumul);
      prob /= (Float)totcounts(ich);
      pgp.line(ind,prob);
      Vector<Float> xx(2,thr_count);
      Vector<Float> yy(2,0); yy(1)=1;
      pgp.line(xx,yy);
      xx(0)=0; xx(1)=maxcount;
      yy=threshold;
      pgp.line(xx,yy);
    }
  }
// request another dry pass to do the flags
  return RFA::DRY;
}

// -----------------------------------------------------------------------
// RFAUVBinner::getDesc
// Returns description of parameters
// -----------------------------------------------------------------------
String RFAUVBinner::getDesc ()
{
  String desc( RFDataMapper::description()+"; " );
  char s[256];
  sprintf(s,"%s=%g %s=%d,%d ",RF_THR,threshold,RF_NBINS,nbin_uv,nbin_y);
  desc += s + RFAFlagCubeBase::getDesc();
  return desc;
}

// -----------------------------------------------------------------------
// RFAUVBinner::getDefaults
// Returns record of default parameters
// -----------------------------------------------------------------------
const RecordInterface & RFAUVBinner::getDefaults ()
{
  static Record rec;
// create record description on first entry
  if( !rec.nfields() )
  {
    rec = RFAFlagCubeBase::getDefaults();
    rec.define(RF_NAME,"UVBinner");
    rec.define(RF_COLUMN,"DATA");
    rec.define(RF_EXPR,"+ ABS XX YY");
    rec.define(RF_THR,.001);
    rec.define(RF_NBINS,50);
    
    rec.setComment(RF_COLUMN,"Use column: [DATA|MODEL|CORRected]");
    rec.setComment(RF_EXPR,"Expression for deriving value (e.g. \"ABS XX\", \"+ ABS XX YY\")");
    rec.setComment(RF_THR,"Probability cut-off");
    rec.setComment(RF_NBINS,"Number of bins (single value, or [NUV,NY])");
  }
  return rec;
}

  

