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
#include <trial/Flagging/RFAUVBinner.h> 
#include <aips/BasicMath/Math.h>
#include <aips/BasicSL/Constants.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/MaskArrMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Slice.h>
#include <trial/MSVis/VisBuffer.h>
#include <trial/System/PGPlotterInterface.h>
    
#include <aips/stdio.h>
#include <aips/stdlib.h>
    
RFAUVBinner::RFAUVBinner  ( RFChunkStats &ch,const RecordInterface &parm ) : 
    RFAFlagCubeBase(ch,parm),
    RFDataMapper(parm.asArrayString(RF_EXPR),parm.asString(RF_COLUMN)),
    threshold( parm.asDouble(RF_THR) ),
    min_population( parm.asInt(RF_MINPOP) )
//    rowclipper(chunk,flag,threshold,halfwin)
{
// get bin size arguments
  if( isFieldSet(parm,RF_NBINS) ) 
  {
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
  }
// check threshold for validity
  if( threshold >= 1 )
    os<<String("RFAUVBinner: ")+RF_THR+" must be below 1"<<endl<<LogIO::EXCEPTION;
  if( threshold==0 && !min_population )
    os<<String("RFAUVBinner: ")+RF_THR+" and/or "+RF_MINPOP+" must be specified"<<endl<<LogIO::EXCEPTION;
// check if a report is requested for a specific channel
  if( isFieldSet(parm,RF_PLOTCHAN) )
  {
    plot_report = True;
    if( fieldType(parm,RF_PLOTCHAN,TpInt) )
      report_chan = parm.asInt(RF_PLOTCHAN);
    else 
      report_chan = -1;
//     // setup plotter
//     String filename("uvbinner.ps");
//     os<<"Extra plots will be dumped to file "<<filename<<endl<<LogIO::POST;
//     pgp = PGPlotter(filename+"/ps",80);
//     // setup colormap for PS
//     uInt c1=16,nc=64;
//     Float s0=.5,scale=.5/(nc-1);
//     pgp.scir(c1,c1+nc-1);
//     for( uInt c=0; c<nc; c++ )
//       pgp.scr(c1+c,s0+c*scale,s0+c*scale,s0+c*scale);
//     // setup pane layout
//     pgp.subp(2,2);
    econoplot = isFieldSet(parm,RF_ECONOPLOT);
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
    os<<LogIO::WARN<<"missing selected correlations, ignoring this chunk\n"<<LogIO::POST;
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
  }
}

IPosition RFAUVBinner::computeBin( Float uv,Float y,uInt ich )
{
  uInt i = (uInt)((uv-uvmin(ich))/uvbinsize(ich));
  uInt j = (uInt)((y -ymin(ich))/ybinsize(ich));
// loss of precision near max values can sometimes put us into bin 
// N+1, so correct for this:
  if( i >= nbin_uv )
    i = nbin_uv-1;
  if( j >= nbin_y )
    j = nbin_y-1;
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
      if( uv>0 )
      {
        for( uInt ich=0; ich<num(CHAN); ich++ )
        {
          if( !flag.preFlagged(ich,ifr) )
          {
            Int bc = bincounts(computeBin(uv,yvalue(ich,ifr),ich));
            if( bc<0 )
              flag.setFlag(ich,ifr);
            // add point to plot if in low-pop bin
            if( plot_chan==(Int)ich && abs(bc)<=econo_density )
            {
              plot_px(plot_np) = uv;
              plot_py(plot_np) = yvalue(ich,ifr);
              plot_np++;
            }
          }
        }
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
            Float y = yvalue(ich,ifr);
            IPosition binpos( computeBin(uv,y,ich) );
            bincounts(binpos)++;
//            bincounts( computeBin(uv,yvalue(ich,ifr),ich) )++;
            totcounts(ich)++;
          }
    }
  }
  return RFA::CONT;
}

// static Matrix<Float> getImgColorMap ( PGPlotterInterface &pgp )
// {
//   Vector<Int> cir( pgp.qcir() );
//   Matrix<Float> col(3,cir(1)-cir(0)+1);
//   for( uInt i=0; i<col.ncolumn(); i++ )
//   {
//     Vector<Float> column( col.column(i) );
//     column = pgp.qcr(i);
//   }
//   return col;
// }
// 
// static void setImgColorMap ( PGPlotterInterface &pgp,const Matrix<Float> &cmap )
// {
//   Vector<Int> cir( pgp.qcir() );
//   if( cmap.nrow()!=3 || cmap.ncolumn()!=(uInt)(cir(1)-cir(0)+1) )
//     throw( AipsError("Incorrect colormap matrix size") );
//   for( uInt i=0; i<cmap.ncolumn(); i++ )
//     pgp.scr(cir(0)+i,cmap(0,i),cmap(1,i),cmap(2,i));
// }

RFA::IterMode RFAUVBinner::endDry ()
{
// already binned? then it must have been flagged, so stop
  if( binned )
  {
    if( plot_chan>=0 && chunk.pgprep().isAttached() )
    {
      chunk.setReportPanels(2,2);
      makePlot(chunk.pgprep(),plot_chan);
    }
    return RFA::STOP;
  }
// else compute bad bins
  binned = True;
  for( uInt ich=0; ich<num(CHAN); ich++ )
  {
    // bins for this channel
    Matrix<Int> bins( bincounts.xyPlane(ich) );
    Int maxcount = max(bins);
    Vector<Int> cumul(maxcount+1,0);
    // compute total population for each non-zero count
    // (what we compute is actually the total number of points
    // resident in a bin of size N, that is, N*numbins{population=N})
    for( uInt i=0; i<bins.ncolumn(); i++ )
      for( uInt j=0; j<bins.nrow(); j++ )
        if( bins(j,i) )
          cumul( bins(j,i) ) += bins(j,i);
    // convert to cumul(N): number of points residing in a bin of size<=N
    // (cumul(0) is 0 by definition)
    for( Int i=1; i<=maxcount; i++ )
      cumul(i) += cumul(i-1);
    Int thr_count=0;
    if( threshold>0 )
    {
      // compute threshold based on cumulative counts
      Float pop_cutoff = totcounts(ich)*threshold;
      // find the first bin count value where the cumulative bin population 
      // is higher than the threshold
      while( thr_count<=maxcount && cumul(thr_count)<=pop_cutoff )
        thr_count++; 
    }
    // if the explicit bin cut-off is higher, use it instead
    if( thr_count < min_population )
      thr_count = min_population;
    // thr_count is now the first population value exceeding the threshold
    // Bins with populations up to thr_count should be flagged
    LogicalMatrix wh( bins<thr_count );
    bins(wh) = - bins(wh);
    //  set up to produce a plot at end of pass
    if( plot_chan==(Int)ich )
    {
      plot_thr_count = thr_count;
      plot_prob.resize(cumul.shape());
      convertArray(plot_prob,cumul);
      plot_prob /= (Float)totcounts(ich);
      // in econoplot mode, we only want the 10% least populous
      // bins to be plotted as individual dots, the rest as just 
      // a density image. Compute this critical density
      if( econoplot )
      {
        econo_density=0;
        Float cutoff = totcounts(ich)*.1;
        while( cumul(econo_density)<=cutoff && econo_density<maxcount )
          econo_density++;
      }
      else
        econo_density = maxcount;
      // allocate px and py arrays to accumulate coordinates of points
      // in the first <econo_density> bins
      Int np = cumul(econo_density);
      plot_px.resize(np);
      plot_py.resize(np);
      plot_np=0;
      os<<LogIO::DEBUGGING<<name()<<": plot_np expected "<<np<<" "<<cumul(econo_density)<<LogIO::POST;
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
  if( threshold>0 ) 
  {
    sprintf(s,"%s=%g ",RF_THR,threshold);
    desc += s;
  }
  if( min_population ) 
  {
    sprintf(s,"%s=%d ",RF_MINPOP,min_population );
    desc += s;
  }
  sprintf(s,"%s=%d,%d ",RF_NBINS,nbin_uv,nbin_y);
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
    rec.define(RF_MINPOP,0);
    rec.define(RF_NBINS,50);
    rec.define(RF_PLOTCHAN,False);
    rec.define(RF_ECONOPLOT,True);
    
    rec.setComment(RF_COLUMN,"Use column: [DATA|MODEL|CORRected]");
    rec.setComment(RF_EXPR,"Expression for deriving value (e.g. \"ABS XX\", \"+ ABS XX YY\")");
    rec.setComment(RF_THR,"Probability cut-off");
    rec.setComment(RF_MINPOP,"Bin population cut-off");
    rec.setComment(RF_NBINS,"Number of bins (single value, or [NUV,NY])");
    rec.setComment(RF_PLOTCHAN,"Make plot for a specific channel, or F for no plot");
    rec.setComment(RF_ECONOPLOT,"Produce a simplified plot: T/F");
  }
  return rec;
}

void RFAUVBinner::makePlot ( PGPlotterInterface &pgp,uInt ich )
{
  const Float xbox_arr[] = {0,0,1,1,0}, ybox_arr[] = {0,1,1,0,0};
  const Vector<Float> xbox0(IPosition(1,5),xbox_arr),
                    ybox0(IPosition(1,5),ybox_arr);
  Matrix<Int> bins( bincounts.xyPlane(ich) );
  char s1[256],s2[256],s3[256];
  Int maxcount = plot_prob.nelements();
  
// make plot of bin density and flagged bins
  pgp.env(uvmin(ich),uvmax(ich),ymin(ich),ymax(ich),0,0);
  pgp.bbuf();
  sprintf(s1,"%s chunk %d, channel %d, %d by %d bins",
      chunk.msName().chars(),chunk.nchunk(),ich,nbin_uv,nbin_y);
  pgp.lab("UV distance",RFDataMapper::descExpression(),s1);
  // in econo-plot mode, plot image of bin density
  if( econoplot )
  {
    Matrix<Float> img( bins.shape() );
    convertArray(img,bins);
    img += maxcount*.5f; // shift up (for darker greyscales)
    // we want only the least-populous 10% to be plotted as points,
    // the rest as an image. use the critical density
    img( bins<=econo_density ) = 0;
    // setup TR function and plot the image
    const Float tr_array[] = {uvmin(ich)-uvbinsize(ich)/2,uvbinsize(ich),0,
                            ymin(ich)-ybinsize(ich)/2,0,ybinsize(ich) }; 
    const Vector<Float> tr(IPosition(1,6),tr_array);
    pgp.imag(img,maxcount,0,tr);
    // pgp.wedg("RI",.5,4,maxcount,0,"");
  }
  // plot individual points
  if( plot_np != plot_px.nelements() )
  {
    os<<LogIO::DEBUGGING<<": plot_np stats: "<<plot_np<<"/"<<plot_px.nelements()<<LogIO::POST;
    plot_px.resize(plot_np,True);
    plot_py.resize(plot_np,True);
  }
  pgp.pt( plot_px,plot_py,DOT );
  int ci0 = pgp.qci();
  // plot boxes around flagged bins
  Vector<Float> xbox( xbox0*uvbinsize(ich) ),
               ybox( ybox0*ybinsize(ich) );
  for( uInt i=0; i<bins.ncolumn(); i++ )
    for( uInt j=0; j<bins.nrow(); j++ )
      if( bins(j,i)<0 )
      {
        Vector<Float> xb(xbox+uvmin(ich)+j*uvbinsize(ich)),
                     yb(ybox+ymin(ich)+i*ybinsize(ich));
        pgp.line(xb,yb);
        pgp.sci(48);
        Vector<Float> x(2),y(2);
        x(0)=xb(0); x(1)=xb(2); y(0)=yb(0); y(1)=yb(2);
        pgp.line(x,y);
        x(0)=xb(1); x(1)=xb(3); y(0)=yb(1); y(1)=yb(3);
        pgp.line(x,y);
        pgp.sci(ci0);
      }
  
// make probability plot
  // sum up flagged pixels
  Int bincount=0,pixcount=0;
  for( uInt i=0; i<bins.ncolumn(); i++ )
    for( uInt j=0; j<bins.nrow(); j++ )
      if( bins(j,i)<0 )
      {
        bincount++;
        pixcount+= -bins(j,i);
      }
  // make plot of probability distributions
  Vector<Float> ind(maxcount);
  indgen(ind); 
  // zoom display into interesting part of curve
  Float pmax;
  Int popmax = plot_thr_count*3;
  if( popmax>=maxcount || !popmax )
  {
    pmax = 1;
    popmax = maxcount-1;
  }
  else
  {
    pmax = plot_prob(popmax);
  }
  // setup plot
  pgp.env(1,popmax,0,pmax,0,0);
  sprintf(s1,"Total bin population (threshold %d)",plot_thr_count);
  sprintf(s2,"Cumulative probability (%g cut-off)",threshold);
  sprintf(s3,"%d of %d points in %d bins flagged",pixcount,totcounts(ich),bincount);
  pgp.lab(s1,s2,s3);
  // plot probability curve
  pgp.line(ind(Slice(0,popmax+1)),plot_prob(Slice(0,popmax)));
  // plot cut-off points
  pgp.sls(LINE_DOT);
  Vector<Float> xx(2,plot_thr_count+.5);
  Vector<Float> yy(2,0.0f); yy(1)=1;
  pgp.line(xx,yy);
  xx(0)=1; xx(1)=plot_thr_count+.5;
  yy=threshold;
  pgp.line(xx,yy);
  // plot full curve in a sub-box corner
  Float xs=(popmax-1)/3.0;
  Float ys=pmax/3.0;
  Float x0=popmax-1.1*xs;
  Float y0=.1*ys;
  pgp.sls(LINE_FULL);
  pgp.line(xbox0*xs+x0,ybox0*ys+y0); // box around graph
  pgp.line(ind*(xs/maxcount)+x0,plot_prob*ys+y0); // graph
  pgp.sch(.5);
  Vector<Float> cs( pgp.qcs(4)/4 );
  pgp.ptxt(x0-cs(0),y0,0,1,"0");
  pgp.ptxt(x0-cs(0),y0+ys-cs(1)*4,0,1,"1");
  pgp.ptxt(x0,y0+ys+cs(1),0,0,"0");
  pgp.ptxt(x0+xs,y0+ys+cs(1),0,1,String::toString(maxcount));
  pgp.sch(1);

  plot_px.resize();
  plot_py.resize();
  plot_prob.resize();
}
