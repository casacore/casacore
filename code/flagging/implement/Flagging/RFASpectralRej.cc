//# RFASpectralRej.cc: this defines RFASpectralRej
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

#include <flagging/Flagging/RFASpectralRej.h> 
#include <scimath/Functionals/Polynomial.h>
#include <msvis/MSVis/VisibilityIterator.h>
#include <msvis/MSVis/VisBuffer.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/Slice.h>
#include <casa/System/PGPlotterInterface.h>
    
#include <casa/stdio.h>
#include <casa/stdlib.h>
    
namespace casa { //# NAMESPACE CASA - BEGIN

void RFASpectralRej::addSegment ( Int spwid,Double fq0,Double fq1,Int ch0,Int ch1 )
{
  Segment seg = { spwid,fq0,fq1,ch0,ch1 };
  uInt n = segments.nelements();
  segments.resize(n+1);
  segments[n] = seg;
}

// -----------------------------------------------------------------------
// parseRegion
// Helper function to parse one region specification. Returns
// the number of ranges parsed.
// -----------------------------------------------------------------------
void RFASpectralRej::parseRegion ( const RecordInterface &parm )
{
  Bool parsed = False;
  Int spwid = -1;
  if( isFieldSet(parm,RF_SPWID) && fieldType(parm,RF_SPWID,TpInt) )
    spwid = parm.asInt(RF_SPWID) - indexingBase();
// figure out how channel ranges are specified - frequencies or channel #'s
// First version is frequencies
  if( fieldSize(parm,RF_FREQS)>1 )
  {
    Array<Double> freqarr;
    try {
      freqarr = parm.toArrayDouble(RF_FREQS);
      // make sure array is of the right shape (can be reformed into 2xN)
      if( freqarr.ndim()>2 || (freqarr.nelements()%2) !=0 )
        throw( AipsError("") );
    } catch( AipsError x ) {
      os<<"Illegal \""<<RF_FREQS<<"\" array\n"<<LogIO::EXCEPTION;
    }
    Matrix<Double> fq( freqarr.reform(IPosition(2,2,freqarr.nelements()/2)) );
    // enqueue the region specs
    for( uInt i=0; i<fq.ncolumn(); i++ )
    {
      if( fq(0,i) >= fq(1,i) )
      {
        char s[128];
        sprintf(s,"Illegal spectral region %0.2f-%0.2f\n",fq(0,i),fq(1,i));
        os<<s<<LogIO::EXCEPTION;
      }
      addSegment(spwid,fq(0,i),fq(1,i),-1,-1);
    }
    parsed=True;
  }
// second option is channel numbers
  if( fieldSize(parm,RF_CHANS)>1 )
  {
    Array<Int> arr;
    try {
      arr = parm.toArrayInt(RF_CHANS);
      // make sure array is of the right shape (can be reformed into 2xN)
      if( arr.ndim()>2 || (arr.nelements()%2) !=0 )
        throw( AipsError("") );
    } catch( AipsError x ) {
      os<<"Illegal \""<<RF_CHANS<<"\" array\n"<<LogIO::EXCEPTION;
    }
    Matrix<Int> ch( arr.reform(IPosition(2,2,arr.nelements()/2)) );
    // enqueue the region specs
    for( uInt i=0; i<ch.ncolumn(); i++ )
    {
      if( ch(0,i) >= ch(1,i) )
      {
        char s[128];
        sprintf(s,"Illegal spectral region #%d-%d\n",ch(0,i),ch(1,i));
        os<<s<<LogIO::EXCEPTION;
      }
      ch -= (Int)indexingBase();
      addSegment(spwid,0,0,ch(0,i),ch(1,i));
    }
    parsed=True;
  }
  if( !parsed )
    os<<"\""<<RF_FREQS<<"\" or \""<<RF_CHANS<<"\" must be specified\n"<<LogIO::EXCEPTION;
}
    
RFASpectralRej::RFASpectralRej  ( RFChunkStats &ch,const RecordInterface &parm ) : 
    RFAFlagCubeBase(ch,parm),
    RFDataMapper(parm.asArrayString(RF_EXPR),parm.asString(RF_COLUMN)),
    ndeg( parm.asInt(RF_NDEG) ),
    halfwin( parm.asInt(RF_ROW_HW) ),
    threshold( parm.asDouble(RF_ROW_THR) ),
    rowclipper(chunk,flag,threshold,halfwin)
{
// figure out how channel ranges are specified
// if a full region record is specified, parse each element
// otherwise just parse the parameter record itself
  if( isValidRecord(parm,RF_REGION) ) // full region record
  {
    const RecordInterface &reg( parm.asRecord(RF_REGION) );
    for( uInt i=0; i<reg.nfields(); i++ )
    {
      if( reg.type(i) != TpRecord )
        os<<"\""<<RF_REGION<<"\" must be a record of records\n"<<LogIO::EXCEPTION;
      parseRegion(reg.asRecord(i));
    }
  }
  else // else assume only one region specified
    parseRegion(parm);
  
  if( !segments.nelements() )
    os<<"No spectral region has been specified\n"<<LogIO::EXCEPTION;
    
// set up fitter
  Polynomial<AutoDiff<Float> > poly(ndeg);
  fitter.setFunction(poly);
  
// set up debugging info
  if( fieldType(parm,RF_DEBUG,TpArrayInt) )
  {
    Vector<Int> dbg;
    parm.get(RF_DEBUG,dbg);
    Int ifr=0,it=0;
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
    debug = RFDebugPlot(chunk.pgpscr(),-1,ifr,it);
    rowclipper.setDebug(debug);
  }
}

// -----------------------------------------------------------------------
// newChunk
// At each new chunk, figure out which channels fit into the
// specified fitting regions.
// -----------------------------------------------------------------------
Bool RFASpectralRej::newChunk (Int &maxmem)
{
// compute correlations mask, return False if fails
  corrmask = RFDataMapper::corrMask(chunk.visIter());
  if( !corrmask )
  {
    os<<LogIO::WARN<<"missing selected correlations, ignoring this chunk\n"<<LogIO::POST;
    return active=False;
  }
// figure out active channels (i.e. within specified segments)
  Int spwid = chunk.visBuf().spectralWindow();
  fitchan.resize(num(CHAN));
  fitchan.set(False);
  const Vector<Double> & fq( chunk.frequency()*1e-6 );
  for( uInt i=0; i<segments.nelements(); i++)
  {
    const Segment &seg ( segments[i] );
    // compare spectral windows, if specified
    if( seg.spwid >= 0 && seg.spwid != spwid )
      continue;
    if( seg.ch0 >= 0 )  // use channel numbers
    {
      if( (uInt)seg.ch0 < num(CHAN) )
      {
        Int ch1 = num(CHAN)-1;
        if( seg.ch1 < ch1 )
          ch1 = seg.ch1;
        fitchan(Slice(seg.ch0,ch1-seg.ch0+1)) = True;
      }
    }
    else // use frequencies
    {
      fitchan = fitchan || ( fq >= seg.fq0 && fq <= seg.fq1 );
    }
  }
// count number of fitted channels  
  num_fitchan = 0;
  for( uInt i=0; i<num(CHAN); i++ )
  {
    if( fitchan(i) )
    {
      xnorm = i;
      num_fitchan++;
    }
  }
// return if none 
  os<<num_fitchan<<" channels will be fitted in this chunk\n"<<LogIO::POST;
  if( num_fitchan<ndeg+2 )
  {
    os<<LogIO::WARN<<"not enough channels, ignoring chunk\n"<<LogIO::POST;
    return active=False;
  }
// finish with init  
  RFAFlagCubeBase::newChunk(maxmem-=1);
  rowclipper.init(num(IFR),num(TIME));
  return active=True;
}

void RFASpectralRej::endChunk ()
{
  RFAFlagCubeBase::endChunk();
  flag.cleanup();
  rowclipper.cleanup();
}

void RFASpectralRej::startData ()
{
  RFAFlagCubeBase::startData();
  rowclipper.reset();
}

RFA::IterMode RFASpectralRej::iterTime (uInt it)
{
  RFAFlagCubeBase::iterTime(it);
  RFDataMapper::setVisBuffer(chunk.visBuf());
  return RFA::CONT;
}

RFA::IterMode RFASpectralRej::iterRow ( uInt irow )
{
// during first pass, compute diff-median. Also keep track of the AAD.
  uInt iifr = chunk.ifrNum(irow);
  uInt it = chunk.iTime();
  Bool rowfl = chunk.npass() ? flag.rowFlagged(iifr,it) 
                            : flag.rowPreFlagged(iifr,it);
  Bool dbg = ( debug.index(0,iifr,it) >= 0 );
  
  if( !rowfl )   
  {
    Vector<Float> x(num_fitchan),y(num_fitchan);
    Vector<uInt>  chan(num_fitchan);
    uInt np=0;
// loop over channels, collect valid (non-flagged) pixels into the x and y
// vectors
    for( uInt ich=0; ich<num(CHAN); ich++ )
    {
      if( fitchan(ich) && !(chunk.npass() ? flag.anyFlagged(ich,iifr) : flag.preFlagged(ich,iifr)) )
      {
        x(np) = ich/xnorm;
        y(np) = mapValue(ich,irow);
        chan(np++) = ich;
      }
    }
    if( dbg )
      dprintf(os,"Row %d: %d points can be fitted\n",irow,np);
// check that we have enough points to constrain the fit
    if( np > ndeg+2 )
    {
  // resize x/y vectors, and do the fit
      Slice S(0,np);
      Vector<Float> sigma(np,1.0f),x1(x(S)),y1(y(S));
      Vector<Float> c = fitter.fit(x1,y1,sigma);
      Float chisq = fitter.chiSquare();
      rowclipper.setSigma(iifr,it,chisq);
  // produce debugging plot if needed
      if( dbg )
      {
	Polynomial<Float> poly(ndeg);
        poly.setCoefficients(c);
        Vector<Float> yfit(np);
        for( uInt i=0; i<y1.nelements(); i++ )
          yfit(i) = poly(x1(i));
        
        cerr<<"Sigma: "<<sigma<<LogIO::POST;
        dprintf(os,"Row %d: chisq=%f\n",irow,chisq);
 
        Vector<Float> data(num(CHAN));
        for( uInt ich=0; ich<num(CHAN); ich++ )
          data(ich) = mapValue(ich,irow);
        
        PGPlotterInterface &pgp( debug.pgp() );
        Float vmax = ::casa::max( ::casa::max(static_cast<Array<Float> >(y1),static_cast<Array<Float> >(yfit)) );
        Float vmin = ::casa::min( ::casa::min(static_cast<Array<Float> >(y1),static_cast<Array<Float> >(yfit)) ); 
        for( Bool redraw=True; redraw;  )
        {
          pgp.ask(False);
          pgp.subp(1,1);
          pgp.eras();
          pgp.sci(BLACK);
          pgp.env(0,num(CHAN),vmin,vmax,0,0);
          char s[256];
          sprintf(s,"IFR %d (%s), time slot %d",debug.ifr(),chunk.ifrString(debug.ifr()).chars(),debug.time());
          pgp.lab("Channel","",s);
          
          Vector<Float> xdata(data.nelements());
          indgen(xdata);

          pgp.sci(BLACK);
          pgp.line(xdata,data);
          pgp.sci(YELLOW);
          pgp.line(x1*xnorm,yfit);
          
          redraw = debug.queryPlotLimits(vmin,vmax);
        }
        pgp.ask(True);
      } // endif( dbg )
    } // endif( np>ndeg+2)
  } // endif( !rowfl )
  else if( dbg )// a row flag
  {
    dprintf(os,"Row %d is flagged, so ignoring\n",irow);
  }
  
  return RFA::CONT;
}

// -----------------------------------------------------------------------
// endData
// Ends data pass
// -----------------------------------------------------------------------
RFA::IterMode RFASpectralRej::endData ()
{
  RFAFlagCubeBase::endData();
  uInt dum;
  rowclipper.updateSigma(dum,dum);
  return RFA::STOP;
}

// -----------------------------------------------------------------------
// RFASpectralRej::getDesc
// Returns description of parameters
// -----------------------------------------------------------------------
String RFASpectralRej::getDesc ()
{
  String desc( RFDataMapper::description()+";" );
  char s[256];
// build up description of spectral segments
  for( uInt i=0; i<segments.nelements(); i++)
  {
    const Segment &seg ( segments[i] );
    // is spwid specified?
    char s1[32];
    if( seg.spwid >= 0 )
      sprintf(s1,"%d:",seg.spwid);
    else
      s1[0]=0;
    if( seg.ch0 >= 0 )  // use channel numbers
      sprintf(s, " %s#%d-%d",s1,seg.ch0,seg.ch1);
    else
      sprintf(s, " %s%.2f-%.2fMHz",s1,seg.fq0,seg.fq1);
    desc+=s;
  }
  desc+="; ";
  sprintf(s,"%s=%d %s=%.1f %s=%d ",RF_NDEG,ndeg,RF_ROW_THR,threshold,RF_ROW_HW,halfwin);
  desc+=s;
  desc+=RFAFlagCubeBase::getDesc();
  return desc;
}

// -----------------------------------------------------------------------
// RFASpectralRej::getDefaults
// Returns record of default parameters
// -----------------------------------------------------------------------
const RecordInterface & RFASpectralRej::getDefaults ()
{
  static Record rec;
// create record description on first entry
  if( !rec.nfields() )
  {
    rec = RFAFlagCubeBase::getDefaults();
    rec.define(RF_NAME,"SpectralRej");
    rec.define(RF_COLUMN,"DATA");
    rec.define(RF_EXPR,"+ ABS XX YY");
    rec.define(RF_NDEG,(Int)2);
    rec.define(RF_ROW_THR,(Double)5);
    rec.define(RF_ROW_HW,(Int)6);
    rec.define(RF_REGION,False);
    rec.define(RF_SPWID,False);
    rec.define(RF_FREQS,False);
    rec.define(RF_CHANS,False);
    rec.define(RF_DEBUG,False);
    
    rec.setComment(RF_COLUMN,"Use column: [DATA|MODEL|CORRected]");
    rec.setComment(RF_EXPR,"Expression for deriving value (e.g. \"ABS XX\", \"+ ABS XX YY\")");
    rec.setComment(RF_NDEG,"Number of degrees for polynomial fit");
    rec.setComment(RF_ROW_THR,"Row flagging threshold, in AADs");
    rec.setComment(RF_ROW_HW,"Row flagging, half-window of sliding median (0 to use overall median)");
    rec.setComment(RF_SPWID,"Spectral window ID (F or -1 for all)");
    rec.setComment(RF_FREQS,"Range(s) of frequencies (in MHz)");
    rec.setComment(RF_CHANS,"Range(s) of channel numbers");
    rec.setComment(RF_REGION,"For several spectral regions, record of records");
    rec.setComment(RF_DEBUG,"Set to [NIFR,NTIME] or [ANT1,ANT2,NTIME] to produce debugging plots");
  }
  return rec;
}

  


} //# NAMESPACE CASA - END

