//# RFChunkStats.cc: this defines RFChunkStats
//# Copyright (C) 2000,2001,2002,2003
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
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <trial/Flagging/RFChunkStats.h>
#include <trial/Flagging/RedFlagger.h>
#include <trial/MeasurementEquations/VisibilityIterator.h>
#include <trial/MeasurementEquations/VisBuffer.h>
#include <stdio.h>
#include <aips/sstream.h>
#include <trial/Tasking/PGPlotter.h>
    
// when no plotter is specified for screen/report,
// use a null (unattached plotter)
static class PGPlotter nullPGPlotter;
        
RFChunkStats::RFChunkStats( VisibilityIterator &vi,VisBuffer &vb,RedFlagger &rf,
    PGPlotterInterface *pgp_scr,PGPlotterInterface *pgp_rep )
  : visiter(vi),
    visbuf(vb),
    flagger(rf),
    pgp_screen(pgp_scr?pgp_scr:&nullPGPlotter),
    pgp_report(pgp_rep?pgp_rep:&nullPGPlotter)
{
  chunk_no=0;
  counts[ANT] = flagger.numAnt();
  counts[IFR] = flagger.numIfr();
}

const MeasurementSet & RFChunkStats::measSet () const 
{ 
  return flagger.measSet(); 
} 

const String RFChunkStats::msName () const 
{ 
  return flagger.measSet().tableName(); 
} 

const Vector<String> & RFChunkStats::antNames () const 
{ 
  return flagger.antNames(); 
} 

const Vector<Double> & RFChunkStats::frequency () 
{ 
  return visiter.frequency(freq); 
}

PGPlotterInterface & RFChunkStats::pgpscr() const  
{ return flagger.pgpscr(); }
PGPlotterInterface & RFChunkStats::pgprep() const  
{ return flagger.pgprep(); }
void RFChunkStats::setReportPanels (Int nx,Int ny) const 
{ flagger.setReportPanels(nx,ny); }

void RFChunkStats::newChunk ()
{
  itime=-1;
  chunk_no++;
// setup shapes
  visshape = visiter.visibilityShape();
  counts[POLZN] = visshape(0);
  counts[CHAN] = visshape(1);
  counts[TIME] = visiter.nSubInterval();
//  counts[TIME] = flagger.numTime();
//  RedFlagger::logSink()<<LogIO::WARN<< 
//    "RFChunkStats::newChunk(): "
//    "VisIter::nSubInterval() not yet implemented. Using global NTIME instead\n"<<LogIO::POST;
  counts[ROW]  = visiter.nRowChunk();
// get correlation types
  visiter.corrType(corrtypes);
// reset min/max time slots
  start_time = end_time = current_time = 0;

// setups statistics
  nrf_time.resize(num(TIME));
  nrf_time.set(0);
  nrf_ifr.resize(num(IFR));
  nrf_ifr.set(0);
  rows_per_ifr.resize(num(IFR));
  rows_per_ifr.set(0);
  
  nf_ifr_time.resize(num(IFR),num(TIME));
  nf_ifr_time.set(0);
  nf_chan_ifr.resize(num(CHAN),num(IFR));
  nf_chan_ifr.set(0);
      
// build up description of correlations
  corr_string = "";
  for( uInt i=0; i<corrtypes.nelements(); i++ )
    corr_string += " " + Stokes::name( Stokes::type(corrtypes(i)) );
  char s[256];
  
  sprintf(s,"Chunk %d: %s, %d channels, %d time slots, %d baselines, %d rows\n",
            chunk_no,corr_string.chars(),num(CHAN),num(TIME),num(IFR),num(ROW));
  RedFlagger::logSink()<<s<<LogIO::POST;
}

void RFChunkStats::newPass (uInt npass)
{
  itime = -1;
  pass_no = npass;
}

void RFChunkStats::newTime ()
{
// setup IFR numbers for every row in time slot
  ifr_nums.resize( visbuf.antenna1().nelements() );
  ifr_nums = flagger.ifrNumbers( visbuf.antenna1(),visbuf.antenna2() );
// reset stats
  for( uInt i=0; i<ifr_nums.nelements(); i++ )
    rows_per_ifr(ifr_nums(i))++;
// set start/end times
  current_time = (visbuf.time()(0))/(24*3600);
  if( current_time<start_time || start_time==0 )
    start_time = current_time;
  if( current_time>end_time )
    end_time = current_time;
  itime++;
//  dprintf(os,"newTime: %d\n",itime);
}


template<class T> RFlagWord RFChunkStats::getCorrMask ( const Vector<T> &corrspec )
{
  RFlagWord mask=0;
  // loop over polzn spec
  for( uInt i=0; i<corrspec.nelements(); i++ )
  {
    // convert element of polspec to Stokes type
    Stokes::StokesTypes type = Stokes::type( corrspec(i) );
    if( type == Stokes::Undefined ){
      ostringstream oss;
      oss << corrspec(i) << ends;
      throw(AipsError( String("Unknown correlation type: ")+ String(oss)));
    }
    // find this type in current corrarizations
    Int icorr = findCorrType(type,corrtypes);
    if( icorr>=0 )
      mask |= (1<<icorr);
  }
  return mask;
}

template RFlagWord RFChunkStats::getCorrMask<Int>( const Vector<Int> &);
template RFlagWord RFChunkStats::getCorrMask<String>( const Vector<String> &);


uInt RFChunkStats::antToIfr ( uInt ant1,uInt ant2 )
{
  return flagger.ifrNumber((Int)ant1,(Int)ant2);
}

void RFChunkStats::ifrToAnt ( uInt &ant1,uInt &ant2,uInt ifr )
{ 
  flagger.ifrToAnt(ant1,ant2,ifr); 
}

String RFChunkStats::ifrString ( uInt ifr )
{
  uInt a1,a2;
  ifrToAnt(a1,a2,ifr);
  char s[32];
  sprintf(s,"%d-%d",a1,a2);
  return String(s);
}

void RFChunkStats::printStats ()
{
// collapse 2D stats into 1D arrays
  Vector<uInt> nf_ifr(num(IFR)),nf_chan(num(CHAN)),nf_time(num(TIME));
  for( uInt i=0; i<num(TIME); i++)
    nf_time(i) = sum(nf_ifr_time.column(i));
  for( uInt i=0; i<num(IFR); i++)
    nf_ifr(i) = sum(nf_ifr_time.row(i));
  for( uInt i=0; i<num(CHAN); i++)
    nf_chan(i) = sum(nf_chan_ifr.row(i));
    
  uInt n = sum(nf_ifr);
  fprintf( stderr,"%d pixel flags have been raised\n",n );
  fprintf( stderr,"(%f%% of pixels flagged)\n",n*100.0/(num(ROW)*num(CHAN)*num(CORR)));
  n = sum(nrf_ifr);
  fprintf( stderr,"%d row flags have been raised\n",n );
  fprintf( stderr,"(%f%% of rows flagged)\n",n*100.0/num(ROW));
// accumulate per-antenna flags 
  Vector<uInt> flperant(num(ANT),0u),rowperant(num(ANT),0u);
  for( uInt i=0; i<num(IFR); i++ )
  {
    uInt a1,a2;
    ifrToAnt(a1,a2,i);
    if( nf_ifr(i) )
    {
      flperant(a1) += nf_ifr(i);
      flperant(a2) += nf_ifr(i);
    }
    if( nrf_ifr(i) )
    {
      rowperant(a1) += nrf_ifr(i);
      rowperant(a2) += nrf_ifr(i);
    }
  }
  
  cerr<<"Flags per antenna: "<<flperant<<endl;
  cerr<<"Flags per IFR  %: "<<(nf_ifr*100u)/(num(CHAN)*num(TIME))<<endl;
  cerr<<"Flags per chan %: "<<(nf_chan*100u)/(num(IFR)*num(TIME))<<endl;
  cerr<<"Flags per time %: "<<(nf_time*100u)/(num(CHAN)*num(IFR))<<endl;
  cerr<<"Row flags per antenna: "<<rowperant<<endl;
  cerr<<"Row flags per IFR  %: "<<(nrf_ifr*100u)/(num(TIME))<<endl;
  cerr<<"Row flags per time %: "<<(nrf_time*100u)/(num(IFR))<<endl;
}

// -----------------------------------------------------------------------
// findCorrType
// -----------------------------------------------------------------------
Int findCorrType( Stokes::StokesTypes type,const Vector<Int> &corr )
{
  if( type == Stokes::Undefined )
    return -1;
  // find this type in current polarizations
  for( uInt j=0; j<corr.nelements(); j++ )
    if( type == corr(j) )
      return j;
  return -1;
}

