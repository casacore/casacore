//# RFANewMedianClip.cc: this defines RFANewMedianClip
//# Copyright (C) 2000,2001,2002,2003,2004
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

#include <flagging/Flagging/RFANewMedianClip.h>
#include <casa/Arrays/ArrayMath.h>
#include <msvis/MSVis/VisBuffer.h>
#include <casa/System/PGPlotterInterface.h>

#include <casa/stdio.h>    
namespace casa { //# NAMESPACE CASA - BEGIN

// -----------------------------------------------------------------------
// RFANewMedianClip
// Accumulator class for computing the median per channels over time.
// Internally, we store a matrix of nifr x nchan medians.
// -----------------------------------------------------------------------
RFANewMedianClip::RFANewMedianClip( RFChunkStats &ch,const RecordInterface &parm ) :
  RFAFlagCubeBase(ch,parm), 
  RFDataMapper(parm.asArrayString(RF_EXPR),parm.asString(RF_COLUMN)),
  threshold( parm.asDouble(RF_THR) )
{
  msl = NULL;
}

RFANewMedianClip::~RFANewMedianClip ()
{
  if( msl ) delete [] msl;
}

uInt RFANewMedianClip::estimateMemoryUse () 
{
  return RFAFlagCubeBase::estimateMemoryUse() +
    evalue.estimateMemoryUse(num(CHAN),num(IFR),num(TIME)) + 2;
}


const RecordInterface & RFANewMedianClip::getDefaults ()
{
  static Record rec;
  // create record description on first entry
  if( !rec.nfields() )
  {
    rec = RFAFlagCubeBase::getDefaults();
    rec.define(RF_NAME,"NewTimeMedian");    
    rec.define(RF_COLUMN,"DATA");
    rec.define(RF_EXPR,"+ ABS XX YY");
    rec.define(RF_THR,Double(5));    
    //    rec.define(RF_DEBUG,False);
    rec.setComment(RF_COLUMN,"Use column: [DATA|MODEL|CORRected]");
    rec.setComment(RF_EXPR,"Expression for deriving value (e.g. \"ABS XX\", \"+ ABS XX YY\")");
    rec.setComment(RF_THR,"Probability cut-off");
    //    rec.setComment(RF_DEBUG,"Set to [CHAN,IFR] to produce debugging plots");
  }
  return rec;
}

// resets for new calculation
Bool RFANewMedianClip::newChunk (Int &maxmem)
{
  // if disk-based flag cube, reserve 2MB for local iterator
  // compute correlations mask, return False if fails
  corrmask = RFDataMapper::corrMask(chunk.visIter());
  if( !corrmask )
  {
    os<<LogIO::WARN<<"missing selected correlations, ignoring this chunk\n"<<LogIO::POST;
    return active=False;
  }

  if( !flag.getMaxMem() )
    maxmem -= 2; 
  uInt halfwin = num(TIME)/2;
  // reserve memory for our bunch of median sliders
  maxmem -= (num(CHAN)*num(IFR)*MedianSlider::objsize(halfwin))/(1024*1024)+1;

  Int mmdiff = (Int)(1.05*evalue.estimateMemoryUse(num(CHAN),num(IFR),num(TIME))); // sufficient memory? reserve it
  if( maxmem>mmdiff ) 
    maxmem -= mmdiff;  
  else // insufficient memory: use disk-based lattice
  {
    mmdiff = 0;
    maxmem -= 2; // reserve 2Mb for the iterator anyway
  }

  // call parent's newChunk  
  if( !RFAFlagCubeBase::newChunk(maxmem) )
    return active=False;

  // create temp lattice for evalues
  evalue.init(num(CHAN),num(IFR),num(TIME), 0, mmdiff ,2);
  //init stdev matrix
  stdev.resize(num(CHAN), num(IFR));
  stdev.set(0);
  stdeved = False;
  // create local flag iterator
  flag_iter = flag.newCustomIter();
  pflagiter = &flag_iter;
  RFAFlagCubeBase::newChunk(maxmem-=1);

  return active=True;
}

void RFANewMedianClip::endChunk ()
{
  RFAFlagCubeBase::endChunk();
// create local flag iterator
  flag_iter = FlagCubeIterator();
  if( msl ) delete [] msl;
  msl = NULL;
}

// startData
// create new median sliders at start of data pass
void RFANewMedianClip::startData ()
{
  //new added
  evalue.reset(False,True);

  RFAFlagCubeBase::startData();
  flag_iter.reset();

  pflagiter = &flag.iterator();
  if( msl ) delete [] msl;
  globalsigma = 0;
  // this is a workaround for a compiler bug that we occasionally see
  uInt tmpnum2 = num(CHAN)*num(IFR);
  uInt halfwin = num(TIME)/2;
  // create nchan x nifr median sliders
  msl = new MedianSlider[tmpnum2];
  for(uInt i=0; i<num(CHAN)*num(IFR); i++)
    msl[i] = MedianSlider(halfwin);
  globalmed = MedianSlider(tmpnum2);
}

// iterTime
RFA::IterMode RFANewMedianClip::iterTime ( uInt it )
{
  evalue.advance(it);
  RFAFlagCubeBase::iterTime(it);
  // gets pointer to visibilities cube
  RFDataMapper::setVisBuffer(chunk.visBuf());
  // Advance sync flag iterator
  flag.advance(it,True);

  return RFA::CONT;
}

// -----------------------------------------------------------------------
// RFANewMedianClip::iterRow
// Processes one row of data for per-channel medians over time
// -----------------------------------------------------------------------
RFA::IterMode RFANewMedianClip::iterRow ( uInt irow )
{
  uInt iifr = chunk.ifrNum(irow);
  uInt it = chunk.iTime();
  Float val = 0;

  Bool rowfl = chunk.npass() ? flag.rowFlagged(iifr,it) 
                            : flag.rowPreFlagged(iifr,it);
  if( rowfl ) {
    // the whole row is flagged, so just advance all median sliders
    for( uInt i=0; i<num(CHAN); i++ ) 
      slider(i,iifr).next();
  } 
  else {
    // loop over channels for this spw, ifr
    for( uInt ich=0; ich<num(CHAN); ich++ )
      {
	// during first pass, look at pre-flags only. During subsequent passes,
	// look at all flags
	Bool fl = chunk.npass() ? flag.anyFlagged(ich,iifr) : flag.preFlagged(ich,iifr);
	val = mapValue(ich,irow);
	slider(ich,iifr).add( val,fl ); 
	if( !fl ) {	
	  evalue(ich,iifr) = val;
	}
      }
  }
  return RFA::CONT;
}


// -----------------------------------------------------------------------
// RFANewMedianClip::endData
// Called at end of iteration 
// -----------------------------------------------------------------------
RFA::IterMode RFANewMedianClip::endData ()
{
  RFAFlagCubeBase::endData();
  return RFA::DRY;
}


void RFANewMedianClip::startDry ()
{
  if(!stdeved) 
    RFAFlagCubeBase::startDry();
  // reset lattices to read-only
  evalue.reset(True,False);
  pflagiter = &flag.iterator();
}



// -----------------------------------------------------------------------
// RFANewMedianClip::iterDry
// Dry run iterator: recomputes the AAD and does flagging
// -----------------------------------------------------------------------
RFA::IterMode RFANewMedianClip::iterDry ( uInt it )
{
  RFAFlagCubeBase::iterDry(it);
  evalue.advance(it);
  Float m = globalmed.median();
  //  already got standard deviation
  if(stdeved) {
    Float upperdiff = 0;
    Float bottomdiff = 0;
    Float thr = 0;
    Bool asymmetry = False;
    for( uInt ifr=0; ifr<num(IFR); ifr++ ) // outer loop over IFRs
      {
	for( uInt ich=0; ich<num(CHAN); ich++ ) // loop over channels
	  {
	    //thr = threshold * stdev(ich, ifr);
	    // globalsigma is the average of standard deviations
	    thr = threshold * globalsigma;
	    if( !flag.anyFlagged(ich, ifr) ) // skip if whole row is flagged
	      {
		Float d = evalue(ich,ifr);
		//	Float m = slider(ich,ifr).median();
		if( abs(d - m) > thr )   // should be clipped?
		  {
		    flag.setFlag(ich,ifr, *pflagiter);
		  }
		else {
		  if(d-m > 0 && d - m > upperdiff)
		    upperdiff = d-m;
		  else if( d - m < 0 && d - m < bottomdiff)
		    bottomdiff = d - m;
		  //		  cout << evalue(ich, ifr) << endl;
		}
	      }
	  } // for(ich)
      } // for(ifr)
    if(abs(bottomdiff) > 1.2 * upperdiff || upperdiff > 1.2 * abs(bottomdiff))
      asymmetry = True; 
    if(asymmetry) {
      //      cout << " flag the asymmetry data" << endl;
      for( uInt ifr=0; ifr<num(IFR); ifr++ ) // outer loop over IFRs
	{
	  for( uInt ich=0; ich<num(CHAN); ich++ ) // loop over channels
	    {
	      //	    Float thr = threshold * stdev(ich, ifr);
	      // try global sigma
	      if(upperdiff < abs(bottomdiff))
		thr = upperdiff;
	      else {
		thr = abs(bottomdiff);
	      }
	      if( !flag.anyFlagged(ich, ifr) ) // skip if whole row is flagged
		{
		  Float d = evalue(ich,ifr);
		  if( abs(d - m) > thr )   // should be clipped?
		    {
		      flag.setFlag(ich,ifr, *pflagiter);
		    }
		}
	    } // for(ich)
	} // for(ifr)
    }
  } else { //compute the standard deviation
    for( uInt ifr=0; ifr<num(IFR); ifr++ ) // outer loop over IFRs
      {
	for( uInt ich=0; ich<num(CHAN); ich++ ) // loop over channels
	  {
	    Bool fl = flag.anyFlagged(ich, ifr);
	    if(!fl) {
	      Float diff = evalue(ich,ifr) - slider(ich,ifr).median();
	      stdev(ich, ifr) += diff * diff;
	    }
	  }
      }
  } // end else
  return RFA::CONT;
}
    
RFA::IterMode RFANewMedianClip::endDry ()
{
  if(stdeved) {
    //    cout << " early return " << endl; 
    return RFA::STOP;
  }
  Bool dummy = False;
  stdeved = True;
  for( uInt ifr=0; ifr<num(IFR); ifr++ ) // outer loop over IFRs
    {
      for( uInt ich=0; ich<num(CHAN); ich++ ) // loop over channels
	{
	  if(slider(ich, ifr).nval()){
	    stdev(ich, ifr) /= slider(ich, ifr).nval();
	    //	  cout << "variance " << stdev(ich, ifr) << endl;
	    stdev(ich, ifr) = sqrt(stdev(ich, ifr));
	    globalmed.add(slider(ich, ifr).median(), dummy);
	  } else {
	    stdev(ich, ifr) = 0;
	  }
	}
    }
  globalsigma = sum(stdev)/(num(CHAN) * num(IFR));
  return RFA::DRY;
}

// -----------------------------------------------------------------------
// getDesc
// -----------------------------------------------------------------------
String RFANewMedianClip::getDesc ()
{
  String desc( RFDataMapper::description()+"; " );
  char s[256];
  sprintf(s," %s=%f",RF_THR, threshold);
  desc += s;
  return RFAFlagCubeBase::getDesc()+s;
}


} //# NAMESPACE CASA - END

