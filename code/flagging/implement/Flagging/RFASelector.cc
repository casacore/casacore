//# RFASelector.cc: this defines RFASelector
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
#include <casa/Exceptions/Error.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/MaskedArray.h>
#include <casa/Arrays/MaskArrMath.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Logging/LogIO.h>
#include <msvis/MSVis/VisibilityIterator.h>
#include <msvis/MSVis/VisBuffer.h>
#include <flagging/Flagging/RFASelector.h>
#include <casa/stdio.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// -----------------------------------------------------------------------
// reformRange
// Reforms an array of 2N elements into a [2,N] matrix
// -----------------------------------------------------------------------
template<class T> Bool RFASelector::reformRange( Matrix<T> &rng,const Array<T> &arr )
{
  if( arr.ndim()>2 || (arr.nelements()%2) !=0 )
    return False;
  rng = arr.reform(IPosition(2,2,arr.nelements()/2));
  return True;
}

template<class T> Array<T> fieldToArray( const RecordInterface &parm,const String &id );

template<> Array<Int> fieldToArray<Int>( const RecordInterface &parm,const String &id )
{ return parm.toArrayInt(id); }
template<> Array<Double> fieldToArray<Double>( const RecordInterface &parm,const String &id )
{ return parm.toArrayDouble(id); }
template<> Array<String> fieldToArray<String>( const RecordInterface &parm,const String &id )
{ return parm.toArrayString(id); }

// -----------------------------------------------------------------------
// RFASelector::parseRange
// Returns a record field of 2N elements as a [2,N] matrix
// -----------------------------------------------------------------------
template<class T> Bool RFASelector::parseRange( Matrix<T> &rng,const RecordInterface &parm,const String &id )
{
  if( isFieldSet(parm,id) )
  {
    try 
    {
      Array<T> arr( fieldToArray<T>(parm,id) );
      if( !reformRange(rng,arr) )
        throw( AipsError("") );
      return True;
    } 
    catch( AipsError x ) 
    {
      os<<"Illegal \""<<id<<"\" array\n"<<LogIO::EXCEPTION;
    }
  }
  return False;
}

template Bool RFASelector::reformRange<Int>( Matrix<Int>&,const Array<Int>& );
template Bool RFASelector::reformRange<Double>( Matrix<Double>&,const Array<Double>& );
template Bool RFASelector::reformRange<String>( Matrix<String>&,const Array<String>& );
template Bool RFASelector::parseRange<Int>( Matrix<Int>&,const RecordInterface&,const String&);
template Bool RFASelector::parseRange<Double>( Matrix<Double>&,const RecordInterface&,const String&);
template Bool RFASelector::parseRange<String>( Matrix<String>&,const RecordInterface&,const String&);

// -----------------------------------------------------------------------
// parseTimes
// Converts a field that is an array of Int/Double or Strings into 
// an array of Doubles. Numeric values are converted as is. Strings 
// are fed through MVTime::read to convert into MJDs (or MJSs, if 
// secs is True).
// -----------------------------------------------------------------------
Bool RFASelector::parseTimes ( Array<Double> &times,const RecordInterface &parm,const String &id,Bool secs )
{
  LogIO os(LogOrigin("RFASelector", "parseTimes()", WHERE));
  if( !isFieldSet(parm,id) )
    return False;
  if( fieldType(parm,id,TpString,TpArrayString) ) // String date/times
  {
    Array<String> tt( parm.asArrayString(id) );
    times.resize(tt.shape());
    Bool deltt,deltimes;
    const String *ptt = tt.getStorage(deltt);
    Double *ptimes = times.getStorage(deltimes);
    Int scale = secs ? 24*3600 : 1;
    for( uInt i=0; i<tt.nelements(); i++ )
    {
      Quantity q;
      if( !MVTime::read(q,ptt[i]) ) {
        os<<"bad "<<id<<" specified: "<<ptt[i]<<endl<<LogIO::EXCEPTION;
      }
      ptimes[i] = scale*(Double)MVTime(q);
    }
    tt.freeStorage(ptt,deltt);
    times.putStorage(ptimes,deltimes);
  }
  else if( isField(parm,id,isReal) ) // if not strings, try numeric MJDs
  {
    times = parm.toArrayDouble(id);
  }
  else                              // else can't parse
    return False;
  return True;
}

// -----------------------------------------------------------------------
// RFA_selector::find
// Helper templated method to find an object in an array
// -----------------------------------------------------------------------
template<class T> Bool RFASelector::find( uInt &index,const T &obj,const Vector<T> &arr )
{
  for( uInt i=0; i<arr.nelements(); i++ )
    if( obj == arr(i) )
    {
      index = i;
      return True;
    }
  return False;
}
template Bool RFASelector::find<uInt>(uInt&,const uInt&,const Vector<uInt>&);
template Bool RFASelector::find<Int>(uInt&,const Int&,const Vector<Int>&);
template Bool RFASelector::find<String>(uInt&,const String&,const Vector<String>&);


// -----------------------------------------------------------------------
// addString
// Helper method to build up description strings
// -----------------------------------------------------------------------
void RFASelector::addString( String &str,const String &s1,const char *sep )
{
  if( str.length() )
    str += sep;
  str += s1;
}

// -----------------------------------------------------------------------
// parseMinMax
// Helper function to parse a range specification
// -----------------------------------------------------------------------
Bool RFASelector::parseMinMax( Float &vmin,Float &vmax,const RecordInterface &spec,uInt f0 )
{
  vmin = -C::flt_max; vmax=C::flt_max;
// Option 1: fields named min/max exist... so use them
  Bool named = False;
  if( spec.isDefined(RF_MIN) )
    { vmin = spec.asFloat(RF_MIN); named = True; }
  if( spec.isDefined(RF_MAX) )
    { vmax = spec.asFloat(RF_MAX); named = True; }
  if( named )
    return True;
// Else look at first available field, if a 2-element array, assume
// [min,max] has been specified
  if( spec.shape(f0).nelements()==1 && spec.shape(f0)(0) == 2 )
  {
    Vector<Double> mm = spec.toArrayDouble(f0);
    vmin=mm(0); vmax=mm(1);
    return True;
  }
// Else assume next two record fields are {min,max}
  if( spec.nfields()-f0 > 2 )
  {
    vmin = spec.asFloat(f0);
    vmax = spec.asFloat(f0+1);
  }
  else
    vmax = spec.asFloat(f0);
  return True;
}


// -----------------------------------------------------------------------
// normalize
// Helper function to shift a cyclic value (i.e. angle) into
// the interval [base,base+cycle) by adding/subtarcting an integer
// number of cycles
// -----------------------------------------------------------------------
static Double normalize( Double value,Double base,Double cycle )
{
  if( value < base )
    value += (floor((base-value)/cycle)+1)*cycle;
  else if( value >= base+cycle )
    value -= floor((value-base)/cycle)*cycle;
  return value;  
}
    

// -----------------------------------------------------------------------
// addClipInfo
// -----------------------------------------------------------------------
void RFASelector::addClipInfo( const Vector<String> &expr,Float vmin,Float vmax,Bool clip )
{
// create mapper and clipinfo block
  RFDataMapper *mapper = new RFDataMapper(expr);
  ClipInfo clipinfo = { mapper,vmin,vmax,clip,0.0 };
// if dealing with cyclic values, normalize min/max accordingly
  Double cycle = mapper->getValueCycle();  // e.g. 360 for angles
  if( cycle>0 )
  {
    Double base = mapper->getValueBase();  // e.g. -180 for angles
    // normalize min/max angle into [base,base+cycle)
    clipinfo.vmin = normalize(clipinfo.vmin,base,cycle);
    clipinfo.vmax = normalize(clipinfo.vmax,base,cycle);
    // if order is reversed, then we're spanning a cycle boundary (i.e. 355->5)
    if( clipinfo.vmin>clipinfo.vmax ) 
      clipinfo.vmax += cycle;   // ...so add a cycle
    // use vmin as offset
    clipinfo.offset = clipinfo.vmin;
    clipinfo.vmin = 0;
    clipinfo.vmax -= clipinfo.offset;
  }
// add block to appropriate list  
  Block<ClipInfo> & block( mapper->type()==RFDataMapper::MAPROW ? sel_clip_row : sel_clip );
  uInt ncl = block.nelements();
  block.resize(ncl+1,False,True);
  block[ncl] = clipinfo;
}

// -----------------------------------------------------------------------
// parseClipField
// Helper function to parse a clip specification
// -----------------------------------------------------------------------
void RFASelector::parseClipField( const RecordInterface &spec,Bool clip )
{
  try {
// Syntax one - we have a record of {expr,min,max} or {expr,[min,max]}
// or {expr,max}
    if( spec.name(0) == RF_EXPR )
    {
      Vector<String> expr = spec.asArrayString(0);
      Float vmin,vmax;
      if( !parseMinMax(vmin,vmax,spec,1) )
        throw(AipsError(""));
      addClipInfo(expr,vmin,vmax,clip);
    }
// Syntax two: we have a record of { expr1=[min,max],expr2=[min,max],.. }
    else
    {
      for( uInt i=0; i<spec.nfields(); i++ )
      {
        Vector<String> expr(1,spec.name(i));
        Float vmin=-C::flt_max,vmax=C::flt_max;
        if( spec.dataType(i) == TpRecord )
        {
          uInt f0=0;
          if( spec.asRecord(i).name(0) == RF_EXPR )
          {
            expr = spec.asRecord(i).asArrayString(0);
            f0++;
          }
          if( !parseMinMax(vmin,vmax,spec.asRecord(i),f0) )
            throw(AipsError(""));
        } 
        else 
        {
          if( isArray( spec.type(i) ) )
          {
            Vector<Double> vec = spec.toArrayDouble(i);
            if( vec.nelements() == 1 )
              vmax=vec(0);
            else if( vec.nelements() == 2 )
              { vmin=vec(0); vmax=vec(1); }
            else
              throw(AipsError(""));
          }
          else
          {
            vmax = spec.asFloat(i);
          }
        }
        addClipInfo(expr,vmin,vmax,clip);
      }
    }
  }
  catch( AipsError x ) {
    os<<"Illegal \""<<(clip?RF_CLIP:RF_FLAGRANGE)<<"\" record\n"<<LogIO::EXCEPTION;
  }
}

void RFASelector::addClipInfoDesc ( const Block<ClipInfo> &clip )
{
  for( uInt i=0; i<clip.nelements(); i++ )
  {
    String ss;
    char s1[32]="",s2[32]="";
    if( clip[i].vmin != -C::flt_max )
      sprintf(s1,"%g",clip[i].vmin+clip[i].offset);
    if( clip[i].vmax != C::flt_max )
      sprintf(s2,"%g",clip[i].vmax+clip[i].offset);
    if( clip[i].clip )
    {
      ss += clip[i].mapper->description();
      if( s1[0] )
      { 
        ss += String("<") +s1;
        if( s2[0] ) ss += ",";
      }
      if( s2[0] )
        ss += String(">")+s2;
    }
    else
    {
      if( s1[0] )
        ss += String(s1)+"<=";
      ss += clip[i].mapper->description();
      if( s2[0] )
        ss += String("<=")+s2;
    }
    addString(desc_str,ss);
  }
}

// -----------------------------------------------------------------------
// RFASelector constructor
// -----------------------------------------------------------------------
RFASelector::RFASelector ( RFChunkStats &ch,const RecordInterface &parm ) : 
  RFAFlagCubeBase(ch,parm)
{
  char s[256];

  if( fieldType(parm,RF_FREQS,TpArrayString) ) // frequency range[s], as measures
  {
    Matrix<String> sfq;
    parseRange(sfq,parm,RF_FREQS);
    sel_freq.resize( sfq.shape() );
    // parse array of String frequency quanta
    for( uInt i=0; i<sfq.nrow(); i++ ) 
      for( uInt j=0; i<sfq.ncolumn(); j++ ) 
      {
        Float q; char unit[32];
        if( sscanf(sfq(i,j).chars(),"%f%s",&q,unit)<2 )
          os<<"Illegal \""<<RF_FREQS<<"\" array\n"<<LogIO::EXCEPTION;
        Quantity qq(q,unit);
        sel_freq(i,j) = qq.getValue("Hz");
      }
  } 
  else // freq. specified as MHz
  {
    parseRange(sel_freq,parm,RF_FREQS);
    sel_freq *= 1e+6;
  }
  if( sel_freq.nelements() )
  {
    String fq;
    for( uInt i=0; i<sel_freq.ncolumn(); i++ )
    {
      sprintf(s,"%.2f-%.2f",sel_freq(0,i)*1e-6,sel_freq(1,i)*1e-6);
      addString(fq,s,",");
    }
    addString(desc_str,String(RF_FREQS)+"="+fq+"MHz");
  }
// parse input arguments: channels
  if( parseRange(sel_chan,parm,RF_CHANS) )
  {
    String sch;
    for( uInt i=0; i<sel_chan.ncolumn(); i++ )
    {
      sprintf(s,"%d:%d",sel_chan(0,i),sel_chan(1,i));
      addString(sch,s,",");
    }
    addString(desc_str,String(RF_CHANS)+"="+sch);
    sel_chan(sel_chan>=0) += -(Int)indexingBase();
  }
// parse input arguments: correlations
  if( fieldType(parm,RF_CORR,TpString,TpArrayString))
  {
    String ss;
    Vector<String> scorr( parm.asArrayString(RF_CORR) );
    sel_corr.resize( scorr.nelements() );
    for( uInt i=0; i<scorr.nelements(); i++ )
    {
      sel_corr(i) = Stokes::type( scorr(i) );
      if( sel_corr(i) == Stokes::Undefined )
        os<<"Illegal correlation "<<scorr(i)<<endl<<LogIO::EXCEPTION;
      addString(ss,scorr(i),",");
    }
    addString(desc_str,String(RF_CORR)+"="+ss);
  }
// parse input arguments: Spw ID(s)
  if( fieldType(parm,RF_SPWID,TpInt,TpArrayInt) )
  {
    parm.get(RF_SPWID,sel_spwid);
    String ss;
    for( uInt i=0; i<sel_spwid.nelements(); i++ )
      addString(ss,String::toString(sel_spwid(i)),",");
    addString(desc_str,String(RF_SPWID)+"="+ss);
    sel_spwid -= (Int)indexingBase();
  }
// parse input arguments: Field names or ID(s)
  if( fieldType(parm,RF_FIELD,TpString,TpArrayString) )
  {
    parm.get(RF_FIELD,sel_fieldnames);
    sel_fieldnames.apply(stringUpper);
    String ss;
    for( uInt i=0; i<sel_fieldnames.nelements(); i++ )
      addString(ss,sel_fieldnames(i),",");
    addString(desc_str,String(RF_FIELD)+"="+ss);
  }
  else if( fieldType(parm,RF_FIELD,TpInt,TpArrayInt) )
  {
    parm.get(RF_FIELD,sel_fieldid);
    String ss;
    for( uInt i=0; i<sel_fieldid.nelements(); i++ )
      addString(ss,String::toString(sel_fieldid(i)),",");
    addString(desc_str,String(RF_FIELD)+"="+ss);
    sel_fieldid -= (Int)indexingBase();
  }
// parse input: specific time ranges 
  Array<Double> rng;
  Matrix<Double> timerng;
  if( parseTimes(rng,parm,RF_TIMERANGE) )
  {
    if( !reformRange(timerng,rng) )
      os<<"Illegal \""<<RF_TIMERANGE<<"\" array\n"<<LogIO::EXCEPTION;
    sel_timerng = timerng*(Double)(24*3600);
    addString(desc_str,String(RF_TIMERANGE)+"("+String::toString(timerng.ncolumn())+")");
  }
// parse input arguments: ANT specified by string ID
  LogicalVector sel_ant(num(ANT),False); 
  if( fieldType(parm,RF_ANT,TpString,TpArrayString) )
  {
    Vector<String> sant( parm.asArrayString(RF_ANT) );
    sant.apply(stringUpper);
    const Vector<String> &names( chunk.antNames() );
    for( uInt i=0; i<sant.nelements(); i++ )
    {
      uInt iant;
      if( !find( iant,sant(i),names ) )
        os<<"Illegal antenna ID "<<sant(i)<<endl<<LogIO::EXCEPTION;
      sel_ant(iant)=True;
    }
  }
// else ANT specified directly by indexes
  else if( fieldType(parm,RF_ANT,TpInt,TpArrayInt) )
  {
    Vector<Int> sant = parm.asArrayInt(RF_ANT);
    for( uInt i=0; i<sant.nelements(); i++ )
      sel_ant( sant(i) - (Int)indexingBase() ) = True;
  }
  if( sum(sel_ant) )
  {
    String sant;
    for( uInt i=0; i<num(ANT); i++ )
      if( sel_ant(i) )
        addString(sant,chunk.antNames()(i),",");
    addString(desc_str,String(RF_ANT)+"="+sant);
  }
// parse input: baselines as "X-Y"
  sel_ifr = LogicalVector(num(IFR),False);
  String ifrdesc;
  const Vector<String> &names( chunk.antNames() );
  if( fieldType(parm,RF_BASELINE,TpString,TpArrayString) )
  {
    Vector<String> ss(parm.asArrayString(RF_BASELINE));
    ss.apply(stringUpper);
    for( uInt i=0; i<ss.nelements(); i++ )
    {
      uInt ant1,ant2;
      String ants[2];
      Int res = split(ss(i),ants,2,"-");
      Bool wild1 = (ants[0]=="*" || ants[0]=="" );  // is it a wildcard instead of ID?
      Bool wild2 = (ants[1]=="*" || ants[1]=="" );
      if( res<2 || ( wild1 && wild2 ) )
        os<<"Illegal baseline specification "<<ss(i)<<endl<<LogIO::EXCEPTION;
      Bool val1 = wild1 || find(ant1,ants[0],names);
      Bool val2 = wild2 || find(ant2,ants[1],names);
      // if both antenna IDs are valid, use them
      if( val1 && val2 )
      {
        if( wild1 )
        {
          addString(ifrdesc,ants[1]+"-*",",");
          for( uInt a=0; a<num(ANT); a++ ) 
            sel_ifr( chunk.antToIfr(a,ant2) ) = True;
        }
        else if( wild2 )
        {
          addString(ifrdesc,ants[0]+"-*",",");
          for( uInt a=0; a<num(ANT); a++ ) 
            sel_ifr( chunk.antToIfr(ant1,a) ) = True;
        }
        else
        {
          addString(ifrdesc,ants[0]+"-"+ants[1],",");
          sel_ifr( chunk.antToIfr(ant1,ant2) ) = True;
        }
      }
      else // try to interpret them as numbers instead
      {
        if( sscanf(ss(i).chars(),"%d-%d",&ant1,&ant2)<2 ||
            ant1>=num(ANT) || ant2>=num(ANT) )
          os<<"Illegal baseline specification "<<ss(i)<<endl<<LogIO::EXCEPTION;
        sel_ifr( chunk.antToIfr(ant1-(Int)indexingBase(),ant2-(Int)indexingBase()) ) = True;
        addString(ifrdesc,ss(i),",");
      }
    }
  }
// parse input: baselines as [[x1,y1],[x2,y2],... etc.
  else if( fieldType(parm,RF_BASELINE,TpInt,TpArrayInt) ) 
  {
    Matrix<Int> ant;
    if( parseRange(ant,parm,RF_BASELINE) )
    {
      ant -= (Int)indexingBase();
      for( uInt i=0; i<ant.ncolumn(); i++ )
      {
        if( ant(0,i)==-1 )
        {
          if( ant(1,i)==-1 )
            os<<"Illegal baseline specification [-1,-1]"<<LogIO::EXCEPTION<<endl;
          for( uInt a=0; a<num(ANT); a++ ) 
            sel_ifr( chunk.antToIfr(a,ant(1,i)) ) = True;
          addString(ifrdesc,names(ant(1,i))+"-*",",");
        }
        else if( ant(1,i)==-1 )
        {
          for( uInt a=0; a<num(ANT); a++ ) 
            sel_ifr( chunk.antToIfr(ant(0,i),a) ) = True;
          addString(ifrdesc,names(ant(0,i))+"-*",",");
        }
        else
        {
          sel_ifr(chunk.antToIfr(ant(0,i),ant(1,i))) = True;
          addString(ifrdesc,names(ant(0,i))+"-"+names(ant(1,i)),",");
        }
      }
    }
  }
  if( sum(sel_ifr) )
  {
    String ss;
    addString(desc_str,String(RF_BASELINE)+"="+ifrdesc);
  }
  else // no IFRs were specified
  {
    if( sum(sel_ant) ) // antennas specified? flag only their baselines
    {
      for( uInt a1=0; a1<num(ANT); a1++ )
        if( sel_ant(a1) )
          for( uInt a2=0; a2<num(ANT); a2++ )
            sel_ifr(chunk.antToIfr(a1,a2)) = True;
    }
    else // no antennas either? flag everything
      sel_ifr.resize();
  }
// now, all selection-related arguments are accounted for.
// set flag if some subset has been selected
  Bool have_subset = ( desc_str.length() );
  if( have_subset )
    desc_str+=";";
// unflag specified?
  unflag = ( fieldType(parm,RF_UNFLAG,TpBool) && parm.asBool(RF_UNFLAG) );
  addString(desc_str,unflag?RF_UNFLAG:"flag");
// now, scan arguments for what to flag within the selection
// specific times (specified by center times)
  Vector<Double> ctimes;
  Double timedelta = 10;
  if( parseTimes(ctimes,parm,RF_CENTERTIME) )
  {
    ctimes *= (Double)(24*3600);
    String ss( String(RF_CENTERTIME)+"("+String::toString(ctimes.nelements())+")" );
    Vector<Double> dt;
    if( parseTimes(dt,parm,RF_TIMEDELTA,True) )
    {
      timedelta = dt(0);
      sprintf(s,",dtime=%.1fs",timedelta);
      ss += s;
    }
    addString(desc_str,ss);
    uInt n = ctimes.nelements();
    sel_time.resize(2,n);
    sel_time.row(0) = ctimes - timedelta;
    sel_time.row(1) = ctimes + timedelta;
  }
// flag autocorrelations too?
  sel_autocorr = ( fieldType(parm,RF_AUTOCORR,TpBool) && parm.asBool(RF_AUTOCORR) );
  if( sel_autocorr )
    addString(desc_str,RF_AUTOCORR);
// parse input: quack mode (for VLA)
  if( isFieldSet(parm,RF_QUACK) )
  {
    quack_si = 30.0; // scan interval
    quack_dt = 10.0; // dt to flag at start of scan
    // are specific values given? 
    Vector<Double> qt;
    if( parseTimes(qt,parm,RF_QUACK,True) )
    {
      if( qt.nelements()>2 )
        os<<RF_QUACK<<" must be specified as T, <scaninterval> or [scaninterval,dt]"<<endl<<LogIO::EXCEPTION;
      quack_si = qt(0);
      if( qt.nelements()>1 )
        quack_dt = qt(1);
    }
    sprintf(s,"%s=%ds,%ds",RF_QUACK,(Int)quack_si,(Int)quack_dt);
    addString(desc_str,s);
//    quack_si /= (24*3600);
//    quack_dt /= (24*3600);
  }
  else
    quack_si = 0;
// flag a specific range or clip outside of range?
  if( isValidRecord(parm,RF_CLIP) )
    parseClipField(parm.asRecord(RF_CLIP),True);
  if( isValidRecord(parm,RF_FLAGRANGE) )
    parseClipField(parm.asRecord(RF_FLAGRANGE),False);
  // add to description strings, if something was parsed
  if( sel_clip.nelements() )
  {
    addClipInfoDesc(sel_clip);
    sel_clip_active.resize(sel_clip.nelements());
  }
  if( sel_clip_row.nelements() )
    addClipInfoDesc(sel_clip_row);
  
// if nothing has been specified to flag, flag everything within selection
  flag_everything = ( quack_si==0 && !sel_time.nelements() && !sel_autocorr 
                      && !sel_clip.nelements() && !sel_clip_row.nelements() );
  if( flag_everything )
  {
    if( !have_subset && !unflag)
      os<<"FLAG ALL requested, but no MS subset specified.\n"
          "Refusing to flag the whole measurement set!\n"<<LogIO::EXCEPTION;
    addString(desc_str,"all");
  }
  
//  cerr<<"Selector: "<<desc_str<<endl;
}


RFASelector::~RFASelector ()
{
  for( uInt i=0; i<sel_clip.nelements(); i++ )
    if( sel_clip[i].mapper )
      delete sel_clip[i].mapper;
}

// -----------------------------------------------------------------------
// newChunk
// At each new chunk, figure out what goes where 
// -----------------------------------------------------------------------
Bool RFASelector::newChunk (Int &maxmem)
{
// check correlations and figure out the active correlations mask
  Vector<Int> corrtype;
  chunk.visIter().corrType(corrtype);
  corrmask = 0;
  if( sel_corr.nelements() )
  {
    corrmask = chunk.getCorrMask(sel_corr);
    if( !corrmask )
    {
      os<<"No matching correlations in this chunk\n"<<LogIO::POST;
      return active=False;
    }
  }
  else // no correlations specified so flag everything
    corrmask = chunk.fullCorrMask();
  
// check field IDs and spectral window IDs
  uInt dum;
  if( sel_spwid.nelements() && !find(dum,chunk.visBuf().spectralWindow(),sel_spwid) )
  {
    os<<"Spectral window does not match in this chunk\n"<<LogIO::POST;
    return active=False;
  }
  if( sel_fieldid.nelements() && !find(dum,chunk.visIter().fieldId(),sel_fieldid) )
  {
    os<<"Field ID does not match in this chunk\n"<<LogIO::POST;
    return active=False;
  }
  if( sel_fieldnames.nelements() && !find(dum,chunk.visIter().fieldName(),sel_fieldnames) )
  {
    os<<"Field name does not match in this chunk\n"<<LogIO::POST;
    return active=False;
  }
  
// figure out active channels (i.e. within specified segments)
  flagchan.resize();
  if( sel_freq.ncolumn() || sel_chan.ncolumn() )
  {
    flagchan = LogicalVector(num(CHAN),False);
    const Vector<Double> & fq( chunk.frequency() );
    for( uInt i=0; i<sel_freq.ncolumn(); i++ )
      flagchan = flagchan || ( fq >= sel_freq(0,i) && fq <= sel_freq(1,i) );
    Vector<Int> ch( num(CHAN) );
    indgen(ch);
    Matrix<Int> schan = sel_chan;
    schan( sel_chan<0 ) += (Int)num(CHAN);
    for( uInt i=0; i<sel_chan.ncolumn(); i++ )
    {
      flagchan = flagchan || ( ch >= schan(0,i) && ch <= schan(1,i) );
    }
    if( !sum(flagchan) )
    {
      os<<"No matching frequencies/channels in this chunk\n"<<LogIO::POST;
      return active=False;
    }
    if( allEQ(flagchan,True) )
      flagchan.resize(); // null array = all True
  }
// init all clipping mappers, and check their correlation masks
  if( sel_clip.nelements() )
  {
    // see which mappers are active for this chunk, and accumulate their
    // masks in clip_corrmask
    RFlagWord clip_corrmask=0;
    for( uInt i=0; i<sel_clip.nelements(); i++ ) 
    {
      RFlagWord mask = sel_clip[i].mapper->corrMask(chunk.visIter());
      sel_clip_active(i) = (mask!=0);
      clip_corrmask |= mask;
    }
    sum_sel_clip_active = sum(sel_clip_active);
    // If no explicit correlations were selected by the user,  
    // then use clip_corrmask as the overall mask
    if( !sel_corr.nelements() )
    {
      corrmask = clip_corrmask;
      if( !corrmask )
      {
        os<<"No matching correlations in this chunk\n"<<LogIO::POST;
        return active=False;
      }
    }
  }
// reserve a minimum of memory for ourselves
  maxmem -= 1;
// init flagging cube and off we go...
  RFAFlagCubeBase::newChunk(maxmem);
// see if full row is being flagged, i.e. no subset of channels was selected,
// and no explicit correlations (or all correlations).
  select_fullrow = (!flagchan.nelements() && 
    (!sel_corr.nelements() || corrmask==chunk.fullCorrMask()) );
// for quack
  scan_start = 0;
  return active=True;
}

// -----------------------------------------------------------------------
// processRow
// Raises/clears flags for a single row, depending on current selection
// -----------------------------------------------------------------------
void RFASelector::processRow(uInt ifr,uInt it)
{
  // does the selection include whole rows?
  if( select_fullrow  )
    unflag ? flag.clearRowFlag(ifr,it) : flag.setRowFlag(ifr,it);
  else
  {
  // else apply data flags to selection
    for( uInt ich=0; ich<num(CHAN); ich++ )
      if( !flagchan.nelements() || flagchan(ich) )
        unflag ? flag.clearFlag(ich,ifr) : flag.setFlag(ich,ifr);
  }
}

// -----------------------------------------------------------------------
// iterTime
// -----------------------------------------------------------------------
RFA::IterMode RFASelector::iterTime (uInt it)
{
  RFAFlagCubeBase::iterTime(it);
// extract time
  const Vector<Double> &times( chunk.visBuf().time() );
  Double t0 = times(0);
  if( !allEQ(times,t0) )
    os<<"RFASelector: crash&burn, VisBuffer's given us different times."<<LogIO::EXCEPTION;
// is current time slot within any selected time ranges? return if not
  if( sel_timerng.ncolumn() )
    if( allEQ(sel_timerng.row(0)<=t0 && sel_timerng.row(1)>=t0,False) )
      return RFA::CONT;
  
  const Vector<Int> &ifrs( chunk.ifrNums() );
  
// do we flag the whole selection?  
  Bool flagall = flag_everything;
  if( !flagall )
  {
  // flag autocorrelations
    if( sel_autocorr )
    { 
      for( uInt i=0; i<ifrs.nelements(); i++ )
        if( !sel_ifr.nelements() || sel_ifr(ifrs(i)) )
        {
          uInt a1,a2;
          chunk.ifrToAnt(a1,a2,ifrs(i));
          if( a1==a2 )
            processRow(ifrs(i),it);
        }
    }
  // flag if quacked
    if( quack_si>0 )
    {
      if( t0-scan_start > quack_si ) // new scan interval?
        scan_end = t0 + quack_dt;
      if( t0<=scan_end ) // still within start of interval?
        flagall = True;
      scan_start = t0;
    }
  // flag if within specific timeslots
    if( sel_time.ncolumn() )
      if( anyEQ(sel_timerng.row(0)<=t0 && sel_timerng.row(1)>=t0,True) )
        flagall = True;
  // flag for specific row-based clipping expressions
    if( sel_clip_row.nelements() )
    {
      // setup each row mapper
      for( uInt i=0; i<sel_clip_row.nelements(); i++ ) 
        sel_clip_row[i].mapper->setVisBuffer(chunk.visBuf());
      for( uInt i=0; i<ifrs.nelements(); i++ ) // loop over rows
        for( uInt j=0; j<sel_clip_row.nelements(); j++ ) 
        {
          Float vmin = sel_clip_row[j].vmin, vmax = sel_clip_row[j].vmax;
          Float val = sel_clip_row[j].mapper->mapValue(i) - sel_clip_row[j].offset;
          if( (sel_clip_row[j].clip  && ( val<vmin || val>vmax ) ) ||
              (!sel_clip_row[j].clip && val>=vmin && val<=vmax ) )
            processRow(ifrs(i),it);
        } 
    }
  }
  
// flag whole selection, if still needed
  if( flagall )
  {
    for( uInt i=0; i<ifrs.nelements(); i++ ) // loop over rows
      if( !sel_ifr.nelements() || sel_ifr(ifrs(i)) )
        processRow(ifrs(i),it);
  }
// setup each correlation clip mapper
  for( uInt i=0; i<sel_clip.nelements(); i++ ) 
    if( sel_clip_active(i) )
      sel_clip[i].mapper->setVisBuffer(chunk.visBuf());
  
  return RFA::CONT;
}

// -----------------------------------------------------------------------
// itreRow
// -----------------------------------------------------------------------
RFA::IterMode RFASelector::iterRow (uInt ir)
{
  uInt ifr = chunk.ifrNum(ir);
  if( sel_clip.nelements() && sum_sel_clip_active )
  {
// apply data flags
    for( uInt ich=0; ich<num(CHAN); ich++ )
      if( !flagchan.nelements() || flagchan(ich) )
      {
        for( uInt j=0; j<sel_clip.nelements(); j++ ) 
        {
          Float vmin = sel_clip[j].vmin, vmax = sel_clip[j].vmax;
          Float val = sel_clip[j].mapper->mapValue(ich,ir);
          if( ( sel_clip[j].clip && ( val<vmin || val>vmax ) ) ||
              (!sel_clip[j].clip && val>=vmin && val<=vmax   ) )
            unflag ? flag.clearFlag(ich,ifr) : flag.setFlag(ich,ifr);
        }
      }
  }
  return RFA::CONT;
}

String RFASelector::getDesc ()
{
  return desc_str+" "+RFAFlagCubeBase::getDesc();
}

const RecordInterface & RFASelector::getDefaults ()
{
  static Record rec;
// create record description on first entry
  if( !rec.nfields() )
  {
    rec = RFAFlagCubeBase::getDefaults();
    rec.removeField(RF_FIGNORE); // fignore is meaningless
    rec.define(RF_NAME,"Selector");
    rec.define(RF_SPWID,False);
    rec.define(RF_FIELD,False);
    rec.define(RF_FREQS,False);
    rec.define(RF_CHANS,False);
    rec.define(RF_CORR,False);
    rec.define(RF_ANT,False);
    rec.define(RF_BASELINE,False);
    rec.define(RF_TIMERANGE,False);
    rec.define(RF_AUTOCORR,False);
    rec.define(RF_CENTERTIME,False);
    rec.define(RF_TIMEDELTA,10.0);
    rec.define(RF_QUACK,False);
    rec.define(RF_CLIP,False);
    rec.define(RF_FLAGRANGE,False);
    rec.define(RF_UNFLAG,False);
    
    rec.setComment(RF_SPWID,"Restrict flagging to specific spectral windows (integers)");
    rec.setComment(RF_FIELD,"Restrict flagging to specific field IDs or field names (integers/strings)");
    rec.setComment(RF_FREQS,"Restrict flagging to specific frequency ranges (2,N array of doubles:MHz)");
    rec.setComment(RF_CHANS,"Restrict flagging to specific channels (2,N array of integers)");
    rec.setComment(RF_CORR,"Restrict flagging to specific correlations (array of strings)");
    rec.setComment(RF_ANT,"Restrict flagging to specific antennas (array of strings/integers)");
    rec.setComment(RF_BASELINE,"Restrict flagging to specific baselines (array of strings, e.g., 'A1-A2','A1-*', or 2,N array of integers [[A1,A2],[B1,B2],...])");
    rec.setComment(RF_TIMERANGE,"Restrict flagging to specific time ranges (2,N array of strings or MJDs");
    rec.setComment(RF_AUTOCORR,"Flag autocorrelations (F/T)");
    rec.setComment(RF_CENTERTIME,"Flag specific timeslots (array of strings or MJDs)");
    rec.setComment(RF_TIMEDELTA,String("Time delta for ")+RF_CENTERTIME+", in seconds");
    rec.setComment(RF_QUACK,"Use [SI,DT] for VLA quack-flagging");
    rec.setComment(RF_CLIP,"Flag outside a specific range of values");
    rec.setComment(RF_FLAGRANGE,"Flag inside a specific range of values");
    rec.setComment(RF_UNFLAG,"If T, specified flags are CLEARED");
  }
  return rec;
}

} //# NAMESPACE CASA - END

