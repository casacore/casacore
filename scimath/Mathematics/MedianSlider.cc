//# MedianSlider.h: Optimized sliding-median computator
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

#include <casacore/scimath/Mathematics/MedianSlider.h>
#include <casacore/casa/Exceptions/Error.h>
#include <stdlib.h>    
#include <cstring>                  //# for memcpy with gcc-4.3
    
namespace casacore { //# NAMESPACE CASACORE - BEGIN

MedianSlider::MedianSlider () 
  : buf(0),index(0),valid(0)
{
}

MedianSlider::MedianSlider ( int hw )
{
  halfwin = hw;
  fullwin = hw*2+1;
  index = new uint32_t[fullwin];
  buf   = new float[fullwin];
  valid = new bool[fullwin];
// buffer initially all-null and totally invalid
  for( uint32_t i=0; i<fullwin; i++ ) 
  {
    buf[i] = 0;
    valid[i] = false;
  }
  ibuf=nind=0;
}

MedianSlider::MedianSlider( const MedianSlider &other ) 
  : buf(0),index(0),valid(0)
{
  *this = other;
}

MedianSlider & MedianSlider::operator = ( const MedianSlider &other )
{
  cleanup();
  halfwin = other.halfwin;
  fullwin = other.fullwin;
  index = new uint32_t[fullwin];
  buf   = new float[fullwin];
  valid = new bool[fullwin];
  memcpy(index,other.index,fullwin*sizeof(uint32_t));
  memcpy(buf,other.buf,fullwin*sizeof(float));
  memcpy(valid,other.valid,fullwin*sizeof(bool));
  ibuf=other.ibuf;
  nind=other.nind;
  return *this;
}

void MedianSlider::cleanup ()
{
  if( buf ) delete [] buf;
  if( index ) delete [] index;
  if( valid ) delete [] valid;
  buf=0; index=0; valid=0;
}

MedianSlider::~MedianSlider ()
{
  cleanup();
}

float MedianSlider::prevVal( uint32_t n,bool &flag )
{
  int i = (int)ibuf - (int)n;
  if( i<0 )
    i += fullwin;
  flag = !valid[i];
  return buf[i];
}

float MedianSlider::next( uint32_t n )
{
  float med=0;
  for( uint32_t i=0; i<n; i++ )
    med = add();
  return med;
}

float MedianSlider::add ( const Vector<float> &d,const Vector<bool> &flag )
{
  float med=0;
  for( uint32_t i=0; i<d.nelements(); i++ )
    med = add( d(i),flag(i) );
  return med;
}

float MedianSlider::add ( const Vector<float> &d )
{
  float med=0;
  for( uint32_t i=0; i<d.nelements(); i++ )
    med = add( d(i) );
  return med;
}

float MedianSlider::add ( float din,bool flag )
{
  uint32_t ibuf0 = ibuf;
  float dout = buf[ibuf0];   // outgoing datum
  bool  val_in=!flag,
       val_out = valid[ibuf0]; // outgoing flag
  
// insert new value into buffer
  buf[ibuf] = din;
  valid[ibuf] = val_in;
  if( ++ibuf >= fullwin )
    ibuf = 0;
  
  if( val_out ) // A) valid outgoing datum...
  {
    if( val_in ) // A.1) ..replaced by a valid datum
    {
      if( dout<din )  // inserting larger value
      {
        uint32_t j=0;
        // skip indices up to outgoing value
        for( ; j<nind && index[j]!=ibuf0; j++ ) {}
//        Assure(j<nind);
        // move up indexes (up to incoming value)
        for( j++; j<nind; j++ )
          if( buf[ index[j] ] < din ) 
            index[j-1] = index[j];
          else
            break;
        // insert new index
        index[j-1] = ibuf0;
      } 
      else if( dout>din )  // inserting smaller value
      {   
        uint32_t j=0;
        // skip indices up to incoming value
        for( ; j<nind && buf[index[j]]<din; j++ ) {}
        int j0 = j;
        // go forward to locate the outgoing value
        for( ; j<nind && index[j]!=ibuf0; j++ ) {}
//        Assure(j<nind);
        // move down indices up to the outgoing value
        if( j-j0 ) 
          memmove(index+j0+1,index+j0,(j-j0)*sizeof(index[0]));
        // insert new index
        index[j0] = ibuf0;
      }
      // else if datums are equal, then no change -- fall thorugh
    }
    else // A.2) replaced by an invalid datum
    {
      uint32_t j=0;
      // skip indices up to outgoing datum
      for( ; j<nind && index[j]!=ibuf0; j++ ) {}
//      Assure(j<nind);
      // shift up remaining indices
      if( nind-j-1 )
        memmove(index+j,index+j+1,(nind-j-1)*sizeof(index[0]));
      nind--;
    }
  }
  else // B) invalid outgoing datum...
  {
    if( val_in ) // B.1) ...replaced by valid datum
    {
      uint32_t j=0;
      // skip indices up to incoming value
      for( ; j<nind && buf[index[j]]<din; j++ ) {}
      // move down the remaining indices to make space
      if( nind-j ) 
        memmove(index+j+1,index+j,(nind-j)*sizeof(index[0]));
      index[j] = ibuf0;
      nind++;
    }
    // else B.2) ...replaced by another invalid datum - so no change
  }
  return median();
}


// verify the MedianSlider values
bool MedianSlider::assure ()
{
  float m = median();
  int c1=0,c2=0;
  for( uint32_t i=0; i<fullwin; i++ ) 
    if( valid[i] ) 
    {
      if( buf[i] <= m ) c1++;
      if( buf[i] >= m ) c2++;
    }
  if( std::abs(c1-c2) > 1  )
  {
    throw(AipsError("MedianSlider::assure() failed"));
    return false;
  }
  return true;
}

} //# NAMESPACE CASACORE - END

