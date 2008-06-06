#include <scimath/Mathematics/MedianSlider.h>
#include <casa/Exceptions/Error.h>
#include <stdlib.h>    
#include <cstring>                  //# for memcpy with gcc-4.3
    
namespace casa { //# NAMESPACE CASA - BEGIN

MedianSlider::MedianSlider () 
  : buf(0),index(0),valid(0)
{
}

MedianSlider::MedianSlider ( int hw )
{
  halfwin = hw;
  fullwin = hw*2+1;
  index = new uInt[fullwin];
  buf   = new Float[fullwin];
  valid = new Bool[fullwin];
// buffer initially all-null and totally invalid
  for( uInt i=0; i<fullwin; i++ ) 
  {
    buf[i] = 0;
    valid[i] = False;
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
  index = new uInt[fullwin];
  buf   = new Float[fullwin];
  valid = new Bool[fullwin];
  memcpy(index,other.index,fullwin*sizeof(uInt));
  memcpy(buf,other.buf,fullwin*sizeof(Float));
  memcpy(valid,other.valid,fullwin*sizeof(Bool));
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

Float MedianSlider::prevVal( uInt n,Bool &flag )
{
  int i = (int)ibuf - (int)n;
  if( i<0 )
    i += fullwin;
  flag = !valid[i];
  return buf[i];
}

Float MedianSlider::next( uInt n )
{
  Float med=0;
  for( uInt i=0; i<n; i++ )
    med = add();
  return med;
}

Float MedianSlider::add ( const Vector<Float> &d,const Vector<Bool> &flag )
{
  Float med=0;
  for( uInt i=0; i<d.nelements(); i++ )
    med = add( d(i),flag(i) );
  return med;
}

Float MedianSlider::add ( const Vector<Float> &d )
{
  Float med=0;
  for( uInt i=0; i<d.nelements(); i++ )
    med = add( d(i) );
  return med;
}

Float MedianSlider::add ( Float din,Bool flag )
{
  uInt ibuf0 = ibuf;
  Float dout = buf[ibuf0];   // outgoing datum
  Bool  val_in=!flag,
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
        uInt j=0;
        // skip indices up to outgoing value
        for( ; j<nind && index[j]!=ibuf0; j++ );
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
        uInt j=0;
        // skip indices up to incoming value
        for( ; j<nind && buf[index[j]]<din; j++ );
        int j0 = j;
        // go forward to locate the outgoing value
        for( ; j<nind && index[j]!=ibuf0; j++ );
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
      uInt j=0;
      // skip indices up to outgoing datum
      for( ; j<nind && index[j]!=ibuf0; j++ );
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
      uInt j=0;
      // skip indices up to incoming value
      for( ; j<nind && buf[index[j]]<din; j++ );
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
Bool MedianSlider::assure ()
{
  Float m = median();
  int c1=0,c2=0;
  for( uInt i=0; i<fullwin; i++ ) 
    if( valid[i] ) 
    {
      if( buf[i] <= m ) c1++;
      if( buf[i] >= m ) c2++;
    }
  if( abs(c1-c2) > 1  )
  {
    throw(AipsError("MedianSlider::assure() failed"));
    return False;
  }
  return True;
}

} //# NAMESPACE CASA - END

