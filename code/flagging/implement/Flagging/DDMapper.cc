//# DDMapper.cc: this defines DDMapper
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
#include <flagging/Flagging/DDMapper.h>
#include <flagging/Flagging/RFChunkStats.h>
#include <casa/Utilities/Regex.h>

namespace casa { //# NAMESPACE CASA - BEGIN

DDFunc::DDFunc( FuncSignature fsig,const String &corrstr )
  : DDMapper(),icorr(-1),func(fsig)
{
  corrtype = Stokes::type( corrstr );
  valid = False;
}

Bool DDFunc::reset ( const Vector<Int> &corr )
{ 
  icorr = findCorrType( corrtype,corr );
  if( icorr<0 )
    return valid=False;
  corrmask = (1<<icorr);
  return valid=True;
}

Float DDFunc::map ( const Cube<Complex> &vis,uInt ich,uInt irow ) const
{ 
  return (*func)( vis(icorr,ich,irow) ); 
}

Float DDFunc::real ( const Complex &val)
{
  return val.real();
}

Float DDFunc::imag ( const Complex &val)
{
  return val.imag();
}

DDSumFunc::DDSumFunc( FuncSignature fsig,const String &corr1,const String &corr2 )
  : DDFunc(fsig,corr1),icorr2(-1)
{
  corrtype2 = Stokes::type( corr2 );
  valid = False;
}

Bool DDSumFunc::reset ( const Vector<Int> &corr )
{ 
// case of "I" - look up XX+YY or RR+LL
  if( corrtype == Stokes::I )
  {
    icorr = findCorrType( Stokes::XX,corr );
    icorr2 = findCorrType( Stokes::YY,corr );
    // no XX+YY? try RR+LL
    if( icorr<0 || icorr2<0 )
    {
      icorr = findCorrType( Stokes::RR,corr );
      icorr2 = findCorrType( Stokes::LL,corr );
    }
    // give up if not found
    if( icorr<0 || icorr2<0 )
      return valid=False;
    corrmask = (1<<icorr)|(1<<icorr2);
    return valid=True;
  }
// standard case - just look up correlations directly
  else
  {
    if( !DDFunc::reset(corr) )
      return False;
    icorr2 = findCorrType( corrtype2,corr );
    if( icorr2<0 )
      return valid=False;
    corrmask |= (1<<icorr2);
    return valid=True;
  }
}

Float DDSumFunc::map ( const Cube<Complex> &vis,uInt ich,uInt irow ) const
{ 
  return (*func)( vis(icorr,ich,irow) ) + (*func)( vis(icorr2,ich,irow) ); 
}

DDFuncSum::DDFuncSum( FuncSignature fsig,const String &corr1,const String &corr2 )
  : DDSumFunc(fsig,corr1,corr2)
{}

Float DDFuncSum::map ( const Cube<Complex> &vis,uInt ich,uInt irow ) const
{ 
  return (*func)( vis(icorr,ich,irow) + vis(icorr2,ich,irow) ); 
}

DDFuncDiff::DDFuncDiff( FuncSignature fsig,const String &corr1,const String &corr2 )
  : DDSumFunc(fsig,corr1,corr2)
{}

Float DDFuncDiff::map ( const Cube<Complex> &vis,uInt ich,uInt irow ) const
{ 
  return (*func)( vis(icorr,ich,irow) - vis(icorr2,ich,irow) ); 
}

DDDiffFunc::DDDiffFunc( FuncSignature fsig,const String &corr1,const String &corr2 )
  : DDSumFunc(fsig,corr1,corr2)
{}

Float DDDiffFunc::map ( const Cube<Complex> &vis,uInt ich,uInt irow ) const
{ 
  return (*func)( vis(icorr,ich,irow) ) - (*func)( vis(icorr2,ich,irow) ); 
}

// -----------------------------------------------------------------------
// getDDFunction
// Maps a string to a function pointer
// -----------------------------------------------------------------------
DDFunc::FuncSignature DDFunc::getFunction ( const String &func )
{
  // Map of available functions
  const struct { const char *name; DDFunc::FuncSignature func; } func_map[] = 
  { { "ABS", std::abs },
    { "ARG", std::arg },
    { "RE" , DDFunc::real },
    { "IM" , DDFunc::imag },
    { "RE" , DDFunc::real },
    { "NORM" , std::norm }
  };
  const uInt num_func_map = sizeof(func_map)/sizeof(func_map[0]);

  for( uInt i=0; i<num_func_map; i++ )
    if( func.matches( func_map[i].name ) )
      return func_map[i].func;
  return NULL;
} 

static AipsError funcError ( const String &name )
{
  return AipsError( String("DDMapper: unrecognized function '")+name+"'");
}

// -----------------------------------------------------------------------
// splitExpression
// helper function, converts vector of strings (or single string
// w/whitespace separators) into vector of uppercase Strings.
// -----------------------------------------------------------------------
Vector<String> splitExpression( const Vector<String> &expr0 )
{
  uInt nel = expr0.nelements();
  if( nel == 1 ) // if only one element, try to split it at whitespace
  {
// split expression into array of strings
    String expr[20];
    nel = split(expr0(0),expr,20,RXwhite);
    Vector<String> out(nel);
    for( uInt i=0; i<nel; i++ )
      out(i) = upcase( expr[i] );
    return out;
  }
// else just copy vector, converting to uppercase
  Vector<String> out(nel);
  for( uInt i=0; i<nel; i++ )
    out(i) = upcase( expr0(i) );
  return out;    
}
  
// -----------------------------------------------------------------------
// getDDMapper
// Parses vector of strings to define a mapper
// -----------------------------------------------------------------------
DDMapper * DDFunc::getMapper ( String &descr,const Vector<String> &expr0,Bool throw_excp )
{
// convert to C array
  Vector<String> expr( splitExpression(expr0) );
  uInt nel = expr.nelements();
  
  if( nel == 1 ) // 1 element: assume it's just CORR, and use abs(CORR)
  {
    if( expr(0) == "I" ) // I is special (maps to XX+YY or RR+LL)
      return new DDFuncSum(&std::abs,"I","I");
    return new DDFunc(&std::abs,expr(0));
  }
  else if( nel == 2 ) // 2 elements: assume FUNC CC
  {
    DDFunc::FuncSignature func = getFunction(expr(0));
    if( !func )
      throw( funcError(expr(0)) );
    descr = expr(0)+"("+expr(1)+")";
    if( expr(1) == "I" ) // I is special (maps to XX+YY or RR+LL)
      return new DDFuncSum(func,"I","I");
    return new DDFunc(func,expr(1));
  }
  else if( nel == 4 ) // 4 elements: SUM FUNC CC CC or FUNC SUM CC CC
  {
    DDFunc::FuncSignature func;
    if( expr(0) == "+" || expr(0) == "-" )
    {
      func = getFunction(expr(1));
      if( !func )
        throw( funcError(expr(1)) );
      descr = expr(1)+"("+expr(2)+")"+expr(0)+expr(1)+"("+expr(3)+")";
      if( expr(0) == "+" )
        return new DDSumFunc(func,expr(2),expr(3));
      else
        return new DDDiffFunc(func,expr(2),expr(3));
    }
    if( expr(1) == "+" || expr(1) == "-" )
    {
      func = getFunction(expr(0));
      if( !func )
        throw( funcError(expr(0)) );
      descr = expr(0)+"("+expr(2)+expr(1)+expr(3)+")";
      if( expr(1) == "+" )
        return new DDFuncSum(func,expr(2),expr(3));
      else
        return new DDFuncDiff(func,expr(2),expr(3));
    }
  }
// fall through to error report
  if( throw_excp )
  {
    String err("bad DDMapper expression:");
    for( uInt i=0; i<nel; i++ )
      err += String(" ")+expr(i);
    throw( AipsError(err) );
  }
  return NULL;
}


} //# NAMESPACE CASA - END

