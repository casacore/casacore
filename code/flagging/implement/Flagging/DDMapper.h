//# DDMapper.h: this defines DDMapper
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
#ifndef FLAGGING_DDMAPPER_H
#define FLAGGING_DDMAPPER_H

#include <casa/Arrays/Vector.h> 
#include <casa/Arrays/Cube.h> 
#include <casa/Exceptions/Error.h>
#include <measures/Measures/Stokes.h> 
    
namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Abstract Derived Data Mapper class
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <synopsis>
// The DDMapper class defines an interface for mapping complex visibilities
// into Float derived values. DDMappers are used by several flagging
// agents.
// </synopsis>
//
// <motivation>
// A lot of algorithms are expressed in terms of some real value
// derived from a set of visibilities (i.e., |XX|, |XX|-|YY|, etc.). The
// DDMapper hierarchy provides a uniform interface for deriving such values.
// </motivation>
//
// <todo asof="2001/04/16">
//   <li> add this feature
// </todo>

class DDMapper
{
protected:
  Bool valid;
  uShort corrmask; // mask of affected correlations
  
public:
  DDMapper ()         { valid=False; }
  virtual ~DDMapper () {};
  
  // Given a vector of correlation types, recomputes internal indices.
  // returns True if all indices were found successfully.
  virtual Bool reset ( const Vector<Int> &corr ) =0;
  
  // Maps a slice of visibilities at (*,ich,irow) from the given 
  // viscube into a the derived value. 
  virtual Float map  ( const Cube<Complex> &vis,uInt ich,uInt irow ) const =0;
  
  // Returns the "mask" of correlations which are used by this mapper.
  // by this mapper. Bit "i" is set if corr. "i" is used.
  uShort  corrMask () const         { return corrmask; }

  // Returns True if given correlations is masked
  Bool    masked (uInt icorr) const  { return corrmask&(1<<icorr) != 0; }
  
  // Tells if mapper is valid
  Bool    isValid () { return valid; }
};

// <summary>
// DDDummy: dummy mapper, throws an excpetion if any methods are called
// </summary>
// <use visibility=local>
class DDDummy : public DDMapper
{
public:
  DDDummy ();
  ~DDDummy ();
  
  virtual void puke () const
              { throw(AipsError("Uninitialized DDMapper used")); }
  
  virtual Bool  reset ( const Vector<Int> & )
              { puke(); return False; }
  virtual Float map   ( const Cube<Complex> &,uInt,uInt ) const
              { puke(); return 0.; }

};

// <summary>
// DDFunc: maps correlation A into func(A)
// </summary>
// <use visibility=local>
class DDFunc : public DDMapper
{
public:
  typedef Float (*FuncSignature)(const Complex &);
  
  DDFunc ( FuncSignature fsig,const String &corr );
  ~DDFunc() {};
  
  virtual Bool  reset ( const Vector<Int> &corr );
  virtual Float map   ( const Cube<Complex> &vis,uInt ich,uInt irow ) const;

// Define these functions, because using std::real/imag in getFunction
// matches multiple functions.
  static Float real (const Complex&);
  static Float imag (const Complex&);

// Static function to map a function name into a function pointer
// Functions currently recognized: ABS ARG NORM RE IM 
  static FuncSignature getFunction( const String &name );

// Static function to map string expression into a DDMapper
// Possible syntax is:
//   <FUNC> <CC>
//   SUM <FUNC> <CC> <CC>
//   DIFF <FUNC> <CC> <CC>
//   <FUNC> SUM <CC> <CC>
//   <FUNC> DIFF <CC> <CC>
  static DDMapper * getMapper ( String &desc,const Vector<String> &expr,Bool throw_excp=False );
  
protected:
  Int icorr;
  Stokes::StokesTypes corrtype;
  FuncSignature func;
};

// <summary>
// DDSumFunc: maps two correlations A and B into func(A)+func(B)
// </summary>
// <use visibility=local>
class DDSumFunc : public DDFunc
{
public:
  DDSumFunc ( FuncSignature fsig,const String &corr1,const String &corr2 );
  virtual ~DDSumFunc() {};
  
  virtual Bool  reset ( const Vector<Int> &corr );
  virtual Float map   ( const Cube<Complex> &vis,uInt ich,uInt irow ) const;

protected:
  Int icorr2;
  Stokes::StokesTypes corrtype2;
};

// <summary>
// DDFuncSum: maps two correlations A and B into func(A+B)
// </summary>
// <use visibility=local>
class DDFuncSum : public DDSumFunc
{
public:
  DDFuncSum ( FuncSignature fsig,const String &corr1,const String &corr2 );
  virtual ~DDFuncSum() {};
  
  virtual Float map   ( const Cube<Complex> &vis,uInt ich,uInt irow ) const;
};

// <summary>
// DDFuncDiff: maps two correlations A and B into func(A-B)
// </summary>
// <use visibility=local>
class DDFuncDiff : public DDSumFunc
{
public:
  DDFuncDiff ( FuncSignature fsig,const String &corr1,const String &corr2 );
  virtual ~DDFuncDiff() {};
  
  virtual Float map   ( const Cube<Complex> &vis,uInt ich,uInt irow ) const;
};

// <summary>
// DDDiffFunc: maps two correlations A and B into func(A)-func(B)
// </summary>
// <use visibility=local>
class DDDiffFunc : public DDSumFunc
{
public:
  DDDiffFunc ( FuncSignature fsig,const String &corr1,const String &corr2 );
  virtual ~DDDiffFunc() {};
  
  virtual Float map   ( const Cube<Complex> &vis,uInt ich,uInt irow ) const;
};

// helper function to split an expression into elements
Vector<String> splitExpression( const Vector<String> &expr0 );


} //# NAMESPACE CASA - END

#endif
