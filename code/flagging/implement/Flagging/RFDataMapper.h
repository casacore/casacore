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
#ifndef FLAGGING_RFDATAMAPPER_H
#define FLAGGING_RFDATAMAPPER_H

#include <flagging/Flagging/RFChunkStats.h> 
#include <flagging/Flagging/DDMapper.h> 
#include <casa/Arrays/Cube.h>
#include <scimath/Mathematics/RigidVector.h>
#include <casa/Containers/Block.h>

class RFDataMapper;
class VisBuffer;

// a row mapper member function maps a row to a single value
typedef Float (RFDataMapper::*RowMapperFunc)(uInt);

// <summary>
// RFDataMapper: maps complex visibilities to a single real value
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> DDMapper
// </prerequisite>
//
// <synopsis>
// RFDataMapper provides a mechanism to derive a single real value from 
// a set of complex visibilities in a specific column, using a user-specified 
// expression (i.e. ABS(XX), ABS(XX)-ABS(YY), etc.) This is used by many 
// flagging agents.
// </synopsis>
//
// <motivation>
// To provide a common mechanism for all flagging agents
// </motivation>
//
// <todo asof="2001/04/16">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>
class RFDataMapper
{
public:
  // type of data mapper: row or individual correlations
  typedef enum { MAPROW,MAPCORR } MapperType;
    
  // construct from a column and a DDMapper
  RFDataMapper( const String &col,DDMapper *map );
  // construct from a column and an expression
  RFDataMapper( const Vector<String> &expr,const String &defcol = "" );
  // destructor
  ~RFDataMapper();
  
  // returns type of mapper
  MapperType type ();
  
  // If the value being mapped into is cyclic (i.e. an angle),
  // returns value of full cycle (e.g. 360); otherwise returns 0.
  Double getValueCycle ();
  // Returns base of a cyclic value (e.g. -180, if value is an angle -180..180)
  // If value is non-cyclic, the result is undefined.
  Double getValueBase  ();

  // gets a value from the DDMapper
  Float mapValue ( uInt ich,uInt irow );
  // gets a value from the row mapper
  Float mapValue ( uInt irow );

  // uses mapper to compute a correlations mask
  RFlagWord corrMask (const VisibilityIterator &vi);
  
  // point the datamapper at a visbuffer - called for every new buffer
  void  setVisBuffer (VisBuffer &vb);

  // returns description
  String description () const;
  // returns description of expression
  String descExpression () const;

  // a cube mapper function maps a visbuffer to a data cube. This
  // belongs in private or protected, but the SGI compiler wouldn't hear of it
  typedef Cube<Complex> * (*CubeMapperFunc)(VisBuffer &);
  
protected:

  // static helper function to interpret constructor parameters into a cube mapper
  static CubeMapperFunc getCubeMapper( const String &col,Bool throw_excp = False );
      
  String expr_desc,desc;           // expression and description of data mapper
  DDMapper *ddm;                   // data mapper
  RowMapperFunc rowmapper;         // row mapper
  Cube<Complex> *pviscube;         // pointer to visibilities cube 
  Vector<RigidVector<Double,3> > *puvw; // pointer to UVW matrix
  CubeMapperFunc cubemap; // function to map a chunk to a visibility cube
  MapperType mytype;
  Double full_cycle,cycle_base;     // for cyclic values (i.e. angles)
  
// various row mappers
  Float dummyRowMapper (uInt);
  Float U_RowMapper (uInt);
  Float V_RowMapper (uInt);
  Float W_RowMapper (uInt);
  Float AbsU_RowMapper (uInt);
  Float AbsV_RowMapper (uInt);
  Float AbsW_RowMapper (uInt);
  Float UVD_RowMapper (uInt);
  Float UVA_RowMapper (uInt);
  Float HA_RowMapper (uInt);
  
// required by the HA mapper - sin(declination) of phase center; 
  Double sin_dec;
};

inline RFDataMapper::MapperType RFDataMapper::type ()
{ return mytype; }

inline Float RFDataMapper::mapValue ( uInt ich,uInt irow )
{ return ddm->map(*pviscube,ich,irow); }

inline Float RFDataMapper::mapValue ( uInt irow )
{ return (this->*rowmapper)(irow); }

inline String RFDataMapper::description () const
{ return desc; }
inline String RFDataMapper::descExpression () const
{ return expr_desc; }

inline Double RFDataMapper::getValueCycle ()
{ return full_cycle; }
inline Double RFDataMapper::getValueBase ()
{ return cycle_base; }

#endif
