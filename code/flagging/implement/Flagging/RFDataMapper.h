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
# if !defined(AIPS_RF_DATAMAPPER_H)
#define AIPS_RF_DATAMAPPER_H

#include <trial/Flagging/RFChunkStats.h> 
#include <trial/Flagging/DDMapper.h> 
#include <aips/Arrays/Cube.h>
    
class VisBuffer;

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
  // a cube mapper func maps a visbuffer to a data cube
  typedef Cube<Complex> * (*CubeMapperFunc)(VisBuffer &);

  // construct from a column and a DDMapper
  RFDataMapper( const String &col,DDMapper *map );
  // construct from a column and an expression
  RFDataMapper( const String &col,const Vector<String> &expr );
  // desctruct
  ~RFDataMapper();

  // gets a value from the DDMapper
  Float mapValue ( uInt ich,uInt irow );

  // uses mapper to compute a correlations mask
  RFlagWord corrMask (const VisibilityIterator &vi);
  
  // gets pointer to viscube from visbuffer
  void  setVisBuffer (VisBuffer &vb);

  // returns description
  String description () const;
  // returns description of expression
  String descExpression () const;
  
protected:
  // static helper function to interpret constructor parameters
  static CubeMapperFunc getCubeMapper( const String &col );
      
  String expr_desc,ddm_desc;        // description of data mapper
  DDMapper *ddm;          // data mapper
  Cube<Complex> *pviscube; // pointer to visibilities cube 
  CubeMapperFunc cubemap; // function to map a chunk to a visibility cube
};

inline void RFDataMapper::setVisBuffer ( VisBuffer &vb )
{ pviscube = (*cubemap)(vb); }

inline Float RFDataMapper::mapValue ( uInt ich,uInt irow )
{ return ddm->map(*pviscube,ich,irow); }

inline String RFDataMapper::description () const
{ return ddm_desc; }
inline String RFDataMapper::descExpression () const
{ return expr_desc; }

#endif
