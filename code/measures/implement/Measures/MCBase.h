//# MCBase.h: Base for specific measure conversions
//# Copyright (C) 1995,1996,1997
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
//#
//# $Id$

#if !defined(AIPS_MCBASE_H)
#define AIPS_MCBASE_H

#if defined(_AIX)
#pragma implementation ("MCBase.cc")
#endif

//# Includes
#include <aips/aips.h>

//# Forward Declarations
class MeasValue;
class MCBase;
class MRBase;
class MConvertBase;

//# Typedefs

// <summary>
// Base for specific measure conversions
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class
//   <li> <linkto class=MConvertBase>MConvertBase</linkto>: conversion engine
// </prerequisite>
//
// <etymology>
// Measure, Conversion and Base
// </etymology>
//
// <synopsis>
// MCBase forms the base for the individual state machines doing actual
// conversions between frames. (see e.g. <linkto class=MCEpoch>MCEpoch</linkto>)
//
// It also is a cache for information that is time-consuming to calculate over
// and over again (e.g. Nutation).<br> 
// The user of the Measure classes has no direct interaction with this class.
// </synopsis>
//
// <example>
// Convert (with all steps explicit) a UTC to an IAT time.
// <srcblock>
//      #include <aips/Measures.h>
//      #include <aips/Measures/MEpoch.h>
//	cout << "TAI for UTC = MJD(50237.29): " <<
//		MEpoch::Convert(MEpoch(MVEpoch(Quantity(50237.29, "d")),
//			               MEpoch::Ref(MEpoch::UTC)),
//		                MEpoch::Ref(MEpoch::TAI))() <<
//		endl;
// </srcblock>
// This example will interact with MCEpoch.
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1997/04/15">
//	<li>
// </todo>

class MCBase {

public:
  
  //# Typedefs
  
  //# Constructors
  
  //# Destructor
  virtual ~MCBase();
  
  //# Operators

  //# Enumerations
  // Each derived class should have a list of routines to be called:
  enum Routes {
    N_Routes};
  
  //# Member functions
  // All these functions are called by Measure::Convert classes only
  // <group>  
  // Create conversion state machine list
  virtual void getConvert(MConvertBase &mc,
			  const MRBase &inref,
			  const MRBase &outref) = 0;
  
  // Create help structures for Measure conversion routines
  virtual void initConvert(uInt which, MConvertBase &mc) = 0;
  
  // Delete the pointers used in the MeasConvert help structure cache
  virtual void clearConvert() = 0;
  
  // Routine to convert a Measure from one reference frame to another
  virtual void doConvert(MeasValue &in,
			 MRBase &inref,
			 MRBase &outref,
			 const MConvertBase &mc) = 0;
  // </group>
};

#endif
