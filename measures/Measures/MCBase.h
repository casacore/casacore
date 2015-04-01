//# MCBase.h: Base for specific measure conversions
//# Copyright (C) 1995,1996,1997,1998
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

#ifndef MEASURES_MCBASE_H
#define MEASURES_MCBASE_H

//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MeasValue;
class MCBase;
class MRBase;
class MConvertBase;
class String;

//# Typedefs

// <summary>  Base for specific measure conversions </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
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
// It also has a static routine to calculate the state transition table based
// on a list of transitions. The makeState() method find the shortest route
// (weighted if necessary) for a given list of state transitions. 
//
// The user of the Measure classes has no direct interaction with this class.
// </synopsis>
//
// <example>
// Convert (with all steps explicit) a UTC to an IAT time.
// <srcblock>
//      #include <casacore/measures/Measures.h>
//      #include <casacore/measures/Measures/MCEpoch.h>
//	cout << "TAI for UTC = MJD(50237.29): " <<
//		MEpoch::Convert(MEpoch(MVEpoch(Quantity(50237.29, "d")),
//			               MEpoch::Ref(MEpoch::UTC)),
//		                MEpoch::Ref(MEpoch::TAI))() <<
//		endl;
// </srcblock>
// To get a static state transition matrix:
// <srcblock>
//	static Bool made = False;		// set not yet done
//	enum types {				// states
//		A=0, B, C, D, E, ntyp };
//	enum routes {				// routes
//		A_B, B_C, B_D, C_D, C_E,
//		D_C, C_B, B_A, D_B, E_C, nrout };
//	static uInt list [nrout][3] = {		// description. The third number
//		{A, B, 0},			// is a penalty hop to weight
//		{B, C, 0},			// against using this route
//		{B, D, 0},
//		{C, D, 0},
//		{C, E, 0},
//		{D, C, 0},
//		{C, B, 0},
//		{B, A, 0},
//		{D, B, 0},
//		{E, C, 0} };
//	static uInt state[ntyp][ntyp];	// the resultant transition matrix
//					// diagonal == nrout
//	// Make the state machine
//	MCBase::makeState(state[0], ntyp, nrout, routes);
//      made = True;
// </srcblock>
// </example>
//
// <motivation>
//	To have specific conversion bases
// </motivation>
//
// <todo asof="1998/09/21">
//	<li> Nothing I know
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

protected:
  // The following routines create a state transition matrix from a list
  // of all defined transitions. It uses the following information:
  // <ul>
  //  <li> nrout: the number of transitions; numbered 0, ...
  //  <li> ntyp: the number of states
  //  <li> list: a [nrout][3] list of input and output transition type of
  //		transition and a penalty hop number (<100)
  //  <li> state: a [ntyp][ntyp] transition matrix with diagonal elements set
  //		to nrout.
  // </ul>
  // <group>
  // Routine to make the transition table if necessary
  static void makeState(uInt *state,
			const uInt ntyp, const uInt nrout,
			const uInt list[][3]);
  // Return a fromatted String with matrix information (based on < 100 types)
  static String showState(uInt *state,
			  const uInt ntyp, const uInt nrout,
			  const uInt list[][3]);
private:
  // Routine to find the shortest route between two points 
  static Bool findState(uInt &len, uInt *state, uInt *mcnt, Bool &okall,
			Bool *visit, const uInt *tcnt, const uInt *tree,
			const uInt &in, const uInt &out,
			const uInt ntyp, const uInt nrout,
			const uInt list[][3]);
  // </group>

};


} //# NAMESPACE CASACORE - END

#endif
