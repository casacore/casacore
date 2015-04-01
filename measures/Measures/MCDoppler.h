//# MCDoppler.h: MDoppler conversion routines 
//# Copyright (C) 1995,1996,1997,1998,1999,2002
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

#ifndef MEASURES_MCDOPPLER_H
#define MEASURES_MCDOPPLER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/measures/Measures/MCBase.h>
#include <casacore/measures/Measures/MConvertBase.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MCDoppler;
class String;

//# Typedefs

// <summary> MDoppler conversion routines  </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
//   <li> <linkto class=MCBase>MCBase</linkto> base class
//   <li> <linkto class=MConvertBase>overall conversion</linkto>  class
// </prerequisite>
//
// <etymology>
// Measure, Convert and Doppler
// </etymology>
//
// <synopsis>
// Contains state machinery and caching for actual conversions
// </synopsis>
//
// <example>
// Conversion of a radio Doppler to an optical
// <srcblock>
//	#include <casacore/measures/Measures.h>
//	#include <casacore/measures/Measures/MDoppler.h>
//	MDoppler radio(0.01);		// A radio Doppler value
//	cout << "Doppler radio = " << radio << "; optical = " <<
//		MDoppler::Convert(radio, MDoppler::OPTICAL)() << // Convert 
//		endl;
// </srcblock>
// Setting up a conversion
// <srcblock>
//	MDoppler::Convert to_opt(MDoppler::RADIO, MDoppler::OPTICAL);
//	for (Double d=0; d<0.1; d += 0.005) {
//		cout << "radio = " << d << " to optical = " <<
//			to_opt(d) << endl;
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1996/05/04">
// </todo>

class MCDoppler : public MCBase {

public:

  //# Friends
  // Conversion of data
  friend class MeasConvert<MDoppler>;
  
  //# Constructors
  // Default constructor
  MCDoppler();
  
  //# Destructor
  ~MCDoppler();

  //# Member functions
  // Show the state of the conversion engine (mainly for debugging purposes)
  static String showState();

private:

  //# Enumerations
  // The list of actual routines provided.
  // <note role=warning> Each <src>AA_BB</src> in the list points to routine
  // that can be used in the FromTo list in the getConvert routine.
  // In addition the type to which each is converted should be in the
  // ToRef array, again in the proper order. </note>
  enum Routes {
    RADIO_RATIO, 
    Z_RATIO,
    BETA_RATIO,
    GAMMA_RATIO,
    RATIO_RADIO, 
    RATIO_Z,
    RATIO_BETA,
    RATIO_GAMMA,
    N_Routes };
  
  //# Typedefs
  
  //# Operators
  
  //# General Member Functions
  
  //# Enumerations
  
  //# Cached Data

  //# State machine data
  // Transition list
  static uInt ToRef_p[N_Routes][3];
  // Transition matrix
  static uInt FromTo_p[MDoppler::N_Types][MDoppler::N_Types];
  // Mutex for thread-safety.
  static MutexedInit theirMutexedInit;

  // Fill the global state in a thread-safe way.
  static void fillState()
    { theirMutexedInit.exec(); }
  
  //# Member functions
  
  // Create conversion function pointer
  virtual void getConvert(MConvertBase &mc,
			  const MRBase &inref,
			  const MRBase &outref);
  
  // Create help structures for Measure conversion routines
  virtual void initConvert(uInt which, MConvertBase &mc);
  
  // Delete the pointers used in the MeasConvert help structure cache
  virtual void clearConvert();
  
  // Routine to convert Doppler from one reference frame to another
  virtual void doConvert(MeasValue &in,
			 MRBase &inref,
			 MRBase &outref,
			 const MConvertBase &mc);
  // Conversion routine to cater for inheritance question
  void doConvert(MVDoppler &in,
		 MRBase &inref,
		 MRBase &outref,
		 const MConvertBase &mc);
  
private:
  // Fill the global state in a thread-safe way.
  static void doFillState (void*);  
};


} //# NAMESPACE CASACORE - END

#endif
