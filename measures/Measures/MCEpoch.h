//# MCEpoch.h: MEpoch conversion routines 
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

#ifndef MEASURES_MCEPOCH_H
#define MEASURES_MCEPOCH_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/measures/Measures/MCBase.h>
#include <casacore/measures/Measures/MConvertBase.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MCEpoch;
class Nutation;
class String;

//# Typedefs

// <summary> MEpoch conversion routines </summary>

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
// Measure, Convert and Epoch
// </etymology>
//
// <synopsis>
// Contains state machinery and caching for actual conversions
// </synopsis>
//
// <example>
// Convert (with all steps explicit) a UTC to an IAT time.
// <srcblock>
//	#include <casacore/measures/Measures.h>
//	#include <casacore/measures/Measures/MEpoch.h>
//	#include <casacore/measures/Measures/MCEpoch.h>
//	cout << "TAI for UTC = MJD(50237.29): " <<
//		MEpoch::Convert(MEpoch(MVEpoch(Quantity(50237.29, "d")),
//			               MEpoch::Ref(MEpoch::UTC)),
//		                MEpoch::Ref(MEpoch::TAI))() <<
//		endl;
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1998/09/21">
//	<li> Nothing I know
// </todo>

class MCEpoch : public MCBase {

public:

  //# Friends
  friend class MeasConvert<MEpoch>;
  
  //# Constructors
  // Default constructor
  MCEpoch();
  
  //# Destructor
  ~MCEpoch();

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
    LAST_GAST,
    GAST_LAST,
    LMST_GMST1,
    GMST1_LMST,
    GMST1_UT1,
    UT1_GMST1,
    GAST_UT1,
    UT1_GAST,
    UT1_UTC,
    UTC_UT1,
    UT1_UT2,
    UT2_UT1,
    UTC_TAI,
    TAI_UTC,
    TAI_TDT,
    TDT_TAI,
    TDT_TDB,
    TDB_TDT,
    TDT_TCG,
    TCG_TDT,
    TDB_TCB,
    TCB_TDB,
    N_Routes,
    RAZING
  };
  
  //# Typedefs
  
  //# Operators
  
  //# General Member Functions
  
  //# Enumerations
  
  //# Cached Data
  Nutation *NUTATFROM;
  Nutation *NUTATTO;

  //# State machine data
  // Transition list
  static uInt ToRef_p[N_Routes][3];
  // Transition matrix
  static uInt FromTo_p[MEpoch::N_Types][MEpoch::N_Types];
  // Mutex for thread-safety.
  static MutexedInit theirMutexedInit;

  // Fill the global state in a thread-safe way.
  static void fillState()
    { theirMutexedInit.exec(); }
  
  //# Constructors
  // Copy constructor (not implemented)
  MCEpoch(const MCEpoch &other);
  // Assignment (not implemented)
  MCEpoch &operator=(const MCEpoch &other);
  
  //# Member functions
  
  // Create conversion function pointer
  virtual void getConvert(MConvertBase &mc,
			  const MRBase &inref,
			  const MRBase &outref);
  
  // Create help structures for Measure conversion routines
  virtual void initConvert(uInt which, MConvertBase &mc);
  
  // Delete the pointers used in the MeasConvert help structure cache
  virtual void clearConvert();
  
  // Routine to convert time from one reference frame to another
  virtual void doConvert(MeasValue &in,
			 MRBase &inref,
			 MRBase &outref,
			 const MConvertBase &mc);
  // Conversion routine to cater for inheritance question
  void doConvert(MVEpoch &in,
		 MRBase &inref,
		 MRBase &outref,
		 const MConvertBase &mc);
  
private:
  // Fill the global state in a thread-safe way.
  static void doFillState (void*);  
};


} //# NAMESPACE CASACORE - END

#endif
