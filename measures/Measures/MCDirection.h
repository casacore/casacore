//# MCDirection.h: MDirection conversion routines 
//# Copyright (C) 1995-2000,2002,2004,2007
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

#ifndef MEASURES_MCDIRECTION_H
#define MEASURES_MCDIRECTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/measures/Measures/MCBase.h>
#include <casacore/measures/Measures/MConvertBase.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MeasMath.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MCDirection;
class MVPosition;
class String;

template <class T> class Vector;

//# Typedefs

// <summary> MDirection conversion routines  </summary>

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
// Measure, Convert and Direction
// </etymology>
//
// <synopsis>
// Contains state machinery and caching for actual conversions
// </synopsis>
//
// <example>
// See <linkto module=Measures>Measures</linkto> module description for
// conversion examples.
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="2000/09/12">
//	<li> nothing I know
// </todo>

class MCDirection : public MCBase { 
  
public:

  //# Friends
  // Conversion of data
  friend class MeasConvert<MDirection>;
  
  //# Constructors
  // Default constructor
  MCDirection();
  
  //# Destructor
  ~MCDirection();

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
    GAL_J2000,
    GAL_B1950,
    J2000_GAL,
    B1950_GAL,
    J2000_B1950,
    J2000_B1950_VLA,
    B1950_J2000,
    B1950_VLA_J2000,
    B1950_B1950_VLA,
    B1950_VLA_B1950,
    J2000_JMEAN,
    B1950_BMEAN,
    JMEAN_J2000,
    JMEAN_JTRUE,
    BMEAN_B1950,
    BMEAN_BTRUE,
    JTRUE_JMEAN,
    BTRUE_BMEAN,
    J2000_JNAT,
    JNAT_J2000,
    B1950_APP,
    APP_B1950,
    APP_TOPO,
    HADEC_AZEL,
    HADEC_AZELGEO,
    AZEL_HADEC,
    AZELGEO_HADEC,
    HADEC_TOPO,
    AZEL_AZELSW,
    AZELGEO_AZELSWGEO,
    AZELSW_AZEL,
    AZELSWGEO_AZELGEO,
    APP_JNAT,
    JNAT_APP,
    J2000_ECLIP,
    ECLIP_J2000,
    JMEAN_MECLIP,
    MECLIP_JMEAN,
    JTRUE_TECLIP,
    TECLIP_JTRUE,
    GAL_SUPERGAL,
    SUPERGAL_GAL,
    ITRF_HADEC,
    HADEC_ITRF,
    TOPO_HADEC,
    TOPO_APP,
    ICRS_J2000,
    J2000_ICRS,
    N_Routes,
    // General for Planets
    R_PLANET0,
    R_PLANET,
    R_COMET0,
    R_COMET,
    // Individual planets. Order should be the same as in MDirection.h
    R_MERCURY,
    R_VENUS,
    R_MARS,
    R_JUPITER,
    R_SATURN,
    R_URANUS,
    R_NEPTUNE,
    R_PLUTO,
    R_SUN,
    R_MOON };
  
  //# Typedefs
  
  //# Operators
  
  //# General Member Functions
  
  //# Enumerations
  
  //# Cached Data
  MVPosition *MVPOS1, *MVPOS2, *MVPOS3;
  Vector<Double> *VEC61, *VEC62, *VEC63;
  MeasMath measMath;

  //# State machine data
  // Transition list
  static uInt ToRef_p[N_Routes][3];
  // Transition matrix
  static uInt FromTo_p[MDirection::N_Types][MDirection::N_Types];
  // Mutex for thread-safety.
  static MutexedInit theirMutexedInit;

  // Fill the global state in a thread-safe way.
  static void fillState()
    { theirMutexedInit.exec(); }

  //# Constructors
  // Copy constructor (not implemented)
  MCDirection(const MCDirection &other);
  // Assignment (not implemented)
  MCDirection &operator=(const MCDirection &other);
  
  //# Member functions
  
  // Create conversion function pointer
  virtual void getConvert(MConvertBase &mc,
			  const MRBase &inref, 
			  const MRBase &outref);
  
  // Create help structures for Measure conversion routines
  virtual void initConvert(uInt which, MConvertBase &mc);
  
  // Delete the pointers used in the MeasConvert help structure cache
  virtual void clearConvert();
  
  // Routines to convert directions from one reference frame to another
  virtual void doConvert(MeasValue &in,
			 MRBase &inref,
			 MRBase &outref,
			 const MConvertBase &mc);
  // Conversion routine to cater for inheritance question
  void doConvert(MVDirection &in,
		 MRBase &inref,
		 MRBase &outref,
		 const MConvertBase &mc);
  
private:
  // Fill the global state in a thread-safe way.
  static void doFillState (void*);  
};


} //# NAMESPACE CASACORE - END

#endif

