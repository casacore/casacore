//# MCDirection.h: MDirection conversion routines 
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

#if !defined(AIPS_MCDIRECTION_H)
#define AIPS_MCDIRECTION_H

//# Includes
#include <aips/aips.h>
#include <aips/Measures/MeasBase.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MCBase.h>
#include <aips/Measures/MConvertBase.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MeasMath.h>

//# Forward Declarations
class MCDirection;
class Euler;
class RotMatrix;
class String;
template <class T> class Vector;

//# Typedefs

// <summary> MDirection conversion routines  </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasure" demos="">
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
// <todo asof="1998/09/20">
//	<li> Extend MeasMath a bit more
// </todo>

class MCDirection : public MCBase { 
  
public:

  //# Friends
  // Conversion of data
  friend class MeasConvert<MDirection,MVDirection,MCDirection>;
  
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
    B1950_J2000,
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
    AZEL_HADEC,
    HADEC_TOPO,
    AZEL_AZELSW,
    AZELSW_AZEL,
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
    N_Routes,
    // General for Planets
    R_PLANET0,
    R_PLANET,
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
    R_MOON
  };
  
  //# Typedefs
  
  //# Operators
  
  //# General Member Functions
  
  //# Enumerations
  
  //# Cached Data
  RotMatrix *ROTMAT1;
  Euler *EULER1;
  MVPosition *MVPOS1, *MVPOS2, *MVPOS3;
  Vector<Double> *VEC61, *VEC62, *VEC63;
  MeasMath measMath;

  //# State machine data
  // Has state matrix been made
  static Bool stateMade_p;
  // Transition list
  static uInt ToRef_p[N_Routes][3];
  // Transition matrix
  static uInt FromTo_p[MDirection::N_Types][MDirection::N_Types];

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
  
};

#endif

