//# MeasMath.h: Measure conversion aid routines
//# Copyright (C) 1998
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

#if !defined(AIPS_MEASMATH_H)
#define AIPS_MEASMATH_H

//# Includes
#include <aips/aips.h>
#include <aips/Quanta/RotMatrix.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Measures/MeasFrame.h>

//# Forward Declarations
class Measure;
class MRBase;
class Precession;
class Nutation;
class SolarPos;
class Aberration;

//# Typedefs

// <summary> Measure conversion aid routines </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>MeasConvert</linkto> class 
//   <li> <linkto class=MConvertBase>overall conversion</linkto>  class
// </prerequisite>
//
// <etymology>
// Measureand Mathematics
// </etymology>
//
// <synopsis>
// The conversion of measures like MDirection, MPosition etc have many
// conversion routines in common. This class combines all of these
// conversions, including data caches for re-use.
// 
// The class is always created by the default constructor. For each operation
// (like e.g. Precession application), it has three function:
// <ul>
//   <li> create(): create an instance of the data necessary to convert
//   <li> apply(): apply the conversion (in the sense of from standard to
//	perturbed (e.g. from J2000 to TOPO))
//   <li> deapply(): in the reverse direction of apply
// </ul>
// </synopsis>
//
// <example>
// See <linkto class=MCDirection>MCDirection</linkto> source for how to use
// the class.
// </example>
//
// <motivation> To re-use code for a specific measure conversion, and to ease
// the caching administration for each individual conversion.
// </motivation>
//
// <todo asof="1998/09/30">
//	<li> Nothing I know of
// </todo>

class MeasMath { 
  
public:

  //# Constructors
  // Default constructor
  MeasMath();
  
  //# Destructor
  ~MeasMath();

  //# Member functions
  // Initialise the frame to be used. The apply direction uses the
  // inref if present; the deapply the outref if present, otherwise the
  // other one.
  void initFrame(MRBase &outref, MRBase &inref);

  // Functions to create a particular conversion instance; to apply
  // or deapply the instance.
  // <group>
  // Precession for J2000 (IAU definition) and in coordinates
  //   <group>
  void createPrecession();
  void applyPrecession(MVPosition &in);
  void deapplyPrecession(MVPosition &in);
  //   </group>
  // Precession for B1950 and in coordinates
  //   <group>
  void createPrecessionB1950();
  void applyPrecessionB1950(MVPosition &in);
  void deapplyPrecessionB1950(MVPosition &in);
  //   </group>
  // Nutation for J2000 (IAU standard) and in coordinates
  //   <group>
  void createNutation();
  void applyNutation(MVPosition &in);
  void deapplyNutation(MVPosition &in);
  //   </group>
  // Nutation for B1950 and in coordinates
  //   <group>
  void createNutationB1950();
  void applyNutationB1950(MVPosition &in);
  void deapplyNutationB1950(MVPosition &in);
  //   </group>
  // Precession and Nutation for J2000 or B1950 and in coordinates
  //   <group>
  void createPrecNutat();
  void applyPrecNutat(MVPosition &in);
  void deapplyPrecNutat(MVPosition &in);
  void createPrecNutatB1950();
  void applyPrecNutatB1950(MVPosition &in);
  void deapplyPrecNutatB1950(MVPosition &in);
  //   </group>
  // Aberration for J2000 (IAU definition) and B1950 and in coordinates
  //   <group>
  void createAberration();
  void applyAberration(MVPosition &in);
  void deapplyAberration(MVPosition &in);
  void createAberrationB1950();
  void applyAberrationB1950(MVPosition &in);
  void deapplyAberrationB1950(MVPosition &in);
  //   </group>
  // Solar bending for J2000 (IAU definition) and in coordinates
  //   <group>
  void createSolarPos();
  void applySolarPos(MVPosition &in);
  void deapplySolarPos(MVPosition &in);
  //   </group>
  // Various conversions
  // <group>
  void applyHADECtoITRF(MVPosition &in);
  void deapplyHADECtoITRF(MVPosition &in);
  void applyJ2000toB1950(MVPosition &in);
  void deapplyJ2000toB1950(MVPosition &in);
  void applyETerms(MVPosition &in);
  void deapplyETerms(MVPosition &in);
  // </group>
  // </group>

private: 
  //# Enum
  // Types of frame information groups
  enum FrameType {
    EPOCH = 0,
    POSITION,
    DIRECTION,
    VELOCITY,
    N_FrameType };
  // Types of frame information
  enum FrameInfo {
    TDB = 0,
    LASTR,
    LONG,
    LAT,
    RADIUS,
    N_FrameInfo };

  //# Typedefs
  // To get frame group
  typedef const Measure *const (MeasFrame::*FRFCT)() const;
  // To get frame info
  typedef Bool (MCFrame::*FRINFO)(Double &);

  //# Cached Data
  // Data cached for fast calculations and workspace
  // <group>
  // Frame information
  // <group>
  Bool inOK_p;
  Bool outOK_p;
  Bool frameOK_p[N_FrameType];
  MeasFrame *inFrame_p;
  MeasFrame *outFrame_p;
  MeasFrame *applyFrame_p[N_FrameType];
  MeasFrame *deapplyFrame_p[N_FrameType];
  // </group>
  // Conversion information
  // <group>
  SolarPos *SOLPOSIAU;
  Aberration *ABERIAU, *ABERB1950;
  Nutation *NUTATIAU, *NUTATB1950;
  Precession *PRECESIAU, *PRECESB1950;
  // </group>
  // Workspace
  // <group>
  RotMatrix ROTMAT1;
  MVPosition MVPOS1, MVPOS2, MVPOS3;
  Double g1, g2, g3, lengthE;
  Bool infoOK_p[N_FrameInfo];
  Double info_p[N_FrameInfo];
  // </group>
  // </group>

  //# Constructors
  // Copy constructor (not implemented)
  MeasMath(const MeasMath &other);
  // Assignment (not implemented)
  MeasMath &operator=(const MeasMath &other);
  
  //# Member functions
  // Get proper frame information
  void getFrame(FrameType i);

  // Get information from the frame
  // <thrown>
  //  <li> AipsError if information not available
  // </thrown>
  // <group>
  void getInfo(FrameInfo i);
  // </group>

};

#endif
