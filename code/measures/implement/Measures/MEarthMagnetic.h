//# MEarthMagnetic.h: A Measure: Magnetic field on Earth
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

#if !defined(AIPS_MEARTHMAGNETIC_H)
#define AIPS_MEARTHMAGNETIC_H

#if defined(_AIX)
#pragma implementation ("MEarthMagnetic.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Measures/MeasBase.h>
#include <aips/Measures/MeasRef.h>
#include <trial/Measures/MVEarthMagnetic.h>

//# Forward Declarations
class MEarthMagnetic;
class MCEarthMagnetic;
template <class M, class F, class MC> class MeasConvert;

//# Typedefs

// <summary> A Measure: Magnetic field on Earth </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
// </prerequisite>
//
// <etymology>
// Earth and Magnetic field
// </etymology>
//
// <synopsis>
// MEarthMagnetic forms derived Measure class for Earth' magnetic flux density
// It contains the following magnetic field models:
// <ul>
//   <li> IGRF		international reference field
//   <li> directions	direction types to convert to (e.g. AZEL)
// </ul>
// <note role=warning>
// The IGRF needs a Table
// of coefficients (at 5-year interval) </note>
//
// Conversion between field models is not supported.
// The field can be given in any of the standard <linkto class=MDirection>
// direction</linkto> coordinate systems ny selecting the correct conversion output
// reference type (e.g. MEarthMagnetic::AZEL). 
//
// An <linkto class=EarthFieldMachine> EarthFieldMachine</linkto> has been
// provided to get e.g. the field in a certain direction at a certain height.
//
// </synopsis>
//
// <example>
// <srcblock>
// // Where on Earth
// MPosition pos(MVPosition(Quantity(20,'m'), Quantity(5,'deg'), 
//                          Quantity(52,'deg')), MPosition::WGS84);
// // Time we want it
// MEpoch epo(MVEpoch(50000));
// // Put in frame
// MeasFrame frame(pos, epo);
// // Magnetic field model
// MEarthMagnetic mf;
// // Show field strength in Gauss in AzEl system
// cout <<
//   MEarthMagnetic::Convert(mf, MEarthMagnetic::AZEL)().
//		getValue().getLength("G") << endl;
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1997/02/19">
//	<li> nothing I know
// </todo>

class MEarthMagnetic : public MeasBase<MVEarthMagnetic, MeasRef<MEarthMagnetic> >  {

public:
//# Friends
// Conversion of data
    friend class MeasConvert<MEarthMagnetic, MVEarthMagnetic, MCEarthMagnetic>;

//# Enumerations
// Types of known MEarthMagnetics
// <note role=tip> The order defines the order in the translation matrix FromTo
// in the getConvert routine. Do not change the order without
// changing the array. Additions should be made before N_types, and
// an additional row and column should be coded in FromTo, and
// in showType().</note>
    enum Types {
      ITRF,
      J2000,
      JMEAN,
      JTRUE,
      APP,
      B1950,
      BMEAN,
      BTRUE,
      GALACTIC,
      HADEC,
      AZEL,
      AZELSW,
      JNAT,
      ECLIPTIC,  
      MECLIPTIC,
      TECLIPTIC,
      SUPERGAL,
      N_Types,
      // Models. Firsty one should be IGRF
      IGRF = 32,
      N_Models,
      // All extra bits
      EXTRA = 32,
      // Defaults
      DEFAULT=IGRF,
      // Synonyms
      AZELNE=AZEL 
    };
//# Typedefs
// Measure reference
    typedef MeasRef<MEarthMagnetic> Ref;
// MeasConvert use
    typedef class MeasConvert<MEarthMagnetic,MVEarthMagnetic,MCEarthMagnetic> Convert;

//# Constructors
// <note> In the following constructors and other functions, all 
// <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
// where no offsets or frames are needed in the reference. For reasons
// of compiler limitations the formal arguments had to be specified as
// <em>uInt</em> rather than the Measure enums that should be used as actual 
// arguments.</note>
// Default constructor; generates the default IGRF type
    MEarthMagnetic();
// Create from data and reference
// <group>
    MEarthMagnetic(const MVEarthMagnetic &dt);
    MEarthMagnetic(const MVEarthMagnetic &dt, const MEarthMagnetic::Ref &rf);
    MEarthMagnetic(const MVEarthMagnetic &dt, uInt rf);
    MEarthMagnetic(const Measure *dt);
    MEarthMagnetic(const MeasValue *dt);
    MEarthMagnetic(const MEarthMagnetic::Ref &rf);
// </group>

  // <group>
  MEarthMagnetic(const MEarthMagnetic &);
  MEarthMagnetic &operator=(const MEarthMagnetic &);
  // </group>

//# Destructor
    ~MEarthMagnetic();

//# Operators

//# General Member Functions
// Tell me your type
// <group>
    virtual const String &tellMe() const;
    static const String &showMe();
    virtual uInt type() const;
    static void assert(const Measure &in);
// </group>
// Translate reference code
    static const String &showType(uInt tp);
// Translate string to reference code
// <group>
  static Bool getType(MEarthMagnetic::Types &tp, const String &in);
  Bool giveMe(MEarthMagnetic::Ref &mr, const String &in);
// This one for historic reasons only
  Bool giveMe(const String &in, MEarthMagnetic::Ref &mr);
// </group>
  // Set the offset in the reference (False if non-matching Measure)
  virtual Bool setOffset(const Measure &in);
  // Set the reference type to the specified String. False if illegal
  // string, reference set to DEFAULT.
  virtual Bool setRefString(const String &in);
  // Get the default reference type
  virtual const String &getDefaultType() const;
  // Get the reference type (for records, including codes like R_)
  virtual String getRefString() const;
  // Get my type (as Register)
  static uInt myType();

// Get Measure data
// <group>
    Quantum<Vector<Double> > get(const Unit &inunit) const;
    Quantum<Vector<Double> > getAngle() const;
    Quantum<Vector<Double> > getAngle(const Unit &inunit) const;
// </group>

// Make copy
  virtual Measure *clone() const;

private:
//# Enumerations

//# Data

//# Member functions

};

#endif
