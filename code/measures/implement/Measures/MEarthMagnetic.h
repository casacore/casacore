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

//# Typedefs

// <summary>
//  A Measure: Magnetic field on Earth
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// MEarthMagnetic forms derived Measure class for Earth' magnetic flux density
// It contains the following magnetic field models:
// <ul>
//   <li> DIPOLE	simple dipole model with fixed dipole towards ...
//   <li> MDIPOLE	moving dipole (.... + .../decade)
//   <li> XDIPOLE	excentric dipole (....)
//   <li> MXDIPOLE	moving excentric dipole (....)
//   <li> IGRF		international reference field
// </ul>
// <note role=warning> Only DIPOLE and XDIPOLE are present at the moment.
// The moving ones need a Frame, which uses Tables, which uses Measures...
// Awaits solution of module dependency problem. The IGRF needs a Table
// of coefficients (at 5-year interval) </note>
//
// Conversion between fields is not supported.
//
// The field at a certain position on Earth is obtained with the () operator.
// </synopsis>
//
// <example>
// <srcblock>
// // Where on Earth
// MVPosition pos(Quantity(20,'m'), Quantity(5,'deg'), Quantity(52,'deg'));
// // Make sure it is in Earth centered coordinates
// MVPosition ipos = MPosition::Convert( MPosition(pos, MPosition::WGS84),
//				         MPosition::ITRF)().getValue();
// // Magnetic field model (DIPOLE)
// MEarthMagnetic mf;
// // Show field strength in Gauss
// cout << mf(ipos).getLength("G") << endl;
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1997/02/19">
//	<li> IGRF model
//	<li> wandering pole model
// </todo>

class MEarthMagnetic : public MeasBase<MVEarthMagnetic, MeasRef<MEarthMagnetic> > 
{
public:
//# Friends
// Conversion of data
//# Enumerations
// Types of known MEarthMagnetics
// <note role=tip> The order defines the order in the translation matrix FromTo
// in the getConvert routine. Do not change the order without
// changing the array. Additions should be made before N_types, and
// an additional row and column should be coded in FromTo, and
// in showType().</note>
    enum Types { DIPOLE,
		 MDIPOLE,
		 XDIPOLE,
		 MXDIPOLE,
		 IGRF, 
		 N_Types,
		 DEFAULT=DIPOLE};
//# Typedefs
// Measure reference
    typedef MeasRef<MEarthMagnetic> Ref;

//# Constructors
// <note> In the following constructors and other functions, all 
// <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
// where no offsets or frames are needed in the reference. For reasons
// of compiler limitations the formal arguments had to be specified as
// <em>uInt</em> rather than the Measure enums that should be used as actual 
// arguments.</note>
// Default constructor; generates the default DIPOLE type
    MEarthMagnetic();
// Create from data and reference
// <group>
    MEarthMagnetic(const MVEarthMagnetic &dt);
    MEarthMagnetic(const MVEarthMagnetic &dt, const MEarthMagnetic::Ref &rf);
    MEarthMagnetic(const MVEarthMagnetic &dt, uInt rf);
// </group>

//# Destructor
    ~MEarthMagnetic();

//# Operators

  // Get the field for a position
MVEarthMagnetic operator()(const MVPosition &pos) const;

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
