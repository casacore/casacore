//# EarthField.h: EarthField class
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

#if !defined(AIPS_EARTHFIELD_H)
#define AIPS_EARTHFIELD_H

//# Includes
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/MVPosition.h>

//# Forward Declarations

//# Constants
// Length of P and Q arrays, half length of CL/SL arrays
const Int PQ_LEN = 65;
// Interval (m) for derivatives
const Double DER_INTV = 10000;

// <summary> EarthField class and calculations </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tEarthField" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class for use
//   <li> <linkto class=MeasTable>MeasTable</linkto> class for data
// </prerequisite>
//
// <etymology>
// Earth magnetic Field
// </etymology>
//
// <synopsis>
// EarthField forms the class for Earth magnetic field calculations. It is a 
// simple container with the selected method, and the mean epoch.<br>
// The method is selected from one of the following:
// <ul>
//   <li> EarthField::STANDARD  (at 1998/05/18 the IGRF definition)
//   <li> EarthField::IGRF	(IGRF reference field)
// </ul>
// Epochs can be specified as the MJD (with defined constants
// MeasData::MJD2000 and MeasData::MJD1950 or the actual MJD),
// leading to the following constructors:
// <ul>
//   <li> EarthField() default; assuming IGRF and MJD2000
//   <li> EarthField(method); assuming J2000 as epoch
//   <li> EarthField(method, epoch) with epoch Double(MJD)
// </ul>
// Actual EarthField for a certain position on Earth is calculated by the () 
// operator. Arguments can be:
// <ul>
//   <li> MVPosition: a position on Earth (ITRF)
// </ul>
// The returned value is a 3D vector of the field (nT) in ITRF coordinates.
// The derivative (d<sup>-1</sup>) can be obtained as well by 
// derivative(MVPosition). <br>
// An EarthField can be re-initialed with a different method and/or other
// epoch with the <src>init()</src> functions (same format as constructors).
//
// To bypass the full, lengthy calculation actual returned values are calculated
// using the derivative if within about 50 km (error less than about
// 10<sup>-2</sup> G). A call to refresh() will re-initiate calculations
// from scratch.<br>
// The following details can be set with the 
// <linkto class=Aipsrc>Aipsrc</linkto> mechanism:
// <ul>
//  <li> measures.earthfield.d_interval: approximation radius
//	(km is default unit) over which linear approximation
//	is used
// </ul>
// The field is assumed to be constant over the time-span the class is used.
//
// The calculations are based on a routine provided by the IGRF community. See
// ftp.ngdc.noaa.gov/Solid_Earth/Mainfld_Mag/Models/IAGA, routine IGRFLIB.FOR.
// The values are in nT (10uG).
// </synopsis>
//
// <example>
//  <srcblock>
//	EarthField mine(EarthField::STANDARD,
//                      45837.0);		// define EarthField type
//						// for 84/05/17
//	MPosition pos;
//      MeasTable::Observatory(pos, "WSRT");	// Obervatory position
//  // Make sure correct data
//      MVPosition x(MPosition::Convert(pos, MPosition::ITRF)().getValue());
//	MVEarthMagnetic now = mine(x);		// get EarthField
//  </srcblock>
// </example>
//
// <motivation>
// To have a history container for field calculations
// </motivation>
//
// <todo asof="1998/05/18">
//   <li> nothing I know off
// </todo>

class EarthField {

public:

  //# Constants
  // Default interval to be used for linear approximation (in m)
  static const Double INTV;

  //# Enumerations
  // Types of known EarthField calculations
  enum EarthFieldTypes {
    IGRF,
    NONE,
    STANDARD = IGRF
  };

  //# Constructors
  // Default constructor, generates default J2000 EarthField identification
  EarthField();
  // Copy constructor
  EarthField(const EarthField &other);
  // Constructor with epoch in MJulian days
  explicit EarthField(EarthFieldTypes type, Double catepoch=51544.5);
  // Copy assignment
  EarthField &operator=(const EarthField &other);
  
  //# Destructor
  ~EarthField();
  
  //# Operators
  // Return the EarthField angles
  const Vector<Double> &operator()(const MVPosition &pos);
  
  //# General Member Functions
  // Return derivatives of field (to X, Y, Z)
  const Vector<Double> *derivative(const MVPosition &pos);
  // Re-initialise EarthField object
  // <group>
  void init();
  void init(EarthFieldTypes type, Double catepoch=51544.5);
  // </group>
  // Refresh calculations
  void refresh();
  
  private:
  //# Data members
  // Method to be used
  EarthFieldTypes method_p;
  // Fixed epoch to be used (MJD)
  Double fixedEpoch_p;
  // List of spherical components
  Vector<Double> agh_p;
  // Work arrays for calculations
  // <group>
  Vector<Double> p_p;
  Vector<Double> q_p;
  Vector<Double> cl_p;
  Vector<Double> sl_p;
  // </group>
  // Check position
  MVPosition checkPos;
  // Cached calculated field components
  Double pval[3];
  // Cached derivatives
  Double dval[3][3];
  // To reference results, and use a few in interim calculations, results are
  // calculated in a circular buffer.
  // Current result pointer
  Int lres;
  // Last calculation
  Vector<Double> result[4];
  // Interpolation interval
  static uInt interval_reg;

  //# Member functions
  // Make a copy
  void copy(const EarthField &other);
  // Create correct default fixedEpoch and catalogue field data
  void fillField();
  // Calculate EarthField for longitude and latitude and altitude (m)
  void calcField(const MVPosition &pos);

};
  
#endif


