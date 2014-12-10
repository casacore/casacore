//# Stokes.h: Stokes parameter definitions for interface to table data
//# Copyright (C) 1994,1995,1996,1997,2000
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
//# $Id$

#ifndef MEASURES_STOKES_H
#define MEASURES_STOKES_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Fallible.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Stokes parameter definitions for interface to table data.
// </summary>

// <reviewed tests="tStokes">
// </reviewed>

// <synopsis>
// This enumerates the available Stokes types, but does not define
// the operations for conversion between Stokes types.
// This class is a wrapper for the ENUM and conversion functions.
// </synopsis>

class Stokes {
public:

// The Stokes types are defined by this enum.
//
// <note role=warning>
// <b>DO NOT CHANGE THE ORDER OF THESE TYPES</b>, as the integers corresponding
// to the <src>enum</src> are required for storage in Tables.
// One can add to these types, but the order must be preserved.
// The correlation products are required to have the order indicated with
// values of 1,2,3,4 plus n*4.
// </note>
//
//# The enum comments below are placed in the position they are to make the
//# extracted documentation look nice.
	enum StokesTypes { 
	    // undefined value = 0
	    Undefined=0, 
	    I,  
	    Q,  
	    U,  
	    // standard stokes parameters 
	    V,
	    //
	    RR, 
	    RL, 
	    LR, 
	    // circular correlation products
	    LL,
	    //
	    XX, 
	    XY, 
	    YX, 
	    // linear correlation products
	    YY,
	    //
	    RX, 
	    RY, 
	    LX, 
	    LY,
	    XR, 
	    XL, 
	    YR, 
	    // mixed correlation products
	    YL,
	    //
	    PP, 
	    PQ, 
	    QP, 
	    // general quasi-orthogonal correlation products
	    QQ, 
	    //
	    RCircular, 
	    LCircular, 
	    // single dish polarization types
	    Linear,
	    // Polarized intensity ((Q^2+U^2+V^2)^(1/2))
	    Ptotal,
	    // Linearly Polarized intensity ((Q^2+U^2)^(1/2))
	    Plinear,
	    // Polarization Fraction (Ptotal/I)
	    PFtotal,
	    // Linear Polarization Fraction (Plinear/I)
	    PFlinear,
	    // Linear Polarization Angle (0.5 arctan(U/Q)) (in radians)
	    Pangle
	};

//      The number of StokesTypes.
// <note role=warning>
// <b>Update</b> <src>NumberOfTypes</src> when entries are added.
// </note>
	enum {
	    // The number of StokesTypes.
	    NumberOfTypes = 33
	    };
	
//		convert Int to StokesTypes, returns Stokes::Undefined if 
//		it is an invalid type
	static StokesTypes type(Int stokesNumber);

//		convert String to StokesTypes, returns Stokes::Undefined if
//		it is an unrecognized string.  The valid strings are the
//		same as the characters used in the enum above (i.e.
//		"I" returns Stokes::I, "Linear" returns Stokes::Linear, etc).
        static StokesTypes type(const String & stokesName);

//		convert StokesTypes to String, Stokes::Undefined returns
//		"??".
	static String name(StokesTypes stokesType);

	// get all recognized stokes names in no guaranteed order.
	static Vector<String> allNames(Bool includeUndefined = False);

//              map StokesTypes to receptor number (0 or 1) for the
//              interferometric correlation products.
//              e.g. XY will give receptor1==0 receptor2==1 etc.
//              I,Q,U,V and the single dish types will produce invalid
//              Fallible.
//              <group>
	static Fallible<Int> receptor1(StokesTypes stokesType);
	static Fallible<Int> receptor2(StokesTypes stokesType);
//              </group>

    // These two functions map stokes type to FITS type and vice versa. If you add a 
    // StokesType you should change these functions as well.
    // <ul>
    //     <li> I,Q,U,V <-> 1,2,3,4
    //     <li> RR,LL,RL,LR <-> -1,-2,-3,-4   Note! Not the same as enum order!
    //     <li> XX,YY,XY,YX <-> -5,-6,-7,-8   Note! Not the same as enum order!
    //     <li> Otherwise, FITS type <-> 100 + Int(stokesType). This is not standard FITS.
    // </ul>
    // <group>
        static Int    FITSValue(StokesTypes which);
        static StokesTypes fromFITSValue(Int);
    // </group>
private:
};


} //# NAMESPACE CASACORE - END

#endif

