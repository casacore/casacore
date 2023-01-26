//# QC.h: physical constants with units
//# Copyright (C) 1994,1995,1996,1997,1998,1999
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

#ifndef CASA_QC_H
#define CASA_QC_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Quanta/Quantum.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

//# Typedefs

// <summary>
// Physical constants (i.e. dimensioned values)
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tQuantum">

// <prerequisite>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>

// <etymology>
// A QC is based on the Quantum and C (constants) class
// </etymology>

// <synopsis> 
// QC:name will produce a Quantity (Quantum&lt;double&gt;) value consisting of
// a value and a unit. See the <linkto class=Quantum>Quantum</linkto> class
//  for possibilities of manipulating quanta.
// tQuantum will give a list of the currently available constants
// </synopsis> 

// <example>
// To obtain the velocity of light in pc/a, use:
// <srcblock>
// #include <casacore/casa/Quanta.h>
// double vel_pcpy = (C::c).convert("pc/a").getValue();
// </srcblock>
// </example>

//############################################################################
//# NOTE:  Delete the following listing of the public data members when
//#        public data members are handled properly by the documentation
//#        extractor.
//############################################################################

// The following constants are defined as public data members of class QC.
//
// <note role=caution>
// The following public data member documentation is currently extracted by
// hand, and thus could be out of date if this documentation was not updated
// when the class was modified.
// </note>

// <srcblock>
//
//    // vel of light
//    Quantum<double> c( );
//
//    // Gravitational constant
//    Quantum<double> G( );
//
//    // Planck
//    Quantum<double> h( );
//
//    // HI line
//    Quantum<double> HI( );
//
//    // Gas constant
//    Quantum<double> R( );
//
//    // Avogadro
//    Quantum<double> NA( );
//
//    // electron charge
//    Quantum<double> e( );
//
//    // proton mass
//    Quantum<double> mp( );
//
//    // mp/me
//    Quantum<double> mp_me( );
//
//    // permeability vacuum
//    Quantum<double> mu0( );
//
//    // permittivity vacuum
//    Quantum<double> epsilon0( );
//
//    // Boltzmann
//    Quantum<double> k( );
//
//    // Faraday
//    Quantum<double> F( );
//
//    // mass electron
//    Quantum<double> me( );
//
//    // radius electron
//    Quantum<double> re( );
//
//    // Bohr's radius
//    Quantum<double> a0( );
//
//    // Solar radius
//    Quantum<double> R0( );
//
//    // IAU Gaussian grav. const **2
//    Quantum<double> k2( );
//
//    // quarter turn = 90 degrees = pi/2 radians
//    Quantum<double> qTurn( );
//
//    // half turn = 180 degrees = pi radians
//    Quantum<double> hTurn( );
//
//    // full turn = 360 degrees = 2pi radians
//    Quantum<double> fTurn( );
//
// </srcblock>

// <motivation>
// Physical constants should be known with their proper dimensions
// </motivation>
//
// <todo asof="941110">
// </todo>

class QC {
friend class QC_init;
public:

//# If you change any of the public data members, make the corresponding
//# change above to the documentation of the public data members.

    // vel of light
    inline static const Quantum<double> &c( ) {
        static Quantum<double> result(C::c,"m/s");
        return result;
    }

    // Gravitational constant
    inline static const Quantum<double> &G( ) {
        static Quantum<double> result(6.67259e-11,"N.m2/kg2");
        return result;
    }

    // Planck
    inline static const Quantum<double> &h( ) {
        static Quantum<double> result(6.6260755e-34,"J.s");
        return result;
    }

    // HI line
    inline static const Quantum<double> &HI( ) {
        static Quantum<double> result(1420.405751786, "MHz");
        return result;
    }

    // Gas constant
    inline static Quantum<double> &R( ) {
        static Quantum<double> result(8.314510,"J/K/mol");
        return result;
    }

    // Avogadro
    inline static const Quantum<double> &NA( ) {
        static Quantum<double> result(6.0221367e+23,"mol-1");
        return result;
    }

    // electron charge
    inline static const Quantum<double> &e( ) {
        static Quantum<double> result(1.60217733e-19,"C");
        return result;
    }

    // proton mass
    inline static const Quantum<double> &mp( ) {
        static Quantum<double> result(1.6726231e-27,"kg");
        return result;
    }

    // mp/me
    inline static const Quantum<double> &mp_me( ) {
        static Quantum<double> result(1836.152701,"");
        return result;
    }

    // permeability vacuum
    inline static const Quantum<double> &mu0( ) {
        static Quantum<double> result(4.0e-7*C::pi,"H/m");
        return result;
    }

    // permittivity vacuum
    inline static const Quantum<double> &epsilon0( ) {
        static Quantum<double> result(1.0/(4.0e-7*C::pi*C::c*C::c),"F/m");
        return result;
    }

    // Boltzmann
    inline static const Quantum<double> &k( ) {
        static Quantum<double> result(8.314510/6.0221367e+23,"J/K");
        return result;
    }

    // Faraday
    inline static const Quantum<double> &F( ) {
        static Quantum<double> result(6.0221367e+23*1.60217733e-19,"C/mol");
        return result;
    }

    // mass electron
    inline static const Quantum<double> &me( ) {
        static Quantum<double> result(1.6726231e-27/1836.152701,"kg");
        return result;
    }

    // radius electron
    inline static const Quantum<double> &re( ) {
        static Quantum<double> result(2.8179e-15,"m");
        return result;
    }

    // Bohr's radius
    inline static const Quantum<double> &a0( ) {
        static Quantum<double> result(5.2918e-11,"m");
        return result;
    }

    // Solar radius
    inline static const Quantum<double> &R0( ) {
        static Quantum<double> result(6.9599e+08,"m");
        return result;
    }
    

    // IAU Gaussian grav. const **2
    inline static const Quantum<double> &k2( ) {
        const double IAU_k=0.01720209895;
        static Quantum<double> result(IAU_k*IAU_k,"AU3/d2/S0");
        return result;
    }

    // quarter turn = 90 degrees = pi/2 radians
    inline static const Quantum<double> &qTurn( ) {
        static Quantum<double> result(90.0, "deg");
        return result;
    }

    // half turn = 180 degrees = pi radians
    inline static const Quantum<double> &hTurn( ) {
        static Quantum<double> result(180.0, "deg");
        return result;
    }
    
    // full turn = 360 degrees = 2pi radians
    inline static const Quantum<double> &fTurn( ) {
        static Quantum<double> result(360.0, "deg");
        return result;
    }

};

} //# NAMESPACE CASACORE - END

#endif
