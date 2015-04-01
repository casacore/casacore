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
//#
//# $Id$

#ifndef CASA_QC_H
#define CASA_QC_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/UnitMap.h>

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
// QC:name will produce a Quantity (Quantum&lt;Double&gt;) value consisting of
// a value and a unit. See the <linkto class=Quantum>Quantum</linkto> class
//  for possibilities of manipulating quanta.
// tQuantum will give a list of the currently available constants
// </synopsis> 

// <example>
// To obtain the velocity of light in pc/a, use:
// <srcblock>
// #include <casacore/casa/Quanta.h>
// Double vel_pcpy = (C::c).convert("pc/a").getValue();
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
//    static Quantum<Double> c;
//
//    // Gravitational constant
//    static Quantum<Double> G;
//
//    // Planck
//    static Quantum<Double> h;
//
//    // HI line
//    static Quantum<Double> HI;
//
//    // Gas constant
//    static Quantum<Double> R;
//
//    // Avogadro
//    static Quantum<Double> NA;
//
//    // electron charge
//    static Quantum<Double> e;
//
//    // proton mass
//    static Quantum<Double> mp;
//
//    // mp/me
//    static Quantum<Double> mp_me;
//
//    // permeability vacuum
//    static Quantum<Double> mu0;
//
//    // permittivity vacuum
//    static Quantum<Double> epsilon0;
//
//    // Boltzmann
//    static Quantum<Double> k;
//
//    // Faraday
//    static Quantum<Double> F;
//
//    // mass electron
//    static Quantum<Double> me;
//
//    // radius electron
//    static Quantum<Double> re;
//
//    // Bohr's radius
//    static Quantum<Double> a0;
//
//    // Solar radius
//    static Quantum<Double> R0;
//
//    // IAU Gaussian grav. const **2
//    static Quantum<Double> k2;
//
//    // quarter turn = 90 degrees = pi/2 radians
//    static Quantum<Double> qTurn;
//
//    // half turn = 180 degrees = pi radians
//    static Quantum<Double> hTurn;
//
//    // full turn = 360 degrees = 2pi radians
//    static Quantum<Double> fTurn;
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
    static Quantum<Double> c;

    // Gravitational constant
    static Quantum<Double> G;

    // Planck
    static Quantum<Double> h;

    // HI line
    static Quantum<Double> HI;

    // Gas constant
    static Quantum<Double> R;

    // Avogadro
    static Quantum<Double> NA;

    // electron charge
    static Quantum<Double> e;

    // proton mass
    static Quantum<Double> mp;

    // mp/me
    static Quantum<Double> mp_me;

    // permeability vacuum
    static Quantum<Double> mu0;

    // permittivity vacuum
    static Quantum<Double> epsilon0;

    // Boltzmann
    static Quantum<Double> k;

    // Faraday
    static Quantum<Double> F;

    // mass electron
    static Quantum<Double> me;

    // radius electron
    static Quantum<Double> re;

    // Bohr's radius
    static Quantum<Double> a0;

    // Solar radius
    static Quantum<Double> R0;

    // IAU Gaussian grav. const **2
    static Quantum<Double> k2;

    // quarter turn = 90 degrees = pi/2 radians
    static Quantum<Double> qTurn;

    // half turn = 180 degrees = pi radians
    static Quantum<Double> hTurn;

    // full turn = 360 degrees = 2pi radians
    static Quantum<Double> fTurn;

private:
// This function is used, in conjunction with the
// <linkto class=QC_init>QC_init</linkto>
// class to force construction of statics (see ARM 3.4).
    static void init();
};


// <summary>
// Class used to force construction of <linkto class=QC>QC</linkto>.
// </summary>

// <synopsis>
// A static object of this class is used to make sure that
// <linkto class=QC>QC</linkto>
// is constructed before it is needed, and therefore that its static data
// members are defined.  See Meyers, p. 47.
// </synopsis>

// <use visibility=local>

// <linkfrom anchor="QC_init" classes="QC">
//   <here>QC_init</here> --
// Class used to force construction of <linkto class=QC>QC</linkto>.
// </linkfrom>

class QC_init {
  public:
    QC_init();
    ~QC_init();
  private:
    static uShort count;
};

// <summary>
// Object used to force construction of <linkto class=QC>QC</linkto>.
// </summary>

// <synopsis>
// This static object of the <linkto class=QC_init>QC_init</linkto>
// class is used to make sure that
// <linkto class=QC>QC</linkto>
// is constructed before it is needed, and therefore that its static data
// members are defined.  See Meyers, p. 47.
// </synopsis>

// <use visibility=local>

// <linkfrom anchor="QC initialization object" classes="QC QC_init">
//   <here>QC initialization object</here> --
// Object used to force construction of <linkto class=QC>QC</linkto>.
// </linkfrom>

// <group name="QC initialization object">

static QC_init qc_init;

// </group>

//# Inline Implementations


} //# NAMESPACE CASACORE - END

#endif
