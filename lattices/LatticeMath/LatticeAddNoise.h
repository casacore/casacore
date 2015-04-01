//# LatticeAddNoise.h: add noise to a Lattice
//# Copyright (C) 1997,1998,1999,2000,2001
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

#ifndef LATTICES_LATTICEADDNOISE_H
#define LATTICES_LATTICEADDNOISE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Random.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

template <class T> class MaskedLattice;
template <class T> class Lattice;


// <summary>
// Add noise from specified distribution to a lattice
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="Random">Random</linkto>
// </prerequisite>

// <synopsis>
// This class allows you to add noise from one of many enumerated
// types to a Lattice.  If the Lattice is Complex, then the noise
// is added to real and imaginary separately.
// </synopsis>

// <example>
// <srcblock>
//    Vector<Double> pars(2):
//    pars(0) = 0.5;          // Mean
//    pars(1) = 0.2;          // Variance
//    LatticeAddNoise lan(Random::NORMAL, pars);
//    ArrayLattice<Float> lat(IPosition(2,100,100));
//    lan.add(lat);
// </srcblock>
// </example>


//# <todo asof="yyyy/mm/dd">
//# </todo>

class LatticeAddNoise
{
public:
// Default constructor
   LatticeAddNoise();

// Constructor. An exception will occur if we cannot generate 
// the distribution (e.g. illegal parameters).  
   LatticeAddNoise (Random::Types type,
                    const Vector<Double>& parameters);

// Copy constructor (copy semantics)
   LatticeAddNoise (const LatticeAddNoise& other);

// Assignment (copy semantics)
   LatticeAddNoise& operator=(const LatticeAddNoise& other);

// Destructor
   ~LatticeAddNoise();

// Set a new distribution.  An exception will occur if we cannot generate 
// the distribution (e.g. illegal parameters).  
   void set (Random::Types type,
             const Vector<Double>& parameters);

// Add noise of given type to lattice.  For Complex, the
// noise is added to real and imaginary separately.
// Any mask is ignored when adding the noise. I.e.
// noise is added to masked pixels.
// <group>
   void add (MaskedLattice<Float>& lattice);
   void add (MaskedLattice<Complex>& lattice);
   void add (Lattice<Float>& lattice);
   void add (Lattice<Complex>& lattice);
// </group>

private:

   Random::Types itsType;
   Vector<Double> itsParameters;
   MLCG itsGen;
   Random* itsNoise;

// Add noise to array.  For Complex, noise is added to
// real and imaginary separately.
// <group>
   void addNoiseToArray (Array<Float>& data);
   void addNoiseToArray (Array<Complex>& data);
// </group>

// Make noise generator
   void makeDistribution ();
};


} //# NAMESPACE CASACORE - END

#endif
