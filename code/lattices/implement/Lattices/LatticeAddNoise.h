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

#if !defined(AIPS_LATTICEADDNOISE_H)
#define AIPS_LATTICEADDNOISE_H


//# Includes
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Random.h>


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
//    LatticeAddNoise lan(LatticeAddNoise::NORMAL, pars);
//    ArrayLattice<Float> lat(IPosition(2,100,100));
//    lan.add(lat);
// </srcblock>
// </example>


//# <todo asof="yyyy/mm/dd">
//# </todo>

class LatticeAddNoise
{
public:

enum Types {

// 2 parameters. The binomial distribution models successfully drawing items from a pool. 
// Specify n and p. n is the number of items in the pool, 
// and p, is the probability of each item being successfully drawn.
// It is required that n > 0 and 0 <= p <= 1 
   BINOMIAL,

// 2 parameters. Model a uniform random variable over the closed interval. Specify the
// values low and high. The low parameter is the lowest possible return value and 
// the high parameter is the highest. 
// It is required that low < high.
   DISCRETEUNIFORM,

// 2 parameters, mean and variance.  
// It is required that the mean is non-zero and the variance is positive.
   ERLANG, 

// 1 parameters, the mean.
// It is required that 0 <= mean < 1
   GEOMETRIC, 

// 2 parameters, mean and variance.
// It is required that the variance is positive and that the mean is non-zero 
// and not bigger than the square-root of the variance. 
   HYPERGEOMETRIC,

// 2 parameters, the mean and variance. 
// It is required that the variance is positive.
   NORMAL, 

// 2 parameters, mean and variance.
// It is required that the supplied variance is positive and that the mean is non-zero
   LOGNORMAL,

// 1 parameter, the mean.
   NEGATIVEEXPONENTIAL,

// 1 parameter, the mean. It is required that the mean is non-negative
   POISSON, 

// 2 parameters, low and high.  Model a uniform random variable over the closed 
// interval. The low parameter is the lowest possible return value and 
// the high parameter can never be returned.
// It is required that low < high.
   UNIFORM,

// 2 parameters, alpha and beta. 
// It is required that the alpha parameter is not zero.
   WEIBULL,

// Number of distributions
   NTYPES};

// Default constructor
   LatticeAddNoise();

// Constructor
   LatticeAddNoise (LatticeAddNoise::Types type,
                    const Vector<Double>& parameters);

// Copy constructor (copy semantics)
   LatticeAddNoise (const LatticeAddNoise& other);

// Assignment (copy semantics)
   LatticeAddNoise& operator=(const LatticeAddNoise& other);

// Destructor
   ~LatticeAddNoise();

// Set distribution
   void set (LatticeAddNoise::Types type,
             const Vector<Double>& parameters);

// Add noise of given type to lattice.  For Complex, the
// noise is added to real and imaginary separately.
// <group>
   void add (MaskedLattice<Float>& lattice);
   void add (MaskedLattice<Complex>& lattice);
   void add (Lattice<Float>& lattice);
   void add (Lattice<Complex>& lattice);
// </group>

// Convert enum to string
   static String typeToString (LatticeAddNoise::Types type);

// Convert string to enum
   static LatticeAddNoise::Types stringToType (const String& type);

// A vector of parameters that couyld be used for the give
// distribution type
   static Vector<Double> defaultParameters (const LatticeAddNoise::Types type);

private:

   LatticeAddNoise::Types itsType;
   Vector<Double> itsParameters;
   MLCG itsGen;

// Add noise to array
// <group>
   void addNoiseToArray (Array<Float>& data, Random& noiseGen) const;
   void addNoiseToArray (Array<Complex>& data, Random& noiseGen) const;
// </group>

// Check parameters ok
   void checkPars (LatticeAddNoise::Types type,
                   const Vector<Double>& pars) const;

// Make noise generator
   Random* makeDistribution (LatticeAddNoise::Types type,
                             const Vector<Double>& pars);
};

#endif
