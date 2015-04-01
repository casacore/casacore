//# LatticeAddNoise.cc: add noise to a lattice
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
//# $Id$

#include <casacore/lattices/LatticeMath/LatticeAddNoise.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicSL/Complex.h> 
#include <casacore/casa/BasicMath/Random.h> 
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LatticeAddNoise::LatticeAddNoise()
: itsParameters(0),
  itsNoise(0)
{}
   
LatticeAddNoise::LatticeAddNoise (Random::Types type,
                                  const Vector<Double>& parameters)
: itsType(type),
  itsParameters(parameters.copy()),
  itsNoise(0)
{
   makeDistribution();
}
  
LatticeAddNoise::LatticeAddNoise (const LatticeAddNoise& other)
: itsType(other.itsType),
  itsParameters(other.itsParameters.copy()),
  itsGen(other.itsGen),
  itsNoise(0)
{
   makeDistribution();
}
 
LatticeAddNoise& LatticeAddNoise::operator=(const LatticeAddNoise& other)
{
   if (this != &other) {
      itsType = other.itsType;
      itsParameters.resize(0);
      itsParameters = other.itsParameters;
      itsGen = other.itsGen;
//
      makeDistribution();
   }
   return *this;
}
     
LatticeAddNoise::~LatticeAddNoise()
{
   if (itsNoise) {
      delete itsNoise;
      itsNoise = 0;
   }
}
 
void LatticeAddNoise::set (Random::Types type,
                           const Vector<Double>& parameters)
{
   itsType = type;
   itsParameters.resize(0);
   itsParameters = parameters;
   makeDistribution();
}
 
void LatticeAddNoise::add (MaskedLattice<Float>& lattice)
{
   if (!itsNoise) {
      LogIO os(LogOrigin("LatticeAddNoise", "add", WHERE));
      os << "You have not yet called function 'set'" << LogIO::EXCEPTION;
   }
//
   LatticeIterator<Float> it(lattice);
   for (it.reset(); !it.atEnd(); it++) {
      addNoiseToArray(it.rwCursor());
   }
}

void LatticeAddNoise::add (MaskedLattice<Complex>& lattice)
{
   if (!itsNoise) {
      LogIO os(LogOrigin("LatticeAddNoise", "add", WHERE));
      os << "You have not yet called function 'set'" << LogIO::EXCEPTION;
   }
//
   LatticeIterator<Complex> it(lattice);
   for (it.reset(); !it.atEnd(); it++) {
      addNoiseToArray(it.rwCursor());
   }
}

void LatticeAddNoise::add (Lattice<Float>& lattice)
{
   SubLattice<Float> ml(lattice, True);
   add(ml);
}

void LatticeAddNoise::add (Lattice<Complex>& lattice)
{
   SubLattice<Complex> ml(lattice, True);
   add(ml);
}

 

// Private

void LatticeAddNoise::addNoiseToArray (Array<Float>& data) 
{
   Bool deleteIt;
   Float* p = data.getStorage(deleteIt);
   for (uInt i=0; i<data.nelements(); i++) {
      p[i] += (*itsNoise)();
   }
   data.putStorage(p, deleteIt);
}

void LatticeAddNoise::addNoiseToArray (Array<Complex>& data) 
{
   Bool deleteIt;
   Complex* p = data.getStorage(deleteIt);
   Float rr, ii;
   for (uInt i=0; i<data.nelements(); i++) {

// Add noise to real and imag separately

      rr = real(p[i]) + (*itsNoise)();
      ii = imag(p[i]) + (*itsNoise)();
//
      p[i] = Complex(rr,ii);
   }
   data.putStorage(p, deleteIt);
}


void LatticeAddNoise::makeDistribution ()
{
   if (itsNoise) {
      delete itsNoise;
      itsNoise = 0;
   }
//
   itsNoise = Random::construct(itsType, &itsGen);
   if (itsNoise) {
      if (!itsNoise->checkParameters(itsParameters)) {
         delete itsNoise;
         itsNoise = 0;
         LogIO os(LogOrigin("LatticeAddNoise", "makeDistribution", WHERE));
         os << "The distribution parameters are illegal" << LogIO::EXCEPTION;
      } else {
         itsNoise->setParameters(itsParameters);
      }
   }
}

} //# NAMESPACE CASACORE - END

