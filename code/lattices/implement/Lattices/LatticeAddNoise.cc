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

#include <trial/Lattices/LatticeAddNoise.h>

#include <aips/Arrays/Array.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/LatticeIterator.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Complex.h> 
#include <aips/Mathematics/Random.h> 
#include <aips/Utilities/String.h>


LatticeAddNoise::LatticeAddNoise()
: itsParameters(0)
{}
   
LatticeAddNoise::LatticeAddNoise (LatticeAddNoise::Types type,
                                  const Vector<Double>& parameters)
: itsType(type),
  itsParameters(parameters.copy())
{
   checkPars(itsType, itsParameters);
}
  
LatticeAddNoise::LatticeAddNoise (const LatticeAddNoise& other)
: itsType(other.itsType),
  itsParameters(other.itsParameters.copy()),
  itsGen(other.itsGen)
{}
 
LatticeAddNoise& LatticeAddNoise::operator=(const LatticeAddNoise& other)
{
   if (this != &other) {
      itsType = other.itsType;
      itsParameters.resize(0);
      itsParameters = other.itsParameters;
      itsGen = other.itsGen;
   }
   return *this;
}
     
LatticeAddNoise::~LatticeAddNoise()
{}
 
void LatticeAddNoise::set (LatticeAddNoise::Types type,
                           const Vector<Double>& parameters)
{
   itsType = type;
   itsParameters.resize(0);
   itsParameters = parameters;
//
   checkPars(itsType, itsParameters);
}
 
void LatticeAddNoise::add (MaskedLattice<Float>& lattice)
{
   Random* pNoise = makeDistribution(itsType, itsParameters);
//
   LatticeIterator<Float> it(lattice);
   for (it.reset(); !it.atEnd(); it++) {
      addNoiseToArray(it.rwCursor(), *pNoise);
   }
//
   delete pNoise;
}

void LatticeAddNoise::add (MaskedLattice<Complex>& lattice)
{
   Random* pNoise = makeDistribution(itsType, itsParameters);
//
   LatticeIterator<Complex> it(lattice);
   for (it.reset(); !it.atEnd(); it++) {
      addNoiseToArray(it.rwCursor(), *pNoise);
   }
//
   delete pNoise;
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

 
String LatticeAddNoise::typeToString (LatticeAddNoise::Types type)
{
   String typeOut;
   switch (type) {
   case BINOMIAL:
      typeOut = String("BINOMIAL");
   break;
   case DISCRETEUNIFORM:
      typeOut = String("DISCRETEUNIFORM");
   break;
   case ERLANG:
      typeOut = String("ERLANG");
   break;
   case GEOMETRIC:
      typeOut = String("GEOMETRIC");
   break;
   case HYPERGEOMETRIC:
      typeOut = String("HYPERGEOMETRIC");
   break;
   case NORMAL:
      typeOut = String("NORMAL");
   break;
   case LOGNORMAL:
      typeOut = String("LOGNORMAL");
   break;
   case NEGATIVEEXPONENTIAL:
      typeOut = String("NEGATIVEEXPONENTIAL");
   break;
   case POISSON:
      typeOut = String("POISSON");
   break;
   case UNIFORM:
      typeOut = String("UNIFORM");
   break;
   case WEIBULL:
      typeOut = String("WEIBULL");
   break;
   case NTYPES: 
      throw(AipsError("NTYPES has no string equivalent"));
   break;
   }
//
   return typeOut;
}
 
LatticeAddNoise::Types LatticeAddNoise::stringToType (const String& type)
{
   String typeUp(type);
   typeUp.upcase();
//
// Funny if statement structure gives me minimum match for the
// user given string
      
   String typeDist("BINOMIAL");
   if (typeDist.matches(typeUp)) {
      return BINOMIAL;
   }
//
   typeDist = String("DISCRETEUNIFORM");
   if (typeDist.matches(typeUp)) {
      return DISCRETEUNIFORM;
   }
//
   typeDist = String("ERLANG");
   if (typeDist.matches(typeUp)) {
      return ERLANG;
   }
//
   typeDist = String("GEOMETRIC");
   if (typeDist.matches(typeUp)) {
      return GEOMETRIC;
   }
//
   typeDist = String("HYPERGEOMETRIC");
   if (typeDist.matches(typeUp)) {
      return HYPERGEOMETRIC;
   }
//
   typeDist = String("NORMAL");
   if (typeDist.matches(typeUp)) {
      return NORMAL;
   }
//  
   typeDist = String("LOGNORMAL");
   if (typeDist.matches(typeUp)) {
      return LOGNORMAL;
   }
//
   typeDist = String("NEGATIVEEXPONENTIAL");
   if (typeDist.matches(typeUp)) {
      return NEGATIVEEXPONENTIAL;
   }
//
   typeDist = String("POISSON");
   if (typeDist.matches(typeUp)) {
      return POISSON;
   }
//
   typeDist = String("UNIFORM");
   if (typeDist.matches(typeUp)) {
      return UNIFORM;
   }
//
   typeDist = String("WEIBULL");   
   if (typeDist.matches(typeUp)) {
      return WEIBULL;
   }
// 
   LogIO os(LogOrigin("LatticeAddNoise", "stringToType", WHERE));
   os << "Unrecognized distribution type " << typeUp  << LogIO::EXCEPTION;   
//
   LatticeAddNoise::Types x(NORMAL);
   return x;
}

// Private

void LatticeAddNoise::addNoiseToArray (Array<Float>& data, Random& noiseGen) const
{
   Bool deleteIt;
   Float* p = data.getStorage(deleteIt);
   for (uInt i=0; i<data.nelements(); i++) {
      p[i] += noiseGen();
   }
   data.putStorage(p, deleteIt);
}

void LatticeAddNoise::addNoiseToArray (Array<Complex>& data, Random& noiseGen) const
{
   Bool deleteIt;
   Complex* p = data.getStorage(deleteIt);
   Float rr, ii;
   for (uInt i=0; i<data.nelements(); i++) {

// Add noise to real and imag separately

      rr = real(p[i]) + noiseGen();
      ii = imag(p[i]) + noiseGen();
//
      p[i] = Complex(rr,ii);
   }
   data.putStorage(p, deleteIt);
}


void LatticeAddNoise::checkPars (LatticeAddNoise::Types type,
                                 const Vector<Double>& pars) const
{
   LogIO os(LogOrigin("LatticeAddNoise", "checkNoise", WHERE));
//
   const uInt n0 = defaultParameters(type).nelements();
   if (pars.nelements() != n0) {
     os << "For the " << typeToString(type) << " distribution you must give " 
        << n0 << " parameters" << LogIO::EXCEPTION;
   }
//
   if (type==BINOMIAL) {
      if (pars(0) <= 0.0) {
        os << "For the binomial distribution, it is required pars(0) > 0" << LogIO::EXCEPTION;
      }
      if (pars(1)<0.0 || pars(1)>1) {
        os << "For the binomial distribution, it is required 0 <= pars(1) <=1" << LogIO::EXCEPTION;
      }
   } else if (type==DISCRETEUNIFORM) {
      if (pars(1) <= pars(0)) {
        os << "For the discreteuniform distribution, it is required pars(1) > pars(0)" << LogIO::EXCEPTION;
      }
   } else if (type==ERLANG) {
      if (pars(0)==0.0) {
         os << "For the erlang distribution, it is required pars(0) is non-zero" << LogIO::EXCEPTION;
      }
      if (pars(1)<=0) {
         os << "For the erlang distribution, it is required pars(1) > 0" << LogIO::EXCEPTION;
      }
   } else if (type==GEOMETRIC) {
      if (pars(0)<0 || pars(0)>=1) {
         os << "For the geometric distribution, it is required 0 <= pars(0) < 1" << LogIO::EXCEPTION;
      }
   } else if (type==HYPERGEOMETRIC) {
      if (pars(0)==0.0) {
         os << "For the hypergeometric distribution, it is required pars(0) is non-zero" << LogIO::EXCEPTION;
      }
      if (pars(1)<0) {
         os << "For the hypergeometric distribution, it is required pars(1) > 0" << LogIO::EXCEPTION;
      }
      if (pars(0) > sqrt(pars(1))) {
         os << "For the hypergeometric distribution, it is required pars(0) < sqrt(pars(1))" << LogIO::EXCEPTION;
      }
   } else if (type==NORMAL) {
      if (pars(1)<=0) {
         os << "For the normal distribution, it is required pars(1) > 0" << LogIO::EXCEPTION;
      }
   } else if (type==LOGNORMAL) {
      if (pars(0)==0.0) {
         os << "For the lognormal distribution, it is required pars(0) is non-zero" << LogIO::EXCEPTION;
      }
      if (pars(1)<0) {
         os << "For the lognormal distribution, it is required pars(1) > 0" << LogIO::EXCEPTION;
      }
   } else if (type==POISSON) {
      if (pars(0)<0) {
         os << "For the poission distribution, it is required pars(0) >= 0" << LogIO::EXCEPTION;
      }
   } else if (type==UNIFORM) {
      if (pars(1) <= pars(0)) {
        os << "For the uniform distribution, it is required pars(1) > pars(0)" << LogIO::EXCEPTION;
      }
   } else if (type==WEIBULL) {
      if (pars(0)==0.0) {
         os << "For the weibull distribution, it is required pars(0) is non-zero" << LogIO::EXCEPTION;
      }
   }
}   


Random* LatticeAddNoise::makeDistribution (LatticeAddNoise::Types type,
                                           const Vector<Double>& pars) 
{
   Random* pNoise = 0;
//
   switch (type) {
   case BINOMIAL:
      pNoise = new Binomial(&itsGen, Int(floor(pars(0))), pars(1));
   break;
   case DISCRETEUNIFORM:
      pNoise = new DiscreteUniform(&itsGen, Int(floor(pars(0))), Int(floor(pars(1))));
   break;
   case ERLANG:
      pNoise = new Erlang (&itsGen, pars(0), pars(1));
   break;
   case GEOMETRIC:
      pNoise = new Geometric (&itsGen, pars(0));
   break;
   case HYPERGEOMETRIC:
      pNoise = new HyperGeometric (&itsGen, pars(0), pars(1));
   break;
   case NORMAL:
      pNoise = new Normal (&itsGen, pars(0), pars(1));
   break;
   case LOGNORMAL:
      pNoise = new LogNormal (&itsGen, pars(0), pars(1));
   break;
   case NEGATIVEEXPONENTIAL:
      pNoise = new NegativeExpntl (&itsGen, pars(0));
   break;
   case POISSON:
      pNoise = new Poisson (&itsGen, pars(0));
   break;
   case UNIFORM:
      pNoise = new Uniform (&itsGen, pars(0), pars(1));
   break;
   case WEIBULL:
      pNoise = new Weibull (&itsGen, pars(0), pars(1));
   break;
   case NTYPES:   // shut compiler up
   break;
   }
//
   return pNoise;
}



Vector<Double> LatticeAddNoise::defaultParameters (LatticeAddNoise::Types type)
{
   LogIO os(LogOrigin("LatticeAddNoise", "checkNoise", WHERE));
   Vector<Double> pars(2);
//
   switch (type) {
   case BINOMIAL:
      {
         pars(0) = 1; pars(1) = 0.5;
      }
   break;
   case DISCRETEUNIFORM:
      {
         pars(0) = -100; pars(1) = 100;
      }
   break;
   case ERLANG:
      {
         pars(0) = 0.5; pars(1) = 1.0;
      }
   break;
   case GEOMETRIC:
      {
         pars.resize(1);
         pars(0) = 0.5;
      }
   break;
   case HYPERGEOMETRIC:
      {
         pars(0) = 0.5; pars(1) = 1.0;
      }
   break;
   case NORMAL:
      {
         pars(0) = 0.5; pars(1) = 1.0;
      }
   break;
   case LOGNORMAL:
      {
         pars(0) = 1.0; pars(1) = 1.0;
      }
   break;
   case NEGATIVEEXPONENTIAL:
      {
         pars.resize(1);
         pars(0) = 1.0;
      }
   break;
   case POISSON:
      {
         pars.resize(1);
         pars(0) = 0.5;
      }
   break;
   case UNIFORM:
      {
         pars(0) = -10.0; pars(1) = 10.0;
      }
   break;
   case WEIBULL:
      {
         pars(0) = 0.5; pars(1) = 1;
      }
   break;
   default:
      os << "Unrecognized distribution" << LogIO::EXCEPTION;
   }
//
   return pars;
}

