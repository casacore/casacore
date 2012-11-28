//# FluxCalcQS.cc: Implementation of FluxCalcQS.h
//# Copyright (C) 2010
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
#include <components/ComponentModels/FluxCalcQS.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/String.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>

// Handy for passing anonymous arrays to functions.
#include <scimath/Mathematics/RigidVector.h>

#include <map>


namespace casa { //# NAMESPACE CASA - BEGIN

FluxCalcQS::FluxCalcQS() :
  srcEnum_p(FluxCalcQS::UNKNOWN_SOURCE)
{
  names_p[FCQS::THREEC286] = RVS4("3C286", "1328+307", "1331+305", "J1331+3030").vector();
  directions_p[FCQS::THREEC286] = MDirection(MVDirection(3.539257626070549, 0.5324850225220917),
					     MDirection::J2000);
  names_p[FCQS::THREEC48]  = RVS4("3C48", "0134+329",
                                  "0137+331",              // Together these match 33d09m35s
                                  "J0137+3309").vector();  // for dd.d or ddmm with a margin.
  directions_p[FCQS::THREEC48] = MDirection(MVDirection(0.4262457643630985, 0.5787463318245085),
					    MDirection::J2000);
  names_p[FCQS::THREEC147] = RVS4("3C147", "0538+498", "0542+498", 
                                  "J0542+4951").vector();          // Jhhmm+ddmm, CAS-2020
  directions_p[FCQS::THREEC147] = MDirection(MVDirection(1.4948817765383597, 0.8700805690768509),
					     MDirection::J2000);
  names_p[FCQS::THREEC138] = RVS4("3C138", "0518+165", "0521+166",
                                  "J0521+1638").vector();          // Jhhmm+ddmm, CAS-2020
  directions_p[FCQS::THREEC138] = MDirection(MVDirection(1.401346673041897, 0.2904130912582342),
					     MDirection::J2000);
  names_p[FCQS::NINETEEN34M638] = RigidVector<String, 2>("1934-638","J1939-6342").vector();
  directions_p[FCQS::NINETEEN34M638] = MDirection(MVDirection(5.146176021557448, -1.1119977478136984),
						  MDirection::J2000);
  names_p[FCQS::THREEC295] = RVS4("3C295", "1409+524", "1411+522",
                                  "J1411+5212").vector();          // Jhhmm+ddmm, CAS-2020
  directions_p[FCQS::THREEC295] = MDirection(MVDirection(3.7146787856873478, 0.9111103509091509),
					     MDirection::J2000);
  names_p[FCQS::THREEC196] = RVS4("3C196", "0809+483", "0813+482",
                                  "J0813+4813").vector();          // Jhhmm+ddmm, CAS-2020
  directions_p[FCQS::THREEC196] = MDirection(MVDirection(2.1537362969610023, 0.8415541320803659),
					     MDirection::J2000);
  // added for Perley-Butler 2013 4h 37m 4.375301s 29d 40' 13.819008" CAS-4489 (other alias:B0433+2934)
  names_p[FCQS::THREEC123] = RVS4("3C123", "0433+295", "0437+296",
                                  "J0437+2940").vector();
  directions_p[FCQS::THREEC123] = MDirection(MVDirection(1.2089586878736391, 0.51784800786636209),
					     MDirection::J2000);
  directions_p[FCQS::UNKNOWN_SOURCE] = MDirection();	// Default.
}

// Defined even though it's pure virtual; see http://www.gotw.ca/gotw/031.htm
FluxCalcQS::~FluxCalcQS() { }

Bool FluxCalcQS::operator()(Vector<Flux<Double> >& values,
                            Vector<Flux<Double> >& errors,
                            const Vector<MFrequency>& mfreqs)
{
  uInt nfreqs = mfreqs.nelements();
  
  values.resize(nfreqs);
  errors.resize(nfreqs);

  Bool success = true;
  for(uInt f = 0; f < nfreqs; ++f)
    success &= (*this)(values[f], errors[f], mfreqs[f]);
  return success;
}

FluxCalcQS::Source FluxCalcQS::srcNameToEnum(const String& srcName) const
{
  FCQS::Source srcEnum = FCQS::UNKNOWN_SOURCE;  

  for(std::map<FCQS::Source, Vector<String> >::const_iterator it = names_p.begin();
      it != names_p.end(); ++it){
    for(Int i = it->second.nelements(); i--;){
      if(srcName.contains(it->second[i])){
        srcEnum = it->first;
        break;
      }
    }
    if(srcEnum != FCQS::UNKNOWN_SOURCE)
      break;
  }
  return srcEnum;
}

Bool FluxCalcQS::setSource(const String& sourceName)
{
  srcEnum_p = srcNameToEnum(sourceName);
  return srcEnum_p != FCQS::UNKNOWN_SOURCE;
}

FluxCalcQS::Source FluxCalcQS::getSrcEnum()
{
  return srcEnum_p;
}

} //# NAMESPACE CASA - END
