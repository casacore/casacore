//# LatticeStatsBase.cc: base class for LatticeStatistics.cc
//# Copyright (C) 1996,1999,2000,2001
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

#include <casacore/lattices/LatticeMath/LatticeStatsBase.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/IO/ArrayIO.h>

#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

Vector<int32_t> LatticeStatsBase::toStatisticTypes (const String& statsU, 
                                                const std::regex& delimiter)
{ 
   Vector<String> statsStrings = stringToVector(statsU, delimiter);
   return LatticeStatsBase::toStatisticTypes(statsStrings);
}

Vector<int32_t> LatticeStatsBase::toStatisticTypes (const Vector<String>& statsU)
{ 
   const uint32_t n = statsU.nelements();
   Vector<int32_t> statsToPlot(n);
   int32_t n2 = 0;
   for (uint32_t i=0; i<n; i++) {
      int32_t tmp = LatticeStatsBase::toStatisticType(statsU(i));
      if (tmp!=-1) {
         statsToPlot(n2) = tmp;
         n2++;
      }
   }
   statsToPlot.resize(n2, true);
   return statsToPlot;
}

int32_t LatticeStatsBase::toStatisticType (const String& statU)
{ 
   String stat = statU;
   stat.upcase();
   int32_t statToPlot = -1;
   if (stat.contains("NPTS")) {
      statToPlot = NPTS;
   } else if (stat.contains("SUMSQ")) {
      statToPlot = SUMSQ;
   } else if (stat.contains("SUM")) {  
      statToPlot = SUM;
   } else if (stat.contains("MEAN")) {
      statToPlot = MEAN;
   } else if (stat.contains("VAR")) {
      statToPlot = VARIANCE;
   } else if (stat.contains("SIG") ||
              stat.contains("STD")) {
      statToPlot = SIGMA;
   } else if (stat.contains("RMS")) {
      statToPlot = RMS;
   } else if (stat.contains("MIN")) {
      statToPlot = MIN;
   } else if (stat.contains("MAX")) {
      statToPlot = MAX;
   } else if (stat.contains("FLUX")) {
      statToPlot = FLUX;
   } else if (stat.contains("MEDABS")) {
      statToPlot = MEDABSDEVMED;
   } else if (stat.contains("MED")) {
      statToPlot = MEDIAN;
   } else if (stat.contains("QU")) {
      statToPlot = QUARTILE;
   }
   return statToPlot;
}  


String LatticeStatsBase::toStatisticName (int32_t iType)
{
   StatisticsTypes type = StatisticsTypes(iType);
   return toStatisticName(type);
}

String LatticeStatsBase::toStatisticName (StatisticsTypes type)
{
   String name;
   if (type==NPTS) {
      name = "NPTS";
   } else if (type==SUM) {
      name = "SUM";
   } else if (type==SUMSQ) {  
      name = "SUMSQ";
   } else if (type==MEAN) {
      name = "MEAN";
   } else if (type==VARIANCE) {
      name = "VARIANCE";
   } else if (type==SIGMA) {
      name = "SIGMA";
   } else if (type==RMS) {
      name = "RMS";
   } else if (type==MIN) {
      name = "MIN";
   } else if (type==MAX) {
      name = "MAX";
   } else if (type==FLUX) {
      name = "FLUX";
   } else if (type==MEDABSDEVMED) {
      name = "MEDABSDEVMED";
   } else if (type==MEDIAN) {
      name = "MEDIAN";
   } else if (type==QUARTILE) {
      name = "QUARTILE";
   }
   return name;
}  



bool LatticeStatsBase::setNxy (Vector<int32_t>& nxy,
                               ostream& os)
{
   int32_t n = nxy.nelements();
   nxy.resize(2,true);
   if (n > 2) {
      os << "Too many elements for argument nxy" << endl;
      return false;
   } else if (n == 2) {
      nxy(0) = max(1,nxy(0));
      nxy(1) = max(1,nxy(1));
   } else if (n == 1) {
      nxy(0) = max(1,nxy(0));
      nxy(1) = nxy(0);
   } else {
      nxy(0) = 1;
      nxy(1) = 1;
   }
   return true;
}

void LatticeStatsBase::setStorageImageShape(IPosition& storeImageShape,
                                            const bool& last,
                                            const int32_t& axisSize,
                                            const Vector<int32_t>& displayAxes, 
                                            const IPosition& imageShape)
{
   int32_t nStoreImageDim = displayAxes.nelements() + 1;
   storeImageShape.resize(nStoreImageDim);

   if (last) {
      for (int32_t i=0; i<nStoreImageDim-1; i++) storeImageShape(i) = imageShape(displayAxes(i));
      storeImageShape(nStoreImageDim-1) = axisSize;
   } else {
      for (int32_t i=1; i<nStoreImageDim; i++) 
        storeImageShape(i) = imageShape(displayAxes(i-1));
      storeImageShape(0) = axisSize;
   }
}

void LatticeStatsBase::stretchMinMax (float& dMin, 
                                      float& dMax)
{
   float delta = 0.05*(dMax-dMin);
   float absmax = max(abs(dMax),abs(dMin));
   if (delta < 1.0e-5*absmax) delta = 0.01 * absmax;

   if (dMin==dMax) {
      if (dMin==0.0) {
         dMin = -1.0; 
         dMax = 1.0;
      }
      else {
         dMin = dMin - 0.05*dMin; 
         dMax = dMax + 0.05*dMax;
      }
   }
   else {
      dMin = dMin - delta; 
      dMax = dMax + delta;
   }
}

std::set<double> LatticeStatsBase::quartileFracs() {
    const static double fracs[] {0.25, 0.75};
    return std::set<double>(fracs, fracs+2);
}

} //# NAMESPACE CASACORE - END

