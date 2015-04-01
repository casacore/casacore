//# MeasTableMul.cc: Nutation multiplication coefficient for MeasTable
//# Copyright (C) 1995-1999,2000-2004
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
//# $Id: MeasTable.h 21420 2014-03-19 09:18:51Z gervandiepen $

#include <casacore/measures/Measures/MeasTableMul.h>
#include <casacore/casa/Quanta/UnitVal.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  MeasTableMul::MeasTableMul()
    : itsLastUsed (0)
  {}

  void MeasTableMul::clear()
  {
    itsTimes.resize  (0);
    itsUsed.resize   (0);
    itsArrays.resize (0);
  }

  CountedPtr<Matrix<Double> > MeasTableMul::getArray
  (Double time, Double epsilon)
  {
    {   // cache lookup must be thread-safe
      ScopedMutexLock locker(itsMutex);
      // See if a time (within the required epsilon) has already been calculated.
      // If so, return it.
      // Note: locking needs to be done outside this class.
      for (size_t i=0; i<itsTimes.size(); ++i) {
        if (nearAbs (time, itsTimes[i], epsilon)) {
          itsUsed[i] = itsLastUsed++;
          ///cerr << "found existing mulsc at " << i << " for " << time<<endl;
          return itsArrays[i];
        }
      }
      // If nothing has been done yet, size the cache and let a derived
      // class initialize itself.
      if (itsArrays.capacity() == 0) {
        init();
        itsUsed.reserve (32);
        itsTimes.reserve (32);
        itsArrays.reserve (32);   // use a aipsrc variable for this size
      }
    }
    // Let a derived class calculate the coefficient matrix for this epoch.
    // Note: multiple threads can execute this part (which is fine).
    CountedPtr<Matrix<Double> > arr(new Matrix<Double>(itsDefArray.shape()));
    *arr = itsDefArray;
    calc (*arr, time);
    {   // cache insertion must also be thread-safe
      ScopedMutexLock locker(itsMutex);
      // Determine where to insert in the vector.
      // If not full yet, put it at the end and first inser
      // Otherwise use the least recently used entry.
      if (itsArrays.size() < itsArrays.capacity()) {
        ///cerr << "new mulsc at " << itsUsed.size() << " for " << time<<endl;
        itsUsed.push_back   (itsLastUsed);
        itsTimes.push_back  (time);
        itsArrays.push_back (arr);
      } else {
        // Find the least recently used entry.
        size_t inx = 0;
        for (size_t i=0; i<itsUsed.size(); ++i) {
          if (itsUsed[i] < itsUsed[inx]) {
            inx = i;
          }
        }
        itsUsed[inx]   = itsLastUsed;
        itsTimes[inx]  = time;
        itsArrays[inx] = arr;
        ///cerr << "replaced mulsc at " << inx << " for " << time<<endl;
      }
      itsLastUsed++;
    }
    return arr;
  }

  MeasTableMulSCBase::MeasTableMulSCBase()
  {}
  void MeasTableMulSCBase::doInit(Matrix<Double>& result,
                                  Polynomial<Double> poly[],
                                  Int nrowTD, const Long coeffTD[][5],
                                  Int nrowSC, const Short coeffSC[][2])
  {
    for (Int i=0; i<nrowTD; ++i) {
      for (Int j=0; j<2; ++j) {
        poly[2*i+j] = Polynomial<Double>(2);
        poly[2*i+j].setCoefficient(0, coeffTD[i][1+2*j] * C::arcsec*1e-4);
        poly[2*i+j].setCoefficient(1, coeffTD[i][2+2*j] * C::arcsec*1e-5);
      }
    }
    result.resize (4, nrowSC);
    result = 0.;
    for (Int i=0; i<nrowSC; ++i) {
      for (Int j=0; j<2; j++) {
        result(j,i) = coeffSC[i][j] * C::arcsec*1e-4;
      }
    }
  }
  void MeasTableMulSCBase::doCalc(Matrix<Double>& result, Double time,
                                  const Polynomial<Double> poly[],
                                  Int nrowTD, const Long coeffTD[][5])
  {
    for (Int i=0; i<nrowTD; ++i) { // get fundamental argument coefficients
      Long j = coeffTD[i][0];
      result(0,j) = poly[2*i+0](time);
      result(1,j) = poly[2*i+1](time);
      result(2,j) = (poly[2*i+0].derivative())(time);
      result(3,j) = (poly[2*i+1].derivative())(time);
    }
  }


  MeasTableMulSC::MeasTableMulSC()
  {}
  void MeasTableMulSC::init()
  {
    doInit (itsDefArray, itsPoly, 15, theirMULTD, 106, theirMULSC);
  }
  void MeasTableMulSC::calc(Matrix<Double>& result, Double time)
  {
    doCalc (result, time, itsPoly, 15, theirMULTD);
  }


  MeasTableMulSC1950::MeasTableMulSC1950()
  {}
  void MeasTableMulSC1950::init()
  {
    doInit (itsDefArray, itsPoly, 13, theirMULTD, 69, theirMULSC);
  }
  void MeasTableMulSC1950::calc(Matrix<Double>& result, Double time)
  {
    doCalc (result, time, itsPoly, 13, theirMULTD);
  }


  MeasTableMulSC2000Base::MeasTableMulSC2000Base()
  {}
  void MeasTableMulSC2000Base::doInit(Matrix<Double>& result,
                                      Polynomial<Double> poly[],
                                      Int nrowSC, const Long coeffSC[][6])
  {
    result.resize (6, nrowSC);
    result = 0.;
    for (Int i=0; i<nrowSC; ++i) {
      for (Int j=0; j<2; ++j) {
        poly[2*i+j] = Polynomial<Double>(2);
        poly[2*i+j].setCoefficient(0, coeffSC[i][0+3*j]*C::arcsec*1e-7);
        poly[2*i+j].setCoefficient(1, coeffSC[i][1+3*j]*C::arcsec*1e-7);
      }
      result(2,i) = coeffSC[i][1]*C::arcsec*1e-7;
      result(3,i) = coeffSC[i][4]*C::arcsec*1e-7;
      result(4,i) = coeffSC[i][2]*C::arcsec*1e-7;
      result(5,i) = coeffSC[i][5]*C::arcsec*1e-7;
    }
  }
  void MeasTableMulSC2000Base::doCalc(Matrix<Double>& result, Double time,
                                      const Polynomial<Double> poly[],
                                      Int nrowSC)
  {
    for (Int i=0; i<nrowSC; ++i) {
      result(0,i) = poly[2*i+0](time);
      result(1,i) = poly[2*i+1](time);
    }
  }

  MeasTableMulSC2000A::MeasTableMulSC2000A()
  {}
  void MeasTableMulSC2000A::init()
  {
    doInit (itsDefArray, itsPoly, 678, theirMULSC);
  }
  void MeasTableMulSC2000A::calc(Matrix<Double>& result, Double time)
  {
    doCalc (result, time, itsPoly, 678);
  }

  MeasTableMulSC2000B::MeasTableMulSC2000B()
  {}
  void MeasTableMulSC2000B::init()
  {
    doInit (itsDefArray, itsPoly, 77, theirMULSC);
  }
  void MeasTableMulSC2000B::calc(Matrix<Double>& result, Double time)
  {
    doCalc (result, time, itsPoly, 77);
  }


  MeasTableMulAber::MeasTableMulAber()
  {}
  void MeasTableMulAber::init()
  {
    UnitVal AUperDay(1e-8,"AU/d");
    double factor = AUperDay.getFac();
    for (Int i=0; i<3; ++i) {
      for (Int j=0; j<6; ++j) {
        itsPoly[6*i+j] = Polynomial<Double>(2);
        for (Int k=0; k<3; ++k) {
          itsPoly[6*i+j].setCoefficient(k, theirMABERTD[i][k+3*j]*factor);
        }
      }
    }
    itsDefArray.resize (12, 80);
    itsDefArray = 0.;
    for (Int i=0; i<80; ++i) {
      for (Int j=0; j<6; ++j) {
        itsDefArray(j,i) = theirMABER[i][j] * factor;
      }
    }
  }
  void MeasTableMulAber::calc(Matrix<Double>& result, Double time)
  {
    for (Int i=0; i<3; ++i) {	// get fundamental argument coefficients
      for (Int j=0; j<6; ++j) {
	result(j,i)   = itsPoly[6*i+j](time);
	result(j+6,i) = (itsPoly[6*i+j].derivative())(time);
      }
    }
  }

  MeasTableMulAber1950::MeasTableMulAber1950()
  {}
  void MeasTableMulAber1950::init()
  {
    UnitVal AUperDay(1e-8,"AU/d");
    itsFactor = AUperDay.getFac();
    itsDefArray.resize (12,132);
    itsDefArray = 0.;
    for (Int i=0; i<130; ++i) {
      for (Int j=0; j<6; ++j) {
        itsDefArray(j,i) = theirMABER[i][j] * itsFactor;
      }
    }
    for (Int i=0; i<2; ++i) {
      for (Int j=0; j<6; ++j) {
        itsDefArray(j,130+i) = theirABERSPEC[i][j] * itsFactor;
      }
    }
  }
  void MeasTableMulAber1950::calc(Matrix<Double>& result, Double time)
  {
    for (Int i=0; i<10; ++i) {	// get fundamental argument coefficients
      Int k = theirABERT1T[i];
      for (Int j=0; j<6; ++j) {
	result(j,k) = theirMABER[k][j] * itsFactor * time;
	result(j+6,k) = theirMABER[k][j] * itsFactor;        // d/dT
      }
    }
    for (Int i=0; i<2; ++i) {	// get fundamental argument coefficients
      Int k = theirABERT2T[i];
      for (Int j=0; j<6; ++j) {
	result(j,k) *= time;        // Already multiplied by T in ABERT1T
	result(j+6,k) *= 2*time;    // d/dT
      }
    }
    for (Int i=0; i<1; ++i) {	// get fundamental argument coefficients
      Int k = theirABERT3T[i];
      for (Int j=0; j<6; ++j) {
	result(j,k) *= time;        // Already multiplied by T**2 in ABERT2T
	result(j+6,k) *= 1.5*time;  // d/dT: 1.5 * T * 2 * T = 3 * T**2
      }
    }
  }


  MeasTableMulPosSunXY::MeasTableMulPosSunXY()
  {}
  void MeasTableMulPosSunXY::init()
  {
    itsDefArray.resize(8,98);
    itsDefArray = 0.;
    for (Int i=0; i<98; ++i) {
      itsDefArray(0,i) = theirMPOSXY[i][0] * C::degree;
      itsDefArray(1,i) = theirMPOSXY[i][1] * 1e-10;
      itsDefArray(2,i) = theirMPOSXY[i][2] * C::degree;
      itsDefArray(3,i) = theirMPOSXY[i][3] * 1e-10;
    }
  }
  void MeasTableMulPosSunXY::calc(Matrix<Double>& result, Double time)
  {
    for (Int i=84; i<98; ++i) { // get fundamental argument coefficients
      result(1,i) = theirMPOSXY[i][1] * 1e-10 * time;
      result(3,i) = theirMPOSXY[i][3] * 1e-10 * time;
      result(5,i) = theirMPOSXY[i][1] * 1e-10;
      result(7,i) = theirMPOSXY[i][3] * 1e-10;
    }
  }

  MeasTableMulPosSunZ::MeasTableMulPosSunZ()
  {}
  void MeasTableMulPosSunZ::init()
  {
    itsDefArray.resize(4,29);
    itsDefArray = 0.;
    for (Int i=0; i<29; ++i) {
      itsDefArray(0,i) = theirMPOSZ[i][0] * C::degree;
      itsDefArray(1,i) = theirMPOSZ[i][1] * 1e-10;
    }
  }
  void MeasTableMulPosSunZ::calc(Matrix<Double>& result, Double time)
  {
    for (Int i=26; i<29; ++i) { // get fundamental argument coefficients
      result(1,i) = theirMPOSZ[i][1] * 1e-10 * time;
      result(3,i) = theirMPOSZ[i][1] * 1e-10;
    }
  }


  MeasTableMulPosEarthXY::MeasTableMulPosEarthXY()
  {}
  void MeasTableMulPosEarthXY::init()
  {
    itsDefArray.resize(8,189);
    itsDefArray = 0.;
    for (Int i=0; i<189; ++i) {
      itsDefArray(0,i) = theirMPOSXY[i][0] * C::degree;
      itsDefArray(1,i) = theirMPOSXY[i][1] * 1e-10;
      itsDefArray(2,i) = theirMPOSXY[i][2] * C::degree;
      itsDefArray(3,i) = theirMPOSXY[i][3] * 1e-10;
    }
  }
  void MeasTableMulPosEarthXY::calc(Matrix<Double>& result, Double time)
  {
    for (Int i=174; i<189; ++i) { // get fundamental argument coefficients
      result(1,i) = theirMPOSXY[i][1] * 1e-10 * time;
      result(3,i) = theirMPOSXY[i][3] * 1e-10 * time;
      result(5,i) = theirMPOSXY[i][1] * 1e-10;
      result(7,i) = theirMPOSXY[i][3] * 1e-10;
    }
    for (Int i=186; i<189; ++i) { // get fundamental argument coefficients
      result(1,i) *= time;
      result(3,i) *= time;
      result(5,i) *= 2*time;
      result(7,i) *= 2*time;
    }
  }

  MeasTableMulPosEarthZ::MeasTableMulPosEarthZ()
  {}
  void MeasTableMulPosEarthZ::init()
  {
    itsDefArray.resize(4,32);
    itsDefArray = 0.;
    for (Int i=0; i<32; ++i) {
      itsDefArray(0,i) = theirMPOSZ[i][0] * C::degree;
      itsDefArray(1,i) = theirMPOSZ[i][1] * 1e-10;
    }
  }
  void MeasTableMulPosEarthZ::calc(Matrix<Double>& result, Double time)
  {
    for (Int i=28; i<32; ++i) { // get fundamental argument coefficients
      result(1,i) = theirMPOSZ[i][1] * 1e-10 * time;
      result(3,i) = theirMPOSZ[i][1] * 1e-10;
    }
    for (Int i=31; i<32; ++i) { // get fundamental argument coefficients
      result(1,i) *= time;
      result(3,i) *= 2*time;
    }
  }



  // Define the various constants.
  const Long MeasTableMulSC::theirMULTD[15][5] = {
    {0  ,-171996 ,-1742 ,92025 ,89},
    {1  ,2062    ,2     ,-895  ,5},
    {8  ,-13187  ,-16   ,5736  ,-31},
    {9  ,1426    ,-34   ,54    ,-1},
    {10 ,-517    ,12    ,224   ,-6},
    {11 ,217     ,-5    ,-95   ,3},
    {12 ,129     ,1     ,-70   ,0},
    {15 ,17      ,-1    ,0     ,0},
    {17 ,-16     ,1     ,7     ,0},
    {30 ,-2274   ,-2    ,977   ,-5},
    {31 ,712     ,1     ,-7    ,0},
    {32 ,-386    ,-4    ,200   ,0},
    {33 ,-301    ,0     ,129   ,-1},
    {37 ,63      ,1     ,-33   ,0},
    {38 ,-58     ,-1    ,32    ,0}
  };
  const Short MeasTableMulSC::theirMULSC[106][2] = {
    {0	,0	},
    {0	,0	},
    {46	,-24	},
    {11	,0	},
    {-3	,1	},
    
    {-3	,0	},
    {-2	,1	},
    {1	,0	},
    {0	,0	},
    {0	,0	},
    
    {0	,0	},
    {0	,0	},
    {0	,0	},
    {48	,1	},
    {-22	,0	},
    
    {0	,0	},
    {-15	,9	},
    {0	,0	},
    {-12	,6	},
    {-6	,3	},
    
    {-5	,3	},
    {4	,-2	},
    {4	,-2	},
    {-4	,0	},
    {1	,0	},
    
    {1	,0	},
    {-1	,0	},
    {1	,0	},
    {1	,0	},
    {-1	,0	},
    
    {0	,0	},
    {0	,0	},
    {0	,0	},
    {0	,0	},
    {-158	,-1	},
    
    {123	,-53	},
    {63	,-2	},
    {0	,0	},
    {0	,0	},
    {-59	,26	},
    
    {-51	,27	},
    {-38	,16	},
    {29	,-1	},
    {29	,-12	},
    {-31	,13	},
    
    {26	,-1	},
    {21	,-10	},
    {16	,-8	},
    {-13	,7	},
    {-10	,5	},
    
    {-7	,0	},
    {7	,-3	},
    {-7	,3	},
    {-8	,3	},
    {6	,0	},
    
    {6	,-3	},
    {-6	,3	},
    {-7	,3	},
    {6	,-3	},
    {-5	,3	},
    
    {5	,0	},
    {-5	,3	},
    {-4	,0	},
    {4	,0	},
    {-4	,0	},
    
    {-3	,0	},
    {3	,0	},
    {-3	,1	},
    {-3	,1	},
    {-2	,1	},
    
    {-3	,1	},
    {-3	,1	},
    {2	,-1	},
    {-2	,1	},
    {2	,-1	},
    
    {-2	,1	},
    {2	,0	},
    {2	,-1	},
    {1	,-1	},
    {-1	,0	},
    
    {1	,-1	},
    {-2	,1	},
    {-1	,0	},
    {1	,-1	},
    {-1	,1	},
    
    {-1	,1	},
    {1	,0	},
    {1	,0	},
    {1	,-1	},
    {-1	,0	},
    
    {-1	,0	},
    {1	,0	},
    {1	,0	},
    {-1	,0	},
    {1	,0	},
    
    {1	,0	},
    {-1	,0	},
    {-1	,0	},
    {-1	,0	},
    {-1	,0	},
    
    {-1	,0	},
    {-1	,0	},
    {-1	,0	},
    {1	,0	},
    {-1	,0	},
    
    {1	,0}
  };


  const Long MeasTableMulSC1950::theirMULTD[13][5] = {
    {0  ,-172327 ,-1737 ,92100 ,91},
    {1  ,2088    ,2     ,-904  ,4},
    {7  ,-12729  ,-13   ,5522  ,-29},
    {8  ,1261    ,-31   ,0     ,0},
    {9  ,-497    ,12    ,216   ,-6},
    {10 ,214     ,-5    ,-93   ,3},
    {11 ,124     ,1     ,-66   ,0},
    {14 ,16      ,-1    ,0     ,0},
    {16 ,-15     ,1     ,7     ,0},
    {23 ,-2037   ,-2    ,884   ,-5},
    {24 ,675     ,1     ,0     ,0},
    {25 ,-342    ,-4    ,183   ,0},
    {26 ,-261    ,0     ,113   ,-1}
  };
  const Short MeasTableMulSC1950::theirMULSC[69][2] = {
    {0	,0	},
    {0	,0	},
    {45	,-24	},
    {10	,0	},
    {-4	,2	},
    
    {-3	,2	},
    {-2	,0	},
    {0	,0	},
    {0	,0	},
    {0	,0	},
    
    {0	,0	},
    {0	,0	},
    {45	,0	},
    {-21,0	},
    {0	,0	},
    
    {-15,8	},
    {0	,0	},
    {-10,5	},
    {-5	,3	},
    {-5	,3	},
    
    {4	,-2	},
    {3	,-2	},
    {-3	,0	},
    {0	,0	},
    {0	,0	},
    
    {0	,0	},
    {0	,0	},
    {-149,0	},
    {114 ,-50	},
    {60	,0	},
    
    {58	,-31	},
    {-57,30	},
    {-52,22	},
    {-44,23	},
    {-32,14	},
    
    {28	,0	},
    {26	,-11	},
    {-26,11	},
    {25	,0	},
    {19	,-10	},
    
    {14	,-7	},
    {-13,7	},
    {-9	,5	},
    {-7	,0	},
    {7	,-3	},
    
    {6	,0	},
    {-6	,3	},
    {-6	,3	},
    {-6	,3	},
    {6	,-2	},
    
    {-5	,3	},
    {-5	,3	},
    {5	,-3	},
    {-4	,0	},
    {-4	,0	},
    
    {4	,0	},
    {4	,0	},
    {-4	,2	},
    {3	,0	},
    {-3	,0	},
    
    {-3	,0	},
    {-2	,0	},
    {-2	,0	},
    {2	,0	},
    {-2	,0	},
    
    {-2	,0	},
    {-2	,0	},
    {2	,0	},
    {-2	,0	}
  };


  // Luni-Solar nutation coefficients, unit 1e-7 arcsec
  const Long MeasTableMulSC2000A::theirMULSC[678][6] = {
  //           Longitude                Obliquity
  //    sin     t.sin     cos      cos    t.cos   sin
  {-172064161, -174666,  33386, 92052331,  9086, 15377},   //    1
  { -13170906,   -1675, -13696,  5730336, -3015, -4587},   //    2
  {  -2276413,    -234,   2796,   978459,  -485,  1374},   //    3
  {   2074554,     207,   -698,  -897492,   470,  -291},   //    4
  {   1475877,   -3633,  11817,    73871,  -184, -1924},   //    5
  {   -516821,    1226,   -524,   224386,  -677,  -174},   //    6
  {    711159,      73,   -872,    -6750,     0,   358},   //    7
  {   -387298,    -367,    380,   200728,    18,   318},   //    8
  {   -301461,     -36,    816,   129025,   -63,   367},   //    9
  {    215829,    -494,    111,   -95929,   299,   132},   //   10
  {    128227,     137,    181,   -68982,    -9,    39},   //   11
  {    123457,      11,     19,   -53311,    32,    -4},   //   12
  {    156994,      10,   -168,    -1235,     0,    82},   //   13
  {     63110,      63,     27,   -33228,     0,    -9},   //   14
  {    -57976,     -63,   -189,    31429,     0,   -75},   //   15
  {    -59641,     -11,    149,    25543,   -11,    66},   //   16
  {    -51613,     -42,    129,    26366,     0,    78},   //   17
  {     45893,      50,     31,   -24236,   -10,    20},   //   18
  {     63384,      11,   -150,    -1220,     0,    29},   //   19
  {    -38571,      -1,    158,    16452,   -11,    68},   //   20
  {     32481,       0,      0,   -13870,     0,     0},   //   21
  {    -47722,       0,    -18,      477,     0,   -25},   //   22
  {    -31046,      -1,    131,    13238,   -11,    59},   //   23
  {     28593,       0,     -1,   -12338,    10,    -3},   //   24
  {     20441,      21,     10,   -10758,     0,    -3},   //   25
  {     29243,       0,    -74,     -609,     0,    13},   //   26
  {     25887,       0,    -66,     -550,     0,    11},   //   27
  {    -14053,     -25,     79,     8551,    -2,   -45},   //   28
  {     15164,      10,     11,    -8001,     0,    -1},   //   29
  {    -15794,      72,    -16,     6850,   -42,    -5},   //   30
  {     21783,       0,     13,     -167,     0,    13},   //   31
  {    -12873,     -10,    -37,     6953,     0,   -14},   //   32
  {    -12654,      11,     63,     6415,     0,    26},   //   33
  {    -10204,       0,     25,     5222,     0,    15},   //   34
  {     16707,     -85,    -10,      168,    -1,    10},   //   35
  {     -7691,       0,     44,     3268,     0,    19},   //   36
  {    -11024,       0,    -14,      104,     0,     2},   //   37
  {      7566,     -21,    -11,    -3250,     0,    -5},   //   38
  {     -6637,     -11,     25,     3353,     0,    14},   //   39
  {     -7141,      21,      8,     3070,     0,     4},   //   40
  {     -6302,     -11,      2,     3272,     0,     4},   //   41
  {      5800,      10,      2,    -3045,     0,    -1},   //   42
  {      6443,       0,     -7,    -2768,     0,    -4},   //   43
  {     -5774,     -11,    -15,     3041,     0,    -5},   //   44
  {     -5350,       0,     21,     2695,     0,    12},   //   45
  {     -4752,     -11,     -3,     2719,     0,    -3},   //   46
  {     -4940,     -11,    -21,     2720,     0,    -9},   //   47
  {      7350,       0,     -8,      -51,     0,     4},   //   48
  {      4065,       0,      6,    -2206,     0,     1},   //   49
  {      6579,       0,    -24,     -199,     0,     2},   //   50
  {      3579,       0,      5,    -1900,     0,     1},   //   51
  {      4725,       0,     -6,      -41,     0,     3},   //   52
  {     -3075,       0,     -2,     1313,     0,    -1},   //   53
  {     -2904,       0,     15,     1233,     0,     7},   //   54
  {      4348,       0,    -10,      -81,     0,     2},   //   55
  {     -2878,       0,      8,     1232,     0,     4},   //   56
  {     -4230,       0,      5,      -20,     0,    -2},   //   57
  {     -2819,       0,      7,     1207,     0,     3},   //   58
  {     -4056,       0,      5,       40,     0,    -2},   //   59
  {     -2647,       0,     11,     1129,     0,     5},   //   60
  {     -2294,       0,    -10,     1266,     0,    -4},   //   61
  {      2481,       0,     -7,    -1062,     0,    -3},   //   62
  {      2179,       0,     -2,    -1129,     0,    -2},   //   63
  {      3276,       0,      1,       -9,     0,     0},   //   64
  {     -3389,       0,      5,       35,     0,    -2},   //   65
  {      3339,       0,    -13,     -107,     0,     1},   //   66
  {     -1987,       0,     -6,     1073,     0,    -2},   //   67
  {     -1981,       0,      0,      854,     0,     0},   //   68
  {      4026,       0,   -353,     -553,     0,  -139},   //   69
  {      1660,       0,     -5,     -710,     0,    -2},   //   70
  {     -1521,       0,      9,      647,     0,     4},   //   71
  {      1314,       0,      0,     -700,     0,     0},   //   72
  {     -1283,       0,      0,      672,     0,     0},   //   73
  {     -1331,       0,      8,      663,     0,     4},   //   74
  {      1383,       0,     -2,     -594,     0,    -2},   //   75
  {      1405,       0,      4,     -610,     0,     2},   //   76
  {      1290,       0,      0,     -556,     0,     0},   //   77
  {     -1214,       0,      5,      518,     0,     2},   //   78
  {      1146,       0,     -3,     -490,     0,    -1},   //   79
  {      1019,       0,     -1,     -527,     0,    -1},   //   80
  {     -1100,       0,      9,      465,     0,     4},   //   81
  {      -970,       0,      2,      496,     0,     1},   //   82
  {      1575,       0,     -6,      -50,     0,     0},   //   83
  {       934,       0,     -3,     -399,     0,    -1},   //   84
  {       922,       0,     -1,     -395,     0,    -1},   //   85
  {       815,       0,     -1,     -422,     0,    -1},   //   86
  {       834,       0,      2,     -440,     0,     1},   //   87
  {      1248,       0,      0,     -170,     0,     1},   //   88
  {      1338,       0,     -5,      -39,     0,     0},   //   89
  {       716,       0,     -2,     -389,     0,    -1},   //   90
  {      1282,       0,     -3,      -23,     0,     1},   //   91
  {       742,       0,      1,     -391,     0,     0},   //   92
  {      1020,       0,    -25,     -495,     0,   -10},   //   93
  {       715,       0,     -4,     -326,     0,     2},   //   94
  {      -666,       0,     -3,      369,     0,    -1},   //   95
  {      -667,       0,      1,      346,     0,     1},   //   96
  {      -704,       0,      0,      304,     0,     0},   //   97
  {      -694,       0,      5,      294,     0,     2},   //   98
  {     -1014,       0,     -1,        4,     0,    -1},   //   99
  {      -585,       0,     -2,      316,     0,    -1},   //  100
  {      -949,       0,      1,        8,     0,    -1},   //  101
  {      -595,       0,      0,      258,     0,     0},   //  102
  {       528,       0,      0,     -279,     0,     0},   //  103
  {      -590,       0,      4,      252,     0,     2},   //  104
  {       570,       0,     -2,     -244,     0,    -1},   //  105
  {      -502,       0,      3,      250,     0,     2},   //  106
  {      -875,       0,      1,       29,     0,     0},   //  107
  {      -492,       0,     -3,      275,     0,    -1},   //  108
  {       535,       0,     -2,     -228,     0,    -1},   //  109
  {      -467,       0,      1,      240,     0,     1},   //  110
  {       591,       0,      0,     -253,     0,     0},   //  111
  {      -453,       0,     -1,      244,     0,    -1},   //  112
  {       766,       0,      1,        9,     0,     0},   //  113
  {      -446,       0,      2,      225,     0,     1},   //  114
  {      -488,       0,      2,      207,     0,     1},   //  115
  {      -468,       0,      0,      201,     0,     0},   //  116
  {      -421,       0,      1,      216,     0,     1},   //  117
  {       463,       0,      0,     -200,     0,     0},   //  118
  {      -673,       0,      2,       14,     0,     0},   //  119
  {       658,       0,      0,       -2,     0,     0},   //  120
  {      -438,       0,      0,      188,     0,     0},   //  121
  {      -390,       0,      0,      205,     0,     0},   //  122
  {       639,     -11,     -2,      -19,     0,     0},   //  123
  {       412,       0,     -2,     -176,     0,    -1},   //  124
  {      -361,       0,      0,      189,     0,     0},   //  125
  {       360,       0,     -1,     -185,     0,    -1},   //  126
  {       588,       0,     -3,      -24,     0,     0},   //  127
  {      -578,       0,      1,        5,     0,     0},   //  128
  {      -396,       0,      0,      171,     0,     0},   //  129
  {       565,       0,     -1,       -6,     0,     0},   //  130
  {      -335,       0,     -1,      184,     0,    -1},   //  131
  {       357,       0,      1,     -154,     0,     0},   //  132
  {       321,       0,      1,     -174,     0,     0},   //  133
  {      -301,       0,     -1,      162,     0,     0},   //  134
  {      -334,       0,      0,      144,     0,     0},   //  135
  {       493,       0,     -2,      -15,     0,     0},   //  136
  {       494,       0,     -2,      -19,     0,     0},   //  137
  {       337,       0,     -1,     -143,     0,    -1},   //  138
  {       280,       0,     -1,     -144,     0,     0},   //  139
  {       309,       0,      1,     -134,     0,     0},   //  140
  {      -263,       0,      2,      131,     0,     1},   //  141
  {       253,       0,      1,     -138,     0,     0},   //  142
  {       245,       0,      0,     -128,     0,     0},   //  143
  {       416,       0,     -2,      -17,     0,     0},   //  144
  {      -229,       0,      0,      128,     0,     0},   //  145
  {       231,       0,      0,     -120,     0,     0},   //  146
  {      -259,       0,      2,      109,     0,     1},   //  147
  {       375,       0,     -1,       -8,     0,     0},   //  148
  {       252,       0,      0,     -108,     0,     0},   //  149
  {      -245,       0,      1,      104,     0,     0},   //  150
  {       243,       0,     -1,     -104,     0,     0},   //  151
  {       208,       0,      1,     -112,     0,     0},   //  152
  {       199,       0,      0,     -102,     0,     0},   //  153
  {      -208,       0,      1,      105,     0,     0},   //  154
  {       335,       0,     -2,      -14,     0,     0},   //  155
  {      -325,       0,      1,        7,     0,     0},   //  156
  {      -187,       0,      0,       96,     0,     0},   //  157
  {       197,       0,     -1,     -100,     0,     0},   //  158
  {      -192,       0,      2,       94,     0,     1},   //  159
  {      -188,       0,      0,       83,     0,     0},   //  160
  {       276,       0,      0,       -2,     0,     0},   //  161
  {      -286,       0,      1,        6,     0,     0},   //  162
  {       186,       0,     -1,      -79,     0,     0},   //  163
  {      -219,       0,      0,       43,     0,     0},   //  164
  {       276,       0,      0,        2,     0,     0},   //  165
  {      -153,       0,     -1,       84,     0,     0},   //  166
  {      -156,       0,      0,       81,     0,     0},   //  167
  {      -154,       0,      1,       78,     0,     0},   //  168
  {      -174,       0,      1,       75,     0,     0},   //  169
  {      -163,       0,      2,       69,     0,     1},   //  170
  {      -228,       0,      0,        1,     0,     0},   //  171
  {        91,       0,     -4,      -54,     0,    -2},   //  172
  {       175,       0,      0,      -75,     0,     0},   //  173
  {      -159,       0,      0,       69,     0,     0},   //  174
  {       141,       0,      0,      -72,     0,     0},   //  175
  {       147,       0,      0,      -75,     0,     0},   //  176
  {      -132,       0,      0,       69,     0,     0},   //  177
  {       159,       0,    -28,      -54,     0,    11},   //  178
  {       213,       0,      0,       -4,     0,     0},   //  179
  {       123,       0,      0,      -64,     0,     0},   //  180
  {      -118,       0,     -1,       66,     0,     0},   //  181
  {       144,       0,     -1,      -61,     0,     0},   //  182
  {      -121,       0,      1,       60,     0,     0},   //  183
  {      -134,       0,      1,       56,     0,     1},   //  184
  {      -105,       0,      0,       57,     0,     0},   //  185
  {      -102,       0,      0,       56,     0,     0},   //  186
  {       120,       0,      0,      -52,     0,     0},   //  187
  {       101,       0,      0,      -54,     0,     0},   //  188
  {      -113,       0,      0,       59,     0,     0},   //  189
  {      -106,       0,      0,       61,     0,     0},   //  190
  {      -129,       0,      1,       55,     0,     0},   //  191
  {      -114,       0,      0,       57,     0,     0},   //  192
  {       113,       0,     -1,      -49,     0,     0},   //  193
  {      -102,       0,      0,       44,     0,     0},   //  194
  {       -94,       0,      0,       51,     0,     0},   //  195
  {      -100,       0,     -1,       56,     0,     0},   //  196
  {        87,       0,      0,      -47,     0,     0},   //  197
  {       161,       0,      0,       -1,     0,     0},   //  198
  {        96,       0,      0,      -50,     0,     0},   //  199
  {       151,       0,     -1,       -5,     0,     0},   //  200
  {      -104,       0,      0,       44,     0,     0},   //  201
  {      -110,       0,      0,       48,     0,     0},   //  202
  {      -100,       0,      1,       50,     0,     0},   //  203
  {        92,       0,     -5,       12,     0,    -2},   //  204
  {        82,       0,      0,      -45,     0,     0},   //  205
  {        82,       0,      0,      -45,     0,     0},   //  206
  {       -78,       0,      0,       41,     0,     0},   //  207
  {       -77,       0,      0,       43,     0,     0},   //  208
  {         2,       0,      0,       54,     0,     0},   //  209
  {        94,       0,      0,      -40,     0,     0},   //  210
  {       -93,       0,      0,       40,     0,     0},   //  211
  {       -83,       0,     10,       40,     0,    -2},   //  212
  {        83,       0,      0,      -36,     0,     0},   //  213
  {       -91,       0,      0,       39,     0,     0},   //  214
  {       128,       0,      0,       -1,     0,     0},   //  215
  {       -79,       0,      0,       34,     0,     0},   //  216
  {       -83,       0,      0,       47,     0,     0},   //  217
  {        84,       0,      0,      -44,     0,     0},   //  218
  {        83,       0,      0,      -43,     0,     0},   //  219
  {        91,       0,      0,      -39,     0,     0},   //  220
  {       -77,       0,      0,       39,     0,     0},   //  221
  {        84,       0,      0,      -43,     0,     0},   //  222
  {       -92,       0,      1,       39,     0,     0},   //  223
  {       -92,       0,      1,       39,     0,     0},   //  224
  {       -94,       0,      0,        0,     0,     0},   //  225
  {        68,       0,      0,      -36,     0,     0},   //  226
  {       -61,       0,      0,       32,     0,     0},   //  227
  {        71,       0,      0,      -31,     0,     0},   //  228
  {        62,       0,      0,      -34,     0,     0},   //  229
  {       -63,       0,      0,       33,     0,     0},   //  230
  {       -73,       0,      0,       32,     0,     0},   //  231
  {       115,       0,      0,       -2,     0,     0},   //  232
  {      -103,       0,      0,        2,     0,     0},   //  233
  {        63,       0,      0,      -28,     0,     0},   //  234
  {        74,       0,      0,      -32,     0,     0},   //  235
  {      -103,       0,     -3,        3,     0,    -1},   //  236
  {       -69,       0,      0,       30,     0,     0},   //  237
  {        57,       0,      0,      -29,     0,     0},   //  238
  {        94,       0,      0,       -4,     0,     0},   //  239
  {        64,       0,      0,      -33,     0,     0},   //  240
  {       -63,       0,      0,       26,     0,     0},   //  241
  {       -38,       0,      0,       20,     0,     0},   //  242
  {       -43,       0,      0,       24,     0,     0},   //  243
  {       -45,       0,      0,       23,     0,     0},   //  244
  {        47,       0,      0,      -24,     0,     0},   //  245
  {       -48,       0,      0,       25,     0,     0},   //  246
  {        45,       0,      0,      -26,     0,     0},   //  247
  {        56,       0,      0,      -25,     0,     0},   //  248
  {        88,       0,      0,        2,     0,     0},   //  249
  {       -75,       0,      0,        0,     0,     0},   //  250
  {        85,       0,      0,        0,     0,     0},   //  251
  {        49,       0,      0,      -26,     0,     0},   //  252
  {       -74,       0,     -3,       -1,     0,    -1},   //  253
  {       -39,       0,      0,       21,     0,     0},   //  254
  {        45,       0,      0,      -20,     0,     0},   //  255
  {        51,       0,      0,      -22,     0,     0},   //  256
  {       -40,       0,      0,       21,     0,     0},   //  257
  {        41,       0,      0,      -21,     0,     0},   //  258
  {       -42,       0,      0,       24,     0,     0},   //  259
  {       -51,       0,      0,       22,     0,     0},   //  260
  {       -42,       0,      0,       22,     0,     0},   //  261
  {        39,       0,      0,      -21,     0,     0},   //  262
  {        46,       0,      0,      -18,     0,     0},   //  263
  {       -53,       0,      0,       22,     0,     0},   //  264
  {        82,       0,      0,       -4,     0,     0},   //  265
  {        81,       0,     -1,       -4,     0,     0},   //  266
  {        47,       0,      0,      -19,     0,     0},   //  267
  {        53,       0,      0,      -23,     0,     0},   //  268
  {       -45,       0,      0,       22,     0,     0},   //  269
  {       -44,       0,      0,       -2,     0,     0},   //  270
  {       -33,       0,      0,       16,     0,     0},   //  271
  {       -61,       0,      0,        1,     0,     0},   //  272
  {        28,       0,      0,      -15,     0,     0},   //  273
  {       -38,       0,      0,       19,     0,     0},   //  274
  {       -33,       0,      0,       21,     0,     0},   //  275
  {       -60,       0,      0,        0,     0,     0},   //  276
  {        48,       0,      0,      -10,     0,     0},   //  277
  {        27,       0,      0,      -14,     0,     0},   //  278
  {        38,       0,      0,      -20,     0,     0},   //  279
  {        31,       0,      0,      -13,     0,     0},   //  280
  {       -29,       0,      0,       15,     0,     0},   //  281
  {        28,       0,      0,      -15,     0,     0},   //  282
  {       -32,       0,      0,       15,     0,     0},   //  283
  {        45,       0,      0,       -8,     0,     0},   //  284
  {       -44,       0,      0,       19,     0,     0},   //  285
  {        28,       0,      0,      -15,     0,     0},   //  286
  {       -51,       0,      0,        0,     0,     0},   //  287
  {       -36,       0,      0,       20,     0,     0},   //  288
  {        44,       0,      0,      -19,     0,     0},   //  289
  {        26,       0,      0,      -14,     0,     0},   //  290
  {       -60,       0,      0,        2,     0,     0},   //  291
  {        35,       0,      0,      -18,     0,     0},   //  292
  {       -27,       0,      0,       11,     0,     0},   //  293
  {        47,       0,      0,       -1,     0,     0},   //  294
  {        36,       0,      0,      -15,     0,     0},   //  295
  {       -36,       0,      0,       20,     0,     0},   //  296
  {       -35,       0,      0,       19,     0,     0},   //  297
  {       -37,       0,      0,       19,     0,     0},   //  298
  {        32,       0,      0,      -16,     0,     0},   //  299
  {        35,       0,      0,      -14,     0,     0},   //  300
  {        32,       0,      0,      -13,     0,     0},   //  301
  {        65,       0,      0,       -2,     0,     0},   //  302
  {        47,       0,      0,       -1,     0,     0},   //  303
  {        32,       0,      0,      -16,     0,     0},   //  304
  {        37,       0,      0,      -16,     0,     0},   //  305
  {       -30,       0,      0,       15,     0,     0},   //  306
  {       -32,       0,      0,       16,     0,     0},   //  307
  {       -31,       0,      0,       13,     0,     0},   //  308
  {        37,       0,      0,      -16,     0,     0},   //  309
  {        31,       0,      0,      -13,     0,     0},   //  310
  {        49,       0,      0,       -2,     0,     0},   //  311
  {        32,       0,      0,      -13,     0,     0},   //  312
  {        23,       0,      0,      -12,     0,     0},   //  313
  {       -43,       0,      0,       18,     0,     0},   //  314
  {        26,       0,      0,      -11,     0,     0},   //  315
  {       -32,       0,      0,       14,     0,     0},   //  316
  {       -29,       0,      0,       14,     0,     0},   //  317
  {       -27,       0,      0,       12,     0,     0},   //  318
  {        30,       0,      0,        0,     0,     0},   //  319
  {       -11,       0,      0,        5,     0,     0},   //  320
  {       -21,       0,      0,       10,     0,     0},   //  321
  {       -34,       0,      0,       15,     0,     0},   //  322
  {       -10,       0,      0,        6,     0,     0},   //  323
  {       -36,       0,      0,        0,     0,     0},   //  324
  {        -9,       0,      0,        4,     0,     0},   //  325
  {       -12,       0,      0,        5,     0,     0},   //  326
  {       -21,       0,      0,        5,     0,     0},   //  327
  {       -29,       0,      0,       -1,     0,     0},   //  328
  {       -15,       0,      0,        3,     0,     0},   //  329
  {       -20,       0,      0,        0,     0,     0},   //  330
  {        28,       0,      0,        0,     0,    -2},   //  331
  {        17,       0,      0,        0,     0,     0},   //  332
  {       -22,       0,      0,       12,     0,     0},   //  333
  {       -14,       0,      0,        7,     0,     0},   //  334
  {        24,       0,      0,      -11,     0,     0},   //  335
  {        11,       0,      0,       -6,     0,     0},   //  336
  {        14,       0,      0,       -6,     0,     0},   //  337
  {        24,       0,      0,        0,     0,     0},   //  338
  {        18,       0,      0,       -8,     0,     0},   //  339
  {       -38,       0,      0,        0,     0,     0},   //  340
  {       -31,       0,      0,        0,     0,     0},   //  341
  {       -16,       0,      0,        8,     0,     0},   //  342
  {        29,       0,      0,        0,     0,     0},   //  343
  {       -18,       0,      0,       10,     0,     0},   //  344
  {       -10,       0,      0,        5,     0,     0},   //  345
  {       -17,       0,      0,       10,     0,     0},   //  346
  {         9,       0,      0,       -4,     0,     0},   //  347
  {        16,       0,      0,       -6,     0,     0},   //  348
  {        22,       0,      0,      -12,     0,     0},   //  349
  {        20,       0,      0,        0,     0,     0},   //  350
  {       -13,       0,      0,        6,     0,     0},   //  351
  {       -17,       0,      0,        9,     0,     0},   //  352
  {       -14,       0,      0,        8,     0,     0},   //  353
  {         0,       0,      0,       -7,     0,     0},   //  354
  {        14,       0,      0,        0,     0,     0},   //  355
  {        19,       0,      0,      -10,     0,     0},   //  356
  {       -34,       0,      0,        0,     0,     0},   //  357
  {       -20,       0,      0,        8,     0,     0},   //  358
  {         9,       0,      0,       -5,     0,     0},   //  359
  {       -18,       0,      0,        7,     0,     0},   //  360
  {        13,       0,      0,       -6,     0,     0},   //  361
  {        17,       0,      0,        0,     0,     0},   //  362
  {       -12,       0,      0,        5,     0,     0},   //  363
  {        15,       0,      0,       -8,     0,     0},   //  364
  {       -11,       0,      0,        3,     0,     0},   //  365
  {        13,       0,      0,       -5,     0,     0},   //  366
  {       -18,       0,      0,        0,     0,     0},   //  367
  {       -35,       0,      0,        0,     0,     0},   //  368
  {         9,       0,      0,       -4,     0,     0},   //  369
  {       -19,       0,      0,       10,     0,     0},   //  370
  {       -26,       0,      0,       11,     0,     0},   //  371
  {         8,       0,      0,       -4,     0,     0},   //  372
  {       -10,       0,      0,        4,     0,     0},   //  373
  {        10,       0,      0,       -6,     0,     0},   //  374
  {       -21,       0,      0,        9,     0,     0},   //  375
  {       -15,       0,      0,        0,     0,     0},   //  376
  {         9,       0,      0,       -5,     0,     0},   //  377
  {       -29,       0,      0,        0,     0,     0},   //  378
  {       -19,       0,      0,       10,     0,     0},   //  379
  {        12,       0,      0,       -5,     0,     0},   //  380
  {        22,       0,      0,       -9,     0,     0},   //  381
  {       -10,       0,      0,        5,     0,     0},   //  382
  {       -20,       0,      0,       11,     0,     0},   //  383
  {       -20,       0,      0,        0,     0,     0},   //  384
  {       -17,       0,      0,        7,     0,     0},   //  385
  {        15,       0,      0,       -3,     0,     0},   //  386
  {         8,       0,      0,       -4,     0,     0},   //  387
  {        14,       0,      0,        0,     0,     0},   //  388
  {       -12,       0,      0,        6,     0,     0},   //  389
  {        25,       0,      0,        0,     0,     0},   //  390
  {       -13,       0,      0,        6,     0,     0},   //  391
  {       -14,       0,      0,        8,     0,     0},   //  392
  {        13,       0,      0,       -5,     0,     0},   //  393
  {       -17,       0,      0,        9,     0,     0},   //  394
  {       -12,       0,      0,        6,     0,     0},   //  395
  {       -10,       0,      0,        5,     0,     0},   //  396
  {        10,       0,      0,       -6,     0,     0},   //  397
  {       -15,       0,      0,        0,     0,     0},   //  398
  {       -22,       0,      0,        0,     0,     0},   //  399
  {        28,       0,      0,       -1,     0,     0},   //  400
  {        15,       0,      0,       -7,     0,     0},   //  401
  {        23,       0,      0,      -10,     0,     0},   //  402
  {        12,       0,      0,       -5,     0,     0},   //  403
  {        29,       0,      0,       -1,     0,     0},   //  404
  {       -25,       0,      0,        1,     0,     0},   //  405
  {        22,       0,      0,        0,     0,     0},   //  406
  {       -18,       0,      0,        0,     0,     0},   //  407
  {        15,       0,      0,        3,     0,     0},   //  408
  {       -23,       0,      0,        0,     0,     0},   //  409
  {        12,       0,      0,       -5,     0,     0},   //  410
  {        -8,       0,      0,        4,     0,     0},   //  411
  {       -19,       0,      0,        0,     0,     0},   //  412
  {       -10,       0,      0,        4,     0,     0},   //  413
  {        21,       0,      0,       -9,     0,     0},   //  414
  {        23,       0,      0,       -1,     0,     0},   //  415
  {       -16,       0,      0,        8,     0,     0},   //  416
  {       -19,       0,      0,        9,     0,     0},   //  417
  {       -22,       0,      0,       10,     0,     0},   //  418
  {        27,       0,      0,       -1,     0,     0},   //  419
  {        16,       0,      0,       -8,     0,     0},   //  420
  {        19,       0,      0,       -8,     0,     0},   //  421
  {         9,       0,      0,       -4,     0,     0},   //  422
  {        -9,       0,      0,        4,     0,     0},   //  423
  {        -9,       0,      0,        4,     0,     0},   //  424
  {        -8,       0,      0,        4,     0,     0},   //  425
  {        18,       0,      0,       -9,     0,     0},   //  426
  {        16,       0,      0,       -1,     0,     0},   //  427
  {       -10,       0,      0,        4,     0,     0},   //  428
  {       -23,       0,      0,        9,     0,     0},   //  429
  {        16,       0,      0,       -1,     0,     0},   //  430
  {       -12,       0,      0,        6,     0,     0},   //  431
  {        -8,       0,      0,        4,     0,     0},   //  432
  {        30,       0,      0,       -2,     0,     0},   //  433
  {        24,       0,      0,      -10,     0,     0},   //  434
  {        10,       0,      0,       -4,     0,     0},   //  435
  {       -16,       0,      0,        7,     0,     0},   //  436
  {       -16,       0,      0,        7,     0,     0},   //  437
  {        17,       0,      0,       -7,     0,     0},   //  438
  {       -24,       0,      0,       10,     0,     0},   //  439
  {       -12,       0,      0,        5,     0,     0},   //  440
  {       -24,       0,      0,       11,     0,     0},   //  441
  {       -23,       0,      0,        9,     0,     0},   //  442
  {       -13,       0,      0,        5,     0,     0},   //  443
  {       -15,       0,      0,        7,     0,     0},   //  444
  {         0,       0,  -1988,        0,     0, -1679},   //  445
  {         0,       0,    -63,        0,     0,   -27},   //  446
  {        -4,       0,      0,        0,     0,     0},   //  447
  {         0,       0,      5,        0,     0,     4},   //  448
  {         5,       0,      0,       -3,     0,     0},   //  449
  {         0,       0,    364,        0,     0,   176},   //  450
  {         0,       0,  -1044,        0,     0,  -891},   //  451
  {        -3,       0,      0,        1,     0,     0},   //  452
  {         4,       0,      0,       -2,     0,     0},   //  453
  {         0,       0,    330,        0,     0,     0},   //  454
  {         5,       0,      0,       -2,     0,     0},   //  455
  {         3,       0,      0,       -2,     0,     0},   //  456
  {        -3,       0,      0,        1,     0,     0},   //  457
  {        -5,       0,      0,        2,     0,     0},   //  458
  {         3,       0,      0,       -1,     0,     0},   //  459
  {         3,       0,      0,        0,     0,     0},   //  460
  {         3,       0,      0,        0,     0,     0},   //  461
  {         0,       0,      5,        0,     0,     0},   //  462
  {         0,       0,      0,        1,     0,     0},   //  463
  {         4,       0,      0,       -2,     0,     0},   //  464
  {         6,       0,      0,        0,     0,     0},   //  465
  {         5,       0,      0,       -2,     0,     0},   //  466
  {        -7,       0,      0,        0,     0,     0},   //  467
  {       -12,       0,      0,        0,     0,     0},   //  468
  {         5,       0,      0,       -3,     0,     0},   //  469
  {         3,       0,      0,       -1,     0,     0},   //  470
  {        -5,       0,      0,        0,     0,     0},   //  471
  {         3,       0,      0,        0,     0,     0},   //  472
  {        -7,       0,      0,        3,     0,     0},   //  473
  {         7,       0,      0,       -4,     0,     0},   //  474
  {         0,       0,    -12,        0,     0,   -10},   //  475
  {         4,       0,      0,       -2,     0,     0},   //  476
  {         3,       0,      0,       -2,     0,     0},   //  477
  {        -3,       0,      0,        2,     0,     0},   //  478
  {        -7,       0,      0,        3,     0,     0},   //  479
  {        -4,       0,      0,        2,     0,     0},   //  480
  {        -3,       0,      0,        1,     0,     0},   //  481
  {         0,       0,      0,        0,     0,     0},   //  482
  {        -3,       0,      0,        1,     0,     0},   //  483
  {         7,       0,      0,       -3,     0,     0},   //  484
  {        -4,       0,      0,        2,     0,     0},   //  485
  {         4,       0,      0,       -2,     0,     0},   //  486
  {        -5,       0,      0,        3,     0,     0},   //  487
  {         5,       0,      0,        0,     0,     0},   //  488
  {        -5,       0,      0,        2,     0,     0},   //  489
  {         5,       0,      0,       -2,     0,     0},   //  490
  {        -8,       0,      0,        3,     0,     0},   //  491
  {         9,       0,      0,        0,     0,     0},   //  492
  {         6,       0,      0,       -3,     0,     0},   //  493
  {        -5,       0,      0,        2,     0,     0},   //  494
  {         3,       0,      0,        0,     0,     0},   //  495
  {        -7,       0,      0,        0,     0,     0},   //  496
  {        -3,       0,      0,        1,     0,     0},   //  497
  {         5,       0,      0,        0,     0,     0},   //  498
  {         3,       0,      0,        0,     0,     0},   //  499
  {        -3,       0,      0,        2,     0,     0},   //  500
  {         4,       0,      0,       -2,     0,     0},   //  501
  {         3,       0,      0,       -1,     0,     0},   //  502
  {        -5,       0,      0,        2,     0,     0},   //  503
  {         4,       0,      0,       -2,     0,     0},   //  504
  {         9,       0,      0,       -3,     0,     0},   //  505
  {         4,       0,      0,        0,     0,     0},   //  506
  {         4,       0,      0,       -2,     0,     0},   //  507
  {        -3,       0,      0,        2,     0,     0},   //  508
  {        -4,       0,      0,        2,     0,     0},   //  509
  {         9,       0,      0,       -3,     0,     0},   //  510
  {        -4,       0,      0,        0,     0,     0},   //  511
  {        -4,       0,      0,        0,     0,     0},   //  512
  {         3,       0,      0,       -2,     0,     0},   //  513
  {         8,       0,      0,        0,     0,     0},   //  514
  {         3,       0,      0,        0,     0,     0},   //  515
  {        -3,       0,      0,        2,     0,     0},   //  516
  {         3,       0,      0,       -1,     0,     0},   //  517
  {         3,       0,      0,       -1,     0,     0},   //  518
  {        -3,       0,      0,        1,     0,     0},   //  519
  {         6,       0,      0,       -3,     0,     0},   //  520
  {         3,       0,      0,        0,     0,     0},   //  521
  {        -3,       0,      0,        1,     0,     0},   //  522
  {        -7,       0,      0,        0,     0,     0},   //  523
  {         9,       0,      0,        0,     0,     0},   //  524
  {        -3,       0,      0,        2,     0,     0},   //  525
  {        -3,       0,      0,        0,     0,     0},   //  526
  {        -4,       0,      0,        0,     0,     0},   //  527
  {        -5,       0,      0,        3,     0,     0},   //  528
  {       -13,       0,      0,        0,     0,     0},   //  529
  {        -7,       0,      0,        0,     0,     0},   //  530
  {        10,       0,      0,        0,     0,     0},   //  531
  {         3,       0,      0,       -1,     0,     0},   //  532
  {        10,       0,     13,        6,     0,    -5},   //  533
  {         0,       0,     30,        0,     0,    14},   //  534
  {         0,       0,   -162,        0,     0,  -138},   //  535
  {         0,       0,     75,        0,     0,     0},   //  536
  {        -7,       0,      0,        4,     0,     0},   //  537
  {        -4,       0,      0,        2,     0,     0},   //  538
  {         4,       0,      0,       -2,     0,     0},   //  539
  {         5,       0,      0,       -2,     0,     0},   //  540
  {         5,       0,      0,       -3,     0,     0},   //  541
  {        -3,       0,      0,        0,     0,     0},   //  542
  {        -3,       0,      0,        2,     0,     0},   //  543
  {        -4,       0,      0,        2,     0,     0},   //  544
  {        -5,       0,      0,        2,     0,     0},   //  545
  {         6,       0,      0,        0,     0,     0},   //  546
  {         9,       0,      0,        0,     0,     0},   //  547
  {         5,       0,      0,        0,     0,     0},   //  548
  {        -7,       0,      0,        0,     0,     0},   //  549
  {        -3,       0,      0,        1,     0,     0},   //  550
  {        -4,       0,      0,        2,     0,     0},   //  551
  {         7,       0,      0,        0,     0,     0},   //  552
  {        -4,       0,      0,        0,     0,     0},   //  553
  {         4,       0,      0,        0,     0,     0},   //  554
  {        -6,       0,     -3,        3,     0,     1},   //  555
  {         0,       0,     -3,        0,     0,    -2},   //  556
  {        11,       0,      0,        0,     0,     0},   //  557
  {         3,       0,      0,       -1,     0,     0},   //  558
  {        11,       0,      0,        0,     0,     0},   //  559
  {        -3,       0,      0,        2,     0,     0},   //  560
  {        -1,       0,      3,        3,     0,    -1},   //  561
  {         4,       0,      0,       -2,     0,     0},   //  562
  {         0,       0,    -13,        0,     0,   -11},   //  563
  {         3,       0,      6,        0,     0,     0},   //  564
  {        -7,       0,      0,        0,     0,     0},   //  565
  {         5,       0,      0,       -3,     0,     0},   //  566
  {        -3,       0,      0,        1,     0,     0},   //  567
  {         3,       0,      0,        0,     0,     0},   //  568
  {         5,       0,      0,       -3,     0,     0},   //  569
  {        -7,       0,      0,        3,     0,     0},   //  570
  {         8,       0,      0,       -3,     0,     0},   //  571
  {        -4,       0,      0,        2,     0,     0},   //  572
  {        11,       0,      0,        0,     0,     0},   //  573
  {        -3,       0,      0,        1,     0,     0},   //  574
  {         3,       0,      0,       -1,     0,     0},   //  575
  {        -4,       0,      0,        2,     0,     0},   //  576
  {         8,       0,      0,       -4,     0,     0},   //  577
  {         3,       0,      0,       -1,     0,     0},   //  578
  {        11,       0,      0,        0,     0,     0},   //  579
  {        -6,       0,      0,        3,     0,     0},   //  580
  {        -4,       0,      0,        2,     0,     0},   //  581
  {        -8,       0,      0,        4,     0,     0},   //  582
  {        -7,       0,      0,        3,     0,     0},   //  583
  {        -4,       0,      0,        2,     0,     0},   //  584
  {         3,       0,      0,       -1,     0,     0},   //  585
  {         6,       0,      0,       -3,     0,     0},   //  586
  {        -6,       0,      0,        3,     0,     0},   //  587
  {         6,       0,      0,        0,     0,     0},   //  588
  {         6,       0,      0,       -1,     0,     0},   //  589
  {         5,       0,      0,       -2,     0,     0},   //  590
  {        -5,       0,      0,        2,     0,     0},   //  591
  {        -4,       0,      0,        0,     0,     0},   //  592
  {        -4,       0,      0,        2,     0,     0},   //  593
  {         4,       0,      0,        0,     0,     0},   //  594
  {         6,       0,      0,       -3,     0,     0},   //  595
  {        -4,       0,      0,        2,     0,     0},   //  596
  {         0,       0,    -26,        0,     0,   -11},   //  597
  {         0,       0,    -10,        0,     0,    -5},   //  598
  {         5,       0,      0,       -3,     0,     0},   //  599
  {       -13,       0,      0,        0,     0,     0},   //  600
  {         3,       0,      0,       -2,     0,     0},   //  601
  {         4,       0,      0,       -2,     0,     0},   //  602
  {         7,       0,      0,       -3,     0,     0},   //  603
  {         4,       0,      0,        0,     0,     0},   //  604
  {         5,       0,      0,        0,     0,     0},   //  605
  {        -3,       0,      0,        2,     0,     0},   //  606
  {        -6,       0,      0,        2,     0,     0},   //  607
  {        -5,       0,      0,        2,     0,     0},   //  608
  {        -7,       0,      0,        3,     0,     0},   //  609
  {         5,       0,      0,       -2,     0,     0},   //  610
  {        13,       0,      0,        0,     0,     0},   //  611
  {        -4,       0,      0,        2,     0,     0},   //  612
  {        -3,       0,      0,        0,     0,     0},   //  613
  {         5,       0,      0,       -2,     0,     0},   //  614
  {       -11,       0,      0,        0,     0,     0},   //  615
  {         5,       0,      0,       -2,     0,     0},   //  616
  {         4,       0,      0,        0,     0,     0},   //  617
  {         4,       0,      0,       -2,     0,     0},   //  618
  {        -4,       0,      0,        2,     0,     0},   //  619
  {         6,       0,      0,       -3,     0,     0},   //  620
  {         3,       0,      0,       -2,     0,     0},   //  621
  {       -12,       0,      0,        0,     0,     0},   //  622
  {         4,       0,      0,        0,     0,     0},   //  623
  {        -3,       0,      0,        0,     0,     0},   //  624
  {        -4,       0,      0,        0,     0,     0},   //  625
  {         3,       0,      0,        0,     0,     0},   //  626
  {         3,       0,      0,       -1,     0,     0},   //  627
  {        -3,       0,      0,        1,     0,     0},   //  628
  {         0,       0,     -5,        0,     0,    -2},   //  629
  {        -7,       0,      0,        4,     0,     0},   //  630
  {         6,       0,      0,       -3,     0,     0},   //  631
  {        -3,       0,      0,        0,     0,     0},   //  632
  {         5,       0,      0,       -3,     0,     0},   //  633
  {         3,       0,      0,       -1,     0,     0},   //  634
  {         3,       0,      0,        0,     0,     0},   //  635
  {        -3,       0,      0,        1,     0,     0},   //  636
  {        -5,       0,      0,        3,     0,     0},   //  637
  {        -3,       0,      0,        2,     0,     0},   //  638
  {        -3,       0,      0,        2,     0,     0},   //  639
  {        12,       0,      0,        0,     0,     0},   //  640
  {         3,       0,      0,       -1,     0,     0},   //  641
  {        -4,       0,      0,        2,     0,     0},   //  642
  {         4,       0,      0,        0,     0,     0},   //  643
  {         6,       0,      0,        0,     0,     0},   //  644
  {         5,       0,      0,       -3,     0,     0},   //  645
  {         4,       0,      0,       -2,     0,     0},   //  646
  {        -6,       0,      0,        3,     0,     0},   //  647
  {         4,       0,      0,       -2,     0,     0},   //  648
  {         6,       0,      0,       -3,     0,     0},   //  649
  {         6,       0,      0,        0,     0,     0},   //  650
  {        -6,       0,      0,        3,     0,     0},   //  651
  {         3,       0,      0,       -2,     0,     0},   //  652
  {         7,       0,      0,       -4,     0,     0},   //  653
  {         4,       0,      0,       -2,     0,     0},   //  654
  {        -5,       0,      0,        2,     0,     0},   //  655
  {         5,       0,      0,        0,     0,     0},   //  656
  {        -6,       0,      0,        3,     0,     0},   //  657
  {        -6,       0,      0,        3,     0,     0},   //  658
  {        -4,       0,      0,        2,     0,     0},   //  659
  {        10,       0,      0,        0,     0,     0},   //  660
  {        -4,       0,      0,        2,     0,     0},   //  661
  {         7,       0,      0,        0,     0,     0},   //  662
  {         7,       0,      0,       -3,     0,     0},   //  663
  {         4,       0,      0,        0,     0,     0},   //  664
  {        11,       0,      0,        0,     0,     0},   //  665
  {         5,       0,      0,       -2,     0,     0},   //  666
  {        -6,       0,      0,        2,     0,     0},   //  667
  {         4,       0,      0,       -2,     0,     0},   //  668
  {         3,       0,      0,       -2,     0,     0},   //  669
  {         5,       0,      0,       -2,     0,     0},   //  670
  {        -4,       0,      0,        2,     0,     0},   //  671
  {        -4,       0,      0,        2,     0,     0},   //  672
  {        -3,       0,      0,        2,     0,     0},   //  673
  {         4,       0,      0,       -2,     0,     0},   //  674
  {         3,       0,      0,       -1,     0,     0},   //  675
  {        -3,       0,      0,        1,     0,     0},   //  676
  {        -3,       0,      0,        1,     0,     0},   //  677
  {        -3,       0,      0,        2,     0,     0}    //  678
  };


  const Long MeasTableMulSC2000B::theirMULSC[77][6] = {
  //          Longitude                Obliquity
  //  sin       t.sin     cos     cos     t.cos   sin
  {-172064161, -174666,  33386, 92052331,  9086, 15377},   //   1
  { -13170906,   -1675, -13696,  5730336, -3015, -4587},   //   2
  {  -2276413,    -234,   2796,   978459,  -485,  1374},   //   3
  {   2074554,     207,   -698,  -897492,   470,  -291},   //   4
  {   1475877,   -3633,  11817,    73871,  -184, -1924},   //   5
  {   -516821,    1226,   -524,   224386,  -677,  -174},   //   6
  {    711159,      73,   -872,    -6750,     0,   358},   //   7
  {   -387298,    -367,    380,   200728,    18,   318},   //   8
  {   -301461,     -36,    816,   129025,   -63,   367},   //   9
  {    215829,    -494,    111,   -95929,   299,   132},   //  10
  {    128227,     137,    181,   -68982,    -9,    39},   //  11
  {    123457,      11,     19,   -53311,    32,    -4},   //  12
  {    156994,      10,   -168,    -1235,     0,    82},   //  13
  {     63110,      63,     27,   -33228,     0,    -9},   //  14
  {    -57976,     -63,   -189,    31429,     0,   -75},   //  15
  {    -59641,     -11,    149,    25543,   -11,    66},   //  16
  {    -51613,     -42,    129,    26366,     0,    78},   //  17
  {     45893,      50,     31,   -24236,   -10,    20},   //  18
  {     63384,      11,   -150,    -1220,     0,    29},   //  19
  {    -38571,      -1,    158,    16452,   -11,    68},   //  20
  {     32481,       0,      0,   -13870,     0,     0},   //  21
  {    -47722,       0,    -18,      477,     0,   -25},   //  22
  {    -31046,      -1,    131,    13238,   -11,    59},   //  23
  {     28593,       0,     -1,   -12338,    10,    -3},   //  24
  {     20441,      21,     10,   -10758,     0,    -3},   //  25
  {     29243,       0,    -74,     -609,     0,    13},   //  26
  {     25887,       0,    -66,     -550,     0,    11},   //  27
  {    -14053,     -25,     79,     8551,    -2,   -45},   //  28
  {     15164,      10,     11,    -8001,     0,    -1},   //  29
  {    -15794,      72,    -16,     6850,   -42,    -5},   //  30
  {     21783,       0,     13,     -167,     0,    13},   //  31
  {    -12873,     -10,    -37,     6953,     0,   -14},   //  32
  {    -12654,      11,     63,     6415,     0,    26},   //  33
  {    -10204,       0,     25,     5222,     0,    15},   //  34
  {     16707,     -85,    -10,      168,    -1,    10},   //  35
  {     -7691,       0,     44,     3268,     0,    19},   //  36
  {    -11024,       0,    -14,      104,     0,     2},   //  37
  {      7566,     -21,    -11,    -3250,     0,    -5},   //  38
  {     -6637,     -11,     25,     3353,     0,    14},   //  39
  {     -7141,      21,      8,     3070,     0,     4},   //  40
  {     -6302,     -11,      2,     3272,     0,     4},   //  41
  {      5800,      10,      2,    -3045,     0,    -1},   //  42
  {      6443,       0,     -7,    -2768,     0,    -4},   //  43
  {     -5774,     -11,    -15,     3041,     0,    -5},   //  44
  {     -5350,       0,     21,     2695,     0,    12},   //  45
  {     -4752,     -11,     -3,     2719,     0,    -3},   //  46
  {     -4940,     -11,    -21,     2720,     0,    -9},   //  47
  {      7350,       0,     -8,      -51,     0,     4},   //  48
  {      4065,       0,      6,    -2206,     0,     1},   //  49
  {      6579,       0,    -24,     -199,     0,     2},   //  50
  {      3579,       0,      5,    -1900,     0,     1},   //  51
  {      4725,       0,     -6,      -41,     0,     3},   //  52
  {     -3075,       0,     -2,     1313,     0,    -1},   //  53
  {     -2904,       0,     15,     1233,     0,     7},   //  54
  {      4348,       0,    -10,      -81,     0,     2},   //  55
  {     -2878,       0,      8,     1232,     0,     4},   //  56
  {     -4230,       0,      5,      -20,     0,    -2},   //  57
  {     -2819,       0,      7,     1207,     0,     3},   //  58
  {     -4056,       0,      5,       40,     0,    -2},   //  59
  {     -2647,       0,     11,     1129,     0,     5},   //  60
  {     -2294,       0,    -10,     1266,     0,    -4},   //  61
  {      2481,       0,     -7,    -1062,     0,    -3},   //  62
  {      2179,       0,     -2,    -1129,     0,    -2},   //  63
  {      3276,       0,      1,       -9,     0,     0},   //  64
  {     -3389,       0,      5,       35,     0,    -2},   //  65
  {      3339,       0,    -13,     -107,     0,     1},   //  66
  {     -1987,       0,     -6,     1073,     0,    -2},   //  67
  {     -1981,       0,      0,      854,     0,     0},   //  68
  {      4026,       0,   -353,     -553,     0,  -139},   //  69
  {      1660,       0,     -5,     -710,     0,    -2},   //  70
  {     -1521,       0,      9,      647,     0,     4},   //  71
  {      1314,       0,      0,     -700,     0,     0},   //  72
  {     -1283,       0,      0,      672,     0,     0},   //  73
  {     -1331,       0,      8,      663,     0,     4},   //  74
  {      1383,       0,     -2,     -594,     0,    -2},   //  75
  {      1405,       0,      4,     -610,     0,     2},   //  76
  {      1290,       0,      0,     -556,     0,     0}    //  77
  };


  const Long MeasTableMulAber::theirMABERTD[3][18] = {
    { -1719919,	-2,	0,	-25,	0,	0,
      25,	-13,	-1, 1578094,	156,	0,
      10,	32,	1,   684187,	-358,	0},
    {	6434,	141,	0,	28007,	-107,	-1,
	25697,	-95,	-1,	-5904,	-130,	0,
	11141,	-48,	0,	-2559,	-55,	0},
    {	486,	-5,	0,	-236,	-4,	0,
	-216,	-4,	0,	-446,	5,	0,
	-94,	-2,	0,	-193,	2,	0}
  };
  const Short MeasTableMulAber::theirMABER[80][6] = {
    {	0,	0,	0,	0,	0,	0},
    {	0,	0,	0,	0,	0,	0},
    {	0,	0,	0,	0,	0,	0},
    {	31,	1,	1,	-28,	0,	-12},
    {	8,	-28,	25,	8,	11,	3},
    {	8,	-28,	-25,	-8,	-11,	-3},
    {	-25,	0,	0,	23,	0,	10},
    {	21,	0,	0,	-19,	0,	-8},
    {	16,	0,	0,	15,	1,	7},
    {	11,	-1,	-1,	-10,	-1,	-5},
    {	0,	-11,	-10,	0,	-4,	0},
    {	-11,	-2,	-2,	9,	-1,	4},
    {	-7,	-8,	-8,	6,	-3,	3},
    {	-10,	0,	0,	9,	0,	4},
    {	-9,	0,	0,	-9,	0,	-4},
    {	-9,	0,	0,	-8,	0,	-4},
    {	0,	-9,	8,	0,	3,	0},
    {	8,	0,	0,	-8,	0,	-3},
    {	-4,	-7,	-6,	4,	-3,	2},
    {	-4,	-7,	6,	-4,	3,	-2},
    {	-6,	-5,	-4,	5,	-2,	2},	// 21
    {	-1,	-1,	-2,	-7,	1,	-4},
    {	4,	-6,	-5,	-4,	-2,	-2},
    {	0,	-7,	-6,	0,	-3,	0},
    {	5,	-5,	-4,	-5,	-2,	-2},
    {	4,	-1,	1,	4,	0,	2},
    {	-4,	0,	0,	3,	0,	1},
    {	-1,	-3,	-3,	1,	-1,	0},
    {	-1,	-3,	3,	-1,	1,	0},
    {	3,	1,	0,	3,	0,	1},
    {	3,	-1,	-1,	1,	0,	1},
    {	-2,	0,	0,	-3,	0,	-1},
    {	1,	-2,	2,	1,	1,	1},
    {	-2,	-1,	0,	2,	0,	1},
    {	1,	-2,	-2,	-1,	-1,	0},
    {	2,	0,	0,	-2,	0,	-1},
    {	2,	-1,	-1,	-2,	0,	-1},
    {	2,	0,	0,	-2,	0,	-1},
    {	2,	-1,	-1,	-1,	0,	-1},
    {	0,	-2,	-1,	0,	-1,	0},
    {	0,	-1,	-1,	0,	-1,	0},	// 41
    {	-1,	-1,	-1,	1,	-1,	0},
    {	1,	0,	0,	-1,	0,	-1},
    {	0,	-1,	-1,	0,	-1,	0},
    {	-2,	0,	0,	-1,	0,	0},
    {	1,	-1,	1,	1,	0,	0},
    {	-1,	1,	1,	1,	0,	0},
    {	-1,	1,	-1,	-1,	0,	0},
    {	1,	-1,	-1,	0,	0,	0},
    {	0,	1,	1,	0,	0,	0},
    {	0,	-1,	1,	0,	0,	0},
    {	-1,	0,	0,	-1,	0,	0},
    {	1,	0,	0,	-1,	0,	0},
    {	1,	0,	0,	1,	0,	0},
    {	-1,	0,	0,	1,	0,	0},
    {	1,	0,	0,	1,	0,	0},
    {	-1,	0,	0,	1,	0,	0},
    {	1,	0,	0,	-1,	0,	0},
    {	-1,	0,	0,	1,	0,	0},
    {	1,	0,	0,	1,	0,	0},
    {	-1,	0,	0,	1,	0,	0},	// 61
    {	-1,	0,	0,	-1,	0,	0},
    {	0,	-1,	-1,	0,	0,	0},
    {	0,	1,	1,	0,	0,	0},
    {	0,	1,	-1,	0,	0,	0},
    {	0,	1,	-1,	0,	0,	0},
    {	0,	1,	1,	0,	0,	0},
    {	0,	-1,	1,	0,	0,	0},
    {	0,	1,	-1,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	0,	-1,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	0,	-1,	0,	0,	0,	0},
    {	0,	0,	1,	0,	0,	0},
    {	0,	0,	1,	0,	0,	0},
    {	0,	0,	0,	1,	0,	0},
    {	0,	0,	0,	-1,	0,	0}
  };


  // Wim Brouw's old table
  const Short MeasTableMulAber1950::theirMABER[130][6] = {
    // Order: sin(x), cos(x), sin(y), cos(y), sin(z), cos(z)
    {	1,	0,	0,	-157,	0,	358},
    {	715,	0,	0,	-656,	0,	-285},
    {	543,	0,	0,	-498,	0,	-216},
    {	-72,	0,	0,	63,	0,	35},
    {	-60,	0,	0,	55,	0,	24},
    {	38,	0,	0,	-35,	0,	-15},
    {	0,	-31,	28,	0,	12,	0},
    {	0,	0,	0,	26,	0,	-59},
    {	-26,	0,	0,	-24,	0,	-10},
    {	-22,	0,	0,	-20,	0,	-9},
    {	22,	0,	0,	-20,	0,	-9},	// 10
    {	-22,	0,	0,	20,	0,	9},
    {	0,	-18,	17,	0,	7,	0},
    {	16,	0,	0,	15,	0,	6},
    {	0,	16,	14,	0,	6,	0},
    {	0,	16,	14,	0,	6,	0},
    {	0,	12,	-1,	0,	-5,	0},
    {	-12,	0,	0,	11,	0,	5},
    {	11,	0,	0,	10,	0,	4},
    {	11,	0,	0,	-10,	0,	-4},
    {	-11,	0,	0,	-10,	0,	-4},	// 20
    {	-10,	0,	0,	-9,	0,	-4},
    {	-10,	0,	0,	9,	0,	4},
    {	0,	0,	8,	-8,	0,	-3},
    {	0,	0,	8,	-8,	0,	-3},
    {	-8,	0,	0,	7,	0,	3},
    {	-8,	0,	0,	-7,	0,	-3},
    {	0,	8,	7,	0,	3,	0},
    {	0,	-7,	-6,	0,	-3,	0},
    {	0,	-7,	-6,	0,	-3,	0},
    {	0,	7,	6,	0,	3,	0},	// 30
    {	7,	0,	0,	6,	0,	3},
    {	0,	6,	-6,	0,	-3,	0},
    {	-6,	0,	6,	0,	3,	0},
    {	6,	0,	0,	-5,	0,	-2},
    {	-6,	0,	0,	5,	0,	2},
    {	0,	5,	5,	0,	2,	0},
    {	0,	5,	5,	0,	2,	0},
    {	-5,	0,	0,	-5,	0,	-2},
    {	-5,	0,	0,	4,	0,	2},
    {	0,	5,	4,	0,	2,	0},	// 40
    {	0,	0,	0,	0,	0,	-2},
    {	0,	4,	4,	0,	2,	0},
    {	0,	-4,	-3,	0,	-1,	0},
    {	0,	-4,	-3,	0,	-1,	0},
    {	0,	3,	3,	0,	1,	0},
    {	0,	3,	-3,	0,	-1,	0},
    {	0,	3,	3,	0,	1,	0},
    {	0,	3,	3,	0,	1,	0},
    {	0,	0,	0,	0,	-1,	0},
    {	-3,	0,	0,	3,	0,	1},	// 50
    {	3,	0,	0,	-3,	0,	-1},
    {	0,	-3,	-3,	0,	-1,	0},
    {	-3,	0,	0,	3,	0,	1},
    {	-3,	0,	0,	2,	0,	1},
    {	0,	-3,	2,	0,	1,	0},
    {	-3,	0,	0,	2,	0,	1},
    {	3,	0,	0,	-2,	0,	-1},
    {	-3,	0,	0,	2,	0,	1},
    {	-2,	0,	0,	-2,	0,	0},
    {	0,	0,	0,	1,	0,	-3},	// 60
    {	0,	0,	0,	0,	0,	1},
    {	0,	2,	-2,	0,	0,	0},
    {	0,	2,	2,	0,	0,	0},
    {	0,	2,	2,	0,	0,	0},
    {	-2,	0,	0,	2,	0,	0},
    {	2,	0,	0,	2,	0,	0},
    {	0,	-2,	2,	0,	0,	0},
    {	0,	2,	2,	0,	0,	0},
    {	2,	0,	0,	-2,	0,	0},
    {	0,	-2,	-2,	0,	0,	0},	// 70
    {	0,	-2,	-2,	0,	0,	0},
    {	0,	-2,	2,	0,	0,	0},
    {	2,	0,	0,	-2,	0,	0},
    {	2,	0,	0,	-2,	0,	0},
    {	-2,	0,	0,	-2,	0,	0},
    {	2,	0,	0,	1,	0,	0},
    {	0,	1,	1,	0,	0,	0},
    {	1,	0,	0,	-1,	0,	0},
    {	-1,	0,	0,	1,	0,	0},
    {	1,	0,	0,	-1,	0,	0},	// 80
    {	0,	1,	1,	0,	0,	0},
    {	1,	0,	0,	-1,	0,	0},
    {	-1,	0,	0,	1,	0,	0},
    {	0,	-1,	-1,	0,	0,	0},
    {	0,	-1,	-1,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},	// 90
    {	0,	-1,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},	// 100
    {	-1,	0,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    {	0,	-1,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	0,	-1,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    //
    {	701,	0,	0,	-642,	0,	-280},
    {	0,	158,	152,	0,	48,	0},	// 110
    {	0,	159,	147,	0,	61,	0},
    {	34,	0,	0,	-31,	0,	-14},
    {	0,	20,	18,	0,	8,	0},
    {	-17,	0,	0,	16,	0,	7},
    {	0,	12,	11,	0,	4,	0},
    {	11,	0,	0,	-10,	0,	-4},
    {	0,	9,	8,	0,	3,	0},
    {	0,	8,	7,	2,	0,	0},
    {	-5,	0,	0,	5,	0,	2},
    {	-5,	0,	0,	4,	0,	2},	// 120
    {	0,	4,	3,	2,	0,	0},
    {	-3,	0,	0,	3,	0,	1},
    {	0,	3,	2,	0,	1,	0},
    {	-3,	0,	0,	5,	0,	-5},
    {	2,	0,	0,	-2,	0,	-1},
    {	-1,	0,	0,	0,	0,	1},
    {	0,	1,	1,	0,	0,	0},
    {	0,	1,	1,	0,	0,	0},
    {	0,	-1,	0,	0,	0,	0},
  };
  /*
  // new Rob Reid's table (some slight differences)
  const Short MeasTableMulAber::theirMABER[130][6] = {
    // Order:
    //  Delta xdot       Delta ydot     Delta zdot
    // sin,    cos,    sin,     cos,   sin,     cos

    {	1,	0,	0,	-157,	0,	358},   // T
    {	715,	0,	0,	-656,	0,	-285},
    {	543,	0,	0,	-498,	0,	-216},
    {	-72,	0,	0,	63,	0,	35},    // T
    {	-60,	0,	0,	55,	0,	24},
    {	38,	0,	0,	-35,	0,	-15},
    {	0,	-31,	28,	0,	12,	0},
    {	0,	0,	0,	26,	0,	-59},
    {	-26,	0,	0,	-24,	0,	-10},
    {	-22,	0,	0,	-20,	0,	-9},
    {	22,	0,	0,	-20,	0,	-9},	// 10
    {	-22,	0,	0,	20,	0,	9},
    {	0,	-18,	17,	0,	7,	0},
    {	16,	0,	0,	15,	0,	6},
    {	0,	16,	14,	0,	6,	0},
    {	0,	16,	14,	0,	6,	0},
    {	0,	12,	-11,	0,	-5,	0},
    {	-12,	0,	0,	11,	0,	5},
    {	11,	0,	0,	10,	0,	4},
    {	11,	0,	0,	-10,	0,	-4},
    {	-11,	0,	0,	-10,	0,	-4},	// 20
    {	-10,	0,	0,	-9,	0,	-4},
    {	-10,	0,	0,	9,	0,	4},
    {	0,	8,	-8,	0,	-3,	0},
    {	0,	8,	-8,	0,	-3,	0},
    {	-8,	0,	0,	7,	0,	3},
    {	-8,	0,	0,	-7,	0,	-3},
    {	0,	8,	7,	0,	3,	0},
    {	0,	-7,	-6,	0,	-3,	0},
    {	0,	-7,	-6,	0,	-3,	0},
    {	0,	7,	6,	0,	3,	0},	// 30
    {	7,	0,	0,	6,	0,	3},
    {	0,	6,	-6,	0,	-3,	0},
    {	-6,	0,	0,	6,	0,	3},
    {	6,	0,	0,	-5,	0,	-2},
    {	-6,	0,	0,	5,	0,	2},
    {	0,	5,	5,	0,	2,	0},
    {	0,	5,	5,	0,	2,	0},
    {	-5,	0,	0,	-5,	0,	-2},
    {	-5,	0,	0,	4,	0,	2},
    {	0,	5,	4,	0,	2,	0},	// 40
    {	0,	0,	0,	0,	0,	-2},
    {	0,	4,	4,	0,	2,	0},
    {	0,	-4,	-3,	0,	-1,	0},
    {	0,	-4,	-3,	0,	-1,	0},     // T**2
    {	0,	3,	3,	0,	1,	0},
    {	0,	3,	-3,	0,	-1,	0},
    {	0,	3,	3,	0,	1,	0},
    {	0,	3,	3,	0,	1,	0},
    {	0,	0,	0,	0,	-1,	0},
    {	-3,	0,	0,	3,	0,	1},	// 50
    {	3,	0,	0,	-3,	0,	-1},
    {	0,	-3,	-3,	0,	-1,	0},
    {	-3,	0,	0,	3,	0,	1},
    {	-3,	0,	0,	2,	0,	1},     // T
    {	0,	3,	2,	0,	1,	0},     // T**3
    {	-3,	0,	0,	2,	0,	1},
    {	3,	0,	0,	-2,	0,	-1},
    {	-3,	0,	0,	2,	0,	1},
    {	-2,	0,	0,	-2,	0,	0},
    {	0,	0,	0,	1,	0,	-3},	// 60
    {	0,	0,	0,	0,	0,	1},
    {	0,	2,	-2,	0,	0,	0},
    {	0,	2,	2,	0,	0,	0},
    {	0,	2,	2,	0,	0,	0},
    {	-2,	0,	0,	2,	0,	0},
    {	2,	0,	0,	2,	0,	0},
    {	0,	-2,	2,	0,	0,	0},
    {	0,	2,	2,	0,	0,	0},
    {	2,	0,	0,	-2,	0,	0},
    {	0,	-2,	-2,	0,	0,	0},	// 70
    {	0,	-2,	-2,	0,	0,	0},
    {	0,	-2,	2,	0,	0,	0},
    {	2,	0,	0,	-2,	0,	0},
    {	2,	0,	0,	-2,	0,	0},
    {	-2,	0,	0,	-2,	0,	0},
    {	2,	0,	0,	1,	0,	0},
    {	0,	1,	1,	0,	0,	0},
    {	1,	0,	0,	-1,	0,	0},
    {	-1,	0,	0,	1,	0,	0},
    {	1,	0,	0,	-1,	0,	0},	// 80
    {	0,	1,	1,	0,	0,	0},
    {	1,	0,	0,	-1,	0,	0},
    {	-1,	0,	0,	1,	0,	0},
    {	0,	-1,	-1,	0,	0,	0},
    {	0,	-1,	-1,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},	// 90
    {	0,	-1,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},	// 100
    {	-1,	0,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    {	0,	-1,	0,	0,	0,	0},
    {	-1,	0,	0,	0,	0,	0},
    {	1,	0,	0,	0,	0,	0},
    {	0,	-1,	0,	0,	0,	0},
    {	0,	1,	0,	0,	0,	0},
    //
    {	701,	0,	0,	-642,	0,	-280},
    {	0,	158,	152,	0,	48,	0},	// 110
    {	0,	159,	147,	0,	61,	0},
    {	34,	0,	0,	-31,	0,	-14},
    {	0,	20,	18,	0,	8,	0},     // T
    {	-17,	0,	0,	16,	0,	7},
    {	0,	12,	11,	0,	0,	4},
    {	11,	0,	0,	-10,	0,	-4},
    {	0,	9,	8,	0,	3,	0},
    {	0,	8,	7,	0,	2,	0},
    {	-5,	0,	0,	5,	0,	2},     // T
    {	-5,	0,	0,	4,	0,	2},	// 120, T
    {	0,	4,	3,	0,	2,	0},
    {	-3,	0,	0,	3,	0,	1},
    {	0,	3,	2,	0,	1,	0},
    {	-3,	0,	0,	5,	0,	-5},
    {	2,	0,	0,	-2,	0,	-1},
    {	-1,	0,	0,	0,	0,	1},
    {	0,	1,	1,	0,	0,	0},
    {	0,	1,	1,	0,	0,	0},     // T
    {	0,	-1,	0,	0,	0,	0},     // T
  };
  */

  const Short MeasTableMulAber1950::theirABERT1T[10] = {
    // Includes ABERT2T and ABERT3T, which will end up as T**2 and
    // T**3 respectively.
    0,3,44,54,55,113,119,120,128,129
  };                                 
  const Short MeasTableMulAber1950::theirABERT2T[2] = {
    44, 55
  };
  const Short MeasTableMulAber1950::theirABERT3T[1] = {
    55
  };
  const Double MeasTableMulAber1950::theirABERSPEC[2][6] = {
    {1719971.0,	0,	0,	-1577888.0,	0,	-684523.0},
    {28809.0,	0,	0,	-26429.0,	0,	-11466.0}
  };


  const Double MeasTableMulPosSunXY::theirMPOSXY[98][4] = {
    // XY, Sun(eclip)  
    { 89.996243,	49567508,	  0.007122,	49553856},
    { 90.023036,	27184119,	359.978260,	27222470},
    { 90.013001,	15543566,	359.986984,	15544429},
    { 89.998760,	 8346116,	359.994071,	 8342410},
    {270.00000,	 2938943,	270.00000,	 3386226},
    { 75.67877,	 1201314,	345.68424,	 1201189},
    {355.57060,	  757834,	265.52664,	  758691},
    { 99.6017,	  194166,	  8.6359,	  196403},
    {276.7769,	  193296,	186.7746,	  193244},
    {278.7363,	  188909,	  8.7801,	  189177},
    {279.0072,	143422,	189.2057,	143684},	// 11
    { 98.7637,	140637,	188.7514,	140598},
    { 65.8075,	118366,	335.7846,	118334},
    { 86.7821,	 81367,	356.7759,	 81306},
    {266.7770,	 76708,	356.8019,	 76713},
    {266.5938,	 62227,	176.6625,	 62623},
    { 61.365,	 43663,	331.368,	 43663},
    {156.244,	 37923,	247.016,	 38293},
    {344.302,	 31543,	251.387,	 31756},
    {262.697,	 30883,	172.654,	 30923},
    { 90.000,	30399,	  0.000,	30401},		// 21
    {103.163,	27884,	193.183,	28740},
    {278.234,	22716,	352.523,	26991},
    {258.215,	21619,	167.571,	21346},
    { 89.978,	17674,	  0.022,	17702},
    { 89.962,	13472,	179.980,	13750},
    { 80.376,	11698,	350.371,	11275},
    { 88.031,	10909,	358.047,	10900},
    { 12.883,	10542,	102.926,	10555},
    {196.373,	 9798,	106.615,	 9800},
    { 66.707,	6958,	336.709,	6959},		// 31
    {267.217,	6612,	177.247,	6607},
    {268.524,	6295,	178.506,	6287},
    { 89.718,	6288,	359.697,	6285},
    { 32.99,	5688,	302.71,		5664},
    { 90.03,	4897,	359.97,		4891},
    { 97.86,	3784,	  8.61,		3476},
    {200.32,	3055,	290.29,		3024},
    {206.86,	3104,	 79.86,		2759},
    { 90.39,	2829,	359.63,		2926},
    {280.03,	2862,	190.03,	2862},			// 41
    {280.50,	2858,	 10.52,	2858},
    {107.89,	2378,	197.94,	2382},
    {  9.32,	2317,	 99.33,	2316},
    {280.92,	2264,	190.71,	2336},
    {273.69,	2236,	  3.69,	2218},
    { 86.85,	2188,	176.81,	2187},
    { 98.81,	2003,	188.93,	2005},
    {279.68,	1968,	189.63,	1969},
    { 11.17,	1908,	279.02,	1855},
    { 47.07,	1881,	317.07,	1881},			// 51
    {260.37,	1832,	350.33,	1832},
    {346.50,	1729,	257.13,	1742},
    {174.63,	1637,	 84.58,	1638},
    {153.30,	1658,	 68.45,	1555},
    {127.97,	1574,	 38.01,	1569},
    {108.53,	1549,	 18.95,	1555},
    {278.95,	1159,	  8.95,	1159},
    { 98.80,	1144,	  8.74,	1143},
    {290.91,	1123,	197.58,	1160},
    {288.14,	1083,	 18.12,	1083},			// 61
    { 74.74,	 832,	344.74,	 832},
    { 62.37,	 792,	332.33,	 791},
    {346.10,	 749,	 77.01,	 757},
    {176.20,	 738,	 86.29,	 738},
    {204.53,	 723,	115.05,	 724},
    {309.70,	 718,	 39.74,	 720},
    {122.93,	 712,	 32.92,	 715},
    {130.84,	 701,	 40.48,	 703},
    {299.69,	 695,	209.57,	 696},
    {315.60,	687,	 45.65,	690},			// 71
    {101.30,	640,	185.30,	704},
    {105.72,	660,	195.76,	661},
    {281.71,	622,	 11.40,	630},
    { 89.65,	622,	  0.34,	629},
    {271.69,	609,	181.66,	608},
    {101.35,	585,	 11.31,	586},
    { 88.9,		516,	358.9,	516},
    {310.2,		507,	220.2,	508},
    {130.4,		498,	220.4,	497},
    {351.2,		472,	261.2,	472},		// 81
    {274.9,		458,	184.9,	455},
    {251.9,		435,	161.9,	436},
    {165.0,		428,	 75.5,	442},
    { 90.000,	12965,	270.000,	  63},		// 85: (T terms)
    {234.527,	 8975,	144.488,	8989},
    {196.650,	 7770,	106.330,	7815},
    { 16.208,	 7537,	106.256,	7550},
    { 27.402,	 6060,	297.410,	6056},
    { 16.47,	 5726,	286.565,	5733},
    {196.27,	5615,	286.26,	5613},			// 91
    {252.66,	1011,	344.13,	1029},
    { 86.80,	 875,	356.72,	 873},
    {140.69,	 726,	 50.67,	 727},
    {115.80,	 537,	205.8,	 538},
    {103.44,	 574,	343.3,	 473},
    { 13.1,	 	441,	283.1,	 440},
    {291.2,	 	321,	165.5,	 446}
  };

  const Double MeasTableMulPosSunZ::theirMPOSZ[29][2] = {
    // Z Sun(eclip)
    {246.32367,	1181234},
    {259.53511,	1127775},
    {228.2177,	 480205},
    { 90.0000,	 114995},
    {285.8981,	 112657},
    {152.407,	  32986},
    {245.217,	  27333},
    {254.184,	   9425},
    {122.335,	   8186},
    { 83.42,	   4079},
    {288.63,	3169},			// 11
    {112.58,	2595},
    {224.87,	2453},
    {127.99,	2329},
    {  3.20,	2180},
    {202.72,	1973},
    {295.15,	1452},
    { 59.99,	1358},
    {146.90,	1050},
    { 55.63,	1050},
    {283.32,	1047},			// 21
    {230.88,	 993},
    {249.34,	 872},
    {106.62,	 800},
    {114.3, 	 544},
    {216.2, 	 461},
    {323.28,	5444},			// 27: (T terms)
    {143.14,	3882},
    {270.00,	1334}
  };


  const Double MeasTableMulPosEarthXY::theirMPOSXY[189][4] = {
    // X,Y Heliocentric Earth, ecliptic
    { 90.00087234,	.9998292882e10,	359.99912749,	.9998921102e10},
    { 90.00000000,	  56114420,	270.0000000,	 244269903},
    {347.0626587,	  83525730,	257.0614968,	  83529232},
    {244.12566,	   1046663,	154.12489,	   1046697},
    { 90.0000,	    311084,	  0.0000,	    311084},
    { 89.0578,	    255249,	359.3759,	    257033},
    { 90.0271,	    213728,	179.9916,	    214746},
    {196.7828,	    168118,	106.7816,	    168129},
    { 16.7819,	    167995,	106.7836,	    168006},
    {269.9806,	    144524,	  0.0522,	    144026},
    {269.6489,	109101,	  0.3652,	113511},	// 11
    {271.4276,	 93443,	181.4222,	 93454},
    { 89.9756,	 89914,	  0.0162,	 90056},
    { 90.8784,	 73446,	179.5547,	 74492},
    {  0.8110,	 68144,	 90.8196,	 68133},
    { 98.7707,	 68441,	  9.3357,	 63930},
    {263.7081,	 61124,	173.6982,	 61135},
    {144.633,	 56652,	 54.610,	 56711},
    {270.242,	 54701,	180.200,	 54635},
    {  1.314,	 54095,	 91.322,	 54125},
    {180.963,	52049,	 91.427,	50708},		// 21
    {327.392,	45226,	 57.394,	45229},
    {228.731,	45184,	139.361,	45042},
    {147.393,	44981,	 57.415,	45025},
    {130.279,	40626,	 40.291,	40632},
    {151.268,	 7729,	172.890,	55138},
    {269.997,	25618,	180.003,	25613},
    {269.273,	25582,	179.260,	25583},
    { 75.848,	22789,	165.848,	22791},
    {280.086,	22588,	 10.214,	22779},
    {215.722,	21496,	126.916,	21950},		// 31
    {180.759,	20902,	 90.897,	20623},
    { 90.619,	19997,	  0.625,	20002},
    {162.681,	18226,	 72.680,	18227},
    {342.679,	18226,	 72.681,	18227},
    { 61.086,	17811,	150.582,	17925},
    {214.749,	16119,	118.365,	14976},
    {141.189,	15545,	 51.188,	15545},
    { 89.725,	15170,	359.704,	15278},
    { 32.362,	12894,	122.345,	12898},
    { 60.693,	12810,	150.613,	12823},		// 41
    { 95.556,	 6774,	176.881,	11874},
    { 90.000,	 8587,	  0.000,	 8587},
    { 89.374,	 8296,	359.942,	 8349},
    {182.488,	 7920,	 92.452,	 8073},
    {255.788,	 9449,	 23.725,	 5809},
    {240.578,	 7780,	151.084,	7841},
    {177.781,	 7560,	 87.769,	 7562},
    {141.375,	 7348,	 51.367,	 7352},
    {239.074,	 6535,	149.568,	 6569},
    {237.645,	 6324,	327.648,	6324},		// 51
    { 57.641,	 6298,	327.645,	6297},
    {156.246,	 6213,	243.587,	6363},
    { 31.449,	 6004,	121.422,	6008},
    {182.297,	 5734,	271.031,	6113},
    { 31.69,	 5371,	121.63,		5375},
    {270.56,	 5131,	180.56,		5130},
    {243.06,	 5109,	153.05,		5110},
    {269.99,	 5065,	180.01,		5063},
    {117.64,	 5049,	 27.28,		5064},
    {246.74,	4962,	336.74,	4962},		// 61
    {270.52,	4896,	180.48,	4893},
    {270.98,	4727,	358.98,	4934},
    {257.23,	4811,	347.17,	4842},
    { 67.00,	4808,	336.61,	4791},
    { 75.42,	4735,	165.45,	4734},
    {270.82,	4757,	359.00,	3972},
    {100.50,	4339,	  9.99,	4214},
    { 86.90,	5232,	186.38,	2571},
    {161.82,	3954,	251.82,	3955},
    {215.93,	4114,	116.98,	3753},		// 71
    {164.09,	3799,	 74.07,	3795},
    {327.42,	2048,	258.18,	4839},
    {271.81,	3688,	181.44,	3638},
    {341.29,	3612,	251.28,	3612},
    {161.29,	3611,	251.28,	3612},
    {290.01,	3255,	 20.06,	3260},
    {238.87,	3256,	332.92,	3114},
    {  2.39,	3034,	 92.35,	3035},
    { 54.87,	3248,	350.61,	2674},
    {193.66,	3255,	 78.16,	2484},		// 81
    {119.77,	2807,	209.76,	2807},
    { 95.54,	2772,	  5.54,	2773},
    {269.94,	2611,	180.06,	2600},
    { 65.10,	2493,	155.06,	2504},
    {177.63,	2325,	 87.61,	2323},
    {336.33,	2164,	246.33,	2164},
    {188.68,	2286,	 85.72,	2001},
    {213.94,	2187,	117.66,	2051},
    {  3.91,	2106,	 93.92,	2107},
    {273.05,	2169,	 19.52,	1751},		// 91
    { 90.00,	1888,	  0.00,	1888},
    { 89.73,	1922,	180.34,	1800},
    {210.03,	1857,	303.56,	1786},
    {  2.52,	1791,	 92.58,	1790},
    {333.37,	1711,	 63.34,	1711},
    {346.35,	1505,	256.42,	1508},
    {168.39,	1438,	 78.35,	1436},
    {266.20,	1433,	176.17,	1433},
    { 59.17,	1428,	330.85,	1410},
    {306.36,	1391,	 36.18,	1383},		// 101
    {152.50,	1361,	 62.47,	1360},
    {195.97,	1325,	105.86,	1337},
    {159.31,	1384,	 57.19,	1221},
    { 95.48,	1242,	  5.63,	1244},
    {  9.14,	1192,	 99.16,	1192},
    {189.20,	1192,	 99.18,	1192},
    {  3.28,	1113,	272.48,	1256},
    {304.43,	1160,	 34.43,	1161},
    {275.54,	1160,	  5.55,	1160},
    {102.72,	1099,	192.84,	1098},		// 111
    {268.97,	1051,	358.97,	1050},
    {181.30,	1053,	274.16,	1021},
    { 97.55,	1050,	  3.85,	1012},
    { 88.97,	 985,	358.97,	 985},
    {207.30,	 259,	  8.96,	1355},
    { 89.87,	 980,	180.15,	 954},
    {128.65,	 994,	 30.32,	 912},
    {305.27,	 905,	215.06,	 902},
    {  4.49,	 915,	 91.47,	 860},
    {241.17,	886,	151.21,	887},		// 121
    { 61.20,	861,	151.20,	861},
    { 66.00,	853,	333.65,	830},
    {133.29,	790,	 43.29,	790},
    {270.16,	780,	178.88,	747},
    {189.53,	823,	 83.47,	698},
    { 70.27,	755,	160.31,	756},
    { 90.04,	753,	  0.18,	753},
    {175.69,	906,	 24.30,	534},
    { 38.25,	746,	307.34,	738},
    { 79.50,	743,	349.79, 741},		// 131
    {100.84,	726,	190.47,	732},
    { 23.06,	720,	113.06,	720},
    {203.09,	715,	113.08,	714},
    {217.94,	701,	307.96,	702},
    {278.31,	693,	186.68,	692},
    { 94.95,	696,	186.28,	686},
    {269.39,	655,	  0.69,	698},
    {193.70,	403,	266.57,	855},
    {300.05,	665,	 30.03, 666},
    {344.58,	641,	254.72,	639},		// 141
    {333.46,	637,	 63.56,	636},
    {276.48,	623,	  7.36,	649},
    {152.71,	637,	144.59,	625},
    { 87.41,	640,	  4.96,	620},
    { 89.57,	626,	  0.36,	633},
    {167.54,	622,	 77.49,	623},
    { 99.41,	621,	189.40,	621},
    {279.42,	620,	189.41,	621},
    { 90.00,	620,	  0.00,	620},
    { 77.23,	634,	174.53,	605},		// 151
    { 90.00,	616,	  0.00,	616},
    { 52.74,	614,	322.76,	615},
    { 73.20,	581,	343.28,	582},
    {229.31,	575,	318.52,	584},
    {101.4,		435,	 41.26,	686},
    {268.9,		571,	178.9,	571},
    {162.3,		572,	252.7,	559},
    { 95.8,		542,	  5.8,	542},
    {265.2,		540,	174.8,	540},
    { 89.9,		535,	180.1,	528},	// 161
    {194.0,		499,	101.9,	539},
    { 29.8,		474,	310.0,	538},
    {167.1,		486,	 77.1,	486},
    {  4.2,		480,	275.8,	484},
    {123.8,		464,	215.4,	456},
    {354.9,		437,	264.9,	437},
    {335.8,		425,	 65.8,	425},
    {263.4,		307,	345.9,	501},
    { 78.2,		412,	348.2,	412},
    {299.7,		406,	 29.8,	407},	// 171
    {178.9,		402,	268.9,	402},
    { 56.2,	      	402,	326.2,	402},
    {303.9,		399,	213.8,	400},
    { 90.00000,	1234019,	 90.00000,	930472},    // 175: (T terms)
    {232.9938,	 515000,	142.9903,	515065},
    {130.051,	  12907,	 40.049,	 12908},
    {105.014,	  10686,	323.41,		  4646},
    {271.93,	   1999,	181.93,		  1999},
    { 91.93,	   1997,	181.93,		  1997},
    {107.16,	620,	197.23,	620},		// 181
    {182.69,	599,	272.68,	599},
    {  2.66,	596,	272.70,	596},
    {107.6,		486,	197.8,	488},
    {288.1,		461,	197.2,	464},
    { 46.3,		427,	316.4,	426},
    {270.00,	4147,	 90.00,	5032},    	// 187: (T^2 terms)
    {140.87,	2164,	 50.89,	2166},
    {  1.41,	 996,	255.23,	1021}
  };

  const Double MeasTableMulPosEarthZ::theirMPOSZ[32][2] = {
    //Z factors(ecliptic, helio Earth)
    {180.000,	27962},
    {256.611,	10164},
    {280.555,	 8046},
    {256.72,	 4386},
    {256.62,	 3187},
    {  0.00,	 2272},
    {275.12,	 1816},
    {293.93,	 1640},
    { 76.55,	 1447},
    {103.42,	 1431},
    {103.26,	1121},			// 11
    { 74.37,	1090},
    {180.00,	1036},
    {291.79,	 972},
    {180.19,	 914},
    {278.89,	 880},
    {169.36,	 834},
    {180.00,	 770},
    {263.31,	 720},
    { 59.06,	 692},
    {180.5,	526},			// 21
    {103.2,	520},
    {344.8,	503},
    {280.6,	475},
    {333.7,	453},
    {162.3,	429},
    {353.9,	406},
    { 76.2,	402},
    {185.12558,	2278227},		// 29: (T terms)
    { 90.000,	  54293},
    { 82.189,	  19032},		// 31
    {284.741,	   9722}		// 32: (T^2 terms)
  };


} //# end namespace
