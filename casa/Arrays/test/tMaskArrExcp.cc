//# tMaskArrExcp.cc: Test program for MaskedArray Exceptions
//# Copyright (C) 1994,1995,1996,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$


#include "../MaskedArray.h"
#include "../MaskArrIO.h"
#include "../MaskArrLogi.h"
#include "../MaskArrMath.h"
#include "../Array.h"
#include "../ArrayError.h"
#include "../ArrayLogical.h"
#include "../ArrayMath.h"
#include "../Vector.h"
#include "../Matrix.h"
#include "../Cube.h"
#include "../LogiArray.h"
#include "../LogiVector.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(masked_array_exceptions)

BOOST_AUTO_TEST_CASE( conformance_a_la )
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,11));
  a = 0;
  b = true;
  
  BOOST_CHECK_THROW(MaskedArray<int>(a,b), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE( conformance_la_la )
{
  Vector<int> a(10);
  LogicalArray ab(IPosition(1,10));
  LogicalArray b(IPosition(1,11));
  a = 0;
  ab = true;
  b = true;
  MaskedArray<int> mab (a,ab);
  
  BOOST_CHECK_THROW(MaskedArray<int>(mab,b), ArrayConformanceError);
}
  
BOOST_AUTO_TEST_CASE( conformance_assign_array )
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  a = 0;
  ba = 1;
  b = true;
    
  BOOST_CHECK_THROW(ba(b) = a, ArrayConformanceError);
}
  
BOOST_AUTO_TEST_CASE( conformance_assign_masked_array1 )
{
  Vector<int> a(10);
  LogicalArray ab(IPosition(1,10));
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  a = 0;
  ab = true;
  ba = 1;
  b = true;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(a(ab) = mbab, ArrayConformanceError);
}
  
BOOST_AUTO_TEST_CASE( conformance_assign_masked_array2 )
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  a = 0;
  ba = 1;
  b = true;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(a.assign_conforming( mbab ), ArrayConformanceError);
}
  
BOOST_AUTO_TEST_CASE( read_only_assign )
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  b = true;
  
  MaskedArray<int> ma (a,b,true);
  BOOST_CHECK_THROW(ma = 1, ArrayError);
}
  
BOOST_AUTO_TEST_CASE( read_only_assign1 )
{
  Vector<int> aa(10);
  aa = 0;
  const Vector<int> a(aa);
  LogicalArray b(IPosition(1,10));
  b = true;
    
  MaskedArray<int> ma (a,b,true);
  BOOST_CHECK_THROW(a(b) = a, ArrayError);
}
  
BOOST_AUTO_TEST_CASE( read_only_assign2 )
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  b = true;

  MaskedArray<int> ma (a,b);
  MaskedArray<int> mma (a,b);
  ma.setReadOnly();
  BOOST_CHECK_THROW(ma = mma, ArrayError);
}

BOOST_AUTO_TEST_CASE( read_only_getrwarray )
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  b = true;
  
  MaskedArray<int> ma (a,b,true);
  BOOST_CHECK_THROW(ma.getRWArray(), ArrayError);
}

BOOST_AUTO_TEST_CASE( read_only_getrwstorage )
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  b = true;
  
  MaskedArray<int> ma (a,b,true);
  bool deleteIt;
  BOOST_CHECK_THROW(ma.getRWArrayStorage(deleteIt), ArrayError);
}

BOOST_AUTO_TEST_CASE( read_only_putarraystorage )
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  b = true;
  
  MaskedArray<int> ma (a,b,true);
  bool deleteIt;
  const int *arrS (ma.getArrayStorage(deleteIt));
  int *arrRWS ((int *) arrS);
  BOOST_CHECK_THROW(ma.putArrayStorage (arrRWS, deleteIt), ArrayError);
}

BOOST_AUTO_TEST_CASE( get_compressed_array1 )
{
  Vector<int> a(10);
  LogicalVector b(10);
  a = 0;
  b = true;
  b(5) = false;
  
  MaskedArray<int> ma (a,b,true);
  IPosition shape (2,2,5);
  BOOST_CHECK_THROW(ma.getCompressedArray(shape), ArrayError);
}

BOOST_AUTO_TEST_CASE( get_compressed_array2 )
{
  Vector<int> a(10);
  LogicalVector b(10);
  a = 0;
  b = true;
  b(5) = false;
  
  MaskedArray<int> ma (a,b,true);
  Matrix<int> c (IPosition (2,2,5));
  BOOST_CHECK_THROW(ma.getCompressedArray(c), ArrayError);
}

BOOST_AUTO_TEST_CASE( set_compressed_array )
{
  Vector<int> a(10);
  LogicalVector b(10);
  a = 0;
  b = true;
  b(5) = false;
  
  MaskedArray<int> ma (a,b,true);
  Matrix<int> c (IPosition (2,2,5));
  c = 1;
  BOOST_CHECK_THROW(ma.setCompressedArray(c), ArrayError);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(mask_arr_math_exceptions)

BOOST_AUTO_TEST_CASE( conformance_add_assign1 )
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  a = 0;
  ba = 1;
  b = true;
  
  BOOST_CHECK_THROW(ba(b) += a, ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE( conformance_add_assign2 )
{
  Vector<int> a(10);
  Vector<int> ba(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  ba = 1;
  b = true;
  
  MaskedArray<int> bab (ba,b,true);
  BOOST_CHECK_THROW(bab += a, ArrayError);
}

BOOST_AUTO_TEST_CASE( conformance_add_assign3 )
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  a = 0;
  ba = 1;
  b = true;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(a += mbab, ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE( conformance_add_assign4 )
{
  Vector<int> a(10);
  LogicalArray ab(IPosition(1,10));
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  a = 0;
  ab = true;
  ba = 1;
  b = true;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(a(ab) += mbab, ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(readonly_add_assign)
{
  Vector<int> a(10);
  Vector<int> ba(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  ba = 1;
  b = true;
  
  MaskedArray<int> bab (ba,b,true);
  BOOST_CHECK_THROW(bab += bab, ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_add)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c.assign_conforming( mbab + a ), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_add1)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c.assign_conforming(a + mbab), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_add2)
{
  Vector<int> a(10);
  LogicalArray ab(IPosition(1,10));
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ab = true;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> maab (a,ab);
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c.assign_conforming( maab + mbab ), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_pow1)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c.assign_conforming( pow (mbab, a) ), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_pow2)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> mbab (ba,b);
  BOOST_CHECK_THROW(c.assign_conforming(pow (a, mbab)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_pow3)
{
  Vector<int> a(10);
  LogicalArray ab(IPosition(1,10));
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ab = true;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> maab (a,ab);
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c.assign_conforming( pow (maab, mbab)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_atan2_1)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c.assign_conforming( atan2 (mbab, a)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_atan2_2)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c .assign_conforming( atan2 (a, mbab)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_atan2_3)
{
  Vector<int> a(10);
  LogicalArray ab(IPosition(1,10));
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ab = true;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> maab (a,ab);
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c.assign_conforming( atan2 (maab, mbab)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_minmax)
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  indgen (a);
  b = false;
  MaskedArray<int> ma (a,b);
  int minVal, maxVal;
  IPosition minPos (1,1), maxPos (1,1);
  
  BOOST_CHECK_THROW(minMax (minVal, maxVal, minPos, maxPos, ma), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_min)
{
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  indgen (ba);
  b = false;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(min (mbab), ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_min1)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c.assign_conforming( min (mbab, a)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_min2)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c.assign_conforming( min (a, mbab)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_min3)
{
  Vector<int> a(10);
  LogicalArray ab(IPosition(1,10));
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  Vector<int> c(10);
  a = 0;
  ab = true;
  ba = 1;
  b = true;
  c = 0;
  MaskedArray<int> maab (a,ab);
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(c.assign_conforming( min (maab, mbab)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_min4)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  Vector<int> c(10);
  LogicalArray cb(IPosition(1,10));
  a = 0;
  ba = 1;
  c = 0;
  cb = true;
  
  BOOST_CHECK_THROW(::min (a, ba, c), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_min5)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  Vector<int> c(10);
  LogicalArray cb(IPosition(1,10));
  a = 0;
  ba = 1;
  c = 0;
  cb = true;
  
  BOOST_CHECK_THROW(::min (ba, a, c), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_sum)
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  indgen (a);
  b = false;
  MaskedArray<int> ma (a,b);
  
  BOOST_CHECK_THROW(sum (ma), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_sumsquares)
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  indgen (a);
  b = false;
  MaskedArray<int> ma (a,b);
  
  BOOST_CHECK_THROW(sumsquares (ma), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_product)
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  indgen (a);
  b = false;
  MaskedArray<int> ma (a,b);
  
  BOOST_CHECK_THROW(product (ma), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_mean)
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  indgen (a);
  b = false;
  MaskedArray<int> ma (a,b);
  
  BOOST_CHECK_THROW(mean (ma), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_variance)
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  int mean (0);
  indgen (a);
  b = false;
  b(IPosition(1,0)) = true;
  MaskedArray<int> ma (a,b);
  
  BOOST_CHECK_THROW(variance (ma, mean), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_avdev)
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  int mean (0);
  indgen (a);
  b = false;
  MaskedArray<int> ma (a,b);
  
  BOOST_CHECK_THROW(avdev (ma, mean), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_median)
{
  Vector<int> a(10);
  LogicalArray b(IPosition(1,10));
  indgen (a);
  b = false;
  MaskedArray<int> ma (a,b);
  
  BOOST_CHECK_THROW(median (ma, false), ArrayError);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(mask_arr_logi_exceptions)

BOOST_AUTO_TEST_CASE(conformance_allle1 )
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  a = 0;
  ba = 1;
  b = true;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(allLE (mbab, a), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_alle1)
{
  Vector<int> a(10);
  Vector<int> ba(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  ba = 1;
  b = false;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(allLE (mbab, a), ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_allle2)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  a = 0;
  ba = 1;
  b = true;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(allLE (a, mbab), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_alle2)
{
  Vector<int> a(10);
  Vector<int> ba(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  ba = 1;
  b = false;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(allLE (a, mbab), ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_allle3)
{
  Vector<int> a(10);
  LogicalArray ab(IPosition(1,10));
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  a = 0;
  ab = true;
  ba = 1;
  b = true;
  MaskedArray<int> maab (a,ab);
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(allLE (maab, mbab), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_alle3)
{
  Vector<int> a(10);
  LogicalArray ab(IPosition(1,10));
  Vector<int> ba(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  ab = false;
  ba = 1;
  b = true;
  MaskedArray<int> maab (a,ab);
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(allLE (maab, mbab), ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_anyand1)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray c(IPosition(1,11));
  LogicalArray b(IPosition(1,11));
  a = true;
  c = false;
  b = true;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyAND (mcb, a), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_anyand1)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  a = true;
  c = false;
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyAND (mcb, a), ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_anyand2)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray c(IPosition(1,11));
  LogicalArray b(IPosition(1,11));
  a = true;
  c = false;
  b = true;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyAND (a, mcb), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_anyand2)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  a = false;
  c = true;
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyAND (a, mcb), ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_anyand3)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray ab(IPosition(1,10));
  LogicalArray c(IPosition(1,11));
  LogicalArray b(IPosition(1,11));
  a = false;
  ab = true;
  c = true;
  b = true;
  MaskedLogicalArray maab (a,ab);
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyAND (maab, mcb), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_anyand3)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray ab(IPosition(1,10));
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  a = false;
  ab = true;
  c = true;
  b = false;
  MaskedLogicalArray maab (a,ab);
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyAND (maab, mcb), ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_anyor1)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray c(IPosition(1,11));
  LogicalArray b(IPosition(1,11));
  a = false;
  c = true;
  b = true;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyOR (mcb, a), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_anyor1)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  a = false;
  c = true;
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyOR (mcb, a), ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_anyor2)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray c(IPosition(1,11));
  LogicalArray b(IPosition(1,11));
  a = false;
  c = true;
  b = true;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyOR (a, mcb), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_anyor2)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  a = false;
  c = true;
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyOR (a, mcb), ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_anyor3)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray ab(IPosition(1,10));
  LogicalArray c(IPosition(1,11));
  LogicalArray b(IPosition(1,11));
  a = false;
  ab = true;
  c = true;
  b = true;
  MaskedLogicalArray maab (a,ab);
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyOR (maab, mcb), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_anyor3)
{
  LogicalArray a(IPosition(1,10));
  LogicalArray ab(IPosition(1,10));
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  a = false;
  ab = false;
  c = true;
  b = true;
  MaskedLogicalArray maab (a,ab);
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyOR (maab, mcb), ArrayError);
}

BOOST_AUTO_TEST_CASE(conformance_le1)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  LogicalArray cb(IPosition(1,10));
  a = 0;
  ba = 1;
  b = true;
  cb = false;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(cb.assign_conforming(  (mbab <= a)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_le2)
{
  Vector<int> a(10);
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  LogicalArray cb(IPosition(1,10));
  a = 0;
  ba = 1;
  b = true;
  cb = false;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(cb.assign_conforming(  (a <= mbab)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(conformance_le3)
{
  Vector<int> a(10);
  LogicalArray ab(IPosition(1,10));
  Vector<int> ba(11);
  LogicalArray b(IPosition(1,11));
  LogicalArray cb(IPosition(1,10));
  a = 0;
  ab = true;
  ba = 1;
  b = true;
  cb = false;
  MaskedArray<int> maab (a,ab);
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(cb.assign_conforming( (maab <= mbab)), ArrayConformanceError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_alland1)
{
  Vector<int> a(10);
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  indgen (a);
  c = (a<5);
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(allAND (mcb, true), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_alland2)
{
  Vector<int> a(10);
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  indgen (a);
  c = (a<5);
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(allAND (true, mcb), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_allor1)
{
  Vector<int> a(10);
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  indgen (a);
  c = (a<5);
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(allOR (mcb, false), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_allor2)
{
  Vector<int> a(10);
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  indgen (a);
  c = (a<5);
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(allOR (false, mcb), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_allle1)
{
  Vector<int> a(10);
  Vector<int> ba(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  ba = 1;
  b = false;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(allLE (mbab, 7), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_allle2)
{
  Vector<int> a(10);
  Vector<int> ba(10);
  LogicalArray b(IPosition(1,10));
  a = 0;
  ba = 1;
  b = false;
  MaskedArray<int> mbab (ba,b);
  
  BOOST_CHECK_THROW(allLE (7, mbab), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_anyand4)
{
  Vector<int> a(10);
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  indgen (a);
  c = (a<5);
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyAND (mcb, true), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_anyand5)
{
  Vector<int> a(10);
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  indgen (a);
  c = (a<5);
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyAND (true, mcb), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_anyor4)
{
  Vector<int> a(10);
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  indgen (a);
  c = (a<5);
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyOR (mcb, false), ArrayError);
}

BOOST_AUTO_TEST_CASE(insufficient_elements_anyor5)
{
  Vector<int> a(10);
  LogicalArray c(IPosition(1,10));
  LogicalArray b(IPosition(1,10));
  indgen (a);
  c = (a<5);
  b = false;
  MaskedLogicalArray mcb (c,b);
  
  BOOST_CHECK_THROW(anyOR (false, mcb), ArrayError);
}

BOOST_AUTO_TEST_SUITE_END()
