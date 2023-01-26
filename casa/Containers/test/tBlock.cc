//# tBlock.cc: This program tests the Block class
//# Copyright (C) 1993,1994,1995,1996,2000,2001
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

//# Includes

// Turn on array checking in the inline Block index operators
#if !defined(AIPS_ARRAY_INDEX_CHECK)
#define AIPS_ARRAY_INDEX_CHECK
#endif

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <vector>
#include <limits>
#include <stdint.h>
#include <casacore/casa/namespace.h>

namespace {

struct LifecycleChecker {
  LifecycleChecker() {
    if (ctor_count >= ctor_error_trigger) {
      throw 0;
    }
    ++ctor_count;
  }
  LifecycleChecker(LifecycleChecker const &) {
    if (ctor_count >= ctor_error_trigger) {
      throw 0;
    }
    ++ctor_count;
  }
  ~LifecycleChecker() {
    ++dtor_count;
  }
  LifecycleChecker & operator =(LifecycleChecker const&) {
    if (assign_count >= assign_error_trigger) {
      throw 0;
    }
    ++assign_count;
    return *this;
  }
  static void clear() {
    assign_count = ctor_count = dtor_count = 0;
    assign_error_trigger = ctor_error_trigger = std::numeric_limits<size_t>::max();
  }
  static size_t assign_count;
  static size_t ctor_count;
  static size_t dtor_count;
  static size_t ctor_error_trigger;
  static size_t assign_error_trigger;
};

size_t LifecycleChecker::assign_count = 0;
size_t LifecycleChecker::ctor_count = 0;
size_t LifecycleChecker::dtor_count = 0;
size_t LifecycleChecker::ctor_error_trigger = std::numeric_limits<size_t>::max();
size_t LifecycleChecker::assign_error_trigger = std::numeric_limits<size_t>::max();

}

void doit()
{
    // We do the tests in another function to make it easier to check for
    // memory leaks, i.e. everything should be destructed at the end of this
    // block.
    uint32_t i;

    Block<int32_t> bi1;                        // Block::Block()
    AlwaysAssertExit(bi1.nelements() == 0);// Block::nelements()
    AlwaysAssertExit(bi1.size() == 0);
    AlwaysAssertExit(bi1.empty());
    for (i = 0; i < 200; i++) {
      Block<int32_t> bi(AllocSpec<AlignedAllocator<int32_t, 32> >::value);
      AlwaysAssertExit(0 == bi.storage());
      bi.resize(3);
      AlwaysAssertExit(0 == ((intptr_t)bi.storage()) % 32);
    }
    Block<int32_t> bi2(100);                   // Block::Block(uint32_t)
    AlwaysAssertExit(bi2.nelements() == 100);
    AlwaysAssertExit(bi2.size() == 100);
    AlwaysAssertExit(!bi2.empty());
    for (i = 0; i < 200; i++) {
      Block<int32_t> bi(100UL, AllocSpec<AlignedAllocator<int32_t, 32> >::value);
      AlwaysAssertExit(bi.nelements() == 100);
      AlwaysAssertExit(bi.size() == 100);
      AlwaysAssertExit(bi.capacity() == 100);
      AlwaysAssertExit(!bi.empty());
      AlwaysAssertExit(0 == ((intptr_t)bi.storage()) % 32);
      int32_t *p = bi.storage();
      bi.resize(100UL);
      AlwaysAssertExit(p == bi.storage());
      AlwaysAssertExit(0 == ((intptr_t)bi.storage()) % 32);
      bi.resize(95UL, true);
      AlwaysAssertExit(0 == ((intptr_t)bi.storage()) % 32);
      bi[44] = 9876;
      bi.resize(91UL, true, true);
      AlwaysAssertExit(9876 == bi[44]);
      AlwaysAssertExit(0 == ((intptr_t)bi.storage()) % 32);
      bi.resize(89UL, true, false, ArrayInitPolicies::INIT);
      AlwaysAssertExit(0 == bi[0] && 0 == bi[30]);
      AlwaysAssertExit(0 == ((intptr_t)bi.storage()) % 32);
      p = bi.storage();
      bi.resize(87UL, false, true, ArrayInitPolicies::NO_INIT);
      AlwaysAssertExit(p == bi.storage());
      AlwaysAssertExit(0 == ((intptr_t)bi.storage()) % 32);
      bi.resize(105UL);
      AlwaysAssertExit(0 == ((intptr_t)bi.storage()) % 32);
    }
    {
      LifecycleChecker::clear();
      {
        Block<LifecycleChecker> b(200, ArrayInitPolicies::INIT);
      }
      AlwaysAssertExit(200 <= LifecycleChecker::ctor_count);
      AlwaysAssertExit(200 <= LifecycleChecker::dtor_count);
    }
    {
      LifecycleChecker::clear();
      LifecycleChecker::ctor_error_trigger = 10;
      try {
        Block<LifecycleChecker> b(20, ArrayInitPolicies::INIT);
        AlwaysAssertExit(false);
      } catch (...) {
        AlwaysAssertExit(LifecycleChecker::ctor_count == LifecycleChecker::dtor_count);
      }
    }
    {
      LifecycleChecker::clear();
      LifecycleChecker::ctor_error_trigger = 20 + 5;
      try {
        Block<LifecycleChecker> b(20, ArrayInitPolicies::INIT);
        b.resize(15, true, true, ArrayInitPolicies::NO_INIT);
        AlwaysAssertExit(false);
      } catch (...) {
        AlwaysAssertExit(LifecycleChecker::ctor_count == LifecycleChecker::dtor_count);
      }
    }
    {
      LifecycleChecker::clear();
      LifecycleChecker::ctor_error_trigger = 20 + 5;
      try {
        Block<LifecycleChecker> b(20, ArrayInitPolicies::INIT);
        b.resize(15, true, true, ArrayInitPolicies::INIT);
        AlwaysAssertExit(false);
      } catch (...) {
        AlwaysAssertExit(LifecycleChecker::ctor_count == LifecycleChecker::dtor_count);
      }
    }
    {
      LifecycleChecker::clear();
      LifecycleChecker::ctor_error_trigger = 10 + 5;
      try {
        Block<LifecycleChecker> b(10, ArrayInitPolicies::INIT);
        b.resize(15, true, true, ArrayInitPolicies::NO_INIT);
        AlwaysAssertExit(false);
      } catch (...) {
        AlwaysAssertExit(LifecycleChecker::ctor_count == LifecycleChecker::dtor_count);
      }
    }
    {
      LifecycleChecker::clear();
      LifecycleChecker::ctor_error_trigger = 10 + 10 + 3;
      try {
        Block<LifecycleChecker> b(10, ArrayInitPolicies::INIT);
        b.resize(15, true, true, ArrayInitPolicies::NO_INIT);
      } catch (...) {
        AlwaysAssertExit(false);
      }
      AlwaysAssertExit(LifecycleChecker::ctor_count + 5 == LifecycleChecker::dtor_count);
    }
    {
      LifecycleChecker::clear();
      LifecycleChecker::ctor_error_trigger = 10 + 10 + 3;
      try {
        Block<LifecycleChecker> b(10, ArrayInitPolicies::INIT);
        b.resize(15, true, true, ArrayInitPolicies::INIT);
        AlwaysAssertExit(false);
      } catch (...) {
        AlwaysAssertExit(LifecycleChecker::ctor_count == LifecycleChecker::dtor_count);
      }
    }
    {
      LifecycleChecker::clear();
      {
        Block<LifecycleChecker> b(200, ArrayInitPolicies::NO_INIT, AllocSpec<NewDelAllocator<LifecycleChecker> >::value);
      }
      AlwaysAssertExit(200 <= LifecycleChecker::ctor_count);
      AlwaysAssertExit(200 <= LifecycleChecker::dtor_count);
      AlwaysAssertExit(LifecycleChecker::ctor_count == LifecycleChecker::dtor_count);
    }
    {
      LifecycleChecker::clear();
      {
        Block<LifecycleChecker> b(200, ArrayInitPolicies::NO_INIT, AllocSpec<AlignedAllocator<LifecycleChecker, 32> >::value);
      }
      AlwaysAssertExit(0 == LifecycleChecker::ctor_count);
      AlwaysAssertExit(200 <= LifecycleChecker::dtor_count);
    }
    {
      LifecycleChecker::clear();
      {
        Block<LifecycleChecker> b(200, ArrayInitPolicies::INIT);
      }
      AlwaysAssertExit(200 <= LifecycleChecker::ctor_count);
      AlwaysAssertExit(200 <= LifecycleChecker::dtor_count);
      AlwaysAssertExit(LifecycleChecker::ctor_count == LifecycleChecker::dtor_count);
    }
    {
      LifecycleChecker::clear();
      {
        Block<LifecycleChecker> bi(200);
      }
      AlwaysAssertExit(200 <= LifecycleChecker::ctor_count);
      AlwaysAssertExit(200 <= LifecycleChecker::dtor_count);
      AlwaysAssertExit(LifecycleChecker::ctor_count == LifecycleChecker::dtor_count);
    }
    {
      Block<int32_t> ba(10);
      ba = 10;
      Block<int32_t> bb(ba);
      AlwaysAssertExit(10 == bb[0] && 10 == bb[9]);

      int32_t *p = new int32_t[20];
      ba.prohibitChangingAllocator();
      try {
        ba.replaceStorage(20, p, true);
        AlwaysAssertExit(false);
      } catch (std::exception const &) {
      }
      try {
        ba.replaceStorage(20, p, false);
        AlwaysAssertExit(false);
      } catch (std::exception const &) {
      }
      ba.permitChangingAllocator();
      try {
        ba.replaceStorage(20, p, true);
      } catch (std::exception const &) {
        AlwaysAssertExit(false);
      }
      AlwaysAssertExit(0 == p);

      bb.prohibitChangingAllocator();
      p = DefaultAllocator<int32_t>::type().allocate(20);
      try {
        ba.replaceStorage(20, p, true, AllocSpec<DefaultAllocator<int32_t> >::value);
      } catch (std::exception const &) {
        AlwaysAssertExit(false);
      }
      AlwaysAssertExit(0 == p);
    }
    Block<int32_t> bi7(0);
    AlwaysAssertExit(bi7.nelements() == 0);
    Block<int32_t> bi3(200,5);                 // Block::Block(uint32_t, T)
    AlwaysAssertExit(bi3.nelements() == 200);
    Block<int32_t> bi6(0, 5);
    AlwaysAssertExit(bi6.nelements() == 0);
    for (i=0; i<200; i++) {
	AlwaysAssertExit(5 == bi3[i]);     // Block::operator[](uint32_t)
    }
    Block<int32_t> bi4(bi3);                   // Block::Block(const Block<T> &)
    AlwaysAssertExit(bi4.nelements() == 200);
    for (i=0; i<200; i++) {
	AlwaysAssertExit(5 == bi4[i]);
    }
    Block<int32_t> bi5(bi1);
    AlwaysAssertExit(bi5.nelements() == 0);
    bi2 = bi3;                             // Block::operator=(const Block<T> &)
    AlwaysAssertExit(bi2.nelements() == 200);
    for(i=0; i < 200; i++) {
	AlwaysAssertExit(bi2[i] == 5);
    }
    const Block<int32_t>& bi2ref(bi2);         // Use ref in self-assigment
    bi2 = bi2ref;                          // to avoid compiler warning
    AlwaysAssertExit(bi2.nelements() == 200);
    for(i=0; i < 200; i++) {
	AlwaysAssertExit(bi2[i] == 5);
    }
    bi1.resize(150);                       // Block::resize(uint32_t);
    bi1.set(10);                           // Block::set(T);
    AlwaysAssertExit(bi1.nelements() == 150);
    for(i=0; i<150; i++) {
	AlwaysAssertExit(bi1[i] == 10);
    }
    bi1.resize(100);
    AlwaysAssertExit(bi1.nelements() == 150);
    for(i=0; i<150; i++) {
	AlwaysAssertExit(bi1[i] == 10);
    }
    bi1.resize(100, false);                // Block::resize(uint32_t, bool)
    AlwaysAssertExit(bi1.nelements() == 150);
    for(i=0; i<150; i++) {
	AlwaysAssertExit(bi1[i] == 10);
    }
    bi1.resize(100, true);
    AlwaysAssertExit(bi1.nelements() == 100);
    for(i=0; i<100; i++) {
	AlwaysAssertExit(bi1[i] == 10);
    }
    bi1.resize(150, true, true);           // Block::resize(uint32_t, bool, bool)
    AlwaysAssertExit(bi1.nelements() == 150);
    for(i=0; i<100; i++) {
	AlwaysAssertExit(bi1[i] == 10);
    }
    {                                      // Block::remove(uint32_t)
	Block<int32_t> bi(6);
	bi[0] = 0; bi[1] = 1; bi[2] = 2; bi[3] = 3; bi[4] = 4; bi[5] = 5;
	bi.remove(0);
	AlwaysAssertExit(bi[0] == 1 && bi[4] == 5);
	bi.remove(4);
	AlwaysAssertExit(bi[0] == 1 && bi[3] == 4);
	bi.remove(2);
	AlwaysAssertExit(bi[0] == 1 && bi[1] == 2 && bi[2] == 4);
    }
    // There's no portable way to test for CopyElFalse
    const Block<int32_t> &bi1ref = bi1;
    for(i=0; i<100; i++) {
	AlwaysAssertExit(bi1ref[i] == 10);      // Block::operator[](uint32_t) const
    }
    AlwaysAssertExit(&bi1[0] == bi1.storage()); // Block::storage()
    AlwaysAssertExit(bi1.storage() ==
	   bi1ref.storage());                   // Block::storage() const


    {
	int32_t *in1 = new int[100];
	int32_t *inkeep = in1;
	Block<int32_t> bip(100, in1);
	AlwaysAssertExit(in1 == 0);
	AlwaysAssertExit(&bip[0] == inkeep && bip.nelements() == 100);
	int32_t *in2 = new int[50];
	int32_t *inkeep2 = in2;
	bip.replaceStorage(50, in2);
	AlwaysAssertExit(in2 == 0);
	AlwaysAssertExit(&bip[0] == inkeep2 && bip.nelements() == 50);
	
    }

    {
	int32_t *stored = new int32_t[10];
	Block<int32_t> aliased(10, stored, false);
	AlwaysAssertExit(stored != 0);
	stored[3] = 454;
	AlwaysAssertExit(aliased[3] == 454);
	int32_t *stored2 = new int32_t[10];
	aliased.replaceStorage(10, stored2, false);
	stored2[3] = 999;
	AlwaysAssertExit(aliased[3] == 999);
	delete [] stored;
	delete [] stored2;
    }
    {                                 // Block::iterator
	Block<int32_t> bi(6);
	bi[0] = 0; bi[1] = 1; bi[2] = 2; bi[3] = 3; bi[4] = 4; bi[5] = 5;
	int32_t nrit = 0;
	for (Block<int32_t>::const_iterator iter=bi.begin();
	     iter!=bi.end();
	     iter++) {
	  AlwaysAssertExit(*iter == bi[nrit++]);
	}
	AlwaysAssertExit(nrit == 6);
    
	std::vector<int32_t> vec(bi.begin(), bi.end());
	AlwaysAssertExit(vec.size() == 6);
	AlwaysAssertExit(vec[0] == 0 && vec[1] == 1 && vec[2] == 2 &&
	       vec[3] == 3 && vec[4] == 4 && vec[5] == 5);
    }
    {
        // Check that this no longer leaks (regression test)
        Block<int32_t> leaker(0);
    }

                                      // Block::~Block called at end of fn
}

void testIO()
{
  Block<int32_t> bl(10000);
  for (uint32_t i=0; i<bl.size(); ++i) {
    bl[i] = i+1;
  }
  {
    // Create AipsIO file and write block.
    AipsIO aio("tBlock_tmp.dat", ByteIO::New);
    aio << bl;
  }
  {
    // Read back block and check it.
    AipsIO aio("tBlock_tmp.dat");
    Block<int32_t> bl2;
    aio >> bl2;
    AlwaysAssertExit (bl2.size() == bl.size());
    for (uint32_t i=0; i<bl2.size(); ++i) {
      AlwaysAssertExit (bl[i] == int32_t(i+1));
    }
  }
}

int main()
{
    doit();
    testIO();
    cout << "OK\n";
    return 0;
}

