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
//#
//# $Id$

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

#include <casacore/casa/namespace.h>

void doit()
{
    // We do the tests in another function to make it easier to check for
    // memory leaks, i.e. everything should be destructed at the end of this
    // block.
    uInt i;

    Block<Int> bi1;                        // Block::Block()
    AlwaysAssertExit(bi1.nelements() == 0);// Block::nelements()
    AlwaysAssertExit(bi1.size() == 0);
    AlwaysAssertExit(bi1.empty());
    Block<Int> bi2(100);                   // Block::Block(uInt)
    AlwaysAssertExit(bi2.nelements() == 100);
    AlwaysAssertExit(bi2.size() == 100);
    AlwaysAssertExit(!bi2.empty());
    Block<Int> bi7(0);
    AlwaysAssertExit(bi7.nelements() == 0);
    Block<Int> bi3(200,5);                 // Block::Block(uInt, T)
    AlwaysAssertExit(bi3.nelements() == 200);
    Block<Int> bi6(0, 5);
    AlwaysAssertExit(bi6.nelements() == 0);
    for (i=0; i<200; i++) {
	AlwaysAssertExit(5 == bi3[i]);     // Block::operator[](uInt)
    }
    Block<Int> bi4(bi3);                   // Block::Block(const Block<T> &)
    AlwaysAssertExit(bi4.nelements() == 200);
    for (i=0; i<200; i++) {
	AlwaysAssertExit(5 == bi4[i]);
    }
    Block<Int> bi5(bi1);
    AlwaysAssertExit(bi5.nelements() == 0);
    bi2 = bi3;                             // Block::operator=(const Block<T> &)
    AlwaysAssertExit(bi2.nelements() == 200);
    for(i=0; i < 200; i++) {
	AlwaysAssertExit(bi2[i] == 5);
    }
    bi2 = bi2;
    AlwaysAssertExit(bi2.nelements() == 200);
    for(i=0; i < 200; i++) {
	AlwaysAssertExit(bi2[i] == 5);
    }
    bi1.resize(150);                       // Block::resize(uInt);
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
    bi1.resize(100, False);                // Block::resize(uInt, Bool)
    AlwaysAssertExit(bi1.nelements() == 150);
    for(i=0; i<150; i++) {
	AlwaysAssertExit(bi1[i] == 10);
    }
    bi1.resize(100, True);
    AlwaysAssertExit(bi1.nelements() == 100);
    for(i=0; i<100; i++) {
	AlwaysAssertExit(bi1[i] == 10);
    }
    bi1.resize(150, True, True);           // Block::resize(uInt, Bool, Bool)
    AlwaysAssertExit(bi1.nelements() == 150);
    for(i=0; i<100; i++) {
	AlwaysAssertExit(bi1[i] == 10);
    }
    {                                      // Block::remove(uInt)
	Block<Int> bi(6);
	bi[0] = 0; bi[1] = 1; bi[2] = 2; bi[3] = 3; bi[4] = 4; bi[5] = 5;
	bi.remove(0);
	AlwaysAssertExit(bi[0] == 1 && bi[4] == 5);
	bi.remove(4);
	AlwaysAssertExit(bi[0] == 1 && bi[3] == 4);
	bi.remove(2);
	AlwaysAssertExit(bi[0] == 1 && bi[1] == 2 && bi[2] == 4);
    }
    // There's no portable way to test for CopyElFalse
    const Block<Int> &bi1ref = bi1;
    for(i=0; i<100; i++) {
	AlwaysAssertExit(bi1ref[i] == 10);      // Block::operator[](uInt) const
    }
    AlwaysAssertExit(&bi1[0] == bi1.storage()); // Block::storage()
    AlwaysAssertExit(bi1.storage() ==
	   bi1ref.storage());                   // Block::storage() const


    {
	Int *in1 = new int[100];
	Int *inkeep = in1;
	Block<Int> bip(100, in1);
	AlwaysAssertExit(in1 == 0);
	AlwaysAssertExit(&bip[0] == inkeep && bip.nelements() == 100);
	Int *in2 = new int[50];
	Int *inkeep2 = in2;
	bip.replaceStorage(50, in2);
	AlwaysAssertExit(in2 == 0);
	AlwaysAssertExit(&bip[0] == inkeep2 && bip.nelements() == 50);
	
    }

    {
	Int *stored = new Int[10];
	Block<Int> aliased(10, stored, False);
	AlwaysAssertExit(stored != 0);
	stored[3] = 454;
	AlwaysAssertExit(aliased[3] == 454);
	Int *stored2 = new Int[10];
	aliased.replaceStorage(10, stored2, False);
	stored2[3] = 999;
	AlwaysAssertExit(aliased[3] == 999);
	delete [] stored;
	delete [] stored2;
    }
    {                                 // Block::iterator
	Block<Int> bi(6);
	bi[0] = 0; bi[1] = 1; bi[2] = 2; bi[3] = 3; bi[4] = 4; bi[5] = 5;
	Int nrit = 0;
	for (Block<Int>::const_iterator iter=bi.begin();
	     iter!=bi.end();
	     iter++) {
	  AlwaysAssertExit(*iter == bi[nrit++]);
	}
	AlwaysAssertExit(nrit == 6);
    
	std::vector<Int> vec(bi.begin(), bi.end());
	AlwaysAssertExit(vec.size() == 6);
	AlwaysAssertExit(vec[0] == 0 && vec[1] == 1 && vec[2] == 2 &&
	       vec[3] == 3 && vec[4] == 4 && vec[5] == 5);
    }
    {
        // Check that this no longer leaks (regression test)
        Block<Int> leaker(0);
    }

                                      // Block::~Block called at end of fn
}

void testIO()
{
  Block<Int> bl(10000);
  for (uInt i=0; i<bl.size(); ++i) {
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
    Block<Int> bl2;
    aio >> bl2;
    AlwaysAssertExit (bl2.size() == bl.size());
    for (uInt i=0; i<bl2.size(); ++i) {
      AlwaysAssertExit (bl[i] == Int(i+1));
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

