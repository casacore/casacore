//# tBlock.cc: This program tests the Block class
//# Copyright (C) 1993,1994,1995,1996,2000
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

#include <aips/aips.h>
#include <aips/Containers/Block.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>

// Test the Block class
#include <assert.h>

void doit()
{
    // We do the tests in another function to make it easier to check for
    // memory leaks, i.e. everything should be destructed at the end of this
    // block.
    uInt i;

    Block<Int> bi1;                   // Block::Block()
    assert(bi1.nelements() == 0);     // Block::nelements()
    Block<Int> bi2(100);              // Block::Block(uInt)
    assert(bi2.nelements() == 100);
    Block<Int> bi7(0);
    assert(bi7.nelements() == 0);
    Block<Int> bi3(200,5);            // Block::Block(uInt, T)
    assert(bi3.nelements() == 200);
    Block<Int> bi6(0, 5);
    assert(bi6.nelements() == 0);
    for (i=0; i<200; i++) {
	assert(5 == bi3[i]);          // Block::operator[](uInt)
    }
    Block<Int> bi4(bi3);              // Block::Block(const Block<T> &)
    assert(bi4.nelements() == 200);
    for (i=0; i<200; i++) {
	assert(5 == bi4[i]);
    }
    Block<Int> bi5(bi1);
    assert(bi5.nelements() == 0);
    bi2 = bi3;                       // Block::operator=(const Block<T> &)
    assert(bi2.nelements() == 200);
    for(i=0; i < 200; i++) {
	assert(bi2[i] == 5);
    }
    bi2 = bi2;
    assert(bi2.nelements() == 200);
    for(i=0; i < 200; i++) {
	assert(bi2[i] == 5);
    }
    bi1.resize(150);                  // Block::resize(uInt);
    bi1.set(10);                      // Block::set(T);
    assert(bi1.nelements() == 150);
    for(i=0; i<150; i++) {
	assert(bi1[i] == 10);
    }
    bi1.resize(100);
    assert(bi1.nelements() == 150);
    for(i=0; i<150; i++) {
	assert(bi1[i] == 10);
    }
    bi1.resize(100, False);           // Block::resize(uInt, Bool)
    assert(bi1.nelements() == 150);
    for(i=0; i<150; i++) {
	assert(bi1[i] == 10);
    }
    bi1.resize(100, True);
    assert(bi1.nelements() == 100);
    for(i=0; i<100; i++) {
	assert(bi1[i] == 10);
    }
    bi1.resize(150, True, True);      // Block::resize(uInt, Bool, Bool)
    assert(bi1.nelements() == 150);
    for(i=0; i<100; i++) {
	assert(bi1[i] == 10);
    }
    {                                 // Block::remove(uInt)
	Block<Int> bi(6);
	bi[0] = 0; bi[1] = 1; bi[2] = 2; bi[3] = 3; bi[4] = 4; bi[5] = 5;
	bi.remove(0);
	assert(bi[0] == 1 && bi[4] == 5);
	bi.remove(4);
	assert(bi[0] == 1 && bi[3] == 4);
	bi.remove(2);
	assert(bi[0] == 1 && bi[1] == 2 && bi[2] == 4);
    }
    // There's no portable way to test for CopyElFalse
    const Block<Int> &bi1ref = bi1;
    for(i=0; i<100; i++) {
	assert(bi1ref[i] == 10);     // Block::operator[](uInt) const
    }
    assert(&bi1[0] == bi1.storage()); // Block::storage()
    assert(bi1.storage() ==
	   bi1ref.storage());         // Block::storage() const


    {
	Int *in1 = new int[100];
	Int *inkeep = in1;
	Block<Int> bip(100, in1);
	assert(in1 == 0);
	assert(&bip[0] == inkeep && bip.nelements() == 100);
	Int *in2 = new int[50];
	Int *inkeep2 = in2;
	bip.replaceStorage(50, in2);
	assert(in2 == 0);
	assert(&bip[0] == inkeep2 && bip.nelements() == 50);
	
    }

    {
	Int *stored = new Int[10];
	Block<Int> aliased(10, stored, False);
	assert(stored != 0);
	stored[3] = 454;
	assert(aliased[3] == 454);
	Int *stored2 = new Int[10];
	aliased.replaceStorage(10, stored2, False);
	stored2[3] = 999;
	assert(aliased[3] == 999);
	delete [] stored;
	delete [] stored2;
    }
    {
        // Check that this no longer leaks (regression test)
        Block<Int> leaker(0);
    }

                                      // Block::~Block called at end of fn
}

void doItPtr()
{
    PtrBlock<Int*> block1(5), block2(5);
    assert (block1.nelements() == 5);
    for (uInt i=0; i<block1.nelements(); i++) {
        block1[i] = new Int;
        block2[i] = new Int;
    }
    PtrBlock<Int*> block3(block2);
    block1.deleteAll (3);
    assert (block1.nelements() == 5);
    assert (block1[0] == 0);
    assert (block1[1] == 0);
    assert (block1[2] == 0);
    assert (block1[3] != 0);
    assert (block1[4] != 0);
    block1.deleteAll();
    assert (block1[2] == 0);
    assert (block1[3] == 0);
    assert (block1[4] == 0);

    block2.removeAndDelete (2, True);
    assert (block2.nelements() == 4);
    assert (block2[0] == block3[0]);
    assert (block2[1] == block3[1]);
    assert (block2[2] == block3[3]);
    assert (block2[3] == block3[4]);
    block2.deleteAll();
}

main()
{
    doit();
    doItPtr();
    cout << "OK\n";
    return 0;
}

