//# tCopy.cc: This program tests the functions in Copy.h
//# Copyright (C) 1994,1995,1996,1998,1999,2001
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


#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

// A random class to copy
#include <casacore/casa/Containers/Block.h>


#include <casacore/casa/namespace.h>
// This program tests all functions in Copy.h (objset, objcopy and objmove).
// Especially objmove is tested in all kinds of circumstances
// (overlapping, non-overlapping, strides).
//
// The tests are mainly done for a builtin data type, for which the
// straightforward copy/move operations are inlined as memcpy/memmove.
// To complete the tests, they are also done for data types Block<uInt>
// and void*.

int main()
{
    Int size=100;             // should be a multiple of 4
    Int* ia = new int[size];

    objset(ia, 99, size);

    // Test setting values.
    Int i;
    for (i=0; i<size; i++)
	AlwaysAssertExit(ia[i] == 99);
    objset(ia+1,66, size/2, 2);
    for (i=0; i<size; i += 2)
	AlwaysAssertExit(ia[i] == 99 && ia[i+1] == 66);

    Int* ia2 = new int[size];

    // Test objcopy.
    // Without strides.
    ia2[0] = 0;
    objcopy(ia2+1, ia, size-1);
    for (i=1; i<size-1; i += 2)
	AlwaysAssertExit(ia2[i] == 99 && ia2[i+1] == 66);
    AlwaysAssertExit(ia2[0] == 0 && ia2[size-1] == 99);
    objcopy(ia2, ia, size, 1, 1);
    for (i=0; i<size; i += 2)
	AlwaysAssertExit(ia2[i] == 99 && ia2[i+1] == 66);
    // With toStride=fromStride=2.
    objcopy(ia2+1, ia, size/2, 2, 2);
    for (i=0; i<size; i++)
	AlwaysAssertExit(ia2[i] == 99);
    // With fromStride=2.
    objcopy(ia2, ia+1, size/2, 1, 2);
    for (i=0; i<size/2; i++)
	AlwaysAssertExit(ia2[i] == 66);
    for (i=size/2; i<size; i++)
	AlwaysAssertExit(ia2[i] == 99);
    // With toStride=2.
    objcopy(ia, ia2, size/2, 2, 1);
    for (i=0; i<size; i++)
	AlwaysAssertExit(ia[i] == 66);

    // Test non-overlap moves.
    // Without strides.
    objset(ia, 99, size);
    objset(ia+1,66, size/2, 2);
    ia2[0] = 0;
    objmove(ia2+1, ia, size-1);
    for (i=1; i<size-1; i += 2)
	AlwaysAssertExit(ia2[i] == 99 && ia2[i+1] == 66);
    AlwaysAssertExit(ia2[0] == 0 && ia2[size-1] == 99);
    objmove(ia2, ia, size, 1, 1);
    for (i=0; i<size; i += 2)
	AlwaysAssertExit(ia2[i] == 99 && ia2[i+1] == 66);
    // With toStride=fromStride=2.
    objmove(ia2+1, ia, size/2, 2, 2);
    for (i=0; i<size; i++)
	AlwaysAssertExit(ia2[i] == 99);
    // With fromStride=2.
    objmove(ia2, ia+1, size/2, 1, 2);
    for (i=0; i<size/2; i++)
	AlwaysAssertExit(ia2[i] == 66);
    for (i=size/2; i<size; i++)
	AlwaysAssertExit(ia2[i] == 99);
    // With toStride=2.
    objmove(ia, ia2, size/2, 2, 1);
    for (i=0; i<size; i++)
	AlwaysAssertExit(ia[i] == 66);

    // Test overlapping moves.
    for (i=0; i<size; i++)
	ia[i] = i;
    // Move to left without strides.
    objmove (ia, ia+1, size-1);
    for (i=0; i<size-1; i++)
	AlwaysAssertExit(ia[i] == i+1);
    AlwaysAssertExit(ia[size-1] == size-1);
    // Move to right without strides.
    objmove (ia+1, ia, size-1);
    for (i=1; i<size; i++)
	AlwaysAssertExit(ia[i] == i);
    AlwaysAssertExit(ia[0] == 1);

    // Move to left with toStride=fromStride=2.
    objmove (ia, ia+1, size/2, 2, 2);
    for (i=0; i<size; i+=2)
	AlwaysAssertExit(ia[i] == i+1  &&  ia[i+1] == i+1);
    // Move to right with toStride=fromStride=2.
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia+1, ia, size/2, 2, 2);
    for (i=0; i<size; i+=2)
	AlwaysAssertExit(ia[i] == i  &&  ia[i+1] == i);
    
    // Move in place with different strides.
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia, ia, size);
    for (i=0; i<size; i++)
	AlwaysAssertExit(ia[i] == i);
    objmove (ia, ia, size/2, 1, 2);
    for (i=0; i<size/2; i++)
	AlwaysAssertExit(ia[i] == 2*i);
    for (i=size/2; i<size; i++)
	AlwaysAssertExit(ia[i] == i);
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia, ia, size/2, 2, 1);
    for (i=0; i<size; i+=2)
	AlwaysAssertExit(ia[i] == i/2  &&  ia[i+1] == i+1);

    // Move to the left with different strides.
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia, ia+50, size/2, 2, 1);
    for (i=0; i<size; i+=2)
	AlwaysAssertExit(ia[i] == size/2 + i/2  &&  ia[i+1] == i+1);
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia, ia+2, size/2-1, 1, 2);
    for (i=0; i<size/2-1; i++)
	AlwaysAssertExit(ia[i] == 2*(i+1));
    for (i=size/2-1; i<size; i++)
	AlwaysAssertExit(ia[i] == i);

    // Move to the right with different strides.
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia+size/2, ia, size/2, 1, 2);
    for (i=0; i<size/2; i++)
	AlwaysAssertExit(ia[i] == i);
    for (i=size/2; i<size; i++)
	AlwaysAssertExit(ia[i] == 2*(i-size/2));

    // Move in a way that there is a crosspoint.
    // First in a way that it contracts.
    // First with toStride=2 and fromStride=1.
    // Then with the strides doubled.
    // Finally again with the strides doubled, but one shifted,
    // so that no source point gets overwritten.
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia+size/4, ia, size/2, 1, 2);
    for (i=0; i<size/4; i++)
	AlwaysAssertExit(ia[i] == i);
    for (i=size/4; i<3*size/4; i++)
	AlwaysAssertExit(ia[i] == 2*(i-size/4));
    for (i=3*size/4; i<size; i++)
	AlwaysAssertExit(ia[i] == i);
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia+size/4, ia, size/4, 2, 4);
    for (i=0; i<size/4; i++)
	AlwaysAssertExit(ia[i] == i);
    for (i=size/4; i<3*size/4; i+=2)
	AlwaysAssertExit(ia[i] == 2*(i-size/4)  &&  ia[i+1] == i+1);
    for (i=3*size/4; i<size; i++)
	AlwaysAssertExit(ia[i] == i);
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia+size/4, ia+1, size/4, 2, 4);
    for (i=0; i<size/4; i++)
	AlwaysAssertExit(ia[i] == i);
    for (i=size/4; i<3*size/4; i+=2)
	AlwaysAssertExit(ia[i] == 2*(i-size/4)+1  &&  ia[i+1] == i+1);
    for (i=3*size/4; i<size; i++)
	AlwaysAssertExit(ia[i] == i);

    // Now in a way that it expands.
    // The similar tests are done.
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia, ia+size/4, size/2, 2, 1);
    for (i=0; i<size; i+=2)
	AlwaysAssertExit(ia[i] == i/2+size/4  &&  ia[i+1] == i+1);
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia, ia+size/4, size/4, 4, 2);
    for (i=0; i<size; i+=4)
	AlwaysAssertExit(ia[i] == i/2+size/4  &&  ia[i+1] == i+1  &&
			 ia[i+2] == i+2  &&  ia[i+3] == i+3);
    for (i=0; i<size; i++)
	ia[i] = i;
    objmove (ia+1, ia+size/4, size/4, 4, 2);
    for (i=1; i<size-4; i+=4)
	AlwaysAssertExit(ia[i] == (i-1)/2+size/4  &&  ia[i+1] == i+1  &&
			 ia[i+2] == i+2  &&  ia[i+3] == i+3);
    AlwaysAssertExit(ia[0] == 0  &&  ia[size-3] == (size-3-1)/2+size/4  &&
		     ia[size-2] == size-2  &&  ia[size-1] == size-1);


    Block<int>* ta = new Block<int>[size];
    Block<int> set(3);
    set[0] = 1; set[1] = 2; set[2] = 3;
    objset(ta, set, size, 1);
    for (i=0; i < size; i++)
	AlwaysAssertExit(ta[i][0] == 1 && ta[i][1] == 2 && ta[i][2] == 3);

    set[0] = set[1] = set[2] = 7;
    objset(ta + 1, set, size/2, 2);
    for (i=0; i < size; i+=2) {
	AlwaysAssertExit(ta[i][0] == 1 && ta[i][1] == 2 && ta[i][2] == 3);
	AlwaysAssertExit(ta[i+1][0] == 7 && ta[i+1][1] == 7 && ta[i+1][2] == 7);
    }
    for (i=0; i<size; i++) {
	ta[i][0] = i;
	ta[i][1] = i+size;
	ta[i][2] = i+2*size;
    }
    Block<int>* ta2 = new Block<int>[size];

    // Test objcopy for a non-builtin data type.
    objcopy(ta2, ta, size/2, 1, 2);
    objcopy(ta2+size/2, ta+1, size/2, 1, 2);
    for (i=0; i < size/2; i++)
	AlwaysAssertExit(ta2[i][0] == 2*i  &&
			 ta2[i][1] == 2*i+size  &&
			 ta2[i][2] == 2*i+2*size);
    for (i=size/2; i < size; i++)
	AlwaysAssertExit(ta2[i][0] == 2*(i-size/2)+1  &&
			 ta2[i][1] == 2*(i-size/2)+1+size  &&
			 ta2[i][2] == 2*(i-size/2)+1+2*size);
    objcopy(ta, ta2, size);
    for (i=0; i < size/2; i++)
	AlwaysAssertExit(ta[i][0] == 2*i  &&
			 ta[i][1] == 2*i+size  &&
			 ta[i][2] == 2*i+2*size);
    for (i=size/2; i < size; i++)
	AlwaysAssertExit(ta[i][0] == 2*(i-size/2)+1  &&
			 ta[i][1] == 2*(i-size/2)+1+size  &&
			 ta[i][2] == 2*(i-size/2)+1+2*size);

    // Test objmove for a non-builtin data type.
    for (i=0; i<size; i++) {
	ta[i][0] = i;
	ta[i][1] = i+size;
	ta[i][2] = i+2*size;
    }
    objmove(ta2, ta, size/2, 1, 2);
    objmove(ta2+size/2, ta+1, size/2, 1, 2);
    for (i=0; i < size/2; i++)
	AlwaysAssertExit(ta2[i][0] == 2*i  &&
			 ta2[i][1] == 2*i+size  &&
			 ta2[i][2] == 2*i+2*size);
    for (i=size/2; i < size; i++)
	AlwaysAssertExit(ta2[i][0] == 2*(i-size/2)+1  &&
			 ta2[i][1] == 2*(i-size/2)+1+size  &&
			 ta2[i][2] == 2*(i-size/2)+1+2*size);
    objmove(ta, ta2, size);
    for (i=0; i < size/2; i++)
	AlwaysAssertExit(ta[i][0] == 2*i  &&
			 ta[i][1] == 2*i+size  &&
			 ta[i][2] == 2*i+2*size);
    for (i=size/2; i < size; i++)
	AlwaysAssertExit(ta[i][0] == 2*(i-size/2)+1  &&
			 ta[i][1] == 2*(i-size/2)+1+size  &&
			 ta[i][2] == 2*(i-size/2)+1+2*size);
    objmove (ta, ta+1, size-1, 1, 1);
    for (i=0; i < size/2-1; i++)
	AlwaysAssertExit(ta[i][0] == 2*(i+1)  &&
			 ta[i][1] == 2*(i+1)+size  &&
			 ta[i][2] == 2*(i+1)+2*size);
    for (i=size/2-1; i < size-1; i++)
	AlwaysAssertExit(ta[i][0] == 2*(i+1-size/2)+1  &&
			 ta[i][1] == 2*(i+1-size/2)+1+size  &&
			 ta[i][2] == 2*(i+1-size/2)+1+2*size);
    AlwaysAssertExit(ta[size-1][0] == size-1  &&
		     ta[size-1][1] == size-1+size  &&
		     ta[size-1][2] == size-1+2*size);
    objmove (ta+1, ta, size-1, 1, 1);
    for (i=1; i < size/2; i++)
	AlwaysAssertExit(ta[i][0] == 2*i  &&
			 ta[i][1] == 2*i+size  &&
			 ta[i][2] == 2*i+2*size);
    for (i=size/2; i < size; i++)
	AlwaysAssertExit(ta[i][0] == 2*(i-size/2)+1  &&
			 ta[i][1] == 2*(i-size/2)+1+size  &&
			 ta[i][2] == 2*(i-size/2)+1+2*size);
    AlwaysAssertExit(ta[0][0] == 2  &&
		     ta[0][1] == 2+size  &&
		     ta[0][2] == 2+2*size);


    void** va = new void*[size];
    void** va2 = new void*[size];
    objset(va, static_cast<void*>(0), size);
    objmove(va2, va, size);
    for (i=0; i<size; i++)
	AlwaysAssertExit(va2[i] == 0);

    // Block uses objcopy.
    // Somewhere Block<void*> and Block<const void*> are used.
    // See if they compile and link well.
    PtrBlock<const void*> cbl(10,static_cast<const void*>(0));
    PtrBlock<const void*> cbl2(cbl);
    PtrBlock<void*> bl(10,static_cast<void*>(0));
    PtrBlock<void*> bl2(bl);
    // PtrBlock is based on Block<void*>
    Block<void*> bvl(10,static_cast<void*>(0));
    Block<void*> bvl2(bvl);


    delete [] ia;
    delete [] ia2;
    delete [] ta;
    delete [] ta2;
    delete [] va;
    delete [] va2;

    cout << "OK\n";
    return 0;
}
