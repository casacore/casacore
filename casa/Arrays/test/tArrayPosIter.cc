//# tArrayPosIter.cc: This program tests the class ArrayPosIter
//# Copyright (C) 2004
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

#include "../ArrayPosIter.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_pop_iter)

BOOST_AUTO_TEST_CASE( zero_dim )
{
    IPosition shape(2);
    shape(0) = shape(1) = 5;

    ArrayPositionIterator ai (shape, 0);
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 2);
    IPosition index (2);
    size_t iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        BOOST_CHECK_EQUAL(index(0), iloop%5);
        BOOST_CHECK_EQUAL(index(1), iloop/5);
    }
    BOOST_CHECK (iloop == 25);
    BOOST_CHECK (!ai.atStart());
    BOOST_CHECK (ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 2);
    ai.origin();
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 2);
}

BOOST_AUTO_TEST_CASE( one_dim )
{
    IPosition shape(2);
    shape(0) = shape(1) = 5;

    ArrayPositionIterator ai (shape, 1);
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 2);
    IPosition index (2);
    size_t iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        BOOST_CHECK_EQUAL(index(0), 0);
        BOOST_CHECK_EQUAL(index(1), iloop);
    }
    BOOST_CHECK (iloop == 5);
    BOOST_CHECK (!ai.atStart());
    BOOST_CHECK (ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 2);
    ai.origin();
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 2);
}

BOOST_AUTO_TEST_CASE( two_dim )
{
    IPosition shape(2);
    shape(0) = shape(1) = 5;

    ArrayPositionIterator ai (shape, 2);
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 2);
    IPosition index (2);
    size_t iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        BOOST_CHECK_EQUAL(index(0), 0);
        BOOST_CHECK_EQUAL(index(1), 0);
    }
    BOOST_CHECK (iloop == 1);
    BOOST_CHECK (!ai.atStart());
    BOOST_CHECK (ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 2);
    ai.origin();
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 2);
}

BOOST_AUTO_TEST_CASE( dim_2 )
{
    IPosition shape(3);
    shape(0) = 5;
    shape(1) = 3;
    shape(2) = 7;

    ArrayPositionIterator ai (shape, 2);
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 3);
    IPosition index (3);
    size_t iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        BOOST_CHECK_EQUAL(index(0), 0);
        BOOST_CHECK_EQUAL(index(1), 0);
        BOOST_CHECK_EQUAL(index(2), iloop);
    }
    BOOST_CHECK (iloop == 7);
    BOOST_CHECK (!ai.atStart());
    BOOST_CHECK (ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 3);
    ai.origin();
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 3);
}

BOOST_AUTO_TEST_CASE( dim_0_1 )
{
    IPosition shape(3);
    shape(0) = 5;
    shape(1) = 3;
    shape(2) = 7;

    ArrayPositionIterator ai (shape, IPosition(1,2));
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 3);
    IPosition index (3);
    size_t iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        BOOST_CHECK_EQUAL(index(0), iloop%5);
        BOOST_CHECK_EQUAL(index(1), iloop/5);
        BOOST_CHECK_EQUAL(index(2), 0);
    }
    BOOST_CHECK (iloop == 15);
    BOOST_CHECK (!ai.atStart());
    BOOST_CHECK (ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 3);
    ai.origin();
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 3);
}

BOOST_AUTO_TEST_CASE( dim_2_0 )
{
    IPosition shape(3);
    shape(0) = 5;
    shape(1) = 3;
    shape(2) = 7;

    ArrayPositionIterator ai (shape, IPosition(2,2,0), false);
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 3);
    IPosition index (3);
    size_t iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        BOOST_CHECK_EQUAL(index(0), iloop/7);
        BOOST_CHECK_EQUAL(index(1), 0);
        BOOST_CHECK_EQUAL(index(2), iloop%7);
    }
    BOOST_CHECK (iloop == 35);
    BOOST_CHECK (!ai.atStart());
    BOOST_CHECK (ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 3);
    ai.origin();
    BOOST_CHECK (ai.atStart());
    BOOST_CHECK (!ai.pastEnd());
    BOOST_CHECK (ai.ndim() == 3);
}

BOOST_AUTO_TEST_SUITE_END()
