//# Copyright (C) 1998,1999,2000,2001
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

#ifndef MS_MSKEYS_H
#define MS_MSKEYS_H

#include <casacore/casa/aips.h>

#include <set>
#include <ostream>

namespace casacore {

class String;

// A sub scan is a unique combination of observation ID, array ID, scan number,
// and field ID. Negative values are allowed to indicate all values of the particular
// ID are desired.
struct SubScanKey {
	Int obsID;
	Int arrayID;
	Int scan;
	Int fieldID;
};

// define operator<() so it can be used as a key in std::map
Bool operator<(const SubScanKey& lhs, const SubScanKey& rhs);

String toString(const SubScanKey& subScanKey);

std::ostream& operator<<(std::ostream& os, const SubScanKey& scanKey);

// A scan is a unique combination of observation ID, array ID, and scan number
// Negative values are allowed to indicate all values of the particular
// ID are desired.
struct ScanKey {
	Int obsID;
	Int arrayID;
	Int scan;
};

// create a ScanKey from a SubScanKey, just omits the SubScanKey's fieldID
inline ScanKey scanKey(const SubScanKey& subScanKey) {
	ScanKey key;
	key.obsID = subScanKey.obsID;
	key.arrayID = subScanKey.arrayID;
	key.scan = subScanKey.scan;
	return key;
}

String toString(const ScanKey& scanKey);

// define operator<() so it can be used as a key in std::map
Bool operator<(const ScanKey& lhs, const ScanKey& rhs);

// extract all the unique scan numbers from the specified scans
std::set<Int> scanNumbers(const std::set<ScanKey>& scanKeys);

std::ostream& operator<<(std::ostream& os, const ScanKey& scanKey);

// An ArrayKey is a unique combination of observation ID and array ID
// Negative values are allowed to indicate all values of the particular
// ID are desired.
struct ArrayKey {
	Int obsID;
	Int arrayID;
};

// define operator<() so it can be used as a key in std::map
Bool operator<(const ArrayKey& lhs, const ArrayKey& rhs);

// construct scan keys given a set of scan numbers and an ArrayKey
std::set<ScanKey> scanKeys(const std::set<Int>& scans, const ArrayKey& arrayKey);

}

#endif
