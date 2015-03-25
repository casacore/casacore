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

#include <casacore/ms/MSOper/MSKeys.h>

#include <casacore/casa/BasicSL/String.h>

namespace casacore {

Bool operator<(const SubScanKey& lhs, const SubScanKey& rhs) {
	if (lhs.obsID < rhs.obsID) {
		return True;
    }
	else if (lhs.obsID == rhs.obsID) {
		if (lhs.arrayID < rhs.arrayID) {
			return True;
        }
		else if (lhs.arrayID == rhs.arrayID) {
			if (lhs.scan < rhs.scan) {
				return True;
            }
			else if (lhs.scan == rhs.scan) {
				if (lhs.fieldID < rhs.fieldID) {
					return True;
                }
            }
        }
    }
    return False;
}

String toString(const SubScanKey& subScanKey) {
	return toString(scanKey(subScanKey)) + " fieldID="
		+ String::toString(subScanKey.fieldID);
}

std::ostream& operator<<(std::ostream& os, const SubScanKey& subScanKey) {
	os << toString(subScanKey) << endl;
	return os;
}


String toString(const ScanKey& scanKey) {
	return "observationID=" + String::toString(scanKey.obsID)
		+ " arrayID=" + String::toString(scanKey.arrayID)
		+ " scan number=" + String::toString(scanKey.scan);
}


Bool operator<(const ScanKey& lhs, const ScanKey& rhs) {
	if (lhs.obsID < rhs.obsID) {
		return True;
    }
	else if (lhs.obsID == rhs.obsID) {
		if (lhs.arrayID < rhs.arrayID) {
			return True;
        }
		else if (lhs.arrayID == rhs.arrayID) {
			if (lhs.scan < rhs.scan) {
				return True;
            }
        }
    }
    return False;
}

std::set<Int> scanNumbers(const std::set<ScanKey>& scanKeys) {
	std::set<Int> scanNumbers;
	std::set<ScanKey>::const_iterator iter = scanKeys.begin();
	std::set<ScanKey>::const_iterator end = scanKeys.end();
	while (iter != end) {
		scanNumbers.insert(iter->scan);
		++iter;
	}
	return scanNumbers;
}

ostream& operator<<(ostream& os, const ScanKey& scanKey) {
	os << toString(scanKey) << endl;
	return os;
}


Bool operator<(const ArrayKey& lhs, const ArrayKey& rhs) {
	if (lhs.obsID < rhs.obsID) {
		return True;
    }
	else if (lhs.obsID == rhs.obsID) {
		if (lhs.arrayID < rhs.arrayID) {
			return True;
        }
    }
    return False;
}

std::set<ScanKey> scanKeys(
	const std::set<Int>& scans, const ArrayKey& arrayKey
) {
	std::set<ScanKey> scanKeys;
	std::set<Int>::const_iterator iter = scans.begin();
	std::set<Int>::const_iterator end = scans.end();
	ScanKey scanKey;
	scanKey.obsID = arrayKey.obsID;
	scanKey.arrayID = arrayKey.arrayID;
	while (iter != end) {
		scanKey.scan = *iter;
		scanKeys.insert(scanKey);
		++iter;
	}
	return scanKeys;
}


}

