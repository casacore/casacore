//# tCompare.cc: Test program for the Compare classes
//# Copyright (C) 2013
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
//# $Id: tSort.cc 21299 2013-01-08 09:05:20Z gervandiepen $

//# Includes

#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Utilities/Compare.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// This program tests the Compare classes.
// It sorts some data in ascending and/or descending order.
// The results are written to stdout. A script executing this program,
// compares the output with a reference output file.

// Test case-insensitive sort.
void sort1 (Int option)
{
  String arr[10];
  arr[0] = "aaa";
  arr[1] = "aaac";
  arr[2] = "AAA";
  arr[3] = "AaAb";
  arr[4] = "AaA";
  arr[5] = "AAAd";
  arr[6] = "aaad";
  arr[7] = "aaab";
  arr[8] = "aaab2";
  arr[9] = "aaab1";
  CountedPtr<BaseCompare> cmp(new CompareNoCase());
  Sort sort;
  sort.sortKey (arr, cmp, sizeof(String));
  Vector<uInt> inx;
  sort.sort (inx, 10, option);
  for (uInt i=0; i<inx.size(); ++i) {
    cout << arr[inx[i]] << ' ';
  }
  cout << endl;
}

// Test real interval sort.
void sort2 (Int option)
{
  Double arr[10];
  arr[0] = 1;
  arr[1] = 12;
  arr[2] = 4.5;
  arr[3] = 3;
  arr[4] = 7;
  arr[5] = 7.01;
  arr[6] = 8;
  arr[7] = 9;
  arr[8] = 8.99;
  arr[9] = -5;
  CountedPtr<BaseCompare> cmp(new CompareIntervalReal<Double>(2,1));
  Sort sort;
  sort.sortKey (arr, cmp, sizeof(Double));
  Vector<uInt> inx;
  sort.sort (inx, 10, option);
  for (uInt i=0; i<inx.size(); ++i) {
    cout << arr[inx[i]] << ' ';
  }
  cout << endl;
}

// Test other interval.
void sort3 (Int option)
{
  Double arr[10];
  arr[0] = 1;
  arr[1] = 12;
  arr[2] = 4;
  arr[3] = 3;
  arr[4] = 7;
  arr[5] = 8;
  arr[6] = 7;
  arr[7] = 9;
  arr[8] = -8;
  arr[9] = -5;
  CountedPtr<BaseCompare> cmp(new CompareIntervalReal<Double>(3,0));
  Sort sort;
  sort.sortKey (arr, cmp, sizeof(Double));
  Vector<uInt> inx;
  sort.sort (inx, 10, option);
  for (uInt i=0; i<inx.size(); ++i) {
    cout << arr[inx[i]] << ' ';
  }
  cout << endl;
}

int main()
{
  sort1(Sort::ParSort);
  sort1(Sort::QuickSort);
  sort1(Sort::HeapSort);
  sort1(Sort::InsSort);
  sort1(Sort::ParSort + Sort::NoDuplicates);
  sort1(Sort::QuickSort + Sort::NoDuplicates);
  sort1(Sort::HeapSort + Sort::NoDuplicates);
  sort1(Sort::InsSort + Sort::NoDuplicates);

  sort2(Sort::ParSort);
  sort2(Sort::QuickSort);
  sort2(Sort::HeapSort);
  sort2(Sort::InsSort);
  sort2(Sort::ParSort + Sort::NoDuplicates);
  sort2(Sort::QuickSort + Sort::NoDuplicates);
  sort2(Sort::HeapSort + Sort::NoDuplicates);
  sort2(Sort::InsSort + Sort::NoDuplicates);

  sort3(Sort::ParSort);
  sort3(Sort::QuickSort);
  sort3(Sort::HeapSort);
  sort3(Sort::InsSort);
  sort3(Sort::ParSort + Sort::NoDuplicates);
  sort3(Sort::QuickSort + Sort::NoDuplicates);
  sort3(Sort::HeapSort + Sort::NoDuplicates);
  sort3(Sort::InsSort + Sort::NoDuplicates);

  return 0;                              // exit with success status
}
