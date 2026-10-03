// # nISMBucket.cc :Simulation program for bucket behaviour of IncrementalStman
// # Copyright (C) 1996,1999,2001,2002,2003
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This program is free software; you can redistribute it and/or modify it
// # under the terms of the GNU General Public License as published by the Free
// # Software Foundation; either version 2 of the License, or (at your option)
// # any later version.
// #
// # This program is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// # more details.
// #
// # You should have received a copy of the GNU General Public License along
// # with this program; if not, write to the Free Software Foundation, Inc.,
// # 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Utilities/GenSort.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/namespace.h>

// <summary>
// Simulation program for bucket behaviour of IncrementalStman.
// </summary>

// <synopsis>
// nISMBucket shows how much disk space is needed when using the
// IncrementalStMan for a given scenario. It also shows the gain
// compared to storing each data value separately.
// <p>
// The programs asks for the bucket size and total number of rows.
// Then it asks in a loop for the column(s), value length and how
// often a value varies.
// </synopsis>

int main(int argc, const char* argv[]) {
  if (argc < 2) {
    cout << "This program calculates the number of buckets needed" << endl;
    cout << "for a scenario with the IncrementalStMan" << endl;
    cout << "Start as:" << endl;
    cout << "   nISMBucket <bucketSize> <nrows>" << endl;
    cout << "bucketSize==0 means it will be calculated using the" << endl;
    cout << "tIncrementalStMan heuristics" << endl;
    return 0;
  }
  unsigned int i, bucketSize, nrow, nrent, n, totNrcol;
  Block<unsigned int> nrcol, leng, same;
  istringstream istr1(argv[1]);
  istr1 >> bucketSize;
  if (argc < 3) {
    cerr << "#rows: ";
    cin >> nrow;
  } else {
    istringstream istr2(argv[2]);
    istr2 >> nrow;
  }
  nrent = 0;
  totNrcol = 0;
  cerr << "You can now specify the distribution of the values" << endl;
  cerr << "by giving #columns, value length, and #rows with same value" << endl;
  cerr << "one or more times. For simplicity the lowest #rows has to" << endl;
  cerr << "divide the other ones" << endl;
  while (true) {
    cerr << "#columns (0=end): ";
    cin >> n;
    if (n == 0) {
      break;
    }
    nrent++;
    nrcol.resize(nrent);
    leng.resize(nrent);
    same.resize(nrent);
    nrcol[nrent - 1] = n;
    totNrcol += n;
    cerr << "value length: ";
    cin >> leng[nrent - 1];
    cerr << "#rows with same value: ";
    cin >> same[nrent - 1];
    if (n == 0 || leng[nrent - 1] == 0 || same[nrent - 1] == 0) {
      cerr << "One or more zero values were given" << endl;
      return 1;
    }
  }
  if (nrent < 1) {
    cerr << "No columns given" << endl;
    return 1;
  }
  // Sort the "same" array in ascending order.
  Vector<unsigned int> index;
  genSort(index, same);
  unsigned int lowest = same[index(0)];

  // Calculate the bucket size if not specified.
  // This piece of code is similar to that in ISMBase.cc.
  unsigned int headerSize = 4 * (totNrcol + 1);  // needed per column
  unsigned int fixedSize = 0;
  for (i = 0; i < nrent; i++) {
    fixedSize += nrcol[i] * (2 * 4 + leng[i]);
  }
  if (bucketSize > 0) {
    // The bucket size is defined. Check if at least 2
    // fixed-length items for each row fit in it.
    // When the bucket is smaller than 32768 bytes, check
    // if can hold at least 10 rows (since it makes no sense to
    // have very small buckets).
    if (bucketSize < headerSize + 2 * fixedSize) {
      cout << "bucket too small to hold 2 rows" << endl;
    } else if (bucketSize < 32768) {
      if (bucketSize < headerSize + 10 * fixedSize) {
        cout << "bucket < 32768 and too small to hold 10 rows" << endl;
      }
    }
  }
  if (bucketSize == 0) {
    // Calculate the bucket size.
    // Try to fit 100 rows (with a minimum of 32768 bytes).
    // If that results in a very large size (> 327680) try to fit
    // 10 rows. If that still results in a large size, use 327680
    // but at least 2 rows have to fit in it.
    bucketSize = headerSize + 100 * fixedSize;
    if (bucketSize < 32768) {
      bucketSize = 32768;
    } else if (bucketSize > 327680) {
      bucketSize = headerSize + 10 * fixedSize;
      if (bucketSize > 327680) {
        bucketSize = headerSize + 2 * fixedSize;
        if (bucketSize < 327680) {
          bucketSize = 327680;
        }
      }
    }
  }

  // Check if #rows divide.
  // Now determine how many buckets are needed for this stuff.
  // First determine initial length (i.e. values of all columns).
  // The column index takes 4 initial bytes and 4 bytes per column.
  // Per value the index takes 8 bytes.
  unsigned int initleng = 0;
  unsigned int colindex = 4;
  unsigned int normalleng = 0;
  Block<unsigned int> sameleng(nrent, 0u);
  Block<unsigned int> times(nrent);
  for (i = 0; i < nrent; i++) {
    if (same[i] % lowest != 0) {
      cerr << "Lowest #rows value " << lowest << " does not divide " << same[i] << endl;
    }
    times[i] = same[i] / lowest;
    initleng += nrcol[i] * leng[i];
    colindex += nrcol[i] * 4;
    sameleng[i] = nrcol[i] * (leng[i] + 8);
    normalleng += nrcol[i] * leng[i] * nrow;
  }
  int spare = bucketSize - colindex - initleng;
  if (spare < 0) {
    cerr << "Bucketsize too small; should be at least " << colindex + initleng << " bytes" << endl;
    return 1;
  }

  // Iterate until a bucket is filled.
  unsigned int used = 0;
  unsigned int lastused = 0;
  n = 1;
  while (int(used) <= spare) {
    lastused = used;
    if (n > nrow / lowest) {
      break;
    }
    for (i = 0; i < nrent; i++) {
      if (n % times[i] == 0) {
        used += sameleng[i];
      }
    }
    n++;
  }
  n--;
  n *= lowest;
  unsigned int nbucket = 1 + (nrow - 1) / n;
  unsigned int totalleng = nbucket * (bucketSize + 8);  // add 8 for bucketindex

  // Okay, n is the number of rows fitting in a bucket.
  // Display the values.
  cout << endl;
  cout << endl;
  cout << " #columns          = " << nrcol << endl;
  cout << " value length      = " << leng << endl;
  cout << " #rows/same value  = " << same << endl;
  cout << "#rows         = " << nrow << endl;
  cout << "Bucketsize    = " << bucketSize << endl;
  cout << "#rows/bucket  = " << n << endl;
  cout << "waste/bucket  = " << spare - lastused << endl;
  cout << "#buckets      = " << nbucket << endl;
  cout << "total length  = " << totalleng << endl;
  cout << "normal length = " << normalleng << endl;
  cout << "gain factor   = " << float(normalleng) / totalleng << endl;
  return 0;
}
