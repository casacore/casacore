// # MCBase.cc: Base for specific measure conversions
// # Copyright (C) 1995,1996,1997,1998,2000,2001,2003
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

// # Includes
#include <casacore/measures/Measures/MCBase.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/iomanip.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// # Constructors

// # Destructor
MCBase::~MCBase() {}

// # Operators

// # Member functions
void MCBase::makeState(unsigned int *state, const unsigned int ntyp, const unsigned int nrout, const unsigned int list[][3]) {
  // Make trees
  unsigned int *tcnt = new unsigned int[ntyp];
  unsigned int *tree = new unsigned int[ntyp * ntyp];
  bool *visit = new bool[ntyp];
  unsigned int *mcnt = new unsigned int[ntyp * ntyp];
  for (unsigned int j = 0; j < ntyp; j++) {
    tcnt[j] = 0;
    visit[j] = false;
    for (unsigned int i = 0; i < ntyp; i++) {
      mcnt[i * ntyp + j] = 100 * nrout;
      state[i * ntyp + j] = nrout;
    }
  }
  for (unsigned int i = 0; i < nrout; i++) {
    tree[list[i][0] * ntyp + tcnt[list[i][0]]] = i;
    tcnt[list[i][0]]++;
    // Fill one-step transitions
    mcnt[list[i][0] * ntyp + list[i][1]] = 1 + list[i][2];
    state[list[i][0] * ntyp + list[i][1]] = i;
  }
  // Find shortest route
  for (unsigned int i = 0; i < ntyp; i++) {
    for (unsigned int j = 0; j < ntyp; j++) {
      if (i != j) {
        unsigned int len = 0;
        bool okall = true;
        findState(len, state, mcnt, okall, visit, tcnt, tree, i, j, ntyp, nrout, list);
      }
    }
  }
  // delete trees
  delete[] tcnt;
  delete[] tree;
  delete[] visit;
  delete[] mcnt;
}

bool MCBase::findState(unsigned int &len, unsigned int *state, unsigned int *mcnt, bool &okall, bool *visit,
                       const unsigned int *tcnt, const unsigned int *tree, const unsigned int &in, const unsigned int &out,
                       const unsigned int ntyp, const unsigned int nrout, const unsigned int list[][3]) {
  // Check loop
  if (visit[in]) return false;
  unsigned int minlen = 100 * nrout;
  unsigned int res = nrout;
  // Check if path already known
  if (mcnt[in * ntyp + out] != 100 * nrout) {
    minlen = mcnt[in * ntyp + out];
    res = state[in * ntyp + out];
  } else {
    for (unsigned int i = 0; i < tcnt[in]; i++) {
      unsigned int loclen = 1 + list[tree[in * ntyp + i]][2];
      visit[in] = true;
      unsigned int nin = list[tree[in * ntyp + i]][1];
      if (findState(loclen, state, mcnt, okall, visit, tcnt, tree, nin, out, ntyp, nrout, list)) {
        if (loclen < minlen) {
          minlen = loclen;
          res = tree[in * ntyp + i];
        }
      } else
        okall = false;
    }
    visit[in] = false;
  }
  if (minlen == 100 * nrout) return false;
  if (len == 0 || okall) {
    mcnt[in * ntyp + out] = minlen;
    state[in * ntyp + out] = res;
  }
  len += minlen;
  return true;
}

String MCBase::showState(unsigned int *state, const unsigned int ntyp, const unsigned int, const unsigned int list[][3]) {
  ostringstream oss;
  oss << "   |";
  for (unsigned int i = 0; i < ntyp; i++) oss << setw(3) << i;
  oss << "\n";
  for (unsigned int j = 0; j < 3 * ntyp + 4; j++) oss << '-';
  oss << "\n";
  for (unsigned int i = 0; i < ntyp; i++) {
    oss << setw(3) << i << '|';
    for (unsigned int j = 0; j < ntyp; j++) {
      if (i == j) {
        oss << " --";
      } else {
        oss << setw(3) << state[i * ntyp + j];
      }
    }
    oss << "\n";
    oss << "   |";
    for (unsigned int k = 0; k < ntyp; k++) {
      if (i == k) {
        oss << "   ";
      } else {
        oss << setw(3) << list[state[i * ntyp + k]][1];
      }
    }
    oss << "\n";
  }
  return oss.str();
}

}  // namespace casacore
