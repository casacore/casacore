//# MCBase.cc: Base for specific measure conversions
//# Copyright (C) 1995,1996,1997,1998,2000,2001,2003
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

//# Includes
#include <casacore/measures/Measures/MCBase.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/iomanip.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Destructor
MCBase::~MCBase() {}

//# Operators

//# Member functions
void MCBase::makeState(uint32_t *state,
		       const uint32_t ntyp, const uint32_t nrout,
		       const uint32_t list[][3]) {
  // Make trees
  uint32_t *tcnt = new uint32_t[ntyp];
  uint32_t *tree = new uint32_t[ntyp*ntyp];
  bool *visit= new bool[ntyp];
  uint32_t *mcnt = new uint32_t[ntyp*ntyp];
  for (uint32_t j=0; j<ntyp; j++) {
    tcnt[j] = 0;
    visit[j] = false;
    for (uint32_t i=0; i<ntyp; i++) {
      mcnt[i*ntyp + j]  = 100*nrout;
      state[i*ntyp + j] = nrout;
    }
  }
  for (uint32_t i=0; i<nrout; i++) {
    tree[list[i][0]*ntyp + tcnt[list[i][0]]] = i;
    tcnt[list[i][0]]++;
    // Fill one-step transitions
    mcnt[list[i][0]*ntyp + list[i][1]] = 1 + list[i][2];
    state[list[i][0]*ntyp + list[i][1]] = i;
  }
  // Find shortest route
  for (uint32_t i=0; i<ntyp; i++) {
    for (uint32_t j=0; j<ntyp; j++) {
      if (i != j) {
	uint32_t len = 0;
	bool okall = true;
	findState(len, state, mcnt, okall,
		  visit, tcnt, tree,
		  i, j, ntyp, nrout, list);
      }
    }
  }
  // delete trees
  delete [] tcnt;
  delete [] tree;
  delete [] visit;
  delete [] mcnt;
}

bool MCBase::findState(uint32_t &len, uint32_t *state, uint32_t *mcnt, bool &okall,
		       bool *visit, const uint32_t *tcnt, const uint32_t *tree,
		       const uint32_t &in, const uint32_t &out,
		       const uint32_t ntyp, const uint32_t nrout,
		       const uint32_t list[][3]) {
  // Check loop
  if (visit[in]) return false;
  uint32_t minlen = 100*nrout;
  uint32_t res = nrout;
  // Check if path already known
  if (mcnt[in*ntyp + out] != 100*nrout) {
    minlen = mcnt[in*ntyp + out];
    res = state[in*ntyp + out];
  } else {
    for (uint32_t i=0; i<tcnt[in]; i++) {
      uint32_t loclen = 1+list[tree[in*ntyp+i]][2];
      visit[in] = true;
      uint32_t nin = list[tree[in*ntyp+i]][1];
      if (findState(loclen, state, mcnt, okall,
		    visit, tcnt, tree,
		    nin, out, ntyp, nrout, list)) {
	if (loclen < minlen) {
	  minlen = loclen;
	  res = tree[in*ntyp+i];
	}
      } else okall = false;
    }
    visit[in] = false;
  }
  if (minlen == 100*nrout) return false;
  if (len == 0 || okall) {
    mcnt[in*ntyp + out] = minlen;
    state[in*ntyp + out]= res;
  }
  len += minlen;
  return true;
}

String MCBase::showState(uint32_t *state,
			 const uint32_t ntyp, const uint32_t,
			 const uint32_t list[][3]) {
  ostringstream oss;
  oss << "   |";
  for (uint32_t i=0; i<ntyp; i++) oss << setw(3) << i;
  oss << "\n";
  for (uint32_t j=0; j<3*ntyp+4; j++) oss << '-'; 
  oss << "\n";
  for (uint32_t i=0; i<ntyp; i++) {
    oss << setw(3) << i << '|';
    for (uint32_t j=0; j<ntyp; j++) {
      if (i == j) {
        oss << " --"; 
      } else {
        oss << setw(3) << state[i*ntyp+j];
      }
    }
    oss << "\n";
    oss << "   |";
    for (uint32_t k=0; k<ntyp; k++) {
      if (i == k) {
        oss << "   ";
      } else {
        oss << setw(3) << list[state[i*ntyp+k]][1];
      }
    }
    oss << "\n";
  }
  return String(oss);
}

} //# NAMESPACE CASACORE - END

