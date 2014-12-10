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
//#
//# $Id$

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
void MCBase::makeState(uInt *state,
		       const uInt ntyp, const uInt nrout,
		       const uInt list[][3]) {
  // Make trees
  uInt *tcnt = new uInt[ntyp];
  uInt *tree = new uInt[ntyp*ntyp];
  Bool *visit= new Bool[ntyp];
  uInt *mcnt = new uInt[ntyp*ntyp];
  for (uInt j=0; j<ntyp; j++) {
    tcnt[j] = 0;
    visit[j] = False;
    for (uInt i=0; i<ntyp; i++) {
      mcnt[i*ntyp + j]  = 100*nrout;
      state[i*ntyp + j] = nrout;
    }
  }
  for (uInt i=0; i<nrout; i++) {
    tree[list[i][0]*ntyp + tcnt[list[i][0]]] = i;
    tcnt[list[i][0]]++;
    // Fill one-step transitions
    mcnt[list[i][0]*ntyp + list[i][1]] = 1 + list[i][2];
    state[list[i][0]*ntyp + list[i][1]] = i;
  }
  // Find shortest route
  for (uInt i=0; i<ntyp; i++) {
    for (uInt j=0; j<ntyp; j++) {
      if (i != j) {
	uInt len = 0;
	Bool okall = True;
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

Bool MCBase::findState(uInt &len, uInt *state, uInt *mcnt, Bool &okall,
		       Bool *visit, const uInt *tcnt, const uInt *tree,
		       const uInt &in, const uInt &out,
		       const uInt ntyp, const uInt nrout,
		       const uInt list[][3]) {
  // Check loop
  if (visit[in]) return False;
  uInt minlen = 100*nrout;
  uInt res = nrout;
  // Check if path already known
  if (mcnt[in*ntyp + out] != 100*nrout) {
    minlen = mcnt[in*ntyp + out];
    res = state[in*ntyp + out];
  } else {
    for (uInt i=0; i<tcnt[in]; i++) {
      uInt loclen = 1+list[tree[in*ntyp+i]][2];
      visit[in] = True;
      uInt nin = list[tree[in*ntyp+i]][1];
      if (findState(loclen, state, mcnt, okall,
		    visit, tcnt, tree,
		    nin, out, ntyp, nrout, list)) {
	if (loclen < minlen) {
	  minlen = loclen;
	  res = tree[in*ntyp+i];
	}
      } else okall = False;
    }
    visit[in] = False;
  }
  if (minlen == 100*nrout) return False;
  if (len == 0 || okall) {
    mcnt[in*ntyp + out] = minlen;
    state[in*ntyp + out]= res;
  }
  len += minlen;
  return True;
}

String MCBase::showState(uInt *state,
			 const uInt ntyp, const uInt,
			 const uInt list[][3]) {
  ostringstream oss;
  oss << "   |";
  for (uInt i=0; i<ntyp; i++) oss << setw(3) << i;
  oss << "\n";
  for (uInt j=0; j<3*ntyp+4; j++) oss << '-'; 
  oss << "\n";
  for (uInt i=0; i<ntyp; i++) {
    oss << setw(3) << i << '|';
    for (uInt j=0; j<ntyp; j++) {
      if (i == j) {
        oss << " --"; 
      } else {
        oss << setw(3) << state[i*ntyp+j];
      }
    }
    oss << "\n";
    oss << "   |";
    for (uInt k=0; k<ntyp; k++) {
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

