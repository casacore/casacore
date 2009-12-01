//# tObjectPool.cc -- test ObjectStack
//# Copyright (C) 2007
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
//# $Id: tObjectStack.cc,v 1.1 2007/11/16 04:23:21 wbrouw Exp $

//# Includes
#include <casa/aips.h>
#include <casa/Containers/ObjectStack.h>
#include <casa/Exceptions/Error.h>
#include <casa/vector.h>
#include <casa/iostream.h>

#include <utility>

// Using
using std::pair;

#include <casa/namespace.h>
int main() {

  Bool ok(True);
  try {
    cout << "Test ObjectStack" << endl;
    cout << "---------------------------------------------------" << endl;

    // Some poolobjectsList of pool objects;
    Bool ok = True;
    
    for (uInt i=0; i<10000; ++i) {
    vector<vector<Double>*> list;
    vector<vector<Int>*> listInt;
      for (uInt j=0; j<10; ++j) {
	// Get some objects
	list.push_back(ObjectStack<vector<Double> >::stack().get());
	listInt.push_back(ObjectStack<vector<Int> >::stack().get());
	// Test freshness
	if (!list[j]->empty()) {
	  cout << "List not refreshed " << endl;
	  ok = False;
	}	
	if (!listInt[j]->empty()) {
	  cout << "ListInt not refreshed " << endl;
	  ok = False;
	}	
	// Fill objects
	for (uInt k=0; k<7; ++k) {
	  list[j]->push_back(13.*k);
	  listInt[j]->push_back(k);
	}
	// Test objects
	if (list[j]->size() != 7) {
	  cout << "Incorrect length list " << list[j]->size() << endl;
	  ok = False;
	}	
	if (listInt[j]->size() != 7) {
	  cout << "Incorrect length listInt " << listInt[j]->size() << endl;
	  ok = False;
	}
      }
      // Remove in different order
      for (uInt j=0; j<10; ++j) {
	ObjectStack<vector<Int> >::stack().put(listInt[9-j]);
	ObjectStack<vector<Double> >::stack().put(list[9-j]);
      }
    }
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
    ok = False;
  }
  
  if (!ok) return 1;
  cout << "Tests ok" << endl;
  cout << "---------------------------------------------------" << endl;

  return 0;
}

