//# tStack.cc: This program tests the Stack class
//# Copyright (C) 1993,1994,1995,2000,2001
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

#include <casacore/casa/Containers/Stack.h>
#include <casacore/casa/Containers/StackError.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {

  Stack<int> one;
  Stack<int> two;

  one.push(23);
  one.push(8);
  one.push(41);
  one.push(98);
  one.push(4);
  one.push(71);
  one.push(107);
  one.push(63);
  one.push(51);
  one.push(126);
  one.push(28);
  one.push(119);
  one.push(26);
  one.push(15);
  one.push(32);
  one.push(169);
  one.push(1);
  one.push(45);
  one.push(7);

  while(!one.empty()) {
    cout << one.top() << ".";
    two.push(one.popVal());
  }
  cout << endl;
  while(!two.empty()) {
    cout << two.top() << ".";
    one.push(two.popVal());
  }
  cout << endl;

  one.pop();
  one.pop();
  one.pop();
  one.pop();
  one.pop();
  one.pop();
 
  cout << one.top() << endl;

  one.push(88);

  cout << one.top() << endl;

  one.pop();
  one.pop();
  one.pop();
  one.pop();
  one.pop();
  one.pop();

  cout << one.top() << endl;
  one.pop();
  cout << one.popVal() << endl;
  cout << one.top() << endl;

  try {
    while(1) {
      two.push(one.popVal());
    }
  } catch (EmptyStackError x) {
    cout << "EmptyStackError: " << x.getMesg() << endl;
  } catch (AipsError x) {
    cout << "AipsError: " << x.getMesg() << endl;
  } 

  Stack<int> three = two;
  try {
    while(1) {
      two.pop();
    }
  } catch (EmptyStackError x) {
    cout << "EmptyStackError: " <<  x.getMesg() << endl;
  } catch (AipsError x) {
    cout << "AipsError: " << x.getMesg() << endl;
  } 

  try {
    one.top();
  } catch (EmptyStackError x) {
    cout << "EmptyStackError: " << x.getMesg() << endl;
  } catch (AipsError x) {
    cout << "AipsError: " << x.getMesg() << endl;
  } 

  cout << three.popVal() << endl;
  Stack<int> four;
  four = three;
  while(!three.empty()) {
    cout << three.popVal() << " ";
  }
  cout << endl;
  while(!four.empty()) {
    cout << four.popVal() << ".";
  }
  cout << endl;

  return(0);

}
