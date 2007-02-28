//# tMedianSlider.cc: This program tests tMedianSlider objects
//# Copyright (C) 2000,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
//# License for more details.
//#
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#

//#! Includes

#include <casa/BasicMath/Math.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>
#include <scimath/Mathematics/MedianSlider.h>

#include <casa/namespace.h>
int main(){
  MedianSlider me;
  cout << "Create a MedianSlider me by means of call to MedianSlider () with default arguments" << endl;
  int halfwin = 3;
  MedianSlider m1(halfwin);
  cout << "Create a MedianSlider m1 by means of call to MedianSlider(int hw)" << endl;
  cout << "The half window size of m1 is 3, full window size is 7\n";
  cout << "add {0.5, 3.5, 1.5, 4.5, 2.5, 5.5} with flags {f,t,f,t,f,t} to m1" << endl;

  m1.add(0.5);
  m1.add(3.5, True);
  m1.add(1.5);
  m1.add(4.5, True);
  m1.add(2.5);
  m1.add(5.5, True);
  m1.next();
  cout << "the function next() add 1 flagged value into m1 window, and make it full " << endl;

  cout << "The number of non-flagged values in m1 window is " << m1.nval() << endl;
  cout << "Current median value in m1 window is " << m1.median() << endl;

  m1.add(6.5);
  cout << "\nAdd a non-flagged 6.5" << endl;
  cout << "The number of non-flagged values in m1 window is " << m1.nval() << endl;
  cout << "Current median value in m1 window is " << m1.median() << endl;

  m1.add(7.5);
  cout << "Add a non-flagged 7.5" << endl;
  cout << "The number of non-flagged values in m1 window is " << m1.nval() << endl;
  cout << "Current median value in m1 window is " << m1.median() << endl;

  m1.add(8.5);
  cout << "Add a non-flagged 8.5" << endl;
  cout << "The number of non-flagged values in m1 window is " << m1.nval() << endl;
  cout << "Current median value in m1 window is " << m1.median() << endl;



  MedianSlider m2(m1);
  cout << "\nCreate a MedianSlider m2 by means of call to copy constructor\n";
  cout << "MedianSlider  ( const MedianSlider &other )\n";

  cout << "The number of non-flagged values in m2 window is " << m2.nval() << endl;
  cout << "Current median value in m2 window is " << m2.median() << endl;

  me = m2;
  cout << "Assign m2 to me, so me has the number of non-flagged values " << me.nval() << endl;
  cout << "Current median value in me window is " << me.median() << endl;

  Vector<Float> vl(4);
  vl(0) = 10.5;
  vl(1) = 4.5;
  vl(2) = 5.5;
  vl(3) = 11.5;
  cout << "Create Vector<Float> vl = {10.5, 4.5, 5.5, 11.5}" << endl;
  Vector<Bool> bl(4);
  bl(0) = False;
  bl(1) = True;
  bl(2) = True;
  bl(3) = False;
  cout << "Create Vector<Bool> bl = {False, True, True, False}" << endl;

  m1.add(vl, bl);
  cout << "Add vl and bl to m1, old values are pushed out" << endl;
  cout << "The number of non-flagged values in m1 window is " << m1.nval() << endl;
  cout << "Current median value in m1 window is " << m1.median() << endl;

  Bool flag = False;

  cout << "The value takes 4 step back from end " << m1.prevVal(uInt(4), flag) << endl;
  cout << "The value at the midpoint " << m1.midpoint(flag) << endl;
  cout << "The difference between the current median and the value at the window center " << m1.diff(flag) << endl;
  cout << "The total memory usage for a given half window size is " << m1.objsize(halfwin) << endl;

  m1.add();
  cout << "\nTest add() that add(0, True) to m1 " << endl;
  cout << "Current median value in m1 window is " << m1.median() << endl;
  cout << "Now the value at the midpoint is " << m1.midpoint(flag);
  String boolAsString;
  if(flag == 0)
    boolAsString = "False";
  else
    boolAsString = "True";
  cout <<" with flag " << boolAsString << " the difference between midpoint and current median "
       << m1.diff() << endl;
  //  cout << " Number of values in the m1 window " << m1.size() << endl;

  Vector<Float> vl2(7);
  vl2(0) = 1;
  vl2(1) = 2;
  vl2(2) = 3;
  vl2(3) = 4;
  vl2(4) = 5;
  vl2(5) = 6;
  vl2(6) = 7;
  cout << "\nCreate Vector<Float> vl2 = {1,2,3,4,5,6,7}" << endl;
  m1.add(vl2);
  cout << "Add vl2 to m1, old values are pushed out" << endl;
  cout << "The number of non-flagged values in m1 window is " << m1.nval() << endl;
  cout << "Current median value in m1 window is " << m1.median() << endl;
  return 0;
}
