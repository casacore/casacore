//# tList.cc: This program tests the Dlist class
//# Copyright (C) 1993,1994,1995,1998,2000,2001
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

#include <casacore/casa/Containers/List.h>
#include <casacore/casa/Containers/ListIO.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
static void show(const ConstListIter<int> &);
static void dump(const ConstListIter<int> *);
int main() {

  List<int> one;
  ListIter<int> onePa(one);
  ListIter<int> onePb(one);

  onePa.addRight(3);
  onePa.addRight(72);
  onePa.addRight(16);
  onePa.addRight(7);
  onePa.addRight(31);

  cout << "-- -- -- -- -- (1) -- -- -- -- --" << endl;

  cout << "<onePa> - ";
  while(onePa.atEnd() != True) {
    cout << onePa.getRight() << ".";
    onePa++;
  }
  cout << endl;

  cout << "<onePa> - ";
  onePa--;
  while(onePa.atStart() != True) {
    cout << onePa.getRight() << ".";
    onePa--;
  }
  cout << onePa.getRight() << ".";
  cout << endl;

  cout << "-- -- -- -- -- (2) -- -- -- -- --" << endl;

  onePa.pos(3);
  cout << onePa.getRight() << " & ";
  onePb.pos(3);
  cout << onePb.getRight() << endl;

  onePa.addRight(103);
  
  cout << onePa.getRight() << " & ";
  cout << onePb.getRight() << endl;

  cout << "<b:onePb> - ";
  onePb.toEnd();
  if (onePb.atStart() != True) do {
    onePb--;
    cout << onePb.getRight() << ".";
  } while (onePb.atStart() != True);
  cout << endl;
  cout << "<f:onePb> - ";
  onePb.toStart();
  while(onePb.atEnd() != True) {
    cout << onePb.getRight() << ".";
    onePb++;
  }
  cout << endl;

  cout << "-- -- -- -- -- (3) -- -- -- -- --" << endl;

  cout << onePa.getRight() << " <30> ";
  onePa.step(30);
  cout << onePa.getRight() << " <30> ";
  onePa.step(30);
  cout << onePa.getRight() << " <-25> ";
  onePa.step(-25);
  cout << onePa.getRight() << " <13> ";
  onePa.step(13);
  cout << onePa.getRight() << endl;

  cout << "-- -- -- -- -- (4) -- -- -- -- --" << endl;

  onePa.pos(4);
  onePb.pos(4);
  onePb.removeRight();
  cout << onePa.getRight() << " & ";
  cout << onePb.getRight() << endl;

  cout << "<b:onePb> - ";
  onePb.toEnd();
  if (onePb.atStart() != True) do {
    onePb--;
    cout << onePb.getRight() << ".";
  } while(onePb.atStart() != True);
  cout << endl;
  cout << "<f:onePb> - ";
  onePb.toStart();
  while(onePb.atEnd() != True) {
    cout << onePb.getRight() << ".";
    onePb++;
  }
  cout << endl;

  onePa--;
  onePb--;
  --onePa;
  --onePb;
  onePa.removeRight();
  cout << onePa.getRight() << " & ";
  cout << onePb.getRight() << endl;

  cout << "<b:onePb> - ";
  onePb.toEnd();
  if (onePb.atStart() != True) do {
    onePb--;
    cout << onePb.getRight() << ".";
  } while(onePb.atStart() != True);
  cout << endl;
  cout << "<f:onePb> - ";
  onePb.toStart();
  while(onePb.atEnd() != True) {
    cout << onePb.getRight() << ".";
    onePb++;
  }
  cout << endl;

  cout << "-- -- -- -- -- (5) -- -- -- -- --" << endl;

  --onePa;

  cout << "<onePc> - ";
  ListIter<int> onePc(&onePa);
  while(onePc.atEnd() != True) {
    cout << onePc.getRight() << ".";
    onePc++;
  }
  cout << endl;
  List<int> two;
  onePc = two;

  onePc.addRight(604);
  onePc.addRight(671);
  onePc.addRight(601);
  onePc.addRight(685);
  onePc.addRight(690);
  onePc.addRight(613);
  onePc.addRight(654);
  onePc.addRight(623);
  onePc.addRight(611);

  cout << "<onePc> - ";
  while(onePc.atEnd() != True) {
    cout << onePc.getRight() << ".";
    onePc++;
  }
  cout << endl;

  onePc.pos(4);
  onePa.pos(2);
  onePb.pos(2);

  onePc.swapRight(onePa);

  cout << "<onePb> - ";
  while(onePb.atEnd() != True) {
    cout << onePb.getRight() << ".";
    onePb++;
  }
  cout << endl;

  cout << "<b:onePc> - ";
  onePc.toEnd();
  if (onePc.atStart() != True) do {
    onePc--;
    cout << onePc.getRight() << ".";
  } while(onePc.atStart() != True);
  cout << endl;
  cout << "<f:onePc> - ";
  onePc.toStart();
  while(onePc.atEnd() != True) {
    cout << onePc.getRight() << ".";
    onePc++;
  }
  cout << endl;

  cout << "<b:onePa> - ";
  onePa.toEnd();
  if (onePa.atStart() != True) do {
    onePa--;
    cout << onePa.getRight() << ".";
  } while(onePa.atStart() != True);
  cout << endl;
  cout << "<f:onePa> - ";
  onePa.toStart();
  while(onePa.atEnd() != True) {
    cout << onePa.getRight() << ".";
    onePa++;
  }
  cout << endl;

  cout << "-- -- -- -- -- (6) -- -- -- -- --" << endl;

  List<int> three;
  onePb = three;

  onePc.pos(4);
  onePb.swapRight(onePc);

  cout << "<b:onePb> - ";
  onePb.toEnd();
  if (onePb.atStart() != True)  do {
    onePb--;
    cout << onePb.getRight() << ".";
  } while(onePb.atStart() != True);
  cout << endl;
  cout << "<f:onePb> - ";
  onePb.toStart();
  while(onePb.atEnd() != True) {
    cout << onePb.getRight() << ".";
    onePb++;
  }
  cout << endl;

  cout << "<b:onePa> - ";
  onePa.toEnd();
  if (onePa.atStart() != True) do {
    onePa--;
    cout << onePa.getRight() << ".";
  } while(onePa.atStart() != True);
  cout << endl;
  cout << "<f:onePa> - ";
  onePa.toStart();
  while(onePa.atEnd() != True) {
    cout << onePa.getRight() << ".";
    onePa++;
  }
  cout << endl;
  onePa.toStart();

  cout << "<b:onePc> - ";
  onePc.toEnd();
  if (onePc.atStart() != True) do {
    onePc--;
    cout << onePc.getRight() << ".";
  } while(onePc.atStart() != True);
  cout << endl;
  cout << "<f:onePc> - ";
  onePc.toStart();
  while(onePc.atEnd() != True) {
    cout << onePc.getRight() << ".";
    onePc++;
  }
  cout << endl;
  onePc.toStart();

  onePb.swapRight(onePa);
  for(;onePb.atEnd() != True;onePb++) {}
  onePb.swapRight(onePc);

  cout << "<b:onePa> - ";
  onePa.toEnd();
  if (onePa.atStart() != True) do {
    onePa--;
    cout << onePa.getRight() << ".";
  } while(onePa.atStart() != True);
  cout << endl;
  cout << "<f:onePa> - ";
  onePa.toStart();
  while(onePa.atEnd() != True) {
    cout << onePa.getRight() << ".";
    onePa++;
  }
  cout << endl;
  onePa.toStart();

  cout << "<b:onePb> - ";
  onePb.toEnd();
  if (onePb.atStart() != True) do {
    onePb--;
    cout << onePb.getRight() << ".";
  } while(onePb.atStart() != True);
  cout << endl;
  cout << "<f:onePb> - ";
  onePb.toStart();
  while(onePb.atEnd() != True) {
    cout << onePb.getRight() << ".";
    onePb++;
  }
  cout << endl;
  onePb.toStart();

  cout << "<b:onePc> - ";
  onePc.toEnd();
  if (onePc.atStart() != True) do {
    onePc--;
    cout << onePc.getRight() << ".";
  } while(onePc.atStart() != True);
  cout << endl;
  cout << "<f:onePc> - ";
  onePc.toStart();
  while(onePc.atEnd() != True) {
    cout << onePc.getRight() << ".";
    onePc++;
  }
  cout << endl;
  onePc.toStart();

  List<int> *four = new List<int>;
  ListIter<int> onePd (four);

  onePd.addRight(23);
  onePd.addRight(24);
  onePd.addRight(29);
  onePd.addRight(20);
  onePd.addRight(22);
  onePd.addRight(28);

  try {
    onePd.toStart();
    while (1) {
      cout << onePd.getRight() << " ";
      onePd++;
    }
  } catch (IterError xx) {
    cout << endl << "IterError: " << xx.getMesg() << endl;
  } 

  try {
    onePd--;
    while (1) {
      cout << onePd.getRight() << " ";
      onePd--;
    }
  } catch (IterError xx) {
    cout << endl << "IterError: " << xx.getMesg() << endl;
  } 

  ConstListIter<int> t5,t6;
  ListIter<int> t7,t8;
  t5 = two;
  t6 = four;
  t7 = two;
  t8 = four;
  show(t5);
  show(t6);

  try {
    onePd.toStart();
    while (!onePd.atEnd()) {
      cout << onePd.getRight() << " ";
      onePd++;
    }
    cout << endl;
    onePd.toStart();
    delete four;
    while (!onePd.atEnd()) {
      cout << onePd.getRight() << " ";
      onePd++;
    }
  } catch (IterError xx) {
    cout << endl << ">>> Instance-specific assertion error message:" << endl
         << "#X# IterError: " << xx.getMesg() << endl
         << "<<< End of assertion error message." << endl;
  } 


  ConstListIter<int> t1,t2;
  ListIter<int> t3,t4;
  
  t1 = onePa;
  cout << "-- -- -- -- -- (cout) -- -- -- -- --" << endl;
  cout << t1 << endl;
  t2 = &onePb;
  show(t2);
  t3 = onePc;
  dump(&t3);
  try {
    cout << ">>> Instance-specific assertion error message:" << endl;
    t4 = &onePd;
    show(t4);
  } catch (IterError xx) {
    cout << endl
         << "#X# IterError: " << xx.getMesg() << endl
         << "<<< End of assertion error message." << endl;
  } 

  ListIter<int> oth;
  oth.assign(new List<int>(),True);
  oth.addRight(3);
  oth.addRight(4);
  oth.addRight(5);
  oth.addRight(6);
  show(oth);

  List<int> oth1(oth.container());
  ListIter<int> last_a(&oth1);
  show( last_a );
		  

  return 0;
}

static void show(const ConstListIter<int> &i) {
    cout << "-- -- -- -- -- (show) -- -- -- -- --" << endl;
    cout << i << endl;
}

static void dump(const ConstListIter<int> *i) {
    ConstListIter<int> list(i);
    cout << "-- -- -- -- -- (dump) -- -- -- -- --" << endl;
    cout << "len=" << list.len() << (char) cout.fill() <<  "pos=" << list.pos();
    list.toStart();
    while (list.atEnd() == False) {
	cout << (char) cout.fill() << list.getRight();
	list++;
    }
    cout << endl;
}
