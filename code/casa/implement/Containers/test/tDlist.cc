//# tDlist.cc: This program tests the Dlist class
//# Copyright (C) 1993,1994,1995,2000
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

#include <iostream.h>
#include <aips/Containers/Dlist.h>

main() {

  Dlist<int> one;
  DlistIter<int> onePa(one);
  DlistIter<int> onePb(one);

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
  DlistIter<int> onePc = onePa;
  while(onePc.atEnd() != True) {
    cout << onePc.getRight() << ".";
    onePc++;
  }
  cout << endl;
  Dlist<int> two;
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

  Dlist<int> three;
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
  for(;onePb.atEnd() != True;onePb++);
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

  Dlist<int> *four = new Dlist<int>;
  DlistIter<int> onePd = four;

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
    cout << ">>> Instance-specific assertion error message:" << endl
         << "#X# IterError: " << xx.getMesg() << endl
         << "<<< End of assertion error message." << endl;
  } 


  return 0;
}
