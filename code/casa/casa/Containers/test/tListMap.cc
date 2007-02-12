//# tListMap.cc: This program tests the ListMap class
//# Copyright (C) 1993,1994,1995,1999,2001
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

#include <casa/Containers/ListMap.h>
#include <casa/BasicSL/String.h>
#include <casa/Containers/OrdPairIO.h>
#include <casa/Containers/MapIO.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
//template<class key,class value> void print(const ListMap<key,value> &);

main() {
  ListMap<int,int> map(-1);
  ListMap<String,OrderedPair<String,uInt> > smap(OrderedPair<String,uInt>("dummy",0));
  ListMap<String,int> amap(-1,ListMap<String,int>::Append);

  map(2) = 90;
  map(15) = 79;
  map(27) = 104;
  map(21) = map(27);

  map.setOrder(ListMap<int,int>::Append);
  map(81) = 4;
  map(21) = 109;
  map(29) = map(21);
  map(28) = map(29);
  map(4) = 9;
  map.setOrder(ListMap<int,int>::Prepend);
  map(30) = 4;
  map(6) = 92;

  cout << map << endl;

//print(map);

  smap("fred") = OrderedPair<String,uInt>("wilma",30);
  smap("barney") = OrderedPair<String,uInt>("betty",28);
  smap("homer")  = OrderedPair<String,uInt>("marge",40);
  smap("bambam") = OrderedPair<String,uInt>("pebbles",8);

  cout << smap << endl;

//print(smap);

  smap("barney") = OrderedPair<String,uInt>("madona",25);
  smap("fred") = OrderedPair<String,uInt>("anne",12);
 
  cout << smap << endl;

//print(smap);
//print(smap);

  cout << smap("homer").x() << "," << smap("homer").y() << endl;
  cout << smap("fred").x() << "," << smap("fred").y() << endl;
  cout << smap("barney").x() << "," << smap("barney").y() << endl;

  amap("a") = 97;
  amap("b") = 98;
  amap("c") = 99;
  amap("d") = 100;
  amap("e") = 101;
  amap("f") = 102;

  cout << amap << endl;

//print(amap);

  return(0);

}

//#
//# Moved here to work around YASSB (yet another stupid sun bug)
//#
// template<class key,class value> void print(const ListMap<key,value> &xx){
//   ConstListMapIter<key,value> x(xx);
//   x.toStart();
//   while (!x.atEnd()) {
//     cout << "(" << x.getKey() << "," << x.getVal() << ")" << " ";
//     x++;
//   }
//   cout << endl;
// }
