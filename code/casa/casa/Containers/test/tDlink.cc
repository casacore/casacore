//# tDlink.cc: This program tests the doubly linked list class
//# Copyright (C) 1993,1994,1995,2001
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

#include <casa/Containers/Dlink.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
main() {
  Dlink<int> *hed = new Dlink<int>(23);

  hed = new Dlink<int>(12,0,hed);
  hed = new Dlink<int>(19,0,hed);
  hed = new Dlink<int>(82,0,hed);
  hed = new Dlink<int>(71,0,hed);
  hed = new Dlink<int>(3,0,hed);
  hed = new Dlink<int>(10,0,hed);

  Dlink<int> *cur = hed;
  int forward = 1;
  cout << ">";
  while (cur) {
    cout << (*cur).val() << ".";
    if (forward)
      if ((*cur).next())
	cur = (Dlink<int>*) (*cur).next();
      else {
	cout << endl << "<";
	forward = 0;
      }
    else
      cur = (*cur).prev();
  }
  cout << endl;

  cur = (Dlink<int>*) hed->next()->next()->next()->next();
  Dlink<int> *prev = cur->prev();

  cur = new Dlink<int>(103,prev,cur);
  cur = new Dlink<int>(111,prev,cur);
  cur = new Dlink<int>(172,prev,cur);

  cur = hed;
  forward = 1;
  Dlink<int> *end = 0;
  cout << ">";
  while (cur) {
    cout << (*cur).val() << ".";
    if (forward)
      if ((*cur).next())
	cur = (Dlink<int>*) (*cur).next();
      else {
	cout << endl << "<";
	end = cur;
	forward = 0;
      }
    else
      cur = (*cur).prev();
  }
  cout << endl;

  end = new Dlink<int>(203,end);
  end = new Dlink<int>(221,end);
  end = new Dlink<int>(201,end);

  cur = hed;
  forward = 1;
  cout << ">";
  while (cur) {
    cout << (*cur).val() << ".";
    if (forward)
      if ((*cur).next())
	cur = (Dlink<int>*) (*cur).next();
      else {
	cout << endl << "<";
	forward = 0;
      }
    else
      cur = (*cur).prev();
  }
  cout << endl;

  cur = hed;
  while (cur) {
    (*cur).val()++;
    cur = (Dlink<int>*) (*cur).next();
  }

  cur = hed;
  forward = 1;
  cout << ">";
  while (cur) {
    cout << (*cur).val() << ".";
    if (forward)
      if ((*cur).next())
	cur = (Dlink<int>*) (*cur).next();
      else {
	cout << endl << "<";
	forward = 0;
      }
    else
      cur = (*cur).prev();
  }
  cout << endl;

  return 0;
}
