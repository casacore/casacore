//# tTaQLNode.cc: This program tests parsing of table commands using TaQLNode
//# Copyright (C) 2005
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

#include <casacore/tables/TaQL/TableGram.h>
#include <casacore/tables/TaQL/TaQLNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>

// <summary>
// Test program for parsing of table commands using TaQLNode
// </summary>

// This program tests table commands with an SQL-like grammar.
// The grammar is scanned and parsed using the flex/bison file TableGram.ll/yy
// and with the help of the class TaQLNode.
// It ask for commands until a "q" is given.


void seltab (const String&);
void docomm ();

int main (int argc, const char* argv[])
{
  try {
    if (argc > 1) {
      seltab(argv[1]);
    }else{
      docomm();
    }
  } catch (std::exception& x) {
    cout << "\nCaught an exception: " << x.what() << endl;
    return 1;
  } 
  return 0;               // successfully executed
}


// Ask and execute command till empty string is given.
void docomm()
{
  char comm[1025];
  while (True) {
    cout << "Table command (q=quit): ";
    cin.getline (comm, 1024);
    String str(comm);
    if (str == "q") 
      break;
    try {
      seltab (str);
    } catch (std::exception& x) {
      cout << x.what() << endl;
    } 
  }
}

// Sort and select data.
void seltab (const String& str)
{
  cout << str << endl;
  TaQLNode node;
  try {
    // Parse the command.
    node = TaQLNode::parse (str);
  } catch (std::exception& x) {
    cout << x.what() << endl;
    return;
  }
  ostringstream oss;
  node.show (oss);
  cout << oss.str() << endl;
  // Now see if parsing the result gives the same result.
  TaQLNode node1;
  try {
    // Parse the command.
    node1 = TaQLNode::parse (oss.str());
  } catch (std::exception& x) {
    cout << x.what() << endl;
    return;
  }
  ostringstream oss1;
  node1.show (oss1);
  if (oss.str() != oss1.str()) {
    cout << "Error: different parse result" << endl;
    cout << oss1.str() << endl;
  }
  // Save and restore the parse tree.
  // See if it gives the same result.
  MemoryIO mio;
  AipsIO aio(&mio);
  node.save (aio);
  aio.setpos (0);
  TaQLNode node2 = TaQLNode::restore (aio);
  ostringstream oss2;
  node2.show (oss2);
  if (oss.str() != oss2.str()) {
    cout << "Error: different save/restore result" << endl;
    cout << oss2.str() << endl;
  }
  cout << endl;
}
