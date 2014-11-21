//# tChoice.cc: Test program for class Choice
//# Copyright (C) 2004
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
//#
//# $Id$

#include <casacore/casa/System/Choice.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <iostream>

#include <casacore/casa/namespace.h>
int main()
{
  Vector<String> choices(3);
  choices[0] = "no";
  choices[1] = "yes";
  choices[2] = "none";

  // The default is no.
  AlwaysAssertExit (Choice::choice ("ask", choices) == "no");

  // Set the choice function to asking on stdout.
  Choice::setChoiceFunc (Choice::stdoutChoice);

  // Ask the choice 3 times and show the result.
  std::cout << Choice::choice ("Give choice1", choices) << std::endl;
  std::cout << Choice::choice ("Give choice2", choices) << std::endl;
  std::cout << Choice::choice ("Give choice3", choices) << std::endl;

  return 0;
}
