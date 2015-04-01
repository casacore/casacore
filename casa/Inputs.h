//# Inputs.h: a module for simple command line user interface classes
//# Copyright (C) 1994,1995,1996,1999,2000
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

#ifndef CASA_INPUTS_H
#define CASA_INPUTS_H

#include <casacore/casa/aips.h>

#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Inputs/Param.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module> 
//
// <summary> 
// A module for simple command line user interface classes
// </summary>

// <prerequisite>
//   <li> String
//   <li> The C language int main(int argc, const char* argv[]) convention.
// </prerequisite>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" demos="">
//</reviewed>

// <etymology> 
// The Inputs module name reflects the Casacore convention of pluralizing 
// the name of the major class it contains.
// The Input class name is a reflection of it's role as the early command 
// line user interface for Casacore applications. This class provides "inputs" 
// in the form "key=value" or "-key value."  
//</etymology> 
//
// <synopsis> 

// During the old AIPS++ prototyping stage a basic command line user
// interface was developed. This attempt at easing the trouble of passing
// information to an executable program resulted in a set of C++ classes
// called Param and Input.  The programmer may simply include the Input
// class into their code and have immediate Command Line User Interface
// (CLUI) capabilities.  The programmer's Casacore application is run from
// the unix level prompt by invoking its name and listing linearly on the
// same command line the "keyword=values" or "-keyword values" associated
// with proper execution.  The Input and Param classes will successfully
// parse the command line into the executable program and check for
// appropriateness.

// The CLUI capabilities are further extended to a Graphical User
// Interface through the use of the Khoros Cantata environment. The user
// starts up Cantata from the unix prompt and, by utilizing a series of
// pull down windows, invisibly creates an X-based window for visual
// display of all parameters associated with the Casacore application's
// need for external input.

// The basic command line user interface is an ordered series of
// "keyword=value" pairs, which we call parameters. The names parameter
// and keyword may correctly be used to refer to each other.
// 
// The class Param (see Param.h) implements one single such parameter.
// Values may be Int, Block<Int>, double, Block<double>, Bool, or
// Strings.  In addition to a name and a value, a Param parameter has a
// variety of other attributes, such as a one-line help string (useful
// when being prompted for input or with hypertext identifiers, etc...),
// a type, a range and optional units.  All of these attributes are
// character strings; parsing and error checking is done at a different
// (hidden) level.  The programmer, however, will never interact with a
// parameter through it's Param class interface.  Interaction is done
// with the class Input, which is a container of Param's, with a variety
// of user interface attributes (help-level, debug-level, etc...).
// 
// Although the programmer must supply the user interface with a number
// of predefined program parameters, the user interface itself will
// create a small number of system parameters (help=, debug=).  The
// purpose of these is to tell the task how to communicate with the user
// and it's environment, and give the user control over these items.  For
// example, the user may want to see (debug) messages above a certain
// threshold level.  The programmer simply adds debug levels to their
// code and allows the user to specify how deeply they wish the debugging
// to progress.
// 
// For example, a interactive UNIX shell session may look like:
// 
//<srcblock>
//     1% MyProgram key1=val1 key3=val3
//     2% MyProgram key1=val1 key2=val3 debug=5
//     3% MyProgram help=prompt
//     4% MyProgram help=pane > prog.pane
//</srcblock>
// 
// In command 1% the user has set several parameters for the program
// MyProgram to applicable values.  The 2% command line invokes the
// executable and sets the level of displayed debugging to the programmer
// specified 5th level.  Command 3%: the user is prompted, and parameter
// default values are restored.  Command 4% gives an example of the
// self-describing mode of programs, where a pane description file for
// Khoros has been constructed.  The latter is the first step toward
// building a Khoros Graphic User Interface.
// 
// The Input class is a means for building a linked list of parameters
// and gaining access to them once created.  Input takes care of
// system/environment variables and assigns their values within the
// programmer's code.  The linked list of parameters is limited only by
// the number of names the programmer can dream up.  The programmer need
// not think hard on the order of definition of parameters in Input. The
// list of key=values given on the command line by the user need not be
// in any specific order.
// 
// The definition of parameters is by simply creating an Input and then
// using the appropriate Input Create member function.  Then the
// programmer adds to the list of parameters as necessary.
// </synopsis> 
//
// <example>
// <srcblock>
// 01 #include <casacore/casa/Inputs/Input.h>      // need this if you want it to work
// 02 #include <aips/Plot.h>
// 03 int main(int argc, const char* argv[])
// 04 {
// 05    Input inputs(1);
// 06    // Define our input structure
// 07    inputs.version("Id: xyPlot.C,v 1.1 1993/01/29 20:45:48 bglenden Exp");
// 08    inputs.create("xyfile", 
// 09                "/tmp/xy.aipsio",
// 10                "File which contains xy vectors",
// 11                "InFile");
// 12    inputs.create("overplot", "False", "Multiple plots?", "Bool");
// 13    inputs.create("lines", "True", "Plot lines or points?", "Bool");
// 14    
// 15    // and Fill them from the command line
// 16    inputs.readArguments(argc, argv);
// 17
// 18    try {
// 19      const Char *filename = inputs.getString("xyfile");
// 20      AipsIO xyfile(filename, ByteIO::Old);
// 21      Vector<float> x, y;
// 22      Plot plot;
// 23
// 24      xyfile >> x >> y; // initial vectors
// 25      plot(x,y,inputs.getBool("lines"));
// 26
// 27      for (;;) { // forever
// 28          xyfile >> x >> y;
// 29          if (inputs.getBool("overplot") == True) {
// 30              plot(x,y,inputs.getBool("lines"));
// 31          } else {
// 32              plot.newPlot();
// 33              plot(x,y,inputs.getBool("lines"));
// 34          }
// 35      }
// 36  } catch (AipsIOError x) {
// 37       ; // nothing - no more data
// 38  } catch (AllocError x) {
// 39       cerr << "AllocError : " << x.getMesg() << endl;
// 40       cerr << "Size is : " << x.size() << endl;
// 41  } catch (AipsError x) {
// 42       cerr << "aipserror: error " << x.getMesg() << endl;
// 43       return 1;
// 44  }
// 45
// 46    cout << "Any key to exit:\n";
// 47
// 48  char ch;
// 49  cin.get(ch);
// 50
// 51  return 0;
// 52 }
// </srcblock>
// Let us discuss this program line for line.
// 
// 03 - This is the method of passing the command line through to the
// main body of code.  This obviously makes it mandatory.  The inclusion
// of the argc, argv is very well discussed in Stroustrup, The
// C++ Programming Language, page 87.
// 
// 05 - The instantiation of Input in the variable inputs(1) is done with
// an integer argument of (1) to indicate the constructor should build
// inputs with a pair of system parameters and read in values for them.
// An argument of (0) would build an Input that was empty and would
// obligate the programmer to build a list of Params explicitly.
// 
// 07 - The version of the code is stored within the Input.  Note the
// optional use of RCS keyword substitution.  See the "co" man page for
// details. This allows the code to be automatically updated.
// 
// 08-11 - The create member function of Input builds, in this case, a
// parameter called xyfile, immediately filled with the String containing
// the directory that holds the data. The help String is useful for new
// users or prompting.  The fourth argument of InFile is the optional
// type of the parameter's value.  Any suitable String may be used.
// Missing from this example are the optional fifth and sixth arguments,
// the parameter's value's range and units, respectively.
// 
// 12 - This is another instantiation of a Param inside of Input.  This
// parameter will be referenced by the keyword "overplot".  It is
// initialized to False and is of type Bool.
// 
// 13 - This line is the third and final Param placed in inputs and is
// recognized by the code when accessed with keyword "lines".
// 
// 16 - The call of readArguments(argc, argv) should be done after the
// list of Params has been completed.  This line of code fills the values
// from the command line. A keyword that doesn't match will throw an
// error.
// 
// 19 - At this point the local variable filename is initialized to the
// String value held within the parameter accessed through the key
// "xyfile".  Recall that the value of xyfile was originally set to
// "/tmp/xy.aipsio" but would be replaced with the proper value at
// execution.  The getString member function returns either the default
// value specified during the xyfile parameter's instantiation or the
// value placed into it from the command line use of xyfile=myfile.
// 
// 25 - Here the boolean value of the Param called lines is inserted into
// the call to the function plot.
// 
// 29 - Again the Input interface has its parameter called overplot
// return a boolean to be used as a test for an "if".  The getBool(key)
// Input member function may be reading the default value of the
// appropriate parameter called key or using the value passed from the
// command line.
// 
// 30 & 33 - Another call to plot that uses the boolean value stored in
// the parameter called lines.
// </example> 
// 
//<motivation> 
// This module fit the early needs of a a simple user interface.
// </motivation>

// <todo asof="Thu 199504/06 21:26:43 GMT">
//   <li> possibly replace the Param class with Keywords
// </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif






