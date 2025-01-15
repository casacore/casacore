//# JsonParser.cc: Class for parsing Json key-value lines
//# Copyright (C) 2016
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/Json/JsonKVMap.h>
#include <casacore/casa/Json/JsonParser.h>
#include <casacore/casa/Json/JsonError.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include <stdio.h>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
//# Bison defines WHERE, which is also defined in LogOrigin.h (which
//# is included in auto-template mode).
//# So undefine WHERE first.
#undef WHERE
#include <casacore/casa/stdlib.h>

//# Let clang and gcc ignore some warnings in bison/flex code.
//# Undef YY_NULL because flex can redefine it slightly differently (giving a warning).
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpragmas"
#pragma GCC diagnostic ignored "-Wdeprecated-register"
#pragma GCC diagnostic ignored "-Wsign-compare"

#include "JsonGram.ycc"                  // bison output
#ifdef YY_NULL
# undef YY_NULL
#endif
#include "JsonGram.lcc"                  // flex output
#pragma GCC diagnostic pop


//# Define extern symbols in JsonGram.cc.
//extern int yy_start;
extern char* JsonGramtext;
extern FILE* JsonGramin;
extern int JsonGramparse();
extern void JsonGramrestart(FILE*);

namespace casacore {

  // Initialize statics.
  int JsonParser::theirPosition = 0;
  const char* JsonParser::theirCommand = 0;
  JsonKVMap* JsonParser::theirJsonMap = 0;


  JsonKVMap JsonParser::parseFile (const String& fileName)
  {
    String command;
    char buf[4096];
    std::ifstream ifs(fileName.c_str());
    if (!ifs) {
      throw JsonError("Json file " + fileName + " could not be opened");
    }
    while (ifs.getline(buf, sizeof(buf))) {
      command += buf;
      command += '\n';
    }
    return parse(command);
  }

  JsonKVMap JsonParser::parse (const String& command)
  {
    // Return an empty map if the command is empty.
    Bool empty = true;
    for (size_t i=0; i<command.size(); i++) {
      if (command[i] != ' ') {
        empty = false;
        break;
      }
    }
    if (empty) {
      return JsonKVMap();
    }
    //# Parse the command.
    //# Do a yyrestart(yyin) first to make the flex scanner reentrant.
    JsonGramrestart (JsonGramin);
    yy_start = 1;
    theirCommand = command.c_str();     // get pointer to command string
    theirPosition = 0;                  // initialize string position
    delete theirJsonMap;
    theirJsonMap = 0;
    JsonGramparse();                    // parse command string
    JsonKVMap map(*theirJsonMap);
    delete theirJsonMap;
    theirJsonMap = 0;
    return map;
  }

  int JsonParser::input (char* buf, int max_size)
  {
    int nr=0;
    while (*theirCommand != 0) {
      if (nr >= max_size) {
        break;                         // get max. max_size char.
      }
      buf[nr++] = *theirCommand++;
    }
    return nr;
  }

  String JsonParser::removeEscapes (const String& in)
  {
    String out;
    String::size_type leng = in.length();
    for (String::size_type i=0; i<leng; ++i) {
      if (in[i] == '\\') {
        i++;
        if (i < leng) {
          switch (in[i]) {
          case 'b':
            out += '\b';  // backspace
            break;
          case 'f':
            out += '\f';  // formfeed
            break;
          case 'n':
            out += '\n';  // newline
            break;
          case 'r':
            out += '\r';  // carriage return
            break;
          case 't':
            out += '\t';  // tab
            break;
          case 'u':
            {
              // unicode repr of control character
              Bool ok = False;
              if (i < leng+4) {
                String hex("0X" + in.substr(i+1,4));
                char* endPtr;
                Int64 val = strtoll(hex.chars(), &endPtr, 0);
                if (endPtr == hex.chars()+hex.size()  &&  val < 128) {
                  out += char(val);
                  i += 4;
                  ok = True;
                }
              }
              if (!ok) {
                throw JsonError ("Invalid escaped control character " +
                                 in.substr(i-1,6));
              }
            }
            break;
          default:
            out += in[i];
          }
        }
      } else {
        out += in[i];
      }
    }
    return out;
  }


  void JsonGramerror (const char*)
  {
    std::ostringstream os2;
    os2 << JsonParser::position();
    String s2(os2.str());
    throw JsonError("Json parse error at position "
                    + s2 + " (at or near '" +
                    String(JsonGramtext) + "')");
  }

} // end namespace


// Define the yywrap function for flex.
// Note it is not declared in the .h file, but by flex.
int JsonGramwrap()
{
    return 1;
}
