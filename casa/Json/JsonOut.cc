//# JsonOut.cc: Fill a file or stream in Json format
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$


//# Includes
#include <casacore/casa/Json/JsonOut.h>
#include <casacore/casa/Json/JsonError.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <sstream>
#include <iomanip>
#include <ctype.h>    //# for iscntrl

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  JsonOut::JsonOut()
    : itsStream (cout),
      itsLevel  (0)
  {}

  JsonOut::JsonOut (const String& name)
    : itsFile   (name.chars()),
      itsStream (itsFile),
      itsLevel  (0)
  {}

  JsonOut::JsonOut (ostream& os)
    : itsStream (os),
      itsLevel  (0)
  {}

  // Close the stream.
  JsonOut::~JsonOut()
  {}

  void JsonOut::start (const String& commentStart, const String& commentEnd,
                       const String& indent)
  {
    AlwaysAssert (itsLevel==0, JsonError);
    itsStream << '{' << endl;
    itsIndent       = indent;
    itsIndentStep   = indent;
    itsCommentStart = commentStart;
    itsCommentEnd   = commentEnd;
    itsLevel = 1;
    itsFirstName.resize (1);
    itsFirstName[0] = True;
  }

  void JsonOut::end()
  {
    itsLevel--;
    AlwaysAssert (itsLevel==0, JsonError);
    itsIndent.clear();
    itsStream << '}' << endl;
  }

  void JsonOut::startNested (const String& name, const String& comment)
  {
    AlwaysAssert (itsLevel>0, JsonError);
    writeComment (comment);
    putName (name);
    itsStream << '{' << endl;
    itsIndent += itsIndentStep;
    itsLevel++;
    itsFirstName.resize (itsLevel);
    itsFirstName[itsLevel-1] = True;
  }

  void JsonOut::endNested()
  {
    itsLevel--;
    AlwaysAssert (itsLevel>0, JsonError);
    itsIndent = itsIndent.substr (0, itsIndent.size() - itsIndentStep.size());
    itsStream << itsIndent << '}' << endl;
  }

  void JsonOut::writeKV (const String& name, const ValueHolder& vh)
  {
    if (vh.isNull()) {
      putNull();
    } else {
      switch (vh.dataType()) {
      case TpBool:
        writeKV (name, vh.asBool());
        break;
      case TpChar:
      case TpUChar:
      case TpShort:
      case TpUShort:
      case TpInt:
      case TpUInt:
      case TpInt64:
        writeKV (name, vh.asInt64());
        break;
      case TpFloat:
        writeKV (name, vh.asFloat());
        break;
      case TpDouble:
        writeKV (name, vh.asDouble());
        break;
      case TpComplex:
        writeKV (name, vh.asComplex());
        break;
      case TpDComplex:
        writeKV (name, vh.asDComplex());
        break;
      case TpString:
        writeKV (name, vh.asString());
        break;
      case TpArrayBool:
        writeKV (name, vh.asArrayBool());
        break;
      case TpArrayChar:
      case TpArrayUChar:
      case TpArrayShort:
      case TpArrayUShort:
      case TpArrayInt:
      case TpArrayUInt:
      case TpArrayInt64:
        writeKV (name, vh.asArrayInt64());
        break;
      case TpArrayFloat:
        writeKV (name, vh.asArrayFloat());
        break;
      case TpArrayDouble:
        writeKV (name, vh.asArrayDouble());
        break;
      case TpArrayComplex:
        writeKV (name, vh.asArrayComplex());
        break;
      case TpArrayDComplex:
        writeKV (name, vh.asArrayDComplex());
        break;
      case TpArrayString:
        writeKV (name, vh.asArrayString());
        break;
      case TpRecord:
        writeKV (name, vh.asRecord());
        break;
      default:
        throw JsonError("JsonOut: unknown ValueHolder datatype");
      }
    }
  }

  void JsonOut::writeComment (const String& comment)
  {
    if (!itsCommentStart.empty()  &&  !comment.empty()) {
      itsStream << itsIndent << ' ' << itsCommentStart << ' ' << comment;
      if (!itsCommentEnd.empty()) {
        itsStream << itsCommentEnd;
      }
      itsStream << endl;
    } 
 }

  String JsonOut::indentValue (const String& indent, const String& name) const
  {
    String extra("                    ");
    return indent + extra.substr(0, std::min(15,int(name.size())) + 5);
  }

  void JsonOut::putName (const String& name)
  {
    itsStream << itsIndent;
    if (itsFirstName[itsLevel-1]) {
      itsStream << ' ';
      itsFirstName[itsLevel-1] = False;
    } else {
      itsStream << ',';
    }
    itsStream << '"' << name << "\": ";
  }

  void JsonOut::putNull()
  {
    itsStream << "null";
  }

  void JsonOut::put (Bool value)
  {
    itsStream << (value ? "true" : "false");
  }
  void JsonOut::put (Float value)
  {
    if (! isFinite(value)) {
      putNull();
    } else {
      char buf[16];
      sprintf (buf, "%.7g", value);
      // Add a decimal point if needed, otherwise it is integer.
      unsigned i;
      for (i=0; i<sizeof(buf); ++i) {
        if (buf[i] == '.'  ||  buf[i] == 'e') break;
        if (buf[i] == 0) break;
      }
      if (buf[i] ==0 ) {
        buf[i]   = '.';
        buf[i+1] = '0';
        buf[i+2] = 0;
      }
      itsStream << buf;
    }
  }
  void JsonOut::put (Double value)
  {
    if (! isFinite(value)) {
      putNull();
    } else {
      char buf[24];
      sprintf (buf, "%.16g", value);
      // Add a decimal point if needed, otherwise it is integer.
      unsigned i;
      for (i=0; i<sizeof(buf); ++i) {
        if (buf[i] == '.'  ||  buf[i] == 'e') break;
        if (buf[i] == 0) break;
      }
      if (buf[i] ==0 ) {
        buf[i]   = '.';
        buf[i+1] = '0';
        buf[i+2] = 0;
      }
      itsStream << buf;
    }
  }
  void JsonOut::put (const Complex& value)
  {
    itsStream << "{\"r\":";
    put (value.real());
    itsStream << ", \"i\":";
    put (value.imag());
    itsStream << '}';
  }
  void JsonOut::put (const DComplex& value)
  {
    itsStream << "{\"r\":";
    put (value.real());
    itsStream << ", \"i\":";
    put (value.imag());
    itsStream << '}';
  }
  void JsonOut::put (const char* value)
    { itsStream << '"' << escapeString(value) << '"'; }
  void JsonOut::put (const String& value)
    { itsStream << '"' << escapeString(value) << '"'; }

  void JsonOut::put (const Record& rec)
  {
    itsStream << '{' << endl;
    String oldIndent(itsIndent);
    itsIndent += itsIndentStep;
    itsLevel++;
    itsFirstName.resize (itsLevel);
    itsFirstName[itsLevel-1] = True;
    for (uInt i=0; i<rec.nfields(); ++i) {
      write (rec.name(i), rec.asValueHolder(i));
    }
    itsLevel--;
    itsIndent = oldIndent;
    itsStream << itsIndent << '}';
  }

  String JsonOut::escapeString (const String& in)
  {
    String out;
    out.reserve (in.size());
    for (size_t i=0; i<in.size(); ++i) {
      switch (in[i]) {
      case '\b':
        out.append ("\\b");  // backspace
        break;
      case '\f':
        out.append ("\\f");  // formfeed
        break;
      case '\n':
        out.append ("\\n");  // newline
        break;
      case '\r':
        out.append ("\\r");  // carriage return
        break;
      case '\t':
        out.append ("\\t");  // tab
        break;
      case '"':
      case '\\':
        out.append ('\\');
        out.append (in[i]);
        break;
      default:
        if (iscntrl(in[i])) {
          ostringstream oss;
          oss << "\\u" << std::hex << std::uppercase << std::setfill('0')
              << std::setw(4) << static_cast<int>(in[i]);
          out.append (oss.str());
        } else {
          out.append (in[i]);
        }
      }
    }
    return out;
  }

} //# NAMESPACE CASACORE - END
