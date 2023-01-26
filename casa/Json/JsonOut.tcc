//# JsonOut.tcc: Fill a file or stream in Json format
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


//# Includes
#include <casacore/casa/Json/JsonOut.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  template <typename T>
  inline void JsonOut::write (const String& name, T value,
                              const String& comment)
  {
    writeComment (comment);
    putName (name);
    writeKV (name, value);
    itsStream << endl;
  }

  template <typename T>
  inline void JsonOut::writeKV (const String&, T value)
  {
    put (value);
  }

  template <typename T>
  inline void JsonOut::writeKV (const String& name, const Array<T>& value)
  {
    // Use extra indentation for possible continuation lines.
    putArray (value, indentValue(itsIndent, name), true);
  }

  template <typename T>
  inline void JsonOut::put (T value)
    { itsStream << value; }

  template <typename T>
  inline void JsonOut::putArray (const Array<T>& arr,
                                 const String& indent, bool firstLine)
  {
    putArray (arr, indent, firstLine, false);
  }
  inline void JsonOut::putArray (const Array<String>& arr,
                                 const String& indent, bool firstLine)
  {
    putArray (arr, indent, firstLine, true);
  }

  template <typename T>
  void JsonOut::putArray (const Array<T>& arr, const String& indent,
                          bool firstLine, bool valueEndl)
  {
    if (!firstLine) itsStream << indent;
    itsStream << '[';
    bool first = true;
    if (arr.ndim() <= 1) {
      size_t todo = arr.size();
      typename Array<T>::const_iterator iterEnd = arr.end();
      for (typename Array<T>::const_iterator iter=arr.begin();
           iter!=iterEnd; ++iter) {
        if (first) {
          first = false;
        } else if (!valueEndl) {
          itsStream << ", ";
        } else {
          itsStream << indent << ' ';
        }
        put (*iter);
        todo--;
        if (valueEndl  &&  todo > 0) {
          itsStream << ',' << endl;
        }
      }
    } else {
      ArrayIterator<T> iter(arr, IPosition(1, arr.ndim()-1), false);
      while (! iter.pastEnd()) {
        if (!first) {
          itsStream << ',' << endl;
        }
        putArray (iter.array(), indent+' ', first);
        first = false;
        iter.next();
      }
    }
    itsStream << ']';
  }


} //# NAMESPACE CASACORE - END
