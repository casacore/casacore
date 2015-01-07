//# RetypedArraySetGet.h: Helper functions for users of RetypedArrayEngine
//# Copyright (C) 1994,1995,1996,1999
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

#ifndef TABLES_RETYPEDARRAYSETGET_H
#define TABLES_RETYPEDARRAYSETGET_H

//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Array;
class IPosition;


// <summary>
// Helper functions for users of RetypedArrayEngine
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="dRetypedArrayEngine.cc" demos=dRetypedArrayEngine.h>
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=RetypedArrayEngine>RetypedArrayEngine</linkto>
// </prerequisite>

// <synopsis> 
// The functions in here can be used in the implementation of the
// CopyInfo class inside a SourceType class used by a RetypedArrayEngine.
// </synopsis>

// <example>
// The example in RetypedArrayEngine shows how these functions can be used.
// </example>

// <motivation>
// These functions make the implementation of the set and get
// functions in the SourceType objects of the RetypedArrayEngine easier.
// They are not part of the RetypedArrayEngine.h file to avoid
// the inclusion of that (heavy) file in a SourceType.
// </motivation>

// <group name=RetypedArrayEngineSetGet>

// Copy the entire target array to the source array.
// It will check if the shapes and sizes match.
// <br>
// This very efficient copy function can only be called by the static set
// function in the SourceType when the TargetType array can directly be
// copied to the SourceType array.
// <br>See
// <linkto class=RetypedArrayEngine>RetypedArrayEngine</linkto> for
// more information.
template<class SourceType, class TargetType>
void retypedArrayEngineSet (Array<SourceType>& out,
			    const Array<TargetType>& in);

// Copy the entire source array to the target array.
// It will check if the shapes and sizes match.
// <br>
// This very efficient copy function can only be called by the static set
// function in the SourceType when the TargetType array can directly be
// copied to the SourceType array.
// <br>See
// <linkto class=RetypedArrayEngine>RetypedArrayEngine</linkto> for
// more information.
template<class SourceType, class TargetType>
void retypedArrayEngineGet (Array<TargetType>& out,
			    const Array<SourceType>& in);

// Fill an array with SourceType objects from the target array.
// This is called when the target is incomplete.
// The shape and extra argument can help to set the correct
// elements in the source.
// <br>
// It loops through all elements in the SourceType array and
// calls the SourceType function
// It calls the SourceType function
//  <srcblock>
//    void setElem (const TargetType* data, const IPosition& shape,
//                  const void* extraArgument);
//  </srcblock>
// for each element.
// <note role=tip>
// This retypedArrayEngineSet function is only a convenience function.
// For optimal performance it may be needed to handcode the loop instead
// of using this function.
// </note>
// <br>See
// <linkto class=RetypedArrayEngine>RetypedArrayEngine</linkto> for
// more information.
template<class SourceType, class TargetType>
void retypedArrayEngineSet (Array<SourceType>& out,
			    const Array<TargetType>& in,
			    const IPosition& shape,
			    const void* extraArgument);

// Fill an array with TargetType objects from the source array.
// This is called when the target is incomplete.
// The shape and extra argument can help to get the correct
// elements from the source.
// <br>
// It loops through all elements in the SourceType array and
// calls the SourceType function
//  <srcblock>
//    void getElem (TargetType* data, const IPosition& shape,
//                  const void* extraArgument);
//  </srcblock>
// for each element.
// <note role=tip>
// This retypedArrayEngineGet function is only a convenience function.
// For optimal performance it may be needed to handcode the loop instead
// of using this function.
// </note>
// <br>See
// <linkto class=RetypedArrayEngine>RetypedArrayEngine</linkto> for
// more information.
template<class SourceType, class TargetType>
void retypedArrayEngineGet (Array<TargetType>& out,
			    const Array<SourceType>& in,
			    const IPosition& shape,
			    const void* extraArgument);


// </group>



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/DataMan/RetypedArraySetGet.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
