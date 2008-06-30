//# Fits.h: The fits module -- FITS related classes.
//# Copyright (C) 2005
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

#ifndef FITS_FITS_H
#define FITS_FITS_H

#include <fits/FITS/BasicFITS.h>        
#include <fits/FITS/BinTable.h> 
#include <fits/FITS/blockio.h>         
#include <fits/FITS/CopyRecord.h>        
#include <fits/FITS/FITS2.h>             
#include <fits/FITS/FITSDateUtil.h>    
#include <fits/FITS/FITSError.h>        
#include <fits/FITS/FITSFieldCopier.h>
#include <fits/FITS/FITSHistoryUtil.h>  
#include <fits/FITS/FITSKeywordUtil.h>  
#include <fits/FITS/FITSMultiTable.h>   
#include <fits/FITS/FITSSpectralUtil.h>
#include <fits/FITS/FITSTable.h>       
#include <fits/FITS/FITSTimedTable.h>             
#include <fits/FITS/fits.h>
#include <fits/FITS/fitsio.h>
#include <fits/FITS/hdu.h>
#include <fits/FITS/SDFITSTable.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <module>
//
// <summary> Classes and global functions for system use </summary>

// <reviewed reviewer="" date="" demos="">
// </reviewed>
//
// <synopsis>
//
// This module is a bag of related fits classes and
// global functions.
//
// The following functionality is available:
// <ul>
//  <li> Class <linkto class=FITSFieldCopier:description>
//       FITSFieldCopier</linkto>
//       A FITSFieldCopier for copying Array RecordFields to FitsFields.
//  <li> Class <linkto class=AsciiTableExtension:description>
//       AsciiTableExtension</linkto>
//       (ascii) TABLE extension.
//  <li> Class <linkto class=BinaryTable:description>
//       BinaryTable</linkto>
//       BinaryTable is used to translate a FITS binary table to an aips++ Table.
//       BinaryTable inherits from the FITS BinaryTableExtension class and its
//       primary use is to convert that class to an aips++ Table.
//       The class starts with an already existing FitsInput object, which should
//       be set at a BinaryTableExtension HDU.   Member functions provide a TableDesc 
//       appropriate for the FITS data (to help in constructing an aips++ Table
//       compatible with the BinaryTableExtension), a Table containing the
//       current row of FITS data and a Table containing the next row of FITS data
//       (which can be used to step through the FitsInput, copying each row
//       using the RowCopier class), and a Table containin the entire FITS binary 
//       table from the current row to the end of the table.
//       <motivation>
//       We need a way to get FITS data into aips++ Tables.
//       </motivation>
//  <li> Class <linkto class=BinaryTableExtension:description>
//       BinaryTableExtension</linkto>
//       BINTABLE extension.
//  <li> Class <linkto class=BlockInput:description>
//       BlockInput</linkto>
//       fixed-length blocked sequential input base class.
//  <li> Class <linkto class=BlockIO:description>
//       BlockIO</linkto>
//       fixed-length blocked sequentual I/O base class.
//       BlockIO is a low level base class that implements fixed-length 
//       blocked sequential I/O. Its derived classes, BlockInput and BlockOutput
//       are used by the FitsInput and FitsOutput classes. Users will hardly ever
//       need to use this class directly.
//  <li> Class <linkto class=BlockOutput:description>
//       BlockOutput</linkto>
//       fixed-length blocked sequential output base class.
//  <li> Class <linkto class=ConstFitsKeywordList:description>
//       ConstFitsKeywordList</linkto>
//       list of read-only FITS keywords.
//  <li> Class <linkto class=CopyRecordToRecord:description>
//       CopyRecordToRecord</linkto>
//       Copies fields between Records, possibly to fields with another name.
//  <li> Class <linkto class=CopyRecordToTable:description>
//       CopyRecordToTable</linkto>
//       Copies fields from a Record to columns of a Table.
//       This class should be generalized, and made better. It is the analog of
//       RowCopier, i.e. it copies all the fields from some Record to certain
//       columns of a table. The mapping from fields to columns occurs at
//       construction of the CopyRecordToTable object.
//       <motivation>
//       This class should be generalized, and made better. It is the analog of
//       RowCopier, i.e. it copies all the fields from some Record to certain
//       columns of a table. The mapping from fields to columns occurs at
//       construction of the CopyRecordToTable object.
//       </motivation>
//  <li> Class <linkto class=ExtensionHeaderDataUnit:description>
//       ExtensionHeaderDataUnit</linkto>
//       Base class for generalized exentensions HDU.
//  <li> Class <linkto class=FITS:description>
//       FITS</linkto>
//       Static functions and enumerations.
//       Many of the static functions are utility functions used internally in the
//       implementation of the member functions of the FITS classes. They are placed
//       in a single class to encapsulate them and to avoid adding many names to the 
//       global name space. More important, from the user's perspective, are the
//       enumerations. They form the basic vocabulary of a FITS application. For example,
//       instead of referring to the FITS <src>NAXIS</src> keyword, 
//       <src>FITS::NAXIS</src> should be used.
//  <li> Class <linkto class=FITSDateUtil:description>
//       FITSDateUtil</linkto>
//       A class with static functions to help deal with FITS dates
//       This is a collection of static utility functions for creating and
//       interpreting FITS date keywords (e.g. DATE-OBS).
//       Its never necessary to construct a FITSDateUtil, just use the 
//       static functions to help handle FITS dates.
//       <motivation>
//       The strings that make up the value of FITS dates have a 
//       precise format.  This class encompasses knowlege of the formats
//       used and hopefully simplifies their creation and conversion
//       to and from aips++ MVTimes.
//       </motivation>
//  <li> Class <linkto class=FITSError:description>
//       FITSError</linkto>
//       Default FITS error handling function, typdef, and enumeration.
//       FITSError contains the enumeration specifying the possible error
//       message levels.  It also contains the default error handling function
//       for the FITS classes. 
//       <motivation>
//       Originally, FITS error message were simply sent to an ostream.  In
//       order to have these error messages go to the AIPS++ logger by default,
//       this class was added.  This was made a separate class because both
//       BlockIo and FITS need to use this class.  The anticipated replacements 
//       for the current FITS classes use a somewhat similar scheme.
//       </motivation>
//  <li> Class <linkto class=FITSFieldCopier:description>
//       FITSFieldCopier</linkto>
//       Virtual base class for copying RORecordFields to FitsFields.
//  <li> Class <linkto class=FITSGroupWriter:description>
//       FITSGroupWriter</linkto>
//       Simplified interface to create and write to FITS random groups.
//       Like FITSTableWriter except that this must be the first HDU and
//       all "columns" in the description must have the same type, i.e. float.
//  <li> Class <linkto class=FITSHistoryUtil:description>
//       FITSHistoryUtil</linkto>
//       A class with static functions to help deal with FITS History cards.
//       This is a collection of static utility functions for use with FITS
//       HISTORY keywords.
//       Manipulate HISTORY information. FITS HISTORY cards are interconverted with
//       String as follows:
//       <ul>
//         <li> 'HISTORY ' and trailing blanks are removed from each card.
//         <li> Continuation cards are CARDS that have '>' in the first line.
//         <li> A string is made by concatenating the leading card and all continuation
//              cards.
//       </ul>
//       <motivation>
//       The FitsKeywordList class can be somewhat tedious to use, as it deals with,
//       e.g., char* pointers rather than Strings. This class makes it easy to
//       interconvert between the HISTORY keywords and a Vector of related history
//       information.
//       </motivation>
//  <li> Class <linkto class=FITSKeywordUtil:description>
//       FITSKeywordUtil</linkto>
//       A class with static functions to help deal with FITS Keywords.
//       This class provides functions to conveniently interconvert between AIPS++
//       types and a FitsKeywordList which is needed by the native FITS classes.
//       It is more convenient to maintain the list within AIPS++
//       as a Record, so we only need methods to turn a FitsKeywordList into a 
//       Record, and vice versa.
//       Note that it is not necessary to construct a FITSKeywordUtil object
//       since you can use its static functions directly.
//       <motivation>
//       The FitsKeywordList class can be somewhat tedious to use, as it deals with,
//       e.g., char* pointers rather than Strings. This class makes it easy to
//       interconvert between FITS keywords and AIPS++ types.
//       </motivation>
//  <li> Class <linkto class=FITSMultiTable:description>
//       FITSMultiTable</linkto>
//       View multiple FITS files as a single table.
//       A FITSMultiTable is used to view a collection of FITS files on disk as a 
//       single Table. That is, when next() is called, when one Table ends the next
//       is reopened until all files are exhausted. The FITS files must all have the
//       same description. Something clever should be done about the keywords.
//  <li> Class <linkto class=FITSSpectralUtil:description>
//       FITSSpectralUtil</linkto>
//       A class with static functions to help deal with FITS spectral axes.
//       This class provides functions to extract information from a FITS
//       header about the spectral axis, to setup a FITS header with
//       appropriate information for the spectral axis, and to translate
//       to and from the MFrequency refrence frame codes and their FITS
//       equivalents.
//       It is never necessary to construct a FITSSpectralUtil, just use the 
//       static functions to help handle FITS Spectral axes.
//       <motivation>
//       This is designed to be used after the keywords have been extracted from
//       the FITS file using the <linkto class=FITSKeywordUtil>FITSKeywordUtil</linkto>
//       class.  Extracting spectral axis and related information requires detailed
//       knowledge of FITS conventions that this class strives to encapsulize.
//       </motivation>
//  <li> Class <linkto class=FITSTable:description>
//       FITSTable</linkto>
//       Attach a FITSTabular to a binary or ASCII table.
//       FITSTable is a FITSTabular which is attached to a FITS table (on disk only
//       presently), either Binary or ASCII.
//  <li> Class <linkto class=FITSTableWriter:description>
//       FITSTableWriter</linkto>
//       Simplified interface to create and write to a FITS Binary Table.
//  <li> Class <linkto class=FITSTabular:description>
//       FITSTabular</linkto>
//       Simplified interface to FITS tables with AIPS++ Look and Feel.
//       FITSTablular is an obstract base class which is used for read-only access to
//       tabular FITS-like data structures.
//  <li> Class <linkto class=FITSTimedTable:description>
//       FITSTimedTable</linkto>
//       FITSTimedTable is used to look at FITS tables which have a time column. In
//       particular, it peeks ahead, and knows the time of the currentRow and of the
//       nextRow.
//       It is constructed with a pointer to any FITSTabular. Presently, no memory 
//       management is imposed to ensure that the pointer remains valid.
//  <li> Class <linkto class=FitsArray:description>
//       FitsArray</linkto>
//       FITS array of given type.
//  <li> Class <linkto class=FitsArray:description>
//       <src>FitsArray<FitsBit></src></linkto>
//       FITS array of FitsBit type.
//  <li> Class <linkto class=FitsBase:description>
//       FitsBase</linkto>
//       Base class fore FitsField.
//  <li> Class <linkto class=FitsBit:description>
//       FitsBit</linkto>
//       Helper class for FITS Binary Tables.
//       This class is not intended for general use.  It only has meaning
//       in the context of FITS Binary tables.  There its use is incorporated
//       into the concept of a FitsField, where FitsBit is given a specialized
//       interpretation.
//  <li> Class <linkto class=FitsDiskInput:description>
//       FitsDiskInput</linkto>
//       FITS input from disk.
//  <li> Class <linkto class=FitsDiskOutput:description>
//       FitsDiskOutput</linkto>
//       FITS output to disk.
//  <li> Class <linkto class=FitsField:description>
//       FitsField</linkto>
//       Helper class.
//  <li> Class <linkto class=FitsField:description>
//       <src>FitsField<FitsBit></src></linkto>
//       Helper class.
//  <li> Class <linkto class=FitsFPUtil:description>
//       FitsFPUtil</linkto>
//       Utility functions for floating point values.
//  <li> Class <linkto class=FitsInput:description>
//       FitsInput</linkto>
//       Fixed-length sequential blocked FITS input.
//  <li> Class <linkto class=FitsIO:description>
//       FitsIO</linkto>
//       sequential FITS I/O.
//       FitsIO is a base class that handles all the sequential blocked
//       FITS I/O. Special derived classes do the input and output.
//       No interpretation of the data is attempted here, there are 
//       special FITS classes that handle syntax and interpretation.
//  <li> Class <linkto class=FitsKeyCardTranslator:description>
//       FitsKeyCardTranslator</linkto>
//       Translator between Keyword lists and fixed FITS cars.
//  <li> Class <linkto class=FitsKeyword:description>
//       FitsKeyword</linkto>
//       A FITS keyword contains a name, a value and a comment..
//  <li> Class <linkto class=FitsKeywordList:description>
//       FitsKeywordList</linkto>
//       Linked list of FITS keywords.
//  <li> Class <linkto class=FitsLogical:description>
//       FitsLogical</linkto>
//       FitsLogical is a helper class that is not intended for general use. 
//  <li> Class <linkto class=FitsNameResult:description>
//       FitsNameResult</linkto>
//       Analyse the name of a header card.
//  <li> Class <linkto class=FitsOutput:description>
//       FitsOutput</linkto>
//       Fixed-length sequential blocked FITS output.
//  <li> Class <linkto class=FitsParse:description>
//       FitsParse</linkto>
//       Parse a header card.
//  <li> Class <linkto class=FitsStdInput:description>
//       FitsStdInput</linkto>
//       FITS input from standard input.
//  <li> Class <linkto class=FitsStdOutput:description>
//       FitsStdOutput</linkto>
//       FITS output to standard output.
//  <li> Class <linkto class=FitsTape9Input:description>
//       FitsTape9Input</linkto>
//       FITS input from 9-track tape.
//  <li> Class <linkto class=FitsTape9Output:description>
//       FitsTape9Output</linkto>
//       FITS output to 9-track tape.
//  <li> Class <linkto class=FitsVADesc:description>
//       FitsVADesc</linkto>
//       Variable Length Array Descriptor.
//  <li> Class <linkto class=FitsValueResult:description>
//       FitsValueResult</linkto>
//       Analyse the value of a header card.
//  <li> Class <linkto class=HeaderDataUnit:description>
//       HeaderDataUnit</linkto>
//       Base class that defines a HDU.
//       The class HeaderDataUnit contains what is common to all 
//       header-data-units, including the collection of keywords.
//       From this class a number of FITS header-data-units are 
//       derived, each of them with their own rich assortment of 
//       functions for accessing and manipulating data of specific types.
//  <li> Class <linkto class=ImageExtension:description>
//       ImageExtension</linkto>
//       IMAGE extension of given type.
//  <li> Class <linkto class=NoConvert:description>
//       NoConvert</linkto>
//       FITS templated helper class.
//       NoConvert is a template class that is not intended for
//       general use, it is used internally.
//  <li> Class <linkto class=PrimaryArray:description>
//       PrimaryArray</linkto>
//       Templated primary array base class of given type.
//       A Primary Data Array is represented by the following:
//       <srcblock>
//	      <Type> data_array [NAXIS1][NAXIS2]...[NAXISN]
//       </srcblock>
//       For a PrimaryArray, dims() gives the number of dimensions
//       and dim(i) gives the value of the i-th dimension
//       WARNING!  Multi-dimensional arrays are stored in FORTRAN order, 
//       NOT in C order.  Options on the store, copy, and move functions exist 
//       to convert from one order to the other, if that is necessary.
// 
//       It is important to understand the proper sequence of operations with
//       respect to I/O and data access.  For input, the `read()' functions
//       allocate an internal buffer of the appropriate size, if not already
//       allocated, as well as reading and converting data; a `read()' function
//       must be performed prior to accessing the data, i. e. before executing
//       any `()', `data()', `copy()', or `move()' function.  For output, the
//       `store()' function similarly allocates an internal buffer before
//       transfering data, and must be executed prior to any data access or
//       `write()' function. Note: If you call any version of store(), do not
//       call set_next().
// 
//       Writing portions of an array at a time, rather than the entire array,
//       is a special case.  The `set_next()' function is provided for this
//       purpose. It declares the intention to write out the next N elements and
//       must be executed prior to any `data()' function.  It allocates a buffer
//       of appropriate size, if not already allocated.  Again, via the `data()'
//       functions, one accesses the array as if the entire array were in
//       memory.  The `write()' function always writes the number of current
//       elements in the internal buffer.  The sequence of operations for each
//       portion of the array written would be: 
//       <ul>
//            <li> `set_next(N)', 
//            <li> fill the array using `data(N)' or other `data()' functions
//            <li> `write(fout)'. 
//       </ul> 
//       The `set_next()' function must NOT be used with
//       `read()' or `store()' functions; unpredictable results will occur.  
//  <li> Class <linkto class=PrimaryGroup:description>
//       PrimaryGroup</linkto>
//       Random Group datastructure.
//       <note role=warning>
//       Please note that the NOST has deprecated the Random Group 
//       datastructure, it has been replaced by the much more powerfull
//       BINTABLE extension.
//       </note>
//  <li> Class <linkto class=ReservedFitsKeyword:description>
//       ReservedFitsKeyword</linkto>
//       Reserved FITS keyword.
//  <li> Class <linkto class=ReservedFitsKeywordCollection:description>
//       ReservedFitsKeywordCollection</linkto>
//       Collection of reserved FITS keywords.
//  <li> Class <linkto class=ScalarFITSFieldCopier:description>
//       ScalarFITSFieldCopier</linkto>
//       A FITSFieldCopier for copying scalar non-string RecordFields to FitsFields.
//  <li> Class <linkto class=SDFITSTable:description>
//       SDFITSTable</linkto>
//       SDFITSTable is derived from FITSTable.  It contains additional
//       checks and behaviour appropriate to the Single Dish FITS Convention
//       hence this is a Single Dish FITS Table, or SDFITSTable.
//       This class behaves much like FITSTable.  It additionally verifies
//       that the indicated HDU in the input FITS file follows the SDFITS
//       convention (it has all of the required columns) and it treats
//       keywords as virtual columns when appropriate.  These virtual
//       columns will appear as fields in the currentRecord and description
//       and will NOT appear in the keywords.
//       <motivation>
//       It was useful to encapsulate this behaviour in a class so that
//       the checks on a valid SDFITS table and the treatment of keywords
//       as virtual columns would not need to appear everywhere it might
//       be used.
//      </motivation>
//  <li> Class <linkto class=StringFITSFieldCopier:description>
//       StringFITSFieldCopier</linkto>
//       A FITSFieldCopier for copying String RecordFields to FitsFields.
//  <li> Class <linkto class=VariableArrayFITSFieldCopier:description>
//       VariableArrayFITSFieldCopier</linkto>
//       Copy the current contents of the input RORecordFieldPtr to the output FitsField.
// </ul>
//
// <note role=tip> You may want to look at the individual header files
// to see whether you might not prefer to include only the header
// files you really need; it may be more efficient to do so.
// </note>
//
// </synopsis>
//
//# <todo asof="2005/06/20">
//#   <li>
//# </todo>
//
// </module>


} //# NAMESPACE CASA - END

#endif

