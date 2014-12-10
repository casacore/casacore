//# TableExprData.h: Abstract base class for data object in a TaQL expression
//# Copyright (C) 2000,2001
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


#ifndef TABLES_TABLEEXPRDATA_H
#define TABLES_TABLEEXPRDATA_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class IPosition;
template<class T> class Array;
template<class T> class Block;


// <summary>
// Abstract base class for data object in a TaQL expression.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="TableExprNode">TableExprNode</linkto>.
// </prerequisite>

// <synopsis>
// The Table Query Language (TaQL) is implemented by means of the
// <src>TableExprNode</src> classes. It is primarily meant to do
// selection on tables. However, it is also possible to use it for
// selection on any other set of data resembling tabular data.
// <br>An example of such a data set is a set of
// <linkto class=Record>Record</linkto> objects. TaQL can be used
// to select some of those records based on the contents of one or more
// fields in the records. Note that this example is already directly
// supported by TaQL.
// <br>Another example is when a user has several equally long vectors
// with data. The vectors can be seen as fields and TaQL can be used
// to select entries from the vectors. This example requires that
// this class TableExprData is used.
// <p>
// The <linkto class=TableExprNodeRecordField>TableExprNodeRecordField</linkto>
// and <linkto class=TableExprId>TableExprId</linkto> classes form
// the means by which TaQL can deal with any set of data.
// <br>First the TaQL expression has to be setup. This is done by
// constructing a <src>TableExprNodeRecordField</src> object for each
// 'field' to be used in the expression. <src>TableExprNodeRecordField</src>
// uses a <linkto class=RecordInterface>RecordInterface</linkto> object
// to make the data type of a field in the data set known and to
// map a field name to a field index (the index is the sequence number
// of the field in the record description).
// <br>When evaluating the expression for each member in the data set,
// a <src>TableExprData></src> needs to be passed (which is automatically
// converted to <linkto class=TableExprId>TableExprId</linkto>).
// So a class needs to be written to access the data in the data set.
// It needs to be derived from the abstract base class <src>TableExprData</src>
// defined in this file. An example is given below.
// <p>
// It is also possible that the data set contains records and that
// the selection is based on fields in those records. In such a case
// the record passed to <src>TableExprNodeRecordField</src> should contain
// subrecords representing those records. The field index in the various
// functions as passed as a <src>Block<Int></src> to denote the fields
// in the subrecords (and possibly subsubrecords, etc..
// However, normally records won't be used and <src>fieldNrs[0]</src>
// gives the field index.
// </synopsis>

// <example>
// This example shows how a data set consisting of two vectors
// of scalars can be used.
// <srcblock>
// // Write a class derived from TableExprData to handle the vectors.
// class MyTestClass : public TableExprData
// {
// public:
//   // Constructor checks if both vectors have equal length.
//   MyTestClass (const Vector<Int>& fld1, const Vector<String>& fld2)
//     : itsFld1(fld1), itsFld2(fld2), itsEntry(0)
//     { AlwaysAssert (fld1.nelements() == fld2.nelements(), AipsError); }
//   virtual ~MyTestClass()
//     {}
//   void next()
//     { itsEntry++; }
//   // Note that only the get functions for the possible types are needed.
//   // Also note that all numeric types are handled by TaQL as Double.
//   // The exception should never be thrown unless things are screwed up.
//   virtual Double getDouble (const Block<Int>& fieldNrs) const
//     { switch (fieldNrs[0]) {
//       case 0:
//         return itsFld1(itsEntry);
//       default:
//         throw AipsError();
//       }
//     }
//   virtual String getString (const Block<Int>& fieldNrs) const
//     { switch (fieldNrs[0]) {
//       case 1:
//         return itsFld2(itsEntry);
//       default:
//         throw AipsError();
//       }
//     }
//   virtual DataType dataType (const Block<Int>& fieldNrs) const
//     { switch (fieldNrs[0]) {
//       case 0:
//         return TpInt;
//       case 1:
//         return TpString;
//       default:
//         throw AipsError();
//       }
//     }
//   // Make a Record to give to vectors a name.
//   // The order in which the fields are defined determines the fieldnrs
//   // passed to the get functions.
//   static Record makeRecord()
//     { RecordDesc desc;
//       desc.addField ("fld1", TpInt);
//       desc.addField ("fld2", TpString);
//       return Record(desc);
//     }
// private:
//   Vector<Int>    itsFld1;
//   Vector<String> itsFld2;
//   uInt           itsEntry;
// };
//   
// Vector<uInt> findMatches (const Vector<Int>& fld1,
//                           const Vector<String>& fld2)
// {
//   // Make some expression.
//   // First create a Record to make the names and types known.
//   Record rec(MyTestClass::makeRecord());
//   TableExprNode expr (makeRecordExpr(rec,"fld1") > 10 &&
//                       makeRecordExpr(rec,"fld2") != pattern("*xxx*"));
//   // Now evaluate the expression for each entry in the vector.
//   // Make a MyTestClass object to handle the vectors and put it in
//   // a TableExprId object for the TaQL evaluator.
//   // Note that TableExprId holds a pointer to the original MyTestClass
//   // object, so the TaQL evaluator 'sees' the changes we make by
//   // using the its next() function.
//   MyTestClass subj(fld1, fld2);
//   TableExprId eid(subj);
//   // The matching entry numbers are stored in a vector.
//   Vector<uInt> result(fld1.nelements());
//   uInt nr=0;
//   Bool valb;
//   for (uInt i=0; i<fld1.nelements(); i++) {
//     expr.get (eid, valb);
//     if (valb) {
//       result(nr++) = i;
//     }
//     subj.next();         // Next time the next entry must be used
//   }
//   result.resize (nr, True);
//   return result;
// }
// </srcBlock>
// </example>

// <motivation>
// This class makes it possible that TaQL can be used in a very versatile way.
// </motivation>

//# <todo asof="1996/03/12">
//# </todo>


class TableExprData
{
public:
  // Construct it from a row number.
  TableExprData()
    {;}
  
  virtual ~TableExprData();

  // Get the shape of the given field.
  // Need only be implemented if there are arrays in the data.
  // The default implementation returns an empty IPosition.
  virtual IPosition shape (const Block<Int>& fieldNrs) const;

  // Get the data type of the given field.
  // Note that TpArray types have to be returned for arrays.
  // If the field is unknown, TpOther should be returned.
  // It is used for the isdefined function to check if the field
  // is really defined.
  virtual DataType dataType (const Block<Int>& fieldNrs) const = 0;

  // Get a scalar in the given type.
  // This might involve converting for Double and DComplex.
  // Most default implementations throws an "not possible" exception.
  // The default <src>getDouble</src> invokes <src>getInt</src>.
  // The default <src>getDComplex</src> invokes <src>getDouble</src>.
  // <group>
  virtual Bool     getBool     (const Block<Int>& fieldNrs) const;
  virtual Int64    getInt      (const Block<Int>& fieldNrs) const;
  virtual Double   getDouble   (const Block<Int>& fieldNrs) const;
  virtual DComplex getDComplex (const Block<Int>& fieldNrs) const;
  virtual String   getString   (const Block<Int>& fieldNrs) const;
  // </group>

  // Get an array in the given type.
  // This might involve converting for Double and DComplex.
  // Most default implementations throws an "not possible" exception.
  // The default <src>getArrayDComplex</src> invokes
  // <src>getArrayDouble</src>.
  // <group>
  virtual Array<Bool>     getArrayBool     (const Block<Int>& fieldNrs) const;
  virtual Array<Int64>    getArrayInt      (const Block<Int>& fieldNrs) const;
  virtual Array<Double>   getArrayDouble   (const Block<Int>& fieldNrs) const;
  virtual Array<DComplex> getArrayDComplex (const Block<Int>& fieldNrs) const;
  virtual Array<String>   getArrayString   (const Block<Int>& fieldNrs) const;
  // </group>
};


} //# NAMESPACE CASACORE - END

#endif
