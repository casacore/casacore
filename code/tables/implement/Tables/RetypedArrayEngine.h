//# RetypedArrayEngine.h: Virtual column engine to retype and reshape arrays
//# Copyright (C) 1995,1996,1999
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

#if !defined(AIPS_RETYPEDARRAYENGINE_H)
#define AIPS_RETYPEDARRAYENGINE_H

//# Includes
#include <aips/Tables/BaseMappedArrayEngine.h>
#include <aips/Tables/TableRecord.h>

//# Forward Declarations


// <summary>
// Virtual column engine to retype and reshape arrays.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Brian Glendenning" date="1995/12/20" tests="dRetypedArrayEngine.cc" demos=dRetypedArrayEngine.h>
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=BaseMappedArrayEngine>BaseMappedArrayEngine</linkto>
// </prerequisite>

// <synopsis> 
// RetypedArrayEngine maps a source column containing arrays of objects
// to a target column containing arrays of data of another type. Usually
// the dimensionality of the arrays get smaller during this mapping process.
// The engine makes it possible to store an array of any type in a table.
// <br>
// For example, a column with 2D arrays of StokesVector's can be mapped to
// a column with 3D arrays of floats (of which the first axes has, say,
// length 4). Another example is mapping a 2D array of StokesMatrix's
// to a 4D array of floats.
// <p>
// The mapping process has to be done by a (static) set and get
// function in the SourceType class. When a RetypedArrayEngine object is
// constructed, it is possible to pass information in a TableRecord. This
// TableRecord is indirectly passed to the set/get functions. This is done by
// means of the function newCopyInfo, which can preprocess the information
// in the TableRecord and store it in another object. That object is passed to
// the set and get functions. At the end a function deleteCopyInfo is called
// to delete the object. Of course, it is not needed to allocate such
// an object; newCopyInfo can return a null pointer.
// <note role=tip> Because the variables have to be generic and because of
// limitations in the CFront compiler, several variables have to be
// passed as void* and need to be casted in the set/get functions.
// </note>
//
// The source column data type class has to contain several functions.
// The example shows how they can be implemented.
// <dl>
//  <dt> <src>static String dataTypeId();</src>
//  <dd> has to give the (unique) name of the class.
//  <dt> <src>static IPosition shape();</src>
//  <dd> This has to return the full shape of the elements in the source.
//       E.g. StokesVector will return [4]. StokesMatrix will return [4,4].
//  <dt> <src>static void* newCopyInfo (const TableRecord& record,
//                                  const IPosition& sourceElementShape);</src>
//  <dd> This function has to setup the set/get functions by preprocessing the
//       information contained in the TableRecord and storing it in a so-called
//       "copyInfo" object. A pointer to that object has to be returned, which
//       is kept by the engine and passed to the set/get functions.
//       The "copyInfo" class can be a nested class in the SourceType
//       (as shown in the StokesVector example), but it can also
//       be an independent class.
//       <br>
//       The supplied TableRecord is the TableRecord given when
//       constructing the engine.
//       When no TableRecord was given, it will be empty.
//       The supplied shape is the shape of a source element as given to
//       the constructor of the engine. This can be a full or partial shape.
//       E.g. for a StokesVector it will usually be [4], but it can also,
//       say, [1] if only U is used.
//       The function could check if the information in the TableRecord
//       and the shape match.
//       <br>
//       Of course, a SourceType may not need any extra information.
//       Therefore it is possible to return a null "copyInfo" pointer.
//  <dt> <src>static void deleteCopyInfo (void* copyInfo);</src>
//  <dd> This function has to delete the "copyInfo" object allocated
//       by newCopyInfo. To do so, it needs to cast the pointer to the
//       correct type.
//  <dt> <src>static void set (void* copyInfo, void* out,
//                             const Array<TargetType>& in,
//                             const IPosition& sourceElementShape);</src>
//  <dd> This function is called when an <src>Array<SourceType></src> is read.
//       It has to convert the TargetType array to the SourceType array.
//       In principle, there are two different cases (which can be deduced
//       from the given shape):
//       <ol>
//       <li> The target information is complete. For example: suppose the
//            SourceType is a StokesVector object (containing I, Q, U and V),
//            When the target array contains 4 values per StokesVector,
//            it is complete.
//            <br>
//            In this case the entire source array can be directly copied from
//            the target array when the SourceType object contains no
//            virtual functions and the data are directly contained in it.
//            The function
//            <br>
//            <linkto group=RetypedArraySetGet.h#RetypedArrayEngineSetGet>
//            <src>
//            retypedArrayEngineSet (Array<SourceType>& out,
//                                   const Array<TargetType>& in);
//            </src></linkto><br>
//            can be used for this purpose.
//       <li> When in the example above the target array contains less
//            than 4 values per StokesVector, the target information
//            is incomplete. In this case the set function has to
//            fill the data in one way or another. The information
//            in the "copyInfo" object can assist in it.
//            <br>
//            Each SourceType element has to be set individually, so
//            a loop through the array is required. To assist in this,
//            the loop has been implemented in the function
//            <br>
//            <linkto group=RetypedArraySetGet.h#RetypedArrayEngineSetGet>
//            <src>
//            retypedArrayEngineSet (Array<SourceType>& out,
//                                   const Array<TargetType>& in,
//                                   const void* extraArgument);
//            </src></linkto>
//            <br> It calls the SourceType function
//            <srcblock>
//             void setElem (const TargetType* data, const IPosition& shape,
//                           const void* extraArgument);
//            </srcblock>
//            for each SourceType element. This set function has to
//            fill the SourceType object from the data. It can use the
//            shape and the extraArgument to know how it should do it.
//            <br>
//            Note that the 3-argument function retypedArrayEngineSet is
//            only a convenience function. For optimal performance it may
//            be needed to handcode the loop instead of using this function.
//       </ol>
//       <note role=warning> Note that the given source element shape does
//       not need to match the shape given to the constructor of the engine.
//       It is possible that the user sets the shape of the target array
//       before putting the source array. In that case the system uses the
//       relevant part of the target array shape as the source element shape.
//       </note>
//       <note role=tip> When the out argument is declared (as it should be) as
//       <src>Array<SourceType>& out</src>,
//       the CFront compiler complains about unknown size of
//       SourceType when instantiating Array<SourceType>.
//       Therefore it has to be declared as void* and the set function
//       needs to cast it to <src>Array<SourceType>*</src>.
//       </note>
//  <dt> <src>static void get (void* copyInfo, Array<float>& out,
//                             const void* in,
//                             const IPosition& sourceElementShape);</src>
//  <dd> This function is similar to the set function described above, but
//       is called when an <src>Array<SourceType></src> is written.
//       It has to convert the SourceType array to the TargetType array.
// </dl>
//
// <br>E.g.: A StokesVector has 4 float elements.
// <srcblock>
//    // Construct the column object for the Stokes column.
//    ArrayColumn<StokesVector> stokesColumn (table, "StokesVirtualColumn");
//    // Put an array of StokesVector's with shape 512,512.
//    // This will implicitly set the shape of the underlying
//    // data column to 4,512,512.
//    // This put is very quick (it can copy all data in one go).
//    Array<StokesVector> stokesData (IPosition(2,512,512));
//    stokesColumn.put (rownr, stokesData);
//
//    // Get the column object for the Data column.
//    // Set its shape explicitly to 1,512,512,
//    ArrayColumn<float> dataColumn (table, "DataColumn");
//    dataColumn.setShape (rownr, IPosition(3,1,512,512));
//    // Now a put of the data results in calling the StokesVector::getElem
//    // function for each element with an IPosition(1,1); i.e. the
//    // data array needs only one value for each StokesVector.
//    stokesColumn.put (rownr, stokesData);
// </srcblock>
//
// When reading a table back, the engine has to be registered.
// Otherwise it will be unknown to the table system.
// Similarly, the appropriate ArrayColumnDesc object has to be registered.
// This can be done as follows:
// <pre>
//    RetypedArrayEngine<StokesVector,float>::registerClass();
//    ArrayColumnDesc<StokesVector> tmp(ColumnDesc::registerMap);
// </pre>
// When they are not registered, the open of the table will fail
// telling which class could not be found.
// </synopsis> 

// <motivation>
// This class allows one to store arrays of arbitrary objects in a table.
// It also allows it to be done it in a very efficient way.
// <p>
// The class had to be doubly templated. There were 2 reasons:
// <ol>
//  <li> The typedef trick described on page 321 in Barton/Nackman
//       did not work with the CFront-based ObjectCenter compiler.
//  <li> It was needed to allow derivation from BaseMappedArrayEngine.
// </ol>
// <p>
// Originally it was the idea to have a mandatory nested CopyInfo class in the
// SourceType class and use syntax like SourceType::CopyInfo to access
// functions in it and to keep a pointer to such an object. Alas, the
// CFront compiler could not handle this.
// <p>
// Because the engine can serve only one column, it was possible to
// combine the engine and the column functionality in one class.
// This has been achieved using multiple inheritance.
// The advantage of this is that only one templated class is used,
// so fewer template instantiations are needed.
// </motivation>

// <example>
// The following example shows how a StokesVector could be implemented.
// It doesn't check whether the mask is correct.
// Two more examples are contained in the demo/test program
// <a href="../../../../code/aips/implement/Tables/test/dRetypedArrayEngine.h">
// dRetypedArrayEngine.h</a> and its
// <a href="../../../../code/aips/implement/Tables/test/dRetypedArrayEngine.cc">
// .cc file</a>. Their second example (class RetypedArrayEx2) is similar to
// the StokesVector example below, but contains more extensive checking.
// <srcblock>
// //# Forward Declarations
// template<class T> class Array;
// template<class T> class Vector;
//
// class StokesVector
// {
// public:
//     StokesVector(): I_p(0), Q_p(0), U_p(0), V_p(0) {}
//     StokesVector(double i, double q, double u, double v)
//          : I_p(i), Q_p(q), U_p(u), V_p(v) {}
//     StokesVector(const StokesVector& that): I_p(that.I_p), Q_p(that.Q_p),
//                                             U_p(that.U_p), V_p(that.V_p) {}
//     StokesVector& operator= (const StokesVector& that)
//        { I_p=that.I_p; Q_p=that.Q_p; U_p=that.U_p; V_p=that.V_p;
//          return *this; }
//
//     static String dataTypeId()
//         { return "StokesVector"; }
//
//     // A StokesVector is 1-dim and contains 4 elements.
//     static IPosition shape()
//         { return IPosition (1,4); }
//
//     // Preprocess possible information in the TableRecord.
//     static void* newCopyInfo (const TableRecord& record,
//                               const IPosition& shape)
//         { return new CopyInfo(record, shape); }
//
//     // Delete the object containing preprocessed information.
//     static void* deleteSetDet (void* copyInfo)
//         { delete (CopyInfo*)copyInfo; }
//
//     // Convert a TargetType array to a SourceType array.
//     // Do this in a CopyInfo function to use its preprocessed information.
//     static void set (void* copyInfo, void* out,
//                      const Array<double>& in, const IPosition& shape)
//         { ((CopyInfo*)copyInfo)->set (out, in, shape); }
//
//     // Convert a SourceType array to a TargetType array.
//     // Do this in a CopyInfo function to use its preprocessed information.
//     static void get (void* copyInfo, Array<double>& out,
//                      const void* in, const IPosition& shape)
//         { ((CopyInfo*)copyInfo)->get (out, in, shape); }
//
//     // This nested class is used to hold preprocessed information. It
//     // holds a mask extracted from the TableRecord supplied to the engine.
//     // One can imagine that it could also extract a flag telling
//     // whether the target data is stored as I,Q,U,V or as XX,YY,XY,YX
//     // (although such a conversion would probably be better handled
//     // by a separate virtual column engine).
//     class CopyInfo {
//     public:
//         // The constructor extracts the mask from the record.
//         void CopyInfo (const TableRecord& record)
//             {
//                 RORecordFieldRef<Array<Bool> > field (record, 0);
//                 mask_p = new Vector<Bool>;
//                 *mask_p = *field;
//             }
//         // The set function fills the StokesVector.
//         // It uses the general functions for that purpose.
//         void set (void* vout, const Array<double>& in,
//                   const IPosition& shape)
//             {
//                 Array<StokesVector>& out = *(Array<StokesVector>*)vout;
//                 if (shape.nelements() == 1  &&  shape(0) == 4) {
//                     // All values available, copy in one go.
//                     // This can be done because a StokesVector object
//                     // only contains 4 double values (and no virtual
//                     // function table).
//                     retypedArrayEngineSet (out, in);
//                 }else{
//                     // Only some values available. Fill each
//                     // StokesVector object using the shape and mask.
//                     // The set function below is called for each object.
//                     retypedArrayEngineSet (out, in, shape, (void*)mask_p);
//                 }
//             }
//         // get is the opposite of set.
//         void get (Array<double>& out, const void* vin,
//                   const IPosition& shape)
//             {
//                 const Array<StokesVector>& in =
//                                          *(const Array<StokesVector>*)vin;
//                 if (shape.nelements() == 1  &&  shape(0) == 4) {
//                     retypedArrayEngineGet (out, in);
//                 }else{
//                     retypedArrayEngineGet (out, in, shape, (void*)mask_p);
//                 }
//     private:
//         Vector<Bool>* mask_p;
//     };
//
//     // Set values of StokesVector using the mask.
//     // The shape is not used here.
//     void setElem (const double* data, const IPosition&, const void* maskPtr)
//         {
//              const Vector<Bool>& mask = *(const Vector<Bool>*)maskPtr;
//              I_p = Q_p = U_p = V_p = 0;
//              if (mask(0)) {
//                  I_p = *data++;
//              }
//              if (mask(1)) {
//                  Q_p = *data++;
//              }
//              if (mask(2)) {
//                  U_p = *data++;
//              }
//              if (mask(3)) {
//                  V_p = *data;
//              }
//         }
//     // Get values of StokesVector using the mask (opposite of setElem).
//     void getElem (double* data, const IPosition&, const void* maskPtr);
// private:
//    double I_p, Q_p, U_p, V_p;
// };
//
// main() {
//    // First register the virtual column engine.
//    RetypedArrayEngine<StokesVector,double>::registerClass();
//    // Add ArrayColumnDesc<StokesVector> to column type map.
//    ArrayColumnDesc<StokesVector> tmp(ColumnDesc::registerMap);
//
//    // Build the table description.
//    TableDesc td("", "1", TableDesc::Scratch);
//    td.addColumn (ArrayColumnDesc<double> ("Data"));
//    td.addColumn (ArrayColumnDesc<StokesVector> ("Stokes"));
//
//    // Now create a new table from the description.
//    SetupNewTable newtab("tRetypedArrayEngine_tmp.data", td, Table::New);
//    // Create the virtual column engine with the target columns Data.
//    RetypedArrayEngine<StokesVector,double> engine ("Stokes", "Data");
//    newtab.bindColumn ("Stokes", engine);
//    Table tab(newtab, 50);
//
//    // Fill the table via the virtual columns.
//    ArrayColumn<StokesVector> stokesColumn (tab, "Stokes");
//    Vector<StokesVector> vec(10);
//    uInt i;
//    for (i=0; i<tab.nrow(); i++) {
//        stokesColumn.put (i, vec);
//    }
// }
// </srcblock>
// <note role=caution>
// Due to instantiation problems with the CFront-based ObjectCenter compiler
// (and probably other CFront-based compilers as well) the Array and
// Vector have to be forward declared. Array.h and Vector.h should
// NOT be included in this StokesVector.h, thus the implementations
// should not be inlined (they are too large anyway), but put in a
// separate .cc file where Array.h and Vector.h can be included.
// <p>
// Another compiler problem is that the variable mask_p is not
// automatically converted to a void*, so an explicit cast has to be done.
// </example>

// <templating arg=SourceType>
//  <li> default constructor
//  <li> copy constructor
//  <li> assignment operator
//  <li> <src>static String dataTypeId();</src>
//  <li> <src>static IPosition shape();</src>
//  <li> <src>static void* newCopyInfo (const TableRecord& record, const IPosition& sourceElementShape);</src>
//  <li> <src>static void deleteCopyInfo (void* copyInfo);</src>
//  <li> <src>static void set (void* copyInfo, void* out,
//                             const Array<TargetType>& in,
//                             const IPosition& shape);</src>
//  <li> <src>static void get (void* copyInfo, Array<float>& out,
//                             const void* in, const IPosition& shape);</src>
//  <li> <src>void setElem (const TargetType* data, const IPosition& shape,
//                          const void* extraArgument);</src>
//       <br>when global function retypedArrayEngineSet is used.
//  <li> <src>void getElem (TargetType* data, const IPosition& shape,
//                          const void* extraArgument) const;</src>
//       <br>when global function retypedArrayEngineGet is used.
// </templating>
// <templating arg=TargetType>
//  <li> Default constructor
//  <li> Copy constructor
//  <li> Assignment operator
// </templating>

//# <todo asof="1995/12/29">
//# </todo>


template<class SourceType, class TargetType> class RetypedArrayEngine : public BaseMappedArrayEngine<SourceType,TargetType>
{
public:

    // Construct an engine to map a source column containing arrays with
    // an arbitrary data type to arrays in a target column.
    // TargetColumnName is the name of the column where the converted
    // data will be put and must have data type TargetType.
    // The source column using this engine must have data type SourceType.
    RetypedArrayEngine (const String& sourceColumnName,
			const String& targetColumnName);

    // Construct an engine to map a source column containing arrays with
    // an arbitrary data type to arrays in a target column.
    // TargetColumnName is the name of the column where the converted
    // data will be put and must have data type TargetType.
    // The source column using this engine must have data type SourceType.
    // The shape and record provided is handed to the newCopyInfo function
    // in the SourceType class. It can be used to determine how an element
    // has to be handled when the target data is incomplete.
    RetypedArrayEngine (const String& sourceColumnName,
			const String& targetColumnName,
			const IPosition& sourceElementShape,
			const TableRecord& extraInformation);

    // Destructor is mandatory.
    ~RetypedArrayEngine();

    // Return the type name of the engine (i.e. its class name).
    String dataManagerType() const;

    // Return the name of the class.
    // This includes the names of the template arguments.
    static String className();

    // Register the class name and the static makeObject "constructor".
    // This will make the engine known to the table system.
    // The automatically invoked registration function in DataManReg.cc
    // contains RetypedArrayEngine<double,Int>.
    // Any other instantiation of this class must be registered "manually"
    // (or added to DataManReg.cc).
    static void registerClass();

private:
    // The default constructor is required for reconstruction of the
    // engine when a table is read back.
    RetypedArrayEngine();

    // Copy constructor is only used by clone().
    // (so it is made private).
    RetypedArrayEngine (const RetypedArrayEngine<SourceType,TargetType>&);

    // Assignment is not needed and therefore forbidden
    // (so it is made private and not implemented).
    RetypedArrayEngine<SourceType,TargetType>& operator=
                        (const RetypedArrayEngine<SourceType,TargetType>&);

    // Clone the engine object.
    DataManager* clone() const;

    // Initialize the object for a new table.
    // It defines the keywords containing the engine parameters.
    void create (uInt initialNrrow);

    // Preparing consists of setting the writable switch and
    // adding the initial number of rows in case of create.
    // Furthermore it reads the keywords containing the engine parameters
    // and allocates a CopyInfo object for the SourceType.
    void prepare();

    // Set the shape of the FixedShape arrays in the column.
    // This function only gets called if the column has FixedShape arrays.
    // The shape gets saved and used to set the shape of the arrays
    // in the target in case the target has non-FixedShape arrays.
    void setShapeColumn (const IPosition& shape);

    // Define the shape of the array in the given row.
    // When the shape of the (underlying) target array has already been
    // defined, it checks whether its latter dimensions match the given
    // source shape. When matching, nothing will be done.
    // When mismatching or when the target shape has not been defined
    // yet, the target shape will be defined from the source shape and
    // the source element shape.
    // E.g. in case of a StokesVector a source shape of (512,512)
    // results in a target shape of (4,512,512).
    void setShape (uInt rownr, const IPosition& shape);

    // Get the dimensionality of the array in the given row.
    uInt ndim (uInt rownr);

    // Get the shape of the array in the given row.
    // This is done by stripping the first dimension(s) from the shape
    // of the underlying target array.
    IPosition shape (uInt rownr);

    // Get an array in the given row.
    void getArray (uInt rownr, Array<SourceType>& array);

    // Put an array in the given row.
    void putArray (uInt rownr, const Array<SourceType>& array);

    // Get a section of the array in the given row.
    void getSlice (uInt rownr, const Slicer& slicer, Array<SourceType>& array);

    // Put into a section of the array in the given row.
    void putSlice (uInt rownr, const Slicer& slicer,
		   const Array<SourceType>& array);

    // Get an entire column.
    void getArrayColumn (Array<SourceType>& array);

    // Put an entire column.
    void putArrayColumn (const Array<SourceType>& array);

    // Get a section of all arrays in the column.
    void getColumnSlice (const Slicer& slicer, Array<SourceType>& array);

    // Put a section of all arrays in the column.
    void putColumnSlice (const Slicer& slicer, const Array<SourceType>& array);

    // Check if the shapes of source and target match.
    // Determine the shape of the source elements in the target.
    IPosition checkShape (const Array<SourceType>& source,
			  const Array<TargetType>& target);

    // Copy the target array to the source array.
    // It tries to optimize as much as possible.
    void copyOnGet (Array<SourceType>& array,
		    const Array<TargetType>& target);

    // Copy the source array to the target array.
    // It tries to optimize as much as possible.
    void copyOnPut (const Array<SourceType>& array,
		    Array<TargetType>& target);

    // Determine the shape of a cell in the target column from the
    // shape of the cell in the source column.
    IPosition targetShape (uInt rownr, const IPosition& sourceShape);

    // Convert the Slicer for a source to a Slicer for the target.
    Slicer targetSlicer (const Slicer& sourceSlicer) const;

    //# Now define the data members.
    IPosition shape_p;            //# shape of a source element in the target
    IPosition sourceFixedShape_p; //# The shape in case source has FixedShape
    Bool      isSourceFixedShape_p;
    TableRecord  record_p;
//#    SourceType::CopyInfo* copyInfo_p; //# object used to set/get arrays
    void* copyInfo_p;             //# CFront compiler does not accept above


public:
    //*display 4
    // Define the "constructor" to construct this engine when a
    // table is read back.
    // This "constructor" has to be registered by the user of the engine.
    // If the engine is commonly used, its registration can be added
    // to the registerAllCtor function in DataManReg.cc. 
    // That function gets automatically invoked by the table system.
    static DataManager* makeObject (const String& dataManagerType);
};


#endif
