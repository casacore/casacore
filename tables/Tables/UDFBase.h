//# UDFBase.h: Abstract base class for a user-defined TaQL function
//# Copyright (C) 2010
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

#ifndef TABLES_UDFBASE_H
#define TABLES_UDFBASE_H

//# Includes
#include <tables/Tables/ExprNodeRep.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TaQLStyle.h>
#include <casa/Containers/Block.h>
#include <casa/OS/Mutex.h>
#include <casa/stdmap.h>


namespace casa {

  // <summary>
  // Abstract base class for a user-defined TaQL function
  // </summary>
  //
  // <synopsis>
  // This class makes it possible to add user-defined functions (UDF) to TaQL.
  // A UDF has to be implemented in a class derived from this class. A few
  // functions have to be implemented in the class as described below.
  //
  // A UDF is a class derived from this base class. It must contain the
  // following member functions. See also the example below.
  // <table border=0>
  // <tr>
  //  <td><src>makeObject</src></td>
  //  <td>a static function to create an object of the UDF class. This function
  //    needs to be registered.
  //  </td>
  // </tr>
  // <tr>
  //  <td><src>setup</src></td>
  //  <td>this virtual function is called after the object has been created.
  //   It should initialize the object using the function arguments that
  //   can be obtained using the function <src>operands()</src>. The setup
  //   function should perform the following:
  //   <ul>
  //    <li>Define the data type of the result using <src>setDataType<src>.
  //        The data type should be derived from the data types of the function
  //        arguments. The possible data types are defined in class
  //        TableExprNodeRep.
  //        Note that a UDF can support multiple data types. For example, a
  //        function like <src>min</src> can be used for Int, Double, or a mix.
  //        Function 'checkDT' in class TableExprNodeMulti can be used to
  //        check the data types of the operands and determine the result
  //        data type.
  //    <li>Define the dimensionality of the result using <src>setNDim</src>.
  //        A value of 0 means a scalar. A value of -1 means an array with
  //        a dimensionality that can vary from row to row.
  //    <li>Optionally use <src>setShape</src> to define the shape if the
  //        results are arrays with a shape that is the same for all rows.
  //        It will also set ndim if setNDim was not used yet, otherwise
  //        it checks if it ndim matches.
  //    <li>Optionally set the unit of the result using <src>setUnit</src>.
  //        TaQL has full support of units, so UDFs should behave the same.
  //        It is possible to change the unit of the function arguments.
  //        For example:
  //        <ul>
  //         <li>a function like 'sin' can force its argument to be
  //          in radians; TaQL will scale the argument as needed. This can be
  //          done like
  //          <src>TableExprNodeUnit::adaptUnit (operands()[i], "rad");</src>
  //         <li>A function like 'asin' will have a result in radians.
  //          Such a UDF should set its result unit to rad.
  //         <li>A function like 'min' wants its arguments to have the same
  //          unit and will set its result unit to it. It can be done like:
  //          <src>setUnit (TableExprFuncNode::makeEqualUnits
  //                        (operands(), 0, operands().size()));</src>
  //      <li>Optionally define if the result is a constant value using
  //          <src>setConstant</src>. It means that the function is not
  //          dependent on the row number in the table being queried.
  //          This is usually the case if all UDF arguments are constant.
  //        </ul>
  //        See class TableExprFuncNode for more info about these functions.
  //   </ul>
  //  </td>
  // </tr>
  // <tr>
  //  <td><src>replaceTable</src></td>
  //  <td>The possible Table to use is set by the setup function. However,
  //      the table to use might change which is done by thos function.
  //      It needs to be implemented, even if no Table object is kept.
  //  </td>
  // </tr>
  // <tr>
  //  <td><src>getXXX</src></td>
  //  <td>there are virtual get functions for each possible data type. The
  //      get functions matching the data types that can be set by the setup
  //      function need to be implemented.
  //  </td>
  // </tr>
  // </table>
  //
  // A UDF has to be made known to TaQL by adding it to the UDF registry with
  // its name and 'makeObject' function.
  // UDFs will usually reside in a shared library that is loaded dynamically.
  // TaQL will load a UDF in the following way:
  // <ul>
  //  <li> The UDF name used in TaQL consists of two parts: a library name
  //       and function name separated by a dot. Both parts need to be given.
  //       Note that the library name can also be seen as a UDF scope, so
  //       different UDFs with equal names can be used from different libraries.
  //       A UDF should be registered with this full name.
  //  <li> If a UDF is not found in the registry, it will be tried to load
  //       a shared library using the library name part. The libraries tried
  //       to be loaded are lib<library>.so and libcasa_<library>.so.
  //       On Mac .dylib will be tried. If loaded successfully, a special
  //       function 'register_libname' will be called first. It should
  //       register each UDF in the shared library using UDFBase::register.
  // </ul>
  // </synopsis>
  //
  // <example>
  // <srcblock>
  // class TestUDF: public UDFBase
  // {
  // public:
  //   TestUDF() {}
  //   static UDFBase* makeObject (const String&)
  //     { return new TestUDF(); }
  //   virtual void setup (const Table&, const TaQLStyle&)
  //   {
  //     AlwaysAssert (operands().size() == 1, AipsError);
  //     AlwaysAssert (operands()[0]->dataType() == TableExprNodeRep::NTInt,
  //                   AipsError);
  //     AlwaysAssert (operands()[0]->valueType() == TableExprNodeRep::VTScalar,
  //                   AipsError);
  //     setDataType (TableExprNodeRep::NTBool);
  //     setNDim (0);                                 // scalar result
  //     setConstant (operands()[0].isConstant());    // constant result?
  //        
  //   }
  //   Bool getBool (const TableExprId& id)
  //     { return operands()[0]->getInt(id) == 1; }
  // };
  // </srcblock>
  // This example returns True if the function argument matches 1.
  // It can be seen that it checks if the argument is an integer scalar.
  // </example>

  class UDFBase
  {
  public:
    // The signature of a global or static member function creating an object
    // of the UDF.
    typedef UDFBase* MakeUDFObject (const String& functionName);

    // Only default constructor is needed.
    UDFBase();

    // Destructor.
    virtual ~UDFBase();

    // Evaluate the function and return the result.
    // Their default implementations throw a "not implemented" exception.
    // <group>
    virtual Bool      getBool     (const TableExprId& id);
    virtual Int64     getInt      (const TableExprId& id);
    virtual Double    getDouble   (const TableExprId& id);
    virtual DComplex  getDComplex (const TableExprId& id);
    virtual String    getString   (const TableExprId& id);
    virtual TaqlRegex getRegex    (const TableExprId& id);
    virtual MVTime    getDate     (const TableExprId& id);
    virtual Array<Bool>     getArrayBool     (const TableExprId& id);
    virtual Array<Int64>    getArrayInt      (const TableExprId& id);
    virtual Array<Double>   getArrayDouble   (const TableExprId& id);
    virtual Array<DComplex> getArrayDComplex (const TableExprId& id);
    virtual Array<String>   getArrayString   (const TableExprId& id);
    virtual Array<MVTime>   getArrayDate     (const TableExprId& id);
    // </group>

    // Get the unit.
    const String& getUnit() const
      { return itsUnit; }

    // Replace the Table in this node.
    virtual void replaceTable (const Table&) = 0;

  private:
    // Set up the function object.
    virtual void setup (const Table& table,
                        const TaQLStyle&) = 0;

  protected:
    // Get the operands.
    PtrBlock<TableExprNodeRep*>& operands()
      { return itsOperands; }

    // Set the data type.
    // This function must be called by the setup function of the derived class.
    void setDataType (TableExprNodeRep::NodeDataType);

    // Set the dimensionality of the results.
    // <br> 0 means that the results are scalars.
    // <br> -1 means that the results are arrays with unknown dimensionality.
    // <br> >0 means that the results are arrays with that dimensionality.
    // This function must be called by the setup function of the derived class.
    void setNDim (Int ndim);

    // Set the shape of the results if it is fixed and known.
    void setShape (const IPosition& shape);

    // Set the unit of the result.
    // If this function is not called by the setup function of the derived
    // class, the result has no unit.
    void setUnit (const String& unit);

    // Define if the result is constant (e.g. if all arguments are constant).
    // If this function is not called by the setup function of the derived
    // class, the result is not constant.
    void setConstant (Bool isConstant);

  public:
    // Register a the name and construction function of a UDF (thread-safe).
    // An exception is thrown if this name already exists with a different
    // construction function.
    static void registerUDF (const String& name, MakeUDFObject* func);

    // Initialize the function object.
    void init (const PtrBlock<TableExprNodeRep*>& arg,
               const Table& table, const TaQLStyle&);

    // Get the data type.
    TableExprNodeRep::NodeDataType dataType() const
      { return itsDataType; }

    // Get the dimensionality of the results.
    // (0=scalar, -1=array with variable ndim, >0=array with fixed ndim
    Int ndim() const
      { return itsNDim; }

    // Get the result shape if the same for all results.
    const IPosition& shape() const
      { return itsShape; }

    // Tell if the UDF gives a constant result.
    Bool isConstant() const
      { return itsIsConstant; }

    // Create a UDF object (thread-safe).
    static UDFBase* createUDF (const String& name);

  private:
    //# Data members.
    PtrBlock<TableExprNodeRep*>    itsOperands;
    TableExprNodeRep::NodeDataType itsDataType;
    Int                            itsNDim;
    IPosition                      itsShape;
    String                         itsUnit;
    Bool                           itsIsConstant;
    static map<String, MakeUDFObject*> theirRegistry;
    static Mutex                       theirMutex;
  };

} // end namespace

#endif
