//# ExprNodeSetElem.h: Classes representing a set element in table select expression
//# Copyright (C) 1997,2000,2001,2002,2003
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

#ifndef TABLES_EXPRNODESETELEM_H
#define TABLES_EXPRNODESETELEM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class TableExprNode;

  // Define a shorthand for a shared pointer to TableExprNodeSetElemBase.
  // It will be used at several places.
  class TableExprNodeSetElemBase;
  typedef std::shared_ptr<TableExprNodeSetElemBase> TENSEBShPtr;


  // <summary>
  // Base class for the classes defining set element
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSet
  //   <li> TableExprNodeRep
  // </prerequisite>

  // <synopsis>
  // This class is used to assemble the table expression nodes
  // representing an element in a set. There are various types of elements
  // such as a single value, discrete range and continuous interval.
  // <br>The base class holds the possible start, end and increment value of
  // an element. It also has functions telling the overall expression which
  // columns and aggregate functions are used by an element.
  // </synopsis>

  class TableExprNodeSetElemBase: public TableExprNodeRep
  {
  public:
    // Constructor to initialize the parent.
    explicit TableExprNodeSetElemBase (NodeDataType = NTDouble);
    
    virtual ~TableExprNodeSetElemBase() = default;

    // Show the node.
    void show (ostream& os, uInt indent) const override;

    // Flatten the node tree by adding the node and its children to the vector.
    virtual void flattenTree (std::vector<TableExprNodeRep*>&) override;

    // Is it a discrete set element.
    // Default implementation returns False.
    virtual Bool isDiscrete() const;

    // Is a single value given?
    // Default implementation returns False.
    virtual Bool isSingle() const;

    // Is the interval left or right closed?
    // Default implementation returns False.
    // <group>
    virtual Bool isLeftClosed() const;
    virtual Bool isRightClosed() const;
    // </group>

    // Is the interval given as mid-width?
    // Default implementation returns False.
    virtual Bool isMidWidth() const;

    // Get the start, end or increment expression.
    // Note that the shared pointer returned can be null indicating that a
    // value was not given.
    // <group>
    const TENShPtr& start() const
      { return itsStart; }
    const TENShPtr& end() const
      { return itsEnd; }
    const TENShPtr& increment() const
      { return itsIncr; }
    // </group>

    // Fill a vector with the value(s) from this element by appending them
    // at the end of the vector; the end is given by argument <src>cnt</src>
    // which gets incremented with the number of values appended.
    // This is used by the system to convert a set to a vector.
    // <group>
    virtual void fillVector (Vector<Bool>& vec, Int64& cnt,
                             const TableExprId& id) const;
    virtual void fillVector (Vector<Int64>& vec, Int64& cnt,
                             const TableExprId& id) const;
    virtual void fillVector (Vector<Double>& vec, Int64& cnt,
                             const TableExprId& id) const;
    virtual void fillVector (Vector<DComplex>& vec, Int64& cnt,
                             const TableExprId& id) const;
    virtual void fillVector (Vector<String>& vec, Int64& cnt,
                             const TableExprId& id) const;
    virtual void fillVector (Vector<MVTime>& vec, Int64& cnt,
                             const TableExprId& id) const;
    // </group>

    // Set a flag in the match output array if the corresponding element
    // in the value array is included in this set element.
    // This is used by the system to implement the IN operator.
    // <br>Note that it does NOT set match values to False; it is assumed they
    // are initialized that way.
    // <group>
    virtual void matchBool     (Bool* match, const Bool* value, size_t nval,
                                const TableExprId& id) const;
    virtual void matchInt      (Bool* match, const Int64* value, size_t nval,
                                const TableExprId& id) const;
    virtual void matchDouble   (Bool* match, const Double* value, size_t nval,
                                const TableExprId& id) const;
    virtual void matchDComplex (Bool* match, const DComplex* value, size_t nval,
                                const TableExprId& id) const;
    virtual void matchString   (Bool* match, const String* value, size_t nval,
                                const TableExprId& id) const;
    virtual void matchDate     (Bool* match, const MVTime* value, size_t nval,
                                const TableExprId& id) const;
    // </group>

    // Evaluate the element for the given row and construct a new
    // (constant) element from it.
    // This is used by the system to implement a set in a GIVING clause.
    virtual TENSEBShPtr evaluate (const TableExprId& id) const = 0;

    // Set the expression type (Variable or Constant) depending on the children.
    void setExprType();

    // Let a set node convert itself to the given unit.
    void adaptSetUnits (const Unit&) override;

    // Get the start or end value of a Double or DateTime interval.
    // <group>
    void getStart (const TableExprId& id, Double&) const;
    void getEnd (const TableExprId& id, Double& ) const;
    // </group>

    // Get the start or end value of a String interval.
    // <group>
    void getStart (const TableExprId& id, String&) const;
    void getEnd (const TableExprId& id, String&) const;
    // </group>

  protected:
    // Evaluate the expression for the given row id.
    TENShPtr evalExpr (const TENShPtr& expr, const TableExprId& id) const;

    //# Data members.
    //# They are put here, so function start(), etc. can be inlined.
    //# Also getColumnNodes(), etc. make use of them.
    TENShPtr itsStart;
    TENShPtr itsEnd;
    TENShPtr itsIncr;

  private:
    // A copy of a TableExprNodeSetElem cannot be made.
    TableExprNodeSetElemBase& operator= (const TableExprNodeSetElemBase&);
  };


  // <summary>
  // Class defining a set element containing a single value.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSet
  //   <li> TableExprNodeRep
  // </prerequisite>

  // <synopsis>
  // This class defines a set element containing a single (discrete) value
  // which can be of any data type. The value can be a scalar or an array.
  // It can be used for 3 purposes:
  // <br>- A function argument.
  // <br>- A single index in an array indexing operation.
  // <br>- A single value in a set (used with the IN operator).
  //       This is in fact a bounded discrete ramge (see TableExprNodeSetDiscrete).
  // </synopsis>

  class TableExprNodeSetElemSingle: public TableExprNodeSetElemBase
  {
  public:
    // Create the object for a single expression node.
    explicit TableExprNodeSetElemSingle (const TableExprNode& node);

    ~TableExprNodeSetElemSingle() override = default;

    // It is a discrete set element.
    Bool isDiscrete() const override;

    // A single value is given (which can be an array).
    Bool isSingle() const override;

    // Fill a vector with the value(s) from this element by appending them
    // at the end of the vector; the end is given by argument <src>cnt</src>
    // which gets incremented with the number of values appended.
    // This is used by the system to convert a set to a vector.
    // <group>
    void fillVector (Vector<Bool>& vec, Int64& cnt,
                     const TableExprId& id) const override;
    void fillVector (Vector<Int64>& vec, Int64& cnt,
                     const TableExprId& id) const override;
    void fillVector (Vector<Double>& vec, Int64& cnt,
                     const TableExprId& id) const override;
    void fillVector (Vector<DComplex>& vec, Int64& cnt,
                     const TableExprId& id) const override;
    void fillVector (Vector<String>& vec, Int64& cnt,
                     const TableExprId& id) const override;
    void fillVector (Vector<MVTime>& vec, Int64& cnt,
                     const TableExprId& id) const override;
    // </group>

    // Set a flag in the match output array if the corresponding element
    // in the value array is included in this set element.
    // This is used by the system to implement the IN operator.
    // <br>Note that it does NOT set match values to False; it is assumed they
    // are initialized that way.
    // <group>
    void matchBool     (Bool* match, const Bool* value, size_t nval,
                        const TableExprId& id) const override;
    void matchInt      (Bool* match, const Int64* value, size_t nval,
                        const TableExprId& id) const override;
    void matchDouble   (Bool* match, const Double* value, size_t nval,
                        const TableExprId& id) const override;
    void matchDComplex (Bool* match, const DComplex* value, size_t nval,
                        const TableExprId& id) const override;
    void matchString   (Bool* match, const String* value, size_t nval,
                        const TableExprId& id) const override;
    void matchDate     (Bool* match, const MVTime* value, size_t nval,
                        const TableExprId& id) const override;
    // </group>

    // Evaluate the element for the given row and construct a new
    // (constant) element from it.
    // This is used by the system to implement a set in a GIVING clause.
    TENSEBShPtr evaluate (const TableExprId& id) const override;

  private:
    // Construct an element from the given parts and take over their pointers.
    // It is used by evaluate to construct an element in a rather cheap way.
    TableExprNodeSetElemSingle (const TableExprNodeSetElemSingle& that,
                                const TENShPtr& start);
  };


  
  // <summary>
  // Class defining a set element containing a discrete range.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSet
  //   <li> TableExprNodeRep
  // </prerequisite>

  // <synopsis>
  // Class defining a set element containing a discrete range (start:end:incr)
  // which can be of data type Int, Double and Datetime. They have to be scalars.
  // A range consists of a start, end and increment value, each of them optional.
  // Increment defaults to 1. The end value can be inclusive or exclusive
  // (as in Python).
  // It can be used for 2 purposes:
  // <br>- A slice in an array indexing operation which requires data
  //       type Int. In that case start default to the beginning
  //       of the dimension and end defaults to the end.
  // <br>- A discrete range in a set. Start has to be given.
  //       If end is not given, the result is an unbounded discrete range.
  // <br>For a discrete range, the type of start and end can also be
  // a datetime scalar.
  // <br>A bounded discrete range is automatically
  // converted to a vector, which makes it possible to apply array
  // functions to it (e.g., date() + [0:31]).
  // </synopsis>

  class TableExprNodeSetElemDiscrete: public TableExprNodeSetElemBase
  {
  public:
    // Create the object for a discrete range.
    // Each of the start, end, and incr pointers can be null meaning
    // that they are not given (see the synopsis for an explanation).
    // Optionally the end is inclusive (C++ and Glish style) or exclusive
    // (Python style).
    TableExprNodeSetElemDiscrete (const TableExprNode& start,
                                  const TableExprNode& end,
                                  const TableExprNode& incr,
                                  Bool isEndExcl = False);

    ~TableExprNodeSetElemDiscrete() override = default;

    // It is a discrete set element.
    Bool isDiscrete() const override;

    // Fill a vector with the value(s) from this element by appending them
    // at the end of the vector; the end is given by argument <src>cnt</src>
    // which gets incremented with the number of values appended.
    // This is used by the system to convert a set to a vector.
    // <group>
    void fillVector (Vector<Int64>& vec, Int64& cnt,
                     const TableExprId& id) const override;
    void fillVector (Vector<Double>& vec, Int64& cnt,
                     const TableExprId& id) const override;
    void fillVector (Vector<MVTime>& vec, Int64& cnt,
                     const TableExprId& id) const override;
    // </group>

    // Set a flag in the match output array if the corresponding element
    // in the value array is included in this set element.
    // This is used by the system to implement the IN operator.
    // <br>Note that it does NOT set match values to False; it is assumed they
    // are initialized that way.
    // <group>
    void matchInt      (Bool* match, const Int64* value, size_t nval,
                        const TableExprId& id) const override;
    void matchDouble   (Bool* match, const Double* value, size_t nval,
                        const TableExprId& id) const override;
    void matchDate     (Bool* match, const MVTime* value, size_t nval,
                        const TableExprId& id) const override;
    // </group>

    // Evaluate the element for the given row and construct a new
    // (constant) element from it.
    // This is used by the system to implement a set in a GIVING clause.
    TENSEBShPtr evaluate (const TableExprId& id) const override;

  private:
    // Construct an element from the given parts and take over their pointers.
    // It is used by evaluate to construct an element in a rather cheap way.
    TableExprNodeSetElemDiscrete (const TableExprNodeSetElemDiscrete& that,
                                  const TENShPtr& start, const TENShPtr& end,
                                  const TENShPtr& incr);

    //# Data members
    Bool itsEndExcl;
  };


  
  // <summary>
  // Class defining a set element containing a continuous interval.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSet
  //   <li> TableExprNodeRep
  // </prerequisite>

  // <synopsis>
  // Class defining a set element containing a continuous interval.
  // It consists of a start and/or an end scalar value of type int, double,
  // datetime, or string. Data type int will be converted to double.
  // The interval can be open (exclusive) or closed (inclusive) on either side.
  // The interval can be unbounded by leaving out start or end.
  // <br>Note that a continuous interval can also be given as mid-width
  // using class TableExprNodeSetElemMidWidth.
  // </synopsis>

  class TableExprNodeSetElemCont: public TableExprNodeSetElemBase
  {
  public:
    // Create the object for a continuous bounded interval. It can be
    // open or closed on either side.
    TableExprNodeSetElemCont (Bool isLeftClosed, const TableExprNode& start,
                              const TableExprNode& end, Bool isRightClosed);

    // Create the object for a continuous left-bounded interval.
    TableExprNodeSetElemCont (Bool isLeftClosed, const TableExprNode& start);

    // Create the object for a continuous right-bounded interval.
    TableExprNodeSetElemCont (const TableExprNode& end, Bool isRightClosed);

    // Construct an element from the given parts and take over their pointers.
    // It is used by evaluate to construct an element in a rather cheap way.
    TableExprNodeSetElemCont (const TableExprNodeSetElemCont& that,
                              const TENShPtr& start, const TENShPtr& end);

    ~TableExprNodeSetElemCont() override = default;

    // Is the interval left or right closed?
    // <group>
    Bool isLeftClosed() const override;
    Bool isRightClosed() const override;
    // </group>

    // Set a flag in the match output array if the corresponding element
    // in the value array is included in this set element.
    // This is used by the system to implement the IN operator.
    // <br>Note that it does NOT set match values to False; it is assumed they
    // are initialized that way.
    // <group>
    void matchDouble   (Bool* match, const Double* value, size_t nval,
                        const TableExprId& id) const override;
    void matchString   (Bool* match, const String* value, size_t nval,
                        const TableExprId& id) const override;
    void matchDate     (Bool* match, const MVTime* value, size_t nval,
                        const TableExprId& id) const override;
    // </group>

    // Evaluate the element for the given row and construct a new
    // (constant) element from it.
    // This is used by the system to implement a set in a GIVING clause.
    TENSEBShPtr evaluate (const TableExprId& id) const override;

  protected:
    // Constructor used by the derived class TableExprNodeSetElemMidWidth.
    TableExprNodeSetElemCont (const TableExprNode& mid,
                              const TableExprNode& width);
    
  private:
    // Setup the object for a continuous interval.
    void setup (Bool isLeftClosed, const TableExprNode* start,
                const TableExprNode* end, Bool isRightClosed);

    //# Data members
    Bool itsLeftClosed;
    Bool itsRightClosed;
  };



  // <summary>
  // Class defining a set element containing a continuous mid/width interval.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSet
  //   <li> TableExprNodeRep
  // </prerequisite>

  // <synopsis>
  // Class, derived from TableExprNodeSetElemCont, defining a set element
  // containing a continuous interval given its midpoint and width.
  // It is defined as [mid-width/2,mid+width/2]. It is closed on both sides.
  // It can only be used for data type double and datetime.
  // In case of datetime the width must be a double with default unit d (days).
  // <br>Following the definition of intervals in the MeasurementSet (note 223),
  // the interval is infinite on both sides if the width is zero.
  // </synopsis>

  class TableExprNodeSetElemMidWidth: public TableExprNodeSetElemCont
  {
  public:
    // Create the object for a continuous bounded interval given as mid-width.
    // It is closed on both sides.
    TableExprNodeSetElemMidWidth (const TableExprNode& mid,
                                  const TableExprNode& width);

    ~TableExprNodeSetElemMidWidth() override = default;

    // The interval is given as mid-width.
    Bool isMidWidth() const override;

    // Set a flag in the match output array if the corresponding element
    // in the value array is included in this set element.
    // This is used by the system to implement the IN operator.
    // <br>Note that it does NOT set match values to False; it is assumed they
    // are initialized that way.
    // <group>
    void matchDouble (Bool* match, const Double* value, size_t nval,
                      const TableExprId& id) const override;
    void matchDate   (Bool* match, const MVTime* value, size_t nval,
                      const TableExprId& id) const override;
    // </group>

    // Evaluate the set element for the given row and construct a new
    // (constant) TableExprNodeSetElemCont element from it.
    // This is used by the system to implement a set in a GIVING clause.
    TENSEBShPtr evaluate (const TableExprId& id) const override;
  };


  
  // <summary>
  // Class to hold the table expression nodes for an element in a set.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSet
  //   <li> TableExprNodeRep
  // </prerequisite>

  // <synopsis>
  // This class hold a TableExprNodeSetElemBase pointer, thus points
  // to an object of one its derived classes.
  // <br>At the end of 2022 the old TableExprSetElem was split into several
  // classes, one per element type. This class still exists for backward
  // compatibilty, but is also useful in its own right. Its constructors create
  // one of the derived TableExprNodeSetElemBase objects which mirror the
  // TaQL specification, but can also be used directly in C++. The types are:
  // <ul>
  //  <li> A single value; in TaQL: <src>max(array)   or   [2]</src>
  //  <li> A discrete range; in TaQL: <src>[1:101:3]</src>
  //  <li> A continuous start/end interval; in TaQL: <src>between 3 and 11</src>
  //  <li> A continuous mid/width interval; in TaQL: <src>around 7 in 8</src>
  // </ul>
  // <br>Note the difference between a discrete range and a continuous interval.
  // The discrete range 2,6 consists of the values 1,4,7...100
  // The continuous intervals consists of all values between 3 and 11.
  // </synopsis>

  class TableExprNodeSetElem: public TableExprNodeRep
  {
  public:
    // Create from a Base element.
    explicit TableExprNodeSetElem (const TENSEBShPtr& elem);

    ~TableExprNodeSetElem() override = default;

    // Create the object for a single expression node.
    explicit TableExprNodeSetElem (const TableExprNode& node);

    // Create the object for a discrete range.
    // Each of the start, end, and incr pointers can be zero meaning
    // that they are not given (see the synopsis for an explanation).
    // Optionally the end is inclusive (C++ and Glish style) or exclusive
    // (Python style).
    TableExprNodeSetElem (const TableExprNode* start,
                          const TableExprNode* end,
                          const TableExprNode* incr,
                          Bool isEndExcl = False);

    // Create the object for a continuous bounded interval. It can be
    // open or closed on either side.
    TableExprNodeSetElem (Bool isLeftClosed, const TableExprNode& start,
                          const TableExprNode& end, Bool isRightClosed);

    // Create the object for a continuous left-bounded interval.
    TableExprNodeSetElem (Bool isLeftClosed, const TableExprNode& start);

    // Create the object for a continuous right-bounded interval.
    TableExprNodeSetElem (const TableExprNode& end, Bool isRightClosed);

    // Create the object for a mid-width interval (closed on both sides).
    TableExprNodeSetElem (const TableExprNode& mid, const TableExprNode& width);

    // Get the internal pointer to the underlying TableExprNodeSetElemBase.
    const TENSEBShPtr& getElem() const
      { return itsElem; }
    
    // Show the node.
    void show (ostream& os, uInt indent) const override
      { itsElem->show (os, indent); }

    // Is it a discrete set element.
    Bool isDiscrete() const
      { return itsElem->isDiscrete(); }

    // Is a single value given?
    Bool isSingle() const
      { return itsElem->isSingle(); }

    // Is the interval left or right closed?
    // <group>
    Bool isLeftClosed() const
      { return itsElem->isLeftClosed(); }
    Bool isRightClosed() const
      { return itsElem->isRightClosed(); }
    // </group>

    // Is the interval given as mid-width?
    Bool isMidWidth() const
      { return itsElem->isMidWidth(); }
    
    // Get the start, end or increment expression.
    // Note that the shared pointer returned can be null indicating that a
    // value was not given.
    // <group>
    const TENShPtr& start() const
      { return itsElem->start(); }
    const TENShPtr& end() const
      { return itsElem->end(); }
    const TENShPtr& increment() const
      { return itsElem->increment(); }
    // </group>

  private:
    // Set the data and expression type in the superclass.
    void init();
    
    //# Data members
    TENSEBShPtr itsElem;
  };



} //# NAMESPACE CASACORE - END

#endif
