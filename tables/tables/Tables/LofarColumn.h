//# LofarColumn.h: A Column in the LOFAR Storage Manager
//# Copyright (C) 2009
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

#ifndef TABLES_LOFARCOLUMN_H
#define TABLES_LOFARCOLUMN_H


//# Includes
#include <casa/aips.h>
#include <tables/Tables/StManColumn.h>
#include <tables/Tables/LofarStMan.h>
#include <casa/Arrays/IPosition.h>
#include <casa/OS/Conversion.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward declarations


// <summary>
// A column in the LOFAR Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tLofarStMan.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=LofarStMan>LofarStMan</linkto>
// </prerequisite>

// <synopsis>
// For each column a specific Column class exists.
// </synopsis>

class LofarColumn : public StManColumn
{
public:
  explicit LofarColumn (LofarStMan* parent, int dtype)
    : StManColumn (dtype),
      itsParent   (parent)
  {}
  // Most columns are not writable (only DATA is writable).
  virtual Bool isWritable() const;
  // Set column shape of fixed shape columns; it does nothing.
  virtual void setShapeColumn (const IPosition& shape);
protected:
  LofarStMan* itsParent;
};

// <summary>ANTENNA1 column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class Ant1Column : public LofarColumn
{
public:
  explicit Ant1Column (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual void getIntV (uInt rowNr, Int* dataPtr);
};

// <summary>ANTENNA2 column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class Ant2Column : public LofarColumn
{
public:
  explicit Ant2Column (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual void getIntV (uInt rowNr, Int* dataPtr);
};

// <summary>TIME and TIME_CENTROID column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class TimeColumn : public LofarColumn
{
public:
  explicit TimeColumn (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual void getdoubleV (uInt rowNr, Double* dataPtr);
private:
  Double itsValue;
};

// <summary>INTERVAL and EXPOSURE column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class IntervalColumn : public LofarColumn
{
public:
  explicit IntervalColumn (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual void getdoubleV (uInt rowNr, Double* dataPtr);
private:
  Double itsValue;
};

// <summary>All columns in the LOFAR Storage Manager with value 0.</summary>
// <use visibility=local>
class ZeroColumn : public LofarColumn
{
public:
  explicit ZeroColumn (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual void getIntV (uInt rowNr, Int* dataPtr);
private:
  Int itsValue;
};

// <summary>All columns in the LOFAR Storage Manager with value False.</summary>
// <use visibility=local>
class FalseColumn : public LofarColumn
{
public:
  explicit FalseColumn (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual void getBoolV (uInt rowNr, Bool* dataPtr);
private:
  Bool itsValue;
};

// <summary>UVW column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class UvwColumn : public LofarColumn
{
public:
  explicit UvwColumn (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual IPosition shape (uInt rownr);
  virtual void getArraydoubleV (uInt rowNr, Array<Double>* dataPtr);
};

// <summary>DATA column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class DataColumn : public LofarColumn
{
public:
  explicit DataColumn (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual Bool isWritable() const;
  virtual IPosition shape (uInt rownr);
  virtual void getArrayComplexV (uInt rowNr, Array<Complex>* dataPtr);
  virtual void putArrayComplexV (uInt rowNr, const Array<Complex>* dataPtr);
};

// <summary>FLAG column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class FlagColumn : public LofarColumn
{
public:
  explicit FlagColumn (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual IPosition shape (uInt rownr);
  virtual void getArrayBoolV (uInt rowNr, Array<Bool>* dataPtr);
};

// <summary>WEIGHT column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class WeightColumn : public LofarColumn
{
public:
  explicit WeightColumn (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual IPosition shape (uInt rownr);
  virtual void getArrayfloatV (uInt rowNr, Array<Float>* dataPtr);
};

// <summary>SIGMA column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class SigmaColumn : public LofarColumn
{
public:
  explicit SigmaColumn (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual IPosition shape (uInt rownr);
  virtual void getArrayfloatV (uInt rowNr, Array<Float>* dataPtr);
};

// <summary>FLAG_CATEGORY column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class FlagCatColumn : public LofarColumn
{
public:
  explicit FlagCatColumn (LofarStMan* parent, int dtype)
    : LofarColumn(parent, dtype) {}
  virtual Bool isShapeDefined (uInt rownr);
  virtual IPosition shape (uInt rownr);
};


} //# NAMESPACE CASA - END

#endif
