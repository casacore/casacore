//# LatticeIter.cc: defines the RO_LatticeIterator and LatticeIterator classes
//# Copyright (C) 1994,1995,1996,1997
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

#include <aips/aips.h>

#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeNavigator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Utilities/Assert.h> 

//---------------------RO_LatticeIterator---------------------------

template <class T> RO_LatticeIterator<T>::
RO_LatticeIterator(const Lattice<T> & lattice, const LatticeNavigator & method)
  :theIterPtr(lattice.makeIter(method))
{
  DebugAssert(ok(), AipsError);
};

template <class T> RO_LatticeIterator<T>::
RO_LatticeIterator(const Lattice<T> & lattice, const IPosition & cursorShape)
  :theIterPtr(lattice.makeIter(LatticeStepper(lattice.shape(),cursorShape)))
{
  DebugAssert(ok(), AipsError);
};

template <class T> RO_LatticeIterator<T>::
RO_LatticeIterator(const RO_LatticeIterator<T> & other)
  :theIterPtr(other.theIterPtr)
{
  DebugAssert(ok(), AipsError);
};

template <class T> RO_LatticeIterator<T>::
~RO_LatticeIterator()
{
  // CountedPtr destructor takes care of deletion of the LatticeIterInterface
};

template <class T> RO_LatticeIterator<T> & RO_LatticeIterator<T>::
operator=(const RO_LatticeIterator<T> & other) {
  DebugAssert(ok(), AipsError);
  if (this != &other)
    theIterPtr = other.theIterPtr;
  return *this;
};

template <class T> Bool RO_LatticeIterator<T>::
operator++(Int) {
  return theIterPtr->operator++(0);
};
 
template <class T> Bool RO_LatticeIterator<T>::
operator++() {
  return theIterPtr->operator++();
};
 
template <class T> Bool RO_LatticeIterator<T>::
operator--(int) {
  return theIterPtr->operator--(0);
};

template <class T> Bool RO_LatticeIterator<T>::
operator--() {
  return theIterPtr->operator--();
}; 

template <class T> void RO_LatticeIterator<T>::
reset() {
  theIterPtr->reset();
};

template <class T> Bool RO_LatticeIterator<T>::
atStart() const {
  return theIterPtr->atStart();
};

template <class T> Bool RO_LatticeIterator<T>::
atEnd() const {
  return theIterPtr->atEnd();
};

template <class T> uInt RO_LatticeIterator<T>::
nsteps() const {
  return theIterPtr->nsteps();
};

template <class T> IPosition RO_LatticeIterator<T>::
position() const {
  return theIterPtr->position();
};

template <class T> IPosition RO_LatticeIterator<T>::
endPosition() const {
  return theIterPtr->endPosition();
};

template <class T>  IPosition RO_LatticeIterator<T>::
latticeShape() const {
  return theIterPtr->latticeShape();
};

template <class T> IPosition RO_LatticeIterator<T>::
cursorShape() const {
  return theIterPtr->cursorShape();
};

template <class T> const Vector<T> & RO_LatticeIterator<T>::
vectorCursor() const {
  return theIterPtr->vectorCursor();
};

template <class T> const Matrix<T> & RO_LatticeIterator<T>::
matrixCursor() const {
  return theIterPtr->matrixCursor();
};

template <class T> const Cube<T> & RO_LatticeIterator<T>::
cubeCursor() const {
  return theIterPtr->cubeCursor();
};

template <class T> const Array<T> & RO_LatticeIterator<T>::
cursor() const {
  return theIterPtr->cursor();
};

static LogIO ROlogErr(LogOrigin("RO_LatticeIterator<T>", "ok()"));

template <class T> Bool RO_LatticeIterator<T>::
ok() const {
  if (theIterPtr.null() == True){
    ROlogErr << LogIO::SEVERE 
	   << "The pointer the the actual Lattice Iterator is zero!" 
	   << LogIO::POST;
    return False;
  }
  if (theIterPtr->ok() == False){
    ROlogErr << LogIO::SEVERE 
	   << "The actual Lattice Iterator class is inconsistant" 
	   << LogIO::POST;
    return False;
  }
  return True;
};

//-----------------------LatticeIterator-----------------------------------

template <class T> LatticeIterator<T>::
LatticeIterator(Lattice<T> & lattice, const LatticeNavigator & method)   
  :theIterPtr(lattice.makeIter(method))
{
  DebugAssert(ok(), AipsError);
};

template <class T> LatticeIterator<T>::
LatticeIterator(Lattice<T> & lattice, const IPosition & cursorShape)   
  :theIterPtr(lattice.makeIter(LatticeStepper(lattice.shape(),cursorShape)))
{
  DebugAssert(ok(), AipsError);
};

template <class T> LatticeIterator<T>::
LatticeIterator(const LatticeIterator<T> & other)
  :theIterPtr(other.theIterPtr)
{
  DebugAssert(ok(), AipsError);
};

template <class T> LatticeIterator<T>::
~LatticeIterator()
{
  // CountedPtr destructor takes care of deletion of the LatticeIterInterface
};

template <class T> LatticeIterator<T> & LatticeIterator<T>::
operator=(const LatticeIterator<T> & other) {
  DebugAssert(ok(), AipsError);
  if (this != &other)
    theIterPtr = other.theIterPtr;
  return *this;
};

template <class T> Bool LatticeIterator<T>::
operator++(Int) {
  return theIterPtr->operator++(0);
};
 
template <class T> Bool LatticeIterator<T>::
operator++() {
  return theIterPtr->operator++();
};
 
template <class T> Bool LatticeIterator<T>::operator--(Int) {
  return theIterPtr->operator--(0);
};

template <class T> Bool LatticeIterator<T>::
operator--() {
  return theIterPtr->operator--();
};

template <class T> void LatticeIterator<T>::
reset() {
  theIterPtr->reset();
};

template <class T> Bool LatticeIterator<T>::
atStart() const {
  return theIterPtr->atStart();
};

template <class T> Bool LatticeIterator<T>::
atEnd() const {
  return theIterPtr->atEnd();
};

template <class T> uInt LatticeIterator<T>::
nsteps() const {
  return theIterPtr->nsteps();  
};

template <class T> IPosition LatticeIterator<T>::
position() const {
  return theIterPtr->position();
};

template <class T> IPosition LatticeIterator<T>::
endPosition() const {
  return theIterPtr->endPosition();
};

template <class T> IPosition LatticeIterator<T>::
latticeShape() const {
  return theIterPtr->latticeShape();
};

template <class T> IPosition LatticeIterator<T>::
cursorShape() const {
  return theIterPtr->cursorShape();
};

template <class T> Vector<T> & LatticeIterator<T>::
vectorCursor() {
  return theIterPtr->vectorCursor();
};

template <class T> Matrix<T> & LatticeIterator<T>::
matrixCursor() {
  return theIterPtr->matrixCursor();
};

template <class T> Cube<T> & LatticeIterator<T>::
cubeCursor() {
  return theIterPtr->cubeCursor();
};

template <class T> Array<T> & LatticeIterator<T>::
cursor() {
  return theIterPtr->cursor();
};

static LogIO logErr(LogOrigin("LatticeIterator<T>", "ok()"));

template <class T> Bool LatticeIterator<T>::
ok() const {
  if (theIterPtr.null() == True){
    logErr << LogIO::SEVERE 
	   << "The pointer the the actual Lattice Iterator is zero!" 
	   << LogIO::POST;
    return False;
  }
  if (theIterPtr->ok() == False){
    logErr << LogIO::SEVERE 
	   << "The actual Lattice Iterator class is inconsistant" 
	   << LogIO::POST;
    return False;
  }
  return True;
};
