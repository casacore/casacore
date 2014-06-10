//# Quantum.h: class to manipulate physical, dimensioned quantities
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001
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
//# $Id: Quantum.h 20993 2010-11-08 13:36:32Z gervandiepen $

#ifndef CASA_QVECTOR_H
#define CASA_QVECTOR_H

//# Includes
#include <casa/Quanta/Quantum.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template <class T> class QVector;

typedef QVector<Double> QVD;

// <summary>
// Specialization for Quantum<Vector<T> >
// </summary>

// <use visibility=export>
//
// <prerequisite>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>
//
// <etymology>
// Vector of quantities.
// </etymology>
//
// <synopsis> 
// Objects of type Quantum<Vector<Double> > are used often in our code.
// We need a way to access individual elements easily
// </synopsis>

template <class T> class QVector : public Quantum<Vector<T> > {

 public:

	// zero elements
	QVector();

	QVector(const Vector<T>& v, const Unit& u);

	// Copy constructor (deep copy)
	QVector(const QVector& other);

	~QVector();
	// access single element
	Quantum<T> operator[](uInt index) const;

	size_t size() const;

	size_t nelements() const;

	void scale(T d);

	// add operators as needed
	QVector<T> operator+(const QVector<T>& d) const;
	QVector<T> operator-(const QVector<T>& d) const;
	QVector<T> operator/(T d) const;


	Quantum<T> min() const;

	Quantum<T> max() const;

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casa/Quanta/QVector.tcc>
#endif

#endif
