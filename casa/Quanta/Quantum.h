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
//# $Id$

#ifndef CASA_QUANTUM_H
#define CASA_QUANTUM_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/QBase.h>
#include <casacore/casa/iosstrfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Quantum;

//# Typedefs
typedef Quantum<Double> Quantity;

// <summary>
// Quantities (i.e. dimensioned values)
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tQuantum">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Unit>Unit</linkto>
// </prerequisite>
//
// <etymology>
// A Quantity is defined as a single Double value with attached units.
// From this definition the templated Quantum class arose, to have non-Double,
// non-scalar quantities.  
// </etymology>
//
// <synopsis> 
// Quantities are values with a unit. Their basic specification can be one of
// two forms:
// <srcblock>
// Quantity( Double value, String unit);	// or: Unit unit
// Quantum<Type> ( Type value, String unit)	// or: Unit unit
// </srcblock>
//
// A unit is a string of known unit fields separated
// by 'space' or '.' (to indicate multiply) or '/' (to indicate divide).
// See the <linkto class=Unit>Unit</linkto> class for details.
//
// Example: km/s/(Mpc.s)2  is identical to km.s-1.Mpc-2.s-2
//
//  <h3> Defining a Quantum </h3>
// The following list of constructors is available.
// <note role=tip>
// In the following 'String' can be replaced by 'Unit' everywhere. The
// only difference being a check for a legitimate unit string being executed
// if Unit specified (with exception if error) 
// </note>
// <note role=tip>
// <src>'Quantum<Type>'</src> can, if Type equals Double, be replaced
//		with 'Quantity'
//
// 'Type' can be any simple or non-simple arithmetic type.
//
// E.g. <src><Double>, <Complex>, <Vector<Double> ></src>
// </note>
// <ul>
//   <li> <src>Quantum<Type>()			value 0 generated</src>
//   <li> <src>Quantum<Type>( Quantum<Type>)	copy constructor</src>
//   <li> <src>Quantum<Type>( Type factor)	value factor generated</src>
//   <li> <src>Quantum<Type>( Type factor, Unit unit) specified quantity</src>
//   <li> <src>Quantum<Type>( Type factor, Quantum<any> quant) specified factor,</src>
//						the unit from the quant
// </ul>
// 
//
//  <h3> Manipulating quantities </h3>
// <linkto group="QMath.h#Quantum mathematical operations">Mathematical operators and functions</linkto> and
//  <linkto group="QLogical.h#Quantum logical operations">logical operations</linkto> (comparisons)
// are defined on Quantums. They are,
// of course, only available if the template Type supports them.
// <ul>
// <li> <src>=		assignment of identical <type></src>
// <li> <src>* *=	multiple two Quantums of same <type>, or Quantum and type</src>
// <li> <src>/ /=	divide two Quantums of same <type>, or Quantum and type</src>
// note:
// In multiplication and division, and if <src><type></src> is scalar, the left or
// right-hand side can be of type <src><type></src> (e.g 2.*Quantity is allowed)
// <li> <src>+ +=	add two Quantums of same <type> or Quantum and type</src>
//		and same unit dimensions (else exception)
// <li> - -=	subtract (same as +)
// <li> -	negate Quantum
// <li> +	unary + on Quantum
// <li> <src>== !=	compare unit dimensions and value of same <type>. They will</src>
//		be unequal if the units do not match or the values (possibly
//		converted to common base units). All comparisons work also
//		on a <src>Quantum<type> and <type></src>
// <li> <src>< >	compare unit dimensions. Exception if no match,</src>
//		else compare the values
// <li> <src><= >=	ibid</src>
// <li> pow(Int) raise to an (integer) power
// </ul>
// 
//
//  <h3> Manipulating the value and/or units of quanta </h3>
// Quantities can be converted to other units by the following set of member
// functions:
// <ul>
//   <li> convert()		will convert the quantum to canonical units.
//				E.g. given myval=Quantity(5.,"Jy"),
//				myval.convert() will convert the qunatum to
//				Quantity(5.e-26,"kg.s-2")
//   <li> convert(Unit unit) will convert the quantum to the
//				specified unit with any remaining dimensions
//				expressed in canonical units. E.g given
//				myval as above, myval.convert("W/cm") will
//				make it Quantity(5.e-28,"W/cm.m-1.s")
//   <li> <src>convert(Quantum<Type> quant) will convert the quantum</src>
//				to the units of the specified quant with the
//				same conversion rules as the previous one
// </ul>
// <note role=tip> All converting type methods (i.e. convert(), get() and
// getValue() with specified units), will automatically convert also from
// time to angle units (or v.v) if necessary, as long as they are simple. I.e.
// deg will be converted to h, but asking to convert m/s to m/deg will
// produce the standard conversion to m/deg.rad/s. </note>
//
// Quanta can be checked for having the correct unit dimensions (e.g. before
// addition or comparing) by the following two member functions, which will
// return a Bool value:
// <ul>
//   <li> isConform(Unit unit)
//   <li> <src>isConform(Quantum<Type> quant)</src>
//   <li> check(UnitVal kind)
// </ul>
// or by an assertion, which will throw an exception:<br>
// <ul>
//   <li> assure(UnitVal kind)
// </ul>
//
// The quantum can be retrieved with a change in units by:
// <ul>
//   <li> get()		will return the quantum converted to canonical units.
//				E.g. given myval=Quantity(5.,"Jy"),
//				myval.get() will return
//				Quantity(5.e-26,"kg.s-2")
//   <li> get(Unit unit) 	will return the quantum converted to the
//				specified unit with any remaining dimensions
//				expressed in canonical units. E.g given
//				myval as above, myval.get("W/cm") will
//				return it as Quantity(5.e-28,"W/cm.m-1.s")
//   <li> <src>get(Quantum<Type> quant) will return the quantum converted</src>
//				to the units of the specified quant with the
//				same conversion rules as the previous one
// </ul>
//
// The value and units of a quantum can be set or retrieved separately by the
// following member functions:
// <ul>
//   <li> getValue()		return the value (as Type) of the quantum.
//     <note role=tip> myval.get().getValue() will return the
//			value of myval expressed in canonical units
//     </note>
//   <li> getValue(Unit unit)	return the value (as converted to unit)
//   <li> getUnit()		return the String part of the unit of the
//				quantum (use getFullUnit if interested in
//				the complete Unit, e.g. for re-use)
//   <li> getFullUnit()	        return the complete unit of the Quantum (use
//				getUnit() if interested in String part only)
//   <li> setValue(Type val)	replace the value of the quantum with val,
//				leaving the units the same
//   <li> scale(Type val)	multiply the value (leaving units same) by the
//				specified value
//   <li> setUnit(Unit unit) 	replace the units of the quantum, leaving
//				the value the same.
//   <li> <src>setUnit(Quantum<Type> quant) ibid</src>
//   <li> set(String quantity)	replace the value and unit as deduced from quantity
// </ul>
//
// The output operator (<src><<</src>) will produce the value of the quantum and its
// units. Given <src>Quantity myval(5.,"mJy"), << myval</src> will produce:
//	<src>5.0 mJy</src>; while <src><< myval.get("yW/m2")</src>
// will produce: <src>.00005 yW/m2.s</src>.<br>
// The input operator (<src>>></src>, or the static read functions) will
// convert a String to a Quantum (quantity only for now). The analysis
// will do the following:
// <ul>
//   <li> Check if it can be converted as a time/angle, if so use
//		(<linkto class=MVAngle>MVAngle</linkto>)
//   <li> Check if it can be used as a date/time. if so use
//		(<linkto class=MVTime>MVTime</linkto>)
//   <li> Interpret as a value with units
// </ul>
// <note role=caution> Since e.g. <em>12d</em> could be interpreted as
// being both an angle (12 degrees) or a quantity (12 days), the only way
// is to differentiate them with a decimal point (12.d will be days)</note>
// 
// </synopsis> 
//
// <example>
// An experiment has measured the energy of a photon in keV. The following will
// output the wavelength and frequency of this photon (see the
// <linkto class=QC">QC</linkto> class for quantity constants):
// <srcblock>
//	#include <casacore/casa/Quanta.h>
//	Double myval;			// keV photon energy
//	Quantity quant(myval,"keV");	// make quantity
//	cout << "A photon with energy " << quant << endl
//		<< " has a frequency of "
//		<< (quant/QC::h)->get("GHz") << endl	// h=Planck
//		<< " and a wavelength of "
//		<< (QC::c/quant/QC::h)->get("nm")	// c=light velocity
//		<< " or " << QC::c/quant/QC::h << endl;
// </srcblock>
// </example>	
//
// <motivation>
// Major use is foreseen in all calculations with observed data.
// </motivation>

// <templating arg=Qtype>
//   <li> prefix +,-
//   <li> + - * / and += -= *= /=
//   <li> <src>< <= == != >= ></src>
//   <li> sin 
//   <li> cos 
//   <li> tan  
//   <li> asin 
//   <li> acos
//   <li> atan 
//   <li> atan2 
//   <li> abs 
//   <li> ceil 
//   <li> floor
//   <li> <note role=caution>
//	It is assumed that all these functions return either Bool or
//	the same data type as inputted (i.e. QType). Special functions are
//	provided in this module to convert Int and LogicalArray to Bool;
//	and to convert were necessary to Complex (e.g. abs(Complex)).
//   </note>
// </templating>

// <todo asof="941123">
//   <li> Some inlining (did not work first go)
// </todo>

template <class Qtype> class Quantum : public QBase{
  //# Friends
  // Input, only quantity is supported now
  friend istream& operator>> (istream &is, Quantity &ku);
 public:
  //# Constructors
  // Default constructor, generates '0'
  Quantum();
  // Copy constructor (deep copy)
  Quantum(const Quantum<Qtype> &other);
  // Construct undimensioned quantum (i.e. unit="")
  Quantum(const Qtype &factor);
  // Construct dimensioned quantum (e.g. '1.23 km/Mpc')
  // <thrown>
  //   <li> AipsError if non-matching unit dimensions
  // </thrown>
  // <group>
  Quantum(const Qtype &factor, const Unit &s);
  // </group>
  // Construct quantum with unit copied from existing quantum
  Quantum(const Qtype &factor, const QBase &other);
  
  // Destructor
  ~Quantum();

  //# Operators
  // Assignment (deep copy)
  Quantum<Qtype> &operator=(const Quantum<Qtype> &other);
  
  
  // Unary operations
  // <group>
  const Quantum<Qtype> &operator+() const;
  Quantum<Qtype> operator-() const;
  // </group>
  
  // In place arithmetic functions: left hand side changed in place
  // <thrown>
  //   <li> AipsError if non-conforming units (+ and -)
  //   <li> AipsError if illegal result unit (* and /; programming error)
  // </thrown>
  // <group>
  Quantum<Qtype> &operator+=(const Quantum<Qtype> &other);
  Quantum<Qtype> &operator+=(const Qtype &other);
  Quantum<Qtype> &operator-=(const Quantum<Qtype> &other);
  Quantum<Qtype> &operator-=(const Qtype &other);
  Quantum<Qtype> &operator*=(const Quantum<Qtype> &other);
  Quantum<Qtype> &operator*=(const Qtype &other);
  Quantum<Qtype> &operator/=(const Quantum<Qtype> &other);
  Quantum<Qtype> &operator/=(const Qtype &other);
  // </group>
  
  // Arithmetic operators: return Quantum<T>
  // <thrown>
  //   <li> AipsError if non-conforming units (+ and -)
  // </thrown>
  // See <linkto group="QMath#Quantum mathematical operations">QMath</linkto> class for unequal argument types
  // <group>
  Quantum<Qtype> operator+(const Quantum<Qtype> &other) const;
  Quantum<Qtype> operator-(const Quantum<Qtype> &other) const;
  Quantum<Qtype> operator*(const Quantum<Qtype> &other) const;
  Quantum<Qtype> operator/(const Quantum<Qtype> &other) const;
  // </group>
  
  //# General member functions
  // Get value of quantum in current units (i.e. in units specified in quantum)
  // <group>
  const Qtype &getValue() const;
  Qtype &getValue();
  // </group>
  // Get value in canonical base units
  Qtype getBaseValue() const;
  // Get value in specified units
  Qtype getValue(const Unit &other) const;
  // Get the unit (as Unit) that is attached to the Quantum. (use getUnit() if
  // interested in the String part only, e.g. for output)
  virtual const Unit &getFullUnit() const;
  
  // Re-specify parts of a quantum
  // <group name="set value">
  // Scale ( i.e. multiply) the value of the Quantum without changing units
  void scale(const Qtype &factor);
  // Set the value without changing units
  void setValue(const Qtype &val);
  // Set the value and unit deduced from input string
  // <note role=caution> At the moment the implementation can only convert
  // scalars to the appropiate Quantum. If format for Array input defined,
  // it could easily be changed. In addition recognition of date/time/angle
  // still has to be added </note>
  // <group>
  static Bool read(Quantity &res, const String &in);
  static Bool read(Quantity &res, MUString &in);
  // </group>
  // </group>
  
  // Check if of specified type
  Bool check(const UnitVal &uv) const;
  
  // Assert correct kind
  // <thrown>
  //   <li> AipsError if non-conforming unit dimensions
  // </thrown>
  void assure(const UnitVal &uv) const;
  
  // Return a Quantum converted to specified units
  // <group name="get">
  // Convert to canonical units
  Quantum<Qtype> get() const;
  // Convert to specified units; any remainder will be expressed in canonical
  // units. E.g. conversion of Jy/pc into W/ly2 will result in W/ly2.m-1.s .
  // <thrown>
  //   <li> AipsError if illegal unit
  // </thrown>
  Quantum<Qtype> get(const Unit &s) const;
  // Convert a Quantum to units from specified quantum (ibid example)
  Quantum<Qtype> get(const Quantum<Qtype> &other) const;
  // </group>
  
  // Convert a Quantum to specified units
  // <group>
  // Convert to canonical units
  void convert();
  // Convert to specified units; any remainder will be expressed in canonical
  // units. E.g. conversion of Jy/pc into W/ly2 will result in W/ly2.m-1.s .
  // <thrown>
  //   <li> AipsError if illegal unit
  // </thrown>
  void convert(const Unit &s);
  // Convert a Quantum to units from specified quantum (ibid example)
  void convert(const Quantum<Qtype> &other) ;
  // </group>
  // Get a copy of Quantum
  virtual QBase *clone() const;
  // Print a Quantum
  virtual void print(ostream &os) const;
  // Get the type (using QuantumType) of derived Quantum (faster than Strings)
  // <group>
  virtual uInt type() const;
  static uInt myType();
  // </group>
  
private:
  //# Data members
  // Actual quantum value
  Qtype qVal;

};

// Global functions
// <summary> Global input function </summary>
// Output/Input
// <group name=output>
// only Quantity is supported on input
istream& operator>> (istream &is, Quantity &ku);
Bool readQuantity(Quantity &res, MUString &in);
Bool readQuantity(Quantity &res, const String &in);
// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Quanta/Quantum.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
