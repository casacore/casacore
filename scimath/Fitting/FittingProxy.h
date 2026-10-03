// # DittingProxy.h: This class gives object access to Fitting
// # Copyright (C) 2006
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This program is free software; you can redistribute it and/or modify it
// # under the terms of the GNU General Public License as published by the Free
// # Software Foundation; either version 2 of the License, or (at your option)
// # any later version.
// #
// # This program is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// # more details.
// #
// # You should have received a copy of the GNU General Public License along
// # with this program; if not, write to the Free Software Foundation, Inc.,
// # 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#ifndef SCIMATH_FITTINGPROXY_H
#define SCIMATH_FITTINGPROXY_H

// # Includes

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/Containers/Record.h>

// # Forward declarations
namespace casacore {  // # NAMESPACE CASACORE - BEGIN
class String;
template <class T>
class GenericL2Fit;

// <summary> This class gives Proxy to Fitting connection</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=Fitting>Fitting</linkto>
// </prerequisite>

// <etymology>
// Distributed Object and fitting
// </etymology>

// <synopsis>
// The class makes the connection between the
// <linkto module=Fitting>Fitting</linkto> module and
// other object system. It provides a series of proxy callable
// methods. See Note 197 for details. <br>
// Operations supported
// are all the fitting methods supported in the Fitting module
// </synopsis>

// <example>
// </example>

// <motivation>
// To provide a direct user interface between the user and
// <linkto module=Fitting>Fitting</linkto> related calculations.
// </motivation>

// <todo asof="2004/08/30">
//  <li> Nothing I know of
// </todo>

class FittingProxy {
 public:
  // # Standard constructors/destructors
  FittingProxy();
  virtual ~FittingProxy();

  int getid();
  Record getstate(int id);
  bool init(int id, int n, int tp, double colfac, double lmfac);
  bool done(int id);
  bool reset(int id);
  bool set(int id, int nin, int tpin, double colfac, double lmfac);
  Record functional(int id, const Record& fnc, const Vector<double>& xval,
                    const Vector<double>& yval, const Vector<double>& wt, int mxit,
                    const Record& constraint);
  Record linear(int id, const Record& fnc, const Vector<double>& xval, const Vector<double>& yval,
                const Vector<double>& wt, const Record& constraint);
  Record cxfunctional(int id, const Record& fnc, const Vector<DComplex>& xval,
                      const Vector<DComplex>& yval, const Vector<DComplex>& wt, int mxit,
                      const Record& constraint);
  Record cxlinear(int id, const Record& fnc, const Vector<DComplex>& xval,
                  const Vector<DComplex>& yval, const Vector<DComplex>& wt,
                  const Record& constraint);

 private:
  // Class to aid in distributing different fitters
  class FitType {
   public:
    // # Constructors
    //  Default constructor: no method known
    FitType();
    // Destructor
    ~FitType();
    // # Method
    //  Set a fitter pointer (real or complex)
    //  <group>
    void setFitter(GenericL2Fit<double>* ptr);
    void setFitterCX(GenericL2Fit<DComplex>* ptr);
    // </group>
    // Get a fitter pointer (real or complex)
    // <group>
    GenericL2Fit<double>* const& getFitter() const;
    GenericL2Fit<DComplex>* const& getFitterCX() const;
    // </group>
    // Set the status
    void setStatus(int n, int typ, double colfac, double lmfac);
    // Get the number of terms in condition equation
    int getNceq() const { return nceq_p; };
    // Get the number of unknowns
    int getN() const { return n_p; };
    // Get the number of real unknowns
    int getNreal() const { return nreal_p; };
    // Get the type
    int getType() const { return typ_p; };
    // Get the collinearity factor
    double getColfac() const { return colfac_p; };
    // Get the Levenberg-Marquardt factor
    double getLMfac() const { return lmfac_p; };
    // Set solution done or not
    void setSolved(bool solved);
    // Solution done?
    bool getSolved() const { return soldone_p; };

   private:
    // Copy constructor: not implemented
    FitType(const FitType& other);
    // Assignment: not implemented
    FitType& operator=(const FitType& other);
    // # Data
    //  Pointer to a Fitting Machine: real or complex
    //  <group>
    casacore::GenericL2Fit<double>* fitter_p;
    casacore::GenericL2Fit<DComplex>* fitterCX_p;
    // </group>
    // Number of unknowns
    int n_p;
    // Number of terms in condition equation
    int nceq_p;
    // Number of real unknowns
    int nreal_p;
    // Type
    int typ_p;
    // Collinearity factor
    double colfac_p;
    // Levenberg-Marquardt factor
    double lmfac_p;
    // Solution done?
    bool soldone_p;
    // System's rank deficiency
    unsigned int nr_p;
  };
  // # Member functions
  // # Data
  //  Number of FitType obkects present
  unsigned int nFitter_p;
  // List of FitTypes
  FitType** list_p;
};

}  // namespace casacore

#endif
