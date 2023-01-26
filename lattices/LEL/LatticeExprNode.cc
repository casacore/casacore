//# LatticeExprNode.cc:  this defines LatticeExprNode.cc
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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


#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/LEL/LELLattice.h>
#include <casacore/lattices/LEL/LELConvert.h>
#include <casacore/lattices/LEL/LELBinary.h>
#include <casacore/lattices/LEL/LELUnary.h>
#include <casacore/lattices/LEL/LELCondition.h>
#include <casacore/lattices/LEL/LELFunction.h>
#include <casacore/lattices/LEL/LELSpectralIndex.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/LEL/LELRegion.h>
#include <casacore/lattices/LRegions/LCRegion.h>
#include <casacore/lattices/LRegions/LCSlicer.h>
#include <casacore/lattices/LRegions/LattRegionHolder.h>
#include <casacore/lattices/LEL/LELLattCoord.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h> 
#include <casacore/casa/iostream.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Default constructor
LatticeExprNode::LatticeExprNode()
: donePrepare_p   (false),
  isInvalid_p     (true),
  pAttr_p         (0)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode::default constructor; pExpr_p.nrefs() = "
	<< pExprDouble_p.nrefs() << endl;
#endif
}

// Destructor
LatticeExprNode::~LatticeExprNode()
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode::destructor; pExpr_p.nrefs() = "
	<< pExprDouble_p.nrefs() << endl;
#endif
}


LatticeExprNode::LatticeExprNode(const CountedPtr<LELInterface<float> >& pExpr)
: donePrepare_p   (false),
  dtype_p         (TpFloat),
  isInvalid_p     (true),
  pAttr_p         (&pExpr->getAttribute()),
  pExprFloat_p    (pExpr)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (CountedPtr<LELInterface<T>>&); pExpr_p.nrefs() = "
	<< pExprFloat_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode(const CountedPtr<LELInterface<double> >& pExpr)
: donePrepare_p   (false),
  dtype_p         (TpDouble),
  isInvalid_p     (true),
  pAttr_p         (&pExpr->getAttribute()),
  pExprDouble_p   (pExpr)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (CountedPtr<LELInterface<T>>&); pExpr_p.nrefs() = "
	<< pExprDouble_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode
                             (const CountedPtr<LELInterface<Complex> >& pExpr)
: donePrepare_p   (false),
  dtype_p         (TpComplex),
  isInvalid_p     (true),
  pAttr_p         (&pExpr->getAttribute()),
  pExprComplex_p  (pExpr)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (CountedPtr<LELInterface<T>>&); pExpr_p.nrefs() = "
	<< pExprComplex_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode
                             (const CountedPtr<LELInterface<DComplex> >& pExpr)
: donePrepare_p   (false),
  dtype_p         (TpDComplex),
  isInvalid_p     (true),
  pAttr_p         (&pExpr->getAttribute()),
  pExprDComplex_p (pExpr)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (CountedPtr<LELInterface<T>>&); pExpr_p.nrefs() = "
	<< pExprDComplex_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode(const CountedPtr<LELInterface<bool> >& pExpr)
: donePrepare_p   (false),
  dtype_p         (TpBool),
  isInvalid_p     (true),
  pAttr_p         (&pExpr->getAttribute()),
  pExprBool_p     (pExpr)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (CountedPtr<LELInterface<T>>&); pExpr_p.nrefs() = "
	<< pExprBool_p.nrefs() << endl;
#endif
}


LatticeExprNode::LatticeExprNode(LELInterface<float>* pExpr)
: donePrepare_p   (false),
  dtype_p         (TpFloat),
  isInvalid_p     (true),
  pAttr_p         (&pExpr->getAttribute()),
  pExprFloat_p    (pExpr)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (LELInterface<T>*); pExpr_p.nrefs() = "
	<< pExprFloat_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode(LELInterface<double>* pExpr)
: donePrepare_p   (false),
  dtype_p         (TpDouble),
  isInvalid_p     (true),
  pAttr_p         (&pExpr->getAttribute()),
  pExprDouble_p   (pExpr)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (LELInterface<T>*); pExpr_p.nrefs() = "
	<< pExprDouble_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode(LELInterface<Complex>* pExpr)
: donePrepare_p   (false),
  dtype_p         (TpComplex),
  isInvalid_p     (true),
  pAttr_p         (&pExpr->getAttribute()),
  pExprComplex_p  (pExpr)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (LELInterface<T>*); pExpr_p.nrefs() = "
	<< pExprComplex_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode(LELInterface<DComplex>* pExpr)
: donePrepare_p   (false),
  dtype_p         (TpDComplex),
  isInvalid_p     (true),
  pAttr_p         (&pExpr->getAttribute()),
  pExprDComplex_p (pExpr)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (LELInterface<T>*); pExpr_p.nrefs() = "
	<< pExprDComplex_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode(LELInterface<bool>* pExpr)
: donePrepare_p   (false),
  dtype_p         (TpBool),
  isInvalid_p     (true),
  pAttr_p         (&pExpr->getAttribute()),
  pExprBool_p     (pExpr)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (LELInterface<T>*); pExpr_p.nrefs() = "
	<< pExprBool_p.nrefs() << endl;
#endif
}


LatticeExprNode::LatticeExprNode (int64_t constant) 
: donePrepare_p   (false),
  dtype_p         (TpFloat),
  isInvalid_p     (false),
  pExprFloat_p    (new LELUnaryConst<float> (constant))
{ 
   pAttr_p = &pExprFloat_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: Unary constructor (T); pExpr_p.nrefs() = "
	<< pExprFloat_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (int32_t constant) 
: donePrepare_p   (false),
  dtype_p         (TpFloat),
  isInvalid_p     (false),
  pExprFloat_p    (new LELUnaryConst<float> (constant))
{ 
   pAttr_p = &pExprFloat_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: Unary constructor (T); pExpr_p.nrefs() = "
	<< pExprFloat_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (uint32_t constant) 
: donePrepare_p   (false),
  dtype_p         (TpFloat),
  isInvalid_p     (false),
  pExprFloat_p    (new LELUnaryConst<float> (constant))
{ 
   pAttr_p = &pExprFloat_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: Unary constructor (T); pExpr_p.nrefs() = "
	<< pExprFloat_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (long constant) 
: donePrepare_p   (false),
  dtype_p         (TpFloat),
  isInvalid_p     (false),
  pExprFloat_p    (new LELUnaryConst<float> (constant))
{ 
   pAttr_p = &pExprFloat_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: Unary constructor (T); pExpr_p.nrefs() = "
	<< pExprFloat_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (float constant) 
: donePrepare_p   (false),
  dtype_p         (TpFloat),
  isInvalid_p     (false),
  pExprFloat_p    (new LELUnaryConst<float> (constant))
{ 
   pAttr_p = &pExprFloat_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: Unary constructor (T); pExpr_p.nrefs() = "
	<< pExprFloat_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (double constant) 
: donePrepare_p   (false),
  dtype_p         (TpDouble),
  isInvalid_p     (false),
  pExprDouble_p   (new LELUnaryConst<double> (constant))
{ 
   pAttr_p = &pExprDouble_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: Unary constructor (T); pExpr_p.nrefs() = "
	<< pExprDouble_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const Complex& constant) 
: donePrepare_p   (false),
  dtype_p         (TpComplex),
  isInvalid_p     (false),
  pExprComplex_p  (new LELUnaryConst<Complex> (constant))
{ 
   pAttr_p = &pExprComplex_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: Unary constructor (T); pExpr_p.nrefs() = "
	<< pExprComplex_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const DComplex& constant) 
: donePrepare_p   (false),
  dtype_p         (TpDComplex),
  isInvalid_p     (false),
  pExprDComplex_p (new LELUnaryConst<DComplex> (constant))
{ 
   pAttr_p = &pExprDComplex_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: Unary constructor (T); pExpr_p.nrefs() = "
	<< pExprDComplex_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (bool constant) 
: donePrepare_p   (false),
  dtype_p         (TpBool),
  isInvalid_p     (false),
  pExprBool_p     (new LELUnaryConst<bool> (constant))
{ 
   pAttr_p = &pExprBool_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: Unary constructor (T); pExpr_p.nrefs() = "
	<< pExprBool_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const IPosition& iposition) 
: donePrepare_p   (false),
  dtype_p         (TpOther),
  isInvalid_p     (false),
  iposition_p     (iposition)
{ 
#if defined(AIPS_TRACE)
  cout << "LatticeExprNode:: IPosition constructor" << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const Lattice<float>& lattice) 
: donePrepare_p   (false),
  dtype_p         (TpFloat),
  isInvalid_p     (false),
  pExprFloat_p    (new LELLattice<float> (lattice))
{
   pAttr_p = &pExprFloat_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (const Lattice<T>&); pExpr_p.nrefs() = "
	<< pExprFloat_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const Lattice<double>& lattice) 
: donePrepare_p   (false),
  dtype_p         (TpDouble),
  isInvalid_p     (false),
  pExprDouble_p   (new LELLattice<double> (lattice))
{
   pAttr_p = &pExprDouble_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (const Lattice<T>&); pExpr_p.nrefs() = "
	<< pExprDouble_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const Lattice<Complex>& lattice) 
: donePrepare_p   (false),
  dtype_p         (TpComplex),
  isInvalid_p     (false),
  pExprComplex_p  (new LELLattice<Complex> (lattice))
{
   pAttr_p = &pExprComplex_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (const Lattice<T>&); pExpr_p.nrefs() = "
	<< pExprComplex_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const Lattice<DComplex>& lattice) 
: donePrepare_p   (false),
  dtype_p         (TpDComplex),
  isInvalid_p     (false),
  pExprDComplex_p (new LELLattice<DComplex> (lattice))
{
   pAttr_p = &pExprDComplex_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (const Lattice<T>&); pExpr_p.nrefs() = "
	<< pExprDComplex_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const Lattice<bool>& lattice) 
: donePrepare_p   (false),
  dtype_p         (TpBool),
  isInvalid_p     (false),
  pExprBool_p     (new LELLattice<bool> (lattice))
{
   pAttr_p = &pExprBool_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (Lattice<T>); pExpr_p.nrefs() = "
	<< pExprBool_p.nrefs() << endl;
#endif
}


LatticeExprNode::LatticeExprNode (const MaskedLattice<float>& lattice) 
: donePrepare_p   (false),
  dtype_p         (TpFloat),
  isInvalid_p     (false),
  pExprFloat_p    (new LELLattice<float> (lattice))
{
   pAttr_p = &pExprFloat_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (const MaskedLattice<T>&); pExpr_p.nrefs() = "
	<< pExprFloat_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const MaskedLattice<double>& lattice) 
: donePrepare_p   (false),
  dtype_p         (TpDouble),
  isInvalid_p     (false),
  pExprDouble_p   (new LELLattice<double> (lattice))
{
   pAttr_p = &pExprDouble_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (const MaskedLattice<T>&); pExpr_p.nrefs() = "
	<< pExprDouble_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const MaskedLattice<Complex>& lattice) 
: donePrepare_p   (false),
  dtype_p         (TpComplex),
  isInvalid_p     (false),
  pExprComplex_p  (new LELLattice<Complex> (lattice))
{
   pAttr_p = &pExprComplex_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (const MaskedLattice<T>&); pExpr_p.nrefs() = "
	<< pExprComplex_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const MaskedLattice<DComplex>& lattice) 
: donePrepare_p   (false),
  dtype_p         (TpDComplex),
  isInvalid_p     (false),
  pExprDComplex_p (new LELLattice<DComplex> (lattice))
{
   pAttr_p = &pExprDComplex_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (const MaskedLattice<T>&); pExpr_p.nrefs() = "
	<< pExprDComplex_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const MaskedLattice<bool>& lattice) 
: donePrepare_p   (false),
  dtype_p         (TpBool),
  isInvalid_p     (false),
  pExprBool_p     (new LELLattice<bool> (lattice))
{
   pAttr_p = &pExprBool_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (MaskedLattice<T>); pExpr_p.nrefs() = "
	<< pExprBool_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const LCRegion& region) 
: donePrepare_p   (false),
  dtype_p         (TpBool),
  isInvalid_p     (false),
  pExprBool_p     (new LELRegion (new LattRegionHolder(region)))
{
   pAttr_p = &pExprBool_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (MaskedLattice<T>); pExpr_p.nrefs() = "
	<< pExprBool_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const Slicer& slicer)
: donePrepare_p   (false),
  dtype_p         (TpBool),
  isInvalid_p     (false),
  pExprBool_p     (new LELRegion (new LattRegionHolder(LCSlicer(slicer))))
{
   pAttr_p = &pExprBool_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (MaskedLattice<T>); pExpr_p.nrefs() = "
	<< pExprBool_p.nrefs() << endl;
#endif
}

LatticeExprNode::LatticeExprNode (const LattRegionHolder& region) 
: donePrepare_p   (false),
  dtype_p         (TpBool),
  isInvalid_p     (false),
  pExprBool_p     (new LELRegion (region))
{
   pAttr_p = &pExprBool_p->getAttribute();

#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: constructor (MaskedLattice<T>); pExpr_p.nrefs() = "
	<< pExprBool_p.nrefs() << endl;
#endif
}


LatticeExprNode::LatticeExprNode (const LatticeExprNode& other)
: donePrepare_p   (other.donePrepare_p),
  dtype_p         (other.dtype_p),
  isInvalid_p     (other.isInvalid_p),
  pAttr_p         (other.pAttr_p),
  pExprFloat_p    (other.pExprFloat_p),
  pExprDouble_p   (other.pExprDouble_p),
  pExprComplex_p  (other.pExprComplex_p),
  pExprDComplex_p (other.pExprDComplex_p),
  pExprBool_p     (other.pExprBool_p)
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: copy constructor (LatticeExprNode); pExpr_p.nrefs() = "
	<< pExprDouble_p.nrefs() << endl;
#endif
}

// Assignment operator  
LatticeExprNode& LatticeExprNode::operator= (const LatticeExprNode& other)
{
   if (this != &other) {
      donePrepare_p   = other.donePrepare_p;
      dtype_p         = other.dtype_p;
      isInvalid_p     = other.isInvalid_p;
      pAttr_p         = other.pAttr_p;
      pExprFloat_p    = other.pExprFloat_p;
      pExprDouble_p   = other.pExprDouble_p;
      pExprComplex_p  = other.pExprComplex_p;
      pExprDComplex_p = other.pExprDComplex_p;
      pExprBool_p     = other.pExprBool_p;
   }
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: assignment operator (LatticeExprNode&); pExpr_p.nrefs() = " 
	<< pExprDouble_p.nrefs() << endl;
#endif
   return *this;
}


bool LatticeExprNode::lock (FileLocker::LockType type, uint32_t nattempts)
{
   switch (dataType()) {
   case TpFloat:
     return pExprFloat_p->lock (type, nattempts);
   case TpDouble:
     return pExprDouble_p->lock (type, nattempts);
   case TpComplex:
     return pExprComplex_p->lock (type, nattempts);
   case TpDComplex:
     return pExprDComplex_p->lock (type, nattempts);
   case TpBool:
     return pExprBool_p->lock (type, nattempts);
   default:
     throw (AipsError ("LatticeExprNode::lock - "
		       "unknown data type"));
   }
   return false;
}
void LatticeExprNode::unlock()
{
   switch (dataType()) {
   case TpFloat:
     pExprFloat_p->unlock();
     break;
   case TpDouble:
     pExprDouble_p->unlock();
     break;
   case TpComplex:
     pExprComplex_p->unlock();
     break;
   case TpDComplex:
     pExprDComplex_p->unlock();
     break;
   case TpBool:
     pExprBool_p->unlock();
     break;
   default:
     throw (AipsError ("LatticeExprNode::unlock - "
		       "unknown data type"));
   }
}
bool LatticeExprNode::hasLock (FileLocker::LockType type) const
{
   switch (dataType()) {
   case TpFloat:
     return pExprFloat_p->hasLock (type);
   case TpDouble:
     return pExprDouble_p->hasLock (type);
   case TpComplex:
     return pExprComplex_p->hasLock (type);
   case TpDComplex:
     return pExprDComplex_p->hasLock (type);
   case TpBool:
     return pExprBool_p->hasLock (type);
   default:
      throw (AipsError ("LatticeExprNode::hasLock - "
			"unknown data type"));
   }
   return false;
}
void LatticeExprNode::resync()
{
   switch (dataType()) {
   case TpFloat:
     pExprFloat_p->resync();
     break;
   case TpDouble:
     pExprDouble_p->resync();
     break;
   case TpComplex:
     pExprComplex_p->resync();
     break;
   case TpDComplex:
     pExprDComplex_p->resync();
     break;
   case TpBool:
     pExprBool_p->resync();
     break;
   default:
     throw (AipsError ("LatticeExprNode::resync - "
		       "unknown data type"));
   }
}


bool LatticeExprNode::replaceScalarExpr()
// 
// If the current expression evaluates to a scalar, then it can 
// be optimized in the tree by replacement by a scalar constant 
// expression such as LELUnaryConst
//
{
   switch (dataType()) {
   case TpFloat:
      isInvalid_p = LELInterface<float>::replaceScalarExpr (pExprFloat_p);
      pAttr_p = &pExprFloat_p->getAttribute();
      break;
   case TpDouble:
      isInvalid_p = LELInterface<double>::replaceScalarExpr (pExprDouble_p);
      pAttr_p = &pExprDouble_p->getAttribute();
      break;
   case TpComplex:
      isInvalid_p = LELInterface<Complex>::replaceScalarExpr (pExprComplex_p);
      pAttr_p = &pExprComplex_p->getAttribute();
      break;
   case TpDComplex:
      isInvalid_p = LELInterface<DComplex>::replaceScalarExpr (pExprDComplex_p);
      pAttr_p = &pExprDComplex_p->getAttribute();
      break;
   case TpBool:
      isInvalid_p = LELInterface<bool>::replaceScalarExpr (pExprBool_p);
      pAttr_p = &pExprBool_p->getAttribute();
      break;
   default:
      throw (AipsError ("LatticeExprNode::replaceScalarExpr - "
			"unknown data type"));
   }
   return isInvalid_p;
}

void LatticeExprNode::doPrepare() const
{
   if (!donePrepare_p) {
      LatticeExprNode* This = (LatticeExprNode*)this;
      This->replaceScalarExpr();
      This->donePrepare_p = true;
   }
}

void LatticeExprNode::eval (LELArray<float>& result,
			    const Slicer& section) const
{
// If first time, try to do optimization.
   DebugAssert (dataType() == TpFloat, AipsError);
   if (!donePrepare_p) {
      doPrepare();
   }
// If scalar, remove mask if scalar is valid. Otherwise set false mask.
// If array, evaluate for this section.
   if (isScalar()) {
      LELScalar<float> value = pExprFloat_p->getScalar();
      if (value.mask()) {
	 result.value() = value.value();
	 result.removeMask();
      } else {
	 result.value() = 0;
	 Array<bool> mask (result.shape());
	 mask = false;
	 result.setMask (mask);
      }
   } else {
      pExprFloat_p->eval (result, section);
   }
}

void LatticeExprNode::eval (LELArray<double>& result,
			    const Slicer& section) const
{
// If first time, try to do optimization.
   DebugAssert (dataType() == TpDouble, AipsError);
   if (!donePrepare_p) {
      LatticeExprNode* This = (LatticeExprNode*)this;
      This->replaceScalarExpr();
      This->donePrepare_p = true;
   }
// If scalar, remove mask if scalar is valid. Otherwise set false mask.
// If array, evaluate for this section.
   if (isScalar()) {
      LELScalar<double> value = pExprDouble_p->getScalar();
      if (value.mask()) {
	 result.value() = value.value();
	 result.removeMask();
      } else {
	 result.value() = 0;
	 Array<bool> mask (result.shape());
	 mask = false;
	 result.setMask (mask);
      }
   } else {
      pExprDouble_p->eval (result, section);
   }
}

void LatticeExprNode::eval (LELArray<Complex>& result,
			    const Slicer& section) const
{
// If first time, try to do optimization.
   DebugAssert (dataType() == TpComplex, AipsError);
   if (!donePrepare_p) {
      LatticeExprNode* This = (LatticeExprNode*)this;
      This->replaceScalarExpr();
      This->donePrepare_p = true;
   }
// If scalar, remove mask if scalar is valid. Otherwise set false mask.
// If array, evaluate for this section.
   if (isScalar()) {
      LELScalar<Complex> value = pExprComplex_p->getScalar();
      if (value.mask()) {
	 result.value() = value.value();
	 result.removeMask();
      } else {
	 result.value() = 0;
	 Array<bool> mask (result.shape());
	 mask = false;
	 result.setMask (mask);
      }
   } else {
      pExprComplex_p->eval (result, section);
   }
}

void LatticeExprNode::eval (LELArray<DComplex>& result,
			    const Slicer& section) const
{
// If first time, try to do optimization.
   DebugAssert (dataType() == TpDComplex, AipsError);
   if (!donePrepare_p) {
      LatticeExprNode* This = (LatticeExprNode*)this;
      This->replaceScalarExpr();
      This->donePrepare_p = true;
   }
// If scalar, remove mask if scalar is valid. Otherwise set false mask.
// If array, evaluate for this section.
   if (isScalar()) {
      LELScalar<DComplex> value = pExprDComplex_p->getScalar();
      if (value.mask()) {
	 result.value() = value.value();
	 result.removeMask();
      } else {
	 result.value() = 0;
	 Array<bool> mask (result.shape());
	 mask = false;
	 result.setMask (mask);
      }
   } else {
      pExprDComplex_p->eval (result, section);
   }
}

void LatticeExprNode::eval (LELArray<bool>& result,
			    const Slicer& section) const
{
// If first time, try to do optimization.
   DebugAssert (dataType() == TpBool, AipsError);
   if (!donePrepare_p) {
      LatticeExprNode* This = (LatticeExprNode*)this;
      This->replaceScalarExpr();
      This->donePrepare_p = true;
   }
// If scalar, remove mask if scalar is valid. Otherwise set false mask.
// If array, evaluate for this section.
   if (isScalar()) {
      LELScalar<bool> value = pExprBool_p->getScalar();
      if (value.mask()) {
	 result.value() = value.value();
	 result.removeMask();
      } else {
	 result.value() = false;
	 Array<bool> mask (result.shape());
	 mask = false;
	 result.setMask (mask);
      }
   } else {
      pExprBool_p->eval (result, section);
   }
}


void LatticeExprNode::eval (float& result) const
{
   DebugAssert (dataType() == TpFloat, AipsError);
   result = pExprFloat_p->getScalar().value();
}

void LatticeExprNode::eval (double& result) const
{
   DebugAssert (dataType() == TpDouble, AipsError);
   result = pExprDouble_p->getScalar().value();
}

void LatticeExprNode::eval (Complex& result) const
{
   DebugAssert (dataType() == TpComplex, AipsError);
   result = pExprComplex_p->getScalar().value();
}

void LatticeExprNode::eval (DComplex& result) const
{
   DebugAssert (dataType() == TpDComplex, AipsError);
   result = pExprDComplex_p->getScalar().value();
}

void LatticeExprNode::eval (bool& result) const
{
   DebugAssert (dataType() == TpBool, AipsError);
   result = pExprBool_p->getScalar().value();
}

float LatticeExprNode::getFloat() const
{
   DebugAssert (dataType() == TpFloat, AipsError);
   return pExprFloat_p->getScalar().value();
}

double LatticeExprNode::getDouble() const
{
   DebugAssert (dataType() == TpDouble, AipsError);
   return pExprDouble_p->getScalar().value();
}

Complex LatticeExprNode::getComplex() const
{
   DebugAssert (dataType() == TpComplex, AipsError);
   return pExprComplex_p->getScalar().value();
}

DComplex LatticeExprNode::getDComplex() const
{
   DebugAssert (dataType() == TpDComplex, AipsError);
   return pExprDComplex_p->getScalar().value();
}

bool LatticeExprNode::getBool() const
{
   DebugAssert (dataType() == TpBool, AipsError);
   return pExprBool_p->getScalar().value();
}

Array<float> LatticeExprNode::getArrayFloat() const
{
   DebugAssert (dataType() == TpFloat, AipsError);
   return pExprFloat_p->getArray().value();
}

Array<double> LatticeExprNode::getArrayDouble() const
{
   DebugAssert (dataType() == TpDouble, AipsError);
   return pExprDouble_p->getArray().value();
}

Array<Complex> LatticeExprNode::getArrayComplex() const
{
   DebugAssert (dataType() == TpComplex, AipsError);
   return pExprComplex_p->getArray().value();
}

Array<DComplex> LatticeExprNode::getArrayDComplex() const
{
   DebugAssert (dataType() == TpDComplex, AipsError);
   return pExprDComplex_p->getArray().value();
}

Array<bool> LatticeExprNode::getArrayBool() const
{
   DebugAssert (dataType() == TpBool, AipsError);
   return pExprBool_p->getArray().value();
}



LatticeExprNode operator+(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: unary operator +" << endl;
#endif
   AlwaysAssert (expr.dataType() != TpBool, AipsError);
   return expr;
}

LatticeExprNode operator-(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: Unary operator -" << endl;
#endif
   AlwaysAssert (expr.dataType() != TpBool, AipsError);
   return LatticeExprNode::newNumUnary (LELUnaryEnums::MINUS, expr);
}



LatticeExprNode toFloat(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function float" << endl;
#endif
   return expr.makeFloat();
}

LatticeExprNode toDouble(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function double" << endl;
#endif
   return expr.makeDouble();
}

LatticeExprNode toComplex(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function complex" << endl;
#endif
   return expr.makeComplex();
}

LatticeExprNode toDComplex(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function dcomplex" << endl;
#endif
   return expr.makeDComplex();
}

LatticeExprNode toBool(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function bool" << endl;
#endif
   return expr.makeBool();
}


LatticeExprNode sin(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function sin" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::SIN, expr);
}

LatticeExprNode sinh(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function sinh" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::SINH, expr);
}

LatticeExprNode asin(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function asin" << endl;
#endif
   return LatticeExprNode::newRealFunc1D (LELFunctionEnums::ASIN, expr);
}

LatticeExprNode cos(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function cos" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::COS, expr);
}

LatticeExprNode cosh(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function cosh" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::COSH, expr);
}

LatticeExprNode acos(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function acos" << endl;
#endif
   return LatticeExprNode::newRealFunc1D (LELFunctionEnums::ACOS, expr);
}

LatticeExprNode tan(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function tan" << endl;
#endif
   return LatticeExprNode::newRealFunc1D (LELFunctionEnums::TAN, expr);
}

LatticeExprNode tanh(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function tanh" << endl;
#endif
   return LatticeExprNode::newRealFunc1D (LELFunctionEnums::TANH, expr);
}

LatticeExprNode atan(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function atan" << endl;
#endif
   return LatticeExprNode::newRealFunc1D (LELFunctionEnums::ATAN, expr);
}

LatticeExprNode exp(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function exp" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::EXP, expr);
}

LatticeExprNode log(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function log" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::LOG, expr);
}

LatticeExprNode log10(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function log10" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::LOG10, expr);
}

LatticeExprNode sqrt(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function sqrt" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::SQRT, expr);
}

LatticeExprNode round(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function round" << endl;
#endif
   return LatticeExprNode::newRealFunc1D (LELFunctionEnums::ROUND, expr);
}

LatticeExprNode sign(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function sign" << endl;
#endif
   AlwaysAssert (expr.dataType()==TpFloat || expr.dataType()==TpDouble,
		 AipsError);
   Block<LatticeExprNode> arg(1);
   arg[0] = expr.makeFloat();
   LELFunctionFloat* ptr = new LELFunctionFloat(LELFunctionEnums::SIGN, arg);
   return ptr;
}

LatticeExprNode ceil(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function ceil" << endl;
#endif
   return LatticeExprNode::newRealFunc1D (LELFunctionEnums::CEIL, expr);
}

LatticeExprNode floor(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function floor" << endl;
#endif
   return LatticeExprNode::newRealFunc1D (LELFunctionEnums::FLOOR, expr);
}

LatticeExprNode min(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function min" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::MIN1D, expr);
}

LatticeExprNode max(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function max" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::MAX1D, expr);
}

LatticeExprNode abs(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function abs" << endl;
#endif
   return LatticeExprNode::newNumReal1D (LELFunctionEnums::ABS, expr);
}

LatticeExprNode arg(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function arg" << endl;
#endif
   AlwaysAssert (expr.dataType()==TpComplex || expr.dataType()==TpDComplex,
		 AipsError);
   return LatticeExprNode::newNumReal1D (LELFunctionEnums::ARG, expr);
}

LatticeExprNode real(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function real" << endl;
#endif
   return LatticeExprNode::newNumReal1D (LELFunctionEnums::REAL, expr);
}

LatticeExprNode imag(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function imag" << endl;
#endif
   AlwaysAssert (expr.dataType()==TpComplex || expr.dataType()==TpDComplex,
		 AipsError);
   return LatticeExprNode::newNumReal1D (LELFunctionEnums::IMAG, expr);
}

LatticeExprNode conj(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function conj" << endl;
#endif
   return LatticeExprNode::newComplexFunc1D (LELFunctionEnums::CONJ, expr);
}

LatticeExprNode formComplex(const LatticeExprNode& left,
			    const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function formComplex" << endl;
#endif
   AlwaysAssert ((left.dataType()==TpFloat || left.dataType()==TpDouble)
              && (right.dataType()==TpFloat || right.dataType()==TpDouble),
		 AipsError);
   Block<LatticeExprNode> arg(2);
   if (left.dataType()==TpFloat && right.dataType()==TpFloat) {
      arg[0] = left.makeFloat();
      arg[1] = right.makeFloat();
      return new LELFunctionComplex (LELFunctionEnums::COMPLEX, arg);
   }
   arg[0] = left.makeDouble();
   arg[1] = right.makeDouble();
   LELFunctionDComplex* ptr = new LELFunctionDComplex
                                           (LELFunctionEnums::COMPLEX, arg);
   return ptr;
}


LatticeExprNode sum(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function sum" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::SUM, expr);
}

LatticeExprNode median(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function median" << endl;
#endif
   return LatticeExprNode::newRealFunc1D (LELFunctionEnums::MEDIAN1D, expr);
}

LatticeExprNode mean(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function mean" << endl;
#endif
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::MEAN1D, expr);
}

LatticeExprNode variance(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function variance" << endl;
#endif
   // Use high enough precision for float and Complex.
   if (expr.dataType() == TpFloat) {
      return toFloat(sum(pow(expr - toDouble(mean(expr)), 2)) /
		     max(1, nelements(expr)-1));
   } else if (expr.dataType() == TpComplex) {
      return toComplex(sum(pow(expr - toDComplex(mean(expr)), 2)) /
		       max(1, nelements(expr)-1));
   }
   return sum(pow(expr - mean(expr), 2)) / max(1, nelements(expr)-1);
}

LatticeExprNode stddev(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function variance" << endl;
#endif
   return sqrt(variance(expr));
}

LatticeExprNode avdev(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function variance" << endl;
#endif
   // Use high enough precision for float and Complex.
   if (expr.dataType() == TpFloat) {
      return toFloat(sum(abs(expr - toDouble(mean(expr))))
		     / max(1, nelements(expr)));
   } else if (expr.dataType() == TpComplex) {
      return toComplex(sum(abs(expr - toDComplex(mean(expr))))
		       / max(1, nelements(expr)));
   }
   return sum(abs(expr - mean(expr))) / max(1, nelements(expr));
}

LatticeExprNode fractile (const LatticeExprNode& expr,
			  const LatticeExprNode& fraction)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function fractile" << endl;
#endif
//
// Create a new node for this real numerical function with 2 arguments.
// The result has the same data type as the input.
// 
   DataType dtype = expr.dataType();
   Block<LatticeExprNode> arg(2);
   arg[0] = expr;
   arg[1] = fraction.makeFloat();
   switch (dtype) {
   case TpFloat:
      return new LELFunctionFloat (LELFunctionEnums::FRACTILE1D, arg);
   case TpDouble:
      return new LELFunctionDouble (LELFunctionEnums::FRACTILE1D, arg);
   default:
      throw (AipsError ("LatticeExprNode::fractile - "
			"Bool or complex argument used in real "
			"numerical function"));
   }
   return LatticeExprNode();
}

LatticeExprNode fractileRange (const LatticeExprNode& expr,
			       const LatticeExprNode& fraction)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function fractileRange" << endl;
#endif
//
// Create a new node for this real numerical function with 2 arguments.
// The result has the same data type as the input.
// 
   DataType dtype = expr.dataType();
   Block<LatticeExprNode> arg(2);
   arg[0] = expr;
   arg[1] = fraction.makeFloat();
   switch (dtype) {
   case TpFloat:
       return new LELFunctionFloat (LELFunctionEnums::FRACTILERANGE1D, arg);
   case TpDouble:
       return new LELFunctionDouble (LELFunctionEnums::FRACTILERANGE1D, arg);
   default:
      throw (AipsError ("LatticeExprNode::fractileRange - "
			"Bool or complex argument used in real "
			"numerical function"));
   }
   return LatticeExprNode();
}

LatticeExprNode fractileRange (const LatticeExprNode& expr,
			       const LatticeExprNode& fraction1,
			       const LatticeExprNode& fraction2)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 3d function fractileRange" << endl;
#endif
//
// Create a new node for this real numerical function with 2 arguments.
// The result has the same data type as the input.
// 
   DataType dtype = expr.dataType();
   Block<LatticeExprNode> arg(3);
   arg[0] = expr;
   arg[1] = fraction1.makeFloat();
   arg[2] = fraction2.makeFloat();
   switch (dtype) {
   case TpFloat:
       return new LELFunctionFloat (LELFunctionEnums::FRACTILERANGE1D, arg);
   case TpDouble:
       return new LELFunctionDouble (LELFunctionEnums::FRACTILERANGE1D, arg);
   default:
      throw (AipsError ("LatticeExprNode::fractileRange - "
			"Bool or complex argument used in real "
			"numerical function"));
   }
   return LatticeExprNode();
}


LatticeExprNode rebin (const LatticeExprNode& lat,
                       const LatticeExprNode& bin)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function rebin" << endl;
#endif

   const IPosition& binning = bin.getIPosition();
   const LELLattCoordBase* cbptr =
                            &(lat.getAttribute().coordinates().coordinates());
   const LELLattCoord* cptr = dynamic_cast<const LELLattCoord*>(cbptr);
   AlwaysAssert (cptr != 0, AipsError);
   return cptr->makeRebinLattice (lat, binning);
}



LatticeExprNode atan2 (const LatticeExprNode& left,
		       const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function atan2" << endl;
#endif
   return LatticeExprNode::newNumFunc2D (LELFunctionEnums::ATAN2, left, right);
}

LatticeExprNode pow (const LatticeExprNode& left,
                     const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function pow" << endl;
#endif
   return LatticeExprNode::newNumFunc2D (LELFunctionEnums::POW, left, right);
}

LatticeExprNode fmod (const LatticeExprNode& left,
                      const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function fmod" << endl;
#endif
   return LatticeExprNode::newNumFunc2D (LELFunctionEnums::FMOD, left, right);
}

LatticeExprNode min (const LatticeExprNode& left,
                     const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function min" << endl;
#endif
   return LatticeExprNode::newNumFunc2D (LELFunctionEnums::MIN, left, right);
}

LatticeExprNode max (const LatticeExprNode& left,
                     const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function max" << endl;
#endif
   return LatticeExprNode::newNumFunc2D (LELFunctionEnums::MAX, left, right);
}

LatticeExprNode amp (const LatticeExprNode& left,
                     const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function amp" << endl;
#endif
   AlwaysAssert (left.dataType()!=TpBool && right.dataType()!=TpBool,
		 AipsError);
   return sqrt(pow(left,2) + pow(right,2));
}

LatticeExprNode pa (const LatticeExprNode& left,
                    const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function pa" << endl;
#endif

   AlwaysAssert (left.dataType()!=TpComplex && left.dataType()!=TpDComplex 
		 && left.dataType()!=TpBool, AipsError);
   AlwaysAssert (right.dataType()!=TpComplex && right.dataType()!=TpDComplex 
		 && right.dataType()!=TpBool, AipsError);
   LatticeExprNode expr(atan2(left,right));
   switch (expr.dataType()) {
   case TpFloat:
      return float(90.0/C::pi) * expr;
      break;
   case TpDouble:
      return double(90.0/C::pi) * expr;
      break;
   default:
      throw (AipsError ("LatticeExprNode::pa - Unknown data type"));
   }
   return LatticeExprNode();         // shut compiler up
}

LatticeExprNode spectralindex (const LatticeExprNode& left,
			       const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function spectralindex" << endl;
#endif
   DataType dtype = LatticeExprNode::resultDataType (left.dataType(),
						     right.dataType());
   Block<LatticeExprNode> arg(2);
   switch (dtype) {
   case TpFloat:
      arg[0] = left.makeFloat();
      arg[1] = right.makeFloat();
      return new LELSpectralIndex<float> (arg);
   case TpDouble:
      arg[0] = left.makeDouble();
      arg[1] = right.makeDouble();
      return new LELSpectralIndex<double> (arg);
   default:
      throw (AipsError ("LatticeExprNode::spectralindex - "
			"Bool or Complex argument used in function"));
   }
   return LatticeExprNode();
}


LatticeExprNode operator+ (const LatticeExprNode& left,
                           const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator +" << endl;
#endif
   return LatticeExprNode::newNumBinary (LELBinaryEnums::ADD, left, right);
}

LatticeExprNode operator- (const LatticeExprNode& left,
                           const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode::binary operator -" << endl;
#endif
   if (left.isRegion()  &&  right.isRegion()) {
      return LELRegion::makeDifference (*left.pExprBool_p, *right.pExprBool_p);
   }
   return LatticeExprNode::newNumBinary (LELBinaryEnums::SUBTRACT, left, right);
}

LatticeExprNode operator* (const LatticeExprNode& left,
                           const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator *" << endl;
#endif
   return LatticeExprNode::newNumBinary (LELBinaryEnums::MULTIPLY, left, right);
}

LatticeExprNode operator/ (const LatticeExprNode& left,
                           const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator /" << endl;
#endif
   return LatticeExprNode::newNumBinary (LELBinaryEnums::DIVIDE, left, right);
}


LatticeExprNode operator== (const LatticeExprNode& left,
			    const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator ==" << endl;
#endif
   return LatticeExprNode::newBinaryCmp (LELBinaryEnums::EQ, left, right);
}

LatticeExprNode operator> (const LatticeExprNode& left,
			   const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator >" << endl;
#endif
   return LatticeExprNode::newBinaryCmp (LELBinaryEnums::GT, left, right);
}

LatticeExprNode operator>= (const LatticeExprNode& left,
			    const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator >=" << endl;
#endif
   return LatticeExprNode::newBinaryCmp (LELBinaryEnums::GE, left, right);
}

LatticeExprNode operator< (const LatticeExprNode& left,
			   const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator <" << endl;
#endif
// Note we use GT for LT by reversing the order of the arguments
// requiring less code in LELBinaryCmp
   return LatticeExprNode::newBinaryCmp (LELBinaryEnums::GT, right, left);
}

LatticeExprNode operator<= (const LatticeExprNode& left,
			    const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator <=" << endl;
#endif
// Note we use GE for LE by reversing the order of the arguments
// requiring less code in LELBinaryCmp
   return LatticeExprNode::newBinaryCmp (LELBinaryEnums::GE, right, left);
}

LatticeExprNode operator!= (const LatticeExprNode& left,
			    const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator !=" << endl;
#endif
   return LatticeExprNode::newBinaryCmp (LELBinaryEnums::NE, left, right);
}



LatticeExprNode operator&& (const LatticeExprNode& left,
			    const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator &&" << endl;
#endif
   AlwaysAssert (left.dataType() == TpBool  &&  right.dataType() == TpBool,
		 AipsError);
   if (LatticeExprNode::areRegions (left, right)) {
      return LELRegion::makeIntersection (*left.pExprBool_p,
					  *right.pExprBool_p);
   }
   return LatticeExprNode::newLogBinary (LELBinaryEnums::AND, left, right);
}

LatticeExprNode operator|| (const LatticeExprNode& left,
			    const LatticeExprNode& right)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: binary operator ||" << endl;
#endif
   AlwaysAssert (left.dataType() == TpBool  &&  right.dataType() == TpBool,
		 AipsError);
   if (LatticeExprNode::areRegions (left, right)) {
      return LELRegion::makeUnion (*left.pExprBool_p, *right.pExprBool_p);
   }
   return LatticeExprNode::newLogBinary (LELBinaryEnums::OR, left, right);
}

LatticeExprNode operator! (const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: unary operator !" << endl;
#endif
   AlwaysAssert (expr.dataType() == TpBool, AipsError);
   if (expr.isRegion()) {
      return LELRegion::makeComplement (*expr.pExprBool_p);
   }
   return new LELUnaryBool(LELUnaryEnums::NOT, expr.pExprBool_p);
}

LatticeExprNode LatticeExprNode::operator[] (const LatticeExprNode& cond) const
{
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: operator()" << endl;
#endif
   AlwaysAssert (cond.dataType() == TpBool, AipsError);
   // The condition can be a region or a true boolean expression.
   // If a region, create a SubLattice/Image.
   if (cond.isRegion()) {
      // Cast the condition to a LELRegion object.
      // Thereafter let the coordinates class create a SubLattice/SubImage
      // for that region. It results in an exception if a WCRegion is
      // used without ImageCoordinates.
      const LELRegion& region = (const LELRegion&)(*cond.pExprBool_p);
      AlwaysAssert (!isRegion(), AipsError);
      const LELLattCoordBase* cbptr =
                            &(getAttribute().coordinates().coordinates());
      const LELLattCoord* cptr = dynamic_cast<const LELLattCoord*>(cbptr);
      AlwaysAssert (cptr != 0, AipsError);
      return cptr->makeSubLattice (*this, region.region());
   }
   switch (dataType()) {
   case TpBool:
      AlwaysAssert (!isRegion(), AipsError);
      return new LELCondition<bool> (pExprBool_p, cond.pExprBool_p);
   case TpFloat:
      return new LELCondition<float> (pExprFloat_p, cond.pExprBool_p);
   case TpDouble:
      return new LELCondition<double> (pExprDouble_p, cond.pExprBool_p);
   case TpComplex:
      return new LELCondition<Complex> (pExprComplex_p, cond.pExprBool_p);
   case TpDComplex:
      return new LELCondition<DComplex> (pExprDComplex_p, cond.pExprBool_p);
   default:
       throw (AipsError ("LatticeExprNode::operator[] - unknown datatype"));
   }
   return 0;
}

LatticeExprNode indexin (const LatticeExprNode& axis,
			 const LatticeExprNode& indexFlags)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function indexin" << endl;
#endif
   Block<LatticeExprNode> arg(2);
   arg[0] = axis;
   arg[1] = indexFlags;
   LELFunctionBool* ptr = new LELFunctionBool(LELFunctionEnums::INDEXIN, arg);
   return ptr;
}


LatticeExprNode all (const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function all" << endl;
#endif
   Block<LatticeExprNode> arg(1, toBool(expr));
   LELFunctionBool* ptr = new LELFunctionBool(LELFunctionEnums::ALL, arg);
   return ptr;
}

LatticeExprNode any (const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function any" << endl;
#endif
   Block<LatticeExprNode> arg(1, toBool(expr));
   LELFunctionBool* ptr = new LELFunctionBool(LELFunctionEnums::ANY, arg);
   return ptr;
}

LatticeExprNode ntrue (const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function ntrue" << endl;
#endif
   Block<LatticeExprNode> arg(1, toBool(expr));
   LELFunctionDouble* ptr = new LELFunctionDouble
                                           (LELFunctionEnums::NTRUE, arg);
   return ptr;
}

LatticeExprNode nfalse (const LatticeExprNode& expr)
{  
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function nfalse" << endl;
#endif
   Block<LatticeExprNode> arg(1, toBool(expr));
   LELFunctionDouble* ptr = new LELFunctionDouble
                                           (LELFunctionEnums::NFALSE, arg);
   return ptr;
}

LatticeExprNode nelements(const LatticeExprNode& expr)
{ 
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function nelements" << endl;
#endif
   Block<LatticeExprNode> arg(1, expr);
   if (expr.isRegion()) {
      arg[0] = toBool (expr);
   }
   LELFunctionDouble* ptr = new LELFunctionDouble
                                           (LELFunctionEnums::NELEM, arg);
   return ptr;
}

LatticeExprNode ndim (const LatticeExprNode& expr)
{  
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function ndim" << endl;
#endif
   Block<LatticeExprNode> arg(1, expr);
   LELFunctionFloat* ptr = new LELFunctionFloat(LELFunctionEnums::NDIM, arg);
   return ptr;
}

LatticeExprNode length (const LatticeExprNode& expr,
			const LatticeExprNode& axis)
{  
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 2d function length" << endl;
#endif
   Block<LatticeExprNode> arg(2);
   arg[0] = expr;
   arg[1] = axis;
   LELFunctionFloat* ptr = new LELFunctionFloat(LELFunctionEnums::LENGTH, arg);
   return ptr;
}

LatticeExprNode isNaN (const LatticeExprNode& expr)
{  
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function isNaN" << endl;
#endif
   Block<LatticeExprNode> arg(1, expr);
   LELFunctionBool* ptr = new LELFunctionBool(LELFunctionEnums::ISNAN, arg);
   return ptr;
}

LatticeExprNode mask (const LatticeExprNode& expr)
{  
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function mask" << endl;
#endif
   Block<LatticeExprNode> arg(1, expr);
   if (expr.isRegion()) {
      arg[0] = toBool (expr);
   }
   LELFunctionBool* ptr = new LELFunctionBool(LELFunctionEnums::MASK, arg);
   return ptr;
}

LatticeExprNode value (const LatticeExprNode& expr)
{  
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: 1d function value" << endl;
#endif
   if (expr.dataType() == TpBool) {
      Block<LatticeExprNode> arg(1, toBool(expr));
      return new LELFunctionBool (LELFunctionEnums::VALUE, arg);
   }
   return LatticeExprNode::newNumFunc1D (LELFunctionEnums::VALUE, expr);
}

LatticeExprNode iif (const LatticeExprNode& condition,
		     const LatticeExprNode& arg1,
		     const LatticeExprNode& arg2)
{  
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: function iif" << endl;
#endif
   AlwaysAssert (condition.dataType() == TpBool, AipsError);
   DataType dtype = LatticeExprNode::resultDataType (arg1.dataType(),
						     arg2.dataType());
   Block<LatticeExprNode> arg(3);
   arg[0] = condition.makeBool();
   switch (dtype) {
   case TpFloat:
       arg[1] = arg1.makeFloat();
       arg[2] = arg2.makeFloat();
       return new LELFunctionND<float>(LELFunctionEnums::IIF, arg);
   case TpDouble:
       arg[1] = arg1.makeDouble();
       arg[2] = arg2.makeDouble();
       return new LELFunctionND<double>(LELFunctionEnums::IIF, arg);
   case TpComplex:
       arg[1] = arg1.makeComplex();
       arg[2] = arg2.makeComplex();
       return new LELFunctionND<Complex>(LELFunctionEnums::IIF, arg);
   case TpDComplex:
       arg[1] = arg1.makeDComplex();
       arg[2] = arg2.makeDComplex();
       return new LELFunctionND<DComplex>(LELFunctionEnums::IIF, arg);
   case TpBool:
       arg[1] = arg1.makeBool();
       arg[2] = arg2.makeBool();
       return new LELFunctionND<bool>(LELFunctionEnums::IIF, arg);
   default:
      throw (AipsError ("LatticeExprNode::iif - unknown data type"));
   }
   return LatticeExprNode();
}

LatticeExprNode replace (const LatticeExprNode& arg1,
			 const LatticeExprNode& arg2)
{  
#if defined(AIPS_TRACE)
   cout << "LatticeExprNode:: function replace" << endl;
#endif
   DataType dtype = LatticeExprNode::resultDataType (arg1.dataType(),
						     arg2.dataType());
   Block<LatticeExprNode> arg(2);
   switch (dtype) {
   case TpFloat:
       arg[0] = arg1.makeFloat();
       arg[1] = arg2.makeFloat();
       return new LELFunctionND<float>(LELFunctionEnums::REPLACE, arg);
   case TpDouble:
       arg[0] = arg1.makeDouble();
       arg[1] = arg2.makeDouble();
       return new LELFunctionND<double>(LELFunctionEnums::REPLACE, arg);
   case TpComplex:
       arg[0] = arg1.makeComplex();
       arg[1] = arg2.makeComplex();
       return new LELFunctionND<Complex>(LELFunctionEnums::REPLACE, arg);
   case TpDComplex:
       arg[0] = arg1.makeDComplex();
       arg[1] = arg2.makeDComplex();
       return new LELFunctionND<DComplex>(LELFunctionEnums::REPLACE, arg);
   case TpBool:
       arg[0] = arg1.makeBool();
       arg[1] = arg2.makeBool();
       return new LELFunctionND<bool>(LELFunctionEnums::REPLACE, arg);
   default:
      throw (AipsError ("LatticeExprNode::replace - unknown data type"));
   }
   return LatticeExprNode();
}



bool LatticeExprNode::areRegions (const LatticeExprNode& left,
				  const LatticeExprNode& right)
{
   return (left.isRegion() && right.isRegion());
}

LatticeExprNode LatticeExprNode::newNumUnary (LELUnaryEnums::Operation oper,
					      const LatticeExprNode& expr)
//
// Create a new node for a numerical unary operation.
// The result has the same data type as the input.
//
{
   switch (expr.dataType()) {
   case TpFloat:
      return new LELUnary<float> (oper, expr.pExprFloat_p);
   case TpDouble:
      return new LELUnary<double> (oper, expr.pExprDouble_p);
   case TpComplex:
      return new LELUnary<Complex> (oper, expr.pExprComplex_p);
   case TpDComplex:
      return new LELUnary<DComplex> (oper, expr.pExprDComplex_p);
   default:
      throw (AipsError ("LatticeExprNode::newNumUnary - "
			"Bool argument used in numerical unary operation"));
   }
   return LatticeExprNode();
}


LatticeExprNode LatticeExprNode::newNumFunc1D (LELFunctionEnums::Function func,
					       const LatticeExprNode& expr)
//
// Create a new node for a numerical function with 1 argument.
// The result has the same data type as the input.
//
{
   switch (expr.dataType()) {
   case TpFloat:
      return new LELFunction1D<float> (func, expr.pExprFloat_p);
   case TpDouble:
      return new LELFunction1D<double> (func, expr.pExprDouble_p);
   case TpComplex:
      return new LELFunction1D<Complex> (func, expr.pExprComplex_p);
   case TpDComplex:
      return new LELFunction1D<DComplex> (func, expr.pExprDComplex_p);
   default:
      throw (AipsError ("LatticeExprNode::newNumFunc1D - "
			"Bool argument used in numerical function"));
   }
   return LatticeExprNode();
}


LatticeExprNode LatticeExprNode::newRealFunc1D (LELFunctionEnums::Function func,
						const LatticeExprNode& expr)
//
// Create a new node for a real numerical function with 1 argument.
// The result has the same data type as the input.
// 
{
   switch (expr.dataType()) {
   case TpFloat:
      return new LELFunctionReal1D<float> (func, expr.pExprFloat_p);
   case TpDouble:
      return new LELFunctionReal1D<double> (func, expr.pExprDouble_p);
   default:
      throw (AipsError ("LatticeExprNode::newRealFunc1D - "
			"Bool or complex argument used in real "
			"numerical function"));
   }
   return LatticeExprNode();
}

LatticeExprNode LatticeExprNode::newComplexFunc1D (LELFunctionEnums::Function func,
    						   const LatticeExprNode& expr)
//
// Create a new node for a complex numerical function with 1
// argument. The result has the same data type as the input.
// 
{
   Block<LatticeExprNode> arg(1);
   arg[0] = expr;
   switch (expr.dataType()) {
   case TpComplex:
      return new LELFunctionComplex(func, arg);
   case TpDComplex:
      return new LELFunctionDComplex(func, arg);
   default:
      throw (AipsError ("LatticeExprNode::newComplexFunc1D - "
			"only complex arguments allowed"));
   }
   return LatticeExprNode();
}

LatticeExprNode LatticeExprNode::newNumReal1D (LELFunctionEnums::Function func,
					       const LatticeExprNode& expr)
//
// Create a new node for a numerical function with 1 arguments that 
// returns a real number
//
{
   DataType dtype = expr.dataType();
   Block<LatticeExprNode> arg(1);
   arg[0] = expr;
   switch (dtype) {
   case TpFloat:
   case TpComplex:
      return new LELFunctionFloat (func, arg);
   case TpDouble:
   case TpDComplex:
      return new LELFunctionDouble (func, arg);
   default:
      throw (AipsError ("LatticeExprNode::newNumReal1D - "
			"output type must be real and numeric"));
   }
   return LatticeExprNode();
}





LatticeExprNode LatticeExprNode::newNumFunc2D (LELFunctionEnums::Function func,
					       const LatticeExprNode& left,
					       const LatticeExprNode& right)
//
// Create a new node for a numerical function with 2 arguments.
// The result has the same data type as the combined input type.
// 
{
   DataType dtype = resultDataType (left.dataType(), right.dataType());
   Block<LatticeExprNode> arg(2);
   switch (dtype) {
   case TpFloat:
      arg[0] = left.makeFloat();
      arg[1] = right.makeFloat();
      return new LELFunctionFloat (func, arg);
   case TpDouble:
      arg[0] = left.makeDouble();
      arg[1] = right.makeDouble();
      return new LELFunctionDouble (func, arg);
   case TpComplex:
      arg[0] = left.makeComplex();
      arg[1] = right.makeComplex();
      return new LELFunctionComplex (func, arg);
   case TpDComplex:
      arg[0] = left.makeDComplex();
      arg[1] = right.makeDComplex();
      return new LELFunctionDComplex (func, arg);
   default:
      throw (AipsError ("LatticeExprNode::newNumFunc2D - "
			"Bool argument used in numerical function"));
   }
   return LatticeExprNode();
}


LatticeExprNode LatticeExprNode::newNumBinary (LELBinaryEnums::Operation oper,
					       const LatticeExprNode& left,
					       const LatticeExprNode& right)
//
// Create a new node for a numerical binary operator.
// The result has the same data type as the combined input type.
//
{
  DataType dtype = resultDataType (left.dataType(), right.dataType());
  LatticeExprNode expr0;
  LatticeExprNode expr1;
  switch (dtype) {
  case TpFloat:
    expr0 = left.makeFloat();
    expr1 = right.makeFloat();
    break;
  case TpDouble:
    expr0 = left.makeDouble();
    expr1 = right.makeDouble();
    break;
  case TpComplex:
    expr0 = left.makeComplex();
    expr1 = right.makeComplex();
    break;
  case TpDComplex:
    expr0 = left.makeDComplex();
    expr1 = right.makeDComplex();
    break;
  default:
    throw (AipsError ("LatticeExprNode::newNumBinary - "
		      "Bool argument used in numerical binary operation"));
  }
  // Make the operands the same dimensionality (if needed and possible).
  makeEqualDim (expr0, expr1);
  switch (dtype) {
  case TpFloat:
    return new LELBinary<float> (oper, expr0.pExprFloat_p,
				 expr1.pExprFloat_p);
  case TpDouble:
    return new LELBinary<double> (oper, expr0.pExprDouble_p,
				  expr1.pExprDouble_p);
  case TpComplex:
    return new LELBinary<Complex> (oper, expr0.pExprComplex_p,
				   expr1.pExprComplex_p);
  default:
    return new LELBinary<DComplex> (oper, expr0.pExprDComplex_p,
				    expr1.pExprDComplex_p);
  }
  return LatticeExprNode();
}


LatticeExprNode LatticeExprNode::newLogBinary (LELBinaryEnums::Operation oper,
					       const LatticeExprNode& left,
					       const LatticeExprNode& right)
//
// Create a new node for a logical binary operator.
// The result has the same data type as the combined input type.
//
{
  DataType dtype = resultDataType (left.dataType(), right.dataType());
  LatticeExprNode expr0;
  LatticeExprNode expr1;
  switch (dtype) {
  case TpBool:
    expr0 = left.makeBool();
    expr1 = right.makeBool();
    break;
  default:
    throw (AipsError ("LatticeExprNode::newLogBinary - "
		      "Non-bool argument used in logical binary operation"));
  }
  // Make the operands the same dimensionality (if needed and possible).
  makeEqualDim (expr0, expr1);
  return new LELBinaryBool (oper, expr0.pExprBool_p,
                            expr1.pExprBool_p);
}


LatticeExprNode LatticeExprNode::newBinaryCmp (LELBinaryEnums::Operation oper,
					       const LatticeExprNode& left,
					       const LatticeExprNode& right)
//
// Create a new node for a comparison binary operator.
// The result has the same data type as the combined input type.
//
{
  DataType dtype = resultDataType (left.dataType(), right.dataType());
  LatticeExprNode expr0;
  LatticeExprNode expr1;
  switch (dtype) {
  case TpFloat:
    expr0 = left.makeFloat();
    expr1 = right.makeFloat();
    break;
  case TpDouble:
    expr0 = left.makeDouble();
    expr1 = right.makeDouble();
    break;
  case TpComplex:
    expr0 = left.makeComplex();
    expr1 = right.makeComplex();
    break;
  case TpDComplex:
    expr0 = left.makeDComplex();
    expr1 = right.makeDComplex();
    break;
  case TpBool:
    if (oper != LELBinaryEnums::EQ  &&  oper != LELBinaryEnums::NE) {
      throw (AipsError ("LatticeExprNode::newBinaryCmp - "
			"Bool data type cannot be used with "
			">, >=, <, and <= operator"));
    }
    expr0 = left.makeBool();
    expr1 = right.makeBool();
    break;
  default:
    throw (AipsError ("LatticeExprNode::newBinaryCmp - "
		      "invalid data type used in comparison"));
  }
  // Make the operands the same dimensionality (if needed and possible).
  makeEqualDim (expr0, expr1);
  switch (dtype) {
  case TpFloat:
    return new LELBinaryCmp<float> (oper, expr0.pExprFloat_p,
				    expr1.pExprFloat_p);
  case TpDouble:
    return new LELBinaryCmp<double> (oper, expr0.pExprDouble_p,
				     expr1.pExprDouble_p);
  case TpComplex:
    return new LELBinaryCmp<Complex> (oper, expr0.pExprComplex_p,
				      expr1.pExprComplex_p);
  case TpDComplex:
    return new LELBinaryCmp<DComplex> (oper, expr0.pExprDComplex_p,
				       expr1.pExprDComplex_p);
  default:
    return new LELBinaryBool (oper, expr0.pExprBool_p,
			      expr1.pExprBool_p);
  }
  return LatticeExprNode();
}


DataType LatticeExprNode::resultDataType (DataType left, DataType right)
//
// Work out the resultant data type when two expressions are combined
// Favours the higher precision
//
{
    if (left == right) {
	return left;
    }
    if (left == TpBool  ||  right == TpBool) {
	throw (AipsError ("LatticeExprNode::resultDataType - "
			  "Bool and numeric operands cannot mixed"));
    }
    if (left == TpDComplex  ||  right == TpDComplex) {
	return TpDComplex;
    }
    if (left == TpComplex  ||  right == TpComplex) {
	if (left == TpDouble  ||  right == TpDouble) {
	    return TpDComplex;
	}
	return TpComplex;
    }
    if (left == TpDouble  ||  right == TpDouble) {
	return TpDouble;
    }
    return TpFloat;
}


LELAttribute LatticeExprNode::checkArg (const Block<LatticeExprNode>& arg,
					const Block<int32_t>& argType,
					bool expectArray,
					bool matchAxes)
{
    if (arg.nelements() != argType.nelements()) {
	throw (AipsError ("LatticeExprNode::checkArg - "
			  "invalid number of function arguments"));
    }
    // Compose the resulting LELAttribute from all arguments.
    // Each time it is checked if shapes and coordinates conform.
    LELAttribute attr;
    for (uint32_t i=0; i<arg.nelements(); i++) {
	if (arg[i].dataType() != argType[i]) {
	    throw (AipsError ("LatticeExprNode::checkArg - "
			      "a function argument has invalid data type"));
	}
	attr = LELAttribute (attr, arg[i].getAttribute(), matchAxes);
    }
    if (expectArray  &&  attr.isScalar()){
	throw (AipsError ("LatticeExprNode::checkArg - "
			  "expected a lattice function argument"));
    }
    return attr;
}


const IPosition& LatticeExprNode::getIPosition() const
{
   if (dataType() != TpOther) {
      throw (AipsError ("LatticeExprNode::getIPosition - "
			"node does not contain an IPosition"));
   }
   return iposition_p;
}

CountedPtr<LELInterface<float> > LatticeExprNode::makeFloat() const
{
    switch (dataType()) {
    case TpFloat:
	return pExprFloat_p;
    case TpDouble:
	return new LELConvert<float,double> (pExprDouble_p);
    default:
	throw (AipsError ("LatticeExprNode::makeFloat - "
			  "conversion to float not possible"));
    }
}

CountedPtr<LELInterface<double> > LatticeExprNode::makeDouble() const
{
    switch (dataType()) {
    case TpFloat:
	return new LELConvert<double,float> (pExprFloat_p);
    case TpDouble:
	return pExprDouble_p;
    default:
	throw (AipsError ("LatticeExprNode::makeDouble - "
			  "conversion to double not possible"));
    }
}

CountedPtr<LELInterface<Complex> > LatticeExprNode::makeComplex() const
{
    switch (dataType()) {
    case TpFloat:
	return new LELConvert<Complex,float> (pExprFloat_p);
    case TpDouble:
	return new LELConvert<Complex,double> (pExprDouble_p);
    case TpComplex:
	return pExprComplex_p;
    case TpDComplex:
	return new LELConvert<Complex,DComplex> (pExprDComplex_p);
    default:
	throw (AipsError ("LatticeExprNode::makeComplex - "
			  "conversion to Complex not possible"));
    }
}

CountedPtr<LELInterface<DComplex> > LatticeExprNode::makeDComplex() const
{
    switch (dataType()) {
    case TpFloat:
	return new LELConvert<DComplex,float> (pExprFloat_p);
    case TpDouble:
	return new LELConvert<DComplex,double> (pExprDouble_p);
    case TpComplex:
	return new LELConvert<DComplex,Complex> (pExprComplex_p);
    case TpDComplex:
	return pExprDComplex_p;
    default:
	throw (AipsError ("LatticeExprNode::makeDComplex - "
			  "conversion to DComplex not possible"));
    }
}

CountedPtr<LELInterface<bool> > LatticeExprNode::makeBool() const
{
    if (dataType() != TpBool) {
	throw (AipsError ("LatticeExprNode::makeBool - "
			  "conversion to bool not possible"));
    }
    if (isRegion()) {
        return new LELRegionAsBool ((const LELRegion&)(*pExprBool_p));
    }
    return pExprBool_p;
}


int32_t LatticeExprNode::makeEqualDim (LatticeExprNode& expr0,
				   LatticeExprNode& expr1)
{
  // Compare the coordinates (and shapes).
  const LELAttribute& attr0 = expr0.getAttribute();
  const LELAttribute& attr1 = expr1.getAttribute();
  int32_t result = attr0.compareCoord (attr1);
  if (result == -1) {
    // left is subset of right, so extend left.
    const LELLattCoordBase* cbptr = &(attr0.coordinates().coordinates());
    const LELLattCoord* cptr = dynamic_cast<const LELLattCoord*>(cbptr);
    AlwaysAssert (cptr != 0, AipsError);
    expr0 = cptr->makeExtendLattice (expr0,
				     attr1.shape(),
				     attr1.coordinates().coordinates());
  } else if (result == 1) {
    // right is subset of left, so extend right.
    const LELLattCoordBase* cbptr = &(attr1.coordinates().coordinates());
    const LELLattCoord* cptr = dynamic_cast<const LELLattCoord*>(cbptr);
    AlwaysAssert (cptr != 0, AipsError);
    expr1 = cptr->makeExtendLattice (expr1,
				     attr0.shape(),
				     attr0.coordinates().coordinates());
  } else if (result == 9) {
    throw AipsError ("LatticeExprNode - coordinates of operands mismatch");
  } else if (result != 0) {
    throw AipsError ("LatticeExprNode - shapes of operands mismatch");
  }
  return result;
}

} //# NAMESPACE CASACORE - END

