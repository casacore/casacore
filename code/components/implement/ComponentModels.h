//# ComponentModels.h: classes that define a functional representation of the sky brightness
//# Copyright (C) 1999,2000
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

#ifndef COMPONENTS_COMPONENTMODELS_H
#define COMPONENTS_COMPONENTMODELS_H

#include <components/ComponentModels/ComponentType.h>

#include <components/ComponentModels/Flux.h>

#include <components/ComponentModels/ComponentShape.h>
#include <components/ComponentModels/TwoSidedShape.h>
#include <components/ComponentModels/PointShape.h>
#include <components/ComponentModels/GaussianShape.h>
#include <components/ComponentModels/DiskShape.h>

#include <components/ComponentModels/SpectralModel.h>
#include <components/ComponentModels/ConstantSpectrum.h>
#include <components/ComponentModels/SpectralIndex.h>

#include <components/ComponentModels/SkyCompBase.h>
#include <components/ComponentModels/SkyCompRep.h>
#include <components/ComponentModels/SkyComponent.h>

#include <components/ComponentModels/ComponentList.h>
#include <trial/ComponentModels/DOcomponentlist.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <module>
//
// <summary>classes that define a functional representation of the sky brightness</summary>

// <prerequisite>
//   <li> Measures
// </prerequisite>
//

// <reviewed reviewer="" date="yyyy/mm/dd" demos="">
// </reviewed>

// <etymology>
//#! Except when it is obvious (e.g., "Arrays") explain how the module name
//#! expresses the role of this module.  Example: IPosition is short for
//#! "Integral Position" - a specialized integer vector for specifying
//#! array dimensions and indices.
// </etymology>
//
// <synopsis>

// This module contains classes which parameterise emmision from the sky using
// relatively simple functions. 

//#! What does the module do?  How?  For whom?   This should include code
//#! fragments as appropriate to support text. Code fragments shall be
//#! delimited by <srcblock> </srcblock> tags.  The synopsis section will
//#! usually be dozens of lines long.
// </synopsis>
//
// <example>
//#! One to many concise (~10-40 lines) examples, with a modest amount of
//#! text to support code fragments.   Use <srcblock> and </srcblock> to
//#! delimit example code.
// </example>
//
// <motivation>
//#! Insight into a module is often provided by a description of the
//#! circumstances that led to its conception and design.  Describe
//#! them here.
// </motivation>

// <todo asof="yyyy/mm/dd">
//#! A List of bugs, limitations, extensions or planned refinements.
//#! The programmer should fill in a date in the "asof" field, which
//#! will usually be the date at which the class is submitted for review.
//#! If, during the review, new "todo" items come up, then the "asof"
//#! date should be changed to the end of the review period.
//   <li> add this feature
//   <li> fix this bug
//   <li> discuss possible extension
// </todo>

//#! The module header file can be a big convenience to client programmers,
//#! because it allows them to use classes without studying them closely.
//#! But you -- the author of the module -- may want to notify the client
//#! programmer of some of the circumstances in which they *should* look
//#! more deeply, and get some understanding of the individual classes
//#! that make up the module.  The <note role={tip,caution,warning}> tags
//#! will be useful for this, for example:
//#!
//#!   <note role=tip>
//#!      See  Foo.h if you want to fully understand all of the options
//#!      available for creating Foo objects.
//#!   </note>
//#!
//#!   <note role=warning> Don't even think about iterating through
//#!     large Foo objects (80 MB or more) without first consulting
//#!     FooIterator.h!
//#!   </note>
//#!

// </module>


} //# NAMESPACE CASA - END

#endif
