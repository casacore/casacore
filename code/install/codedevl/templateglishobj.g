# templateglishobj: template for a glish closure object
# Copyright (C) 2003
# Associated Universities, Inc. Washington DC, USA.
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Library General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
# License for more details.
#
# You should have received a copy of the GNU Library General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
#
# Correspondence concerning AIPS++ should be addressed as follows:
#        Internet email: aips2-request@nrao.edu.
#        Postal address: AIPS++ Project Office
#                        National Radio Astronomy Observatory
#                        520 Edgemont Road
#                        Charlottesville, VA 22903-2475 USA
#
# $Id$

# include guard
pragma include once

#
# Include whatever files are necessary.
#
include "logger.g";

# Constructor for this object includes the logger to be used.
# Include other object e.g. defaultplotter, defaultdisplay
# as required. Use full names for these objects.

templateglishobj:=function(templateglishobjlogger=defaultlogger)
  {
    
# Define public and private data and functions. We will return public.

  public:=[=];
  self:=[=];

#------------------------------------------------------------------------ 
# If this is an application object, define the public data and
# capture it in the inputs record. This means that public inherits
# the methods (save, get, defaults, show, etc) of inputs.
#
# Begin definition of public data
  public.name:='default-value';
# End definition of public data

  public:= [=]
  
#------------------------------------------------------------------------
# Private functions
#------------------------------------------------------------------------

# End of private function
#------------------------------------------------------------------------
# Public functions
#------------------------------------------------------------------------
#
# Return name of this type of object
  const public.objectName := function() {return "templateglishobj";};
#
# Always define this function to check validity of this object
  const public.ok := function () {return T;};

# Provide a method for display if at all possible
  const public.display := function() {fail "not yet implemented";};

# Always provide a summary
  const public.summary:=function() {fail "not yet implemented";};

# Always provide a delete operation to destroy the object as a destructor would.
# This may be required to release resources that would otherwise remain
# allocated. This default implementation is often all that is needed.
  const public.delete:=function() { wider public; val public := F; return T;}
  
# End of public functions
#------------------------------------------------------------------------
  
# Return a reference to the public interface
  return ref public;
  
}

# Define demonstration function: return T if successful otherwise fail
const templateglishobjdemo:=function() {
  mytgo:=templateglishobj();
  defaultlogger.note(paste("Demonstation of ", mytgo.objectName()));
  return mytgo.delete();
}

# Define test function: return T if successful otherwise fail
const templateglishobjtest:=function() { fail "not yet implemented";}

# The next is only required for application objects:
#------------------------------------------------------------------------
# Define a const default object to be used and an abbrievation
# of two to three letters, given by the first letters of the default
# object, disambiguated by consonants.
const defaulttemplateglishobj:=templateglishobj();
dtg:=ref defaulttemplateglishobj;

defaultlogger.log('', 'NORMAL', "defaulttemplateglishobj ready for use", 
  'templateglishobj');
defaultlogger.log('', 'NORMAL', "dtg is short for defaulttemplateglishobj", 
  'templateglishobj');
#------------------------------------------------------------------------
