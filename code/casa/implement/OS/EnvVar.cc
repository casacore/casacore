//# EnvVar.cc: Environment variables class
//# Copyright (C) 1993,1994,1995,1998
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

#include <aips/OS/EnvVar.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <stdlib.h>

extern char **environ;  

EnvironmentVariables::EnvironmentVariables() {

  register int i;

  // Change pointer of environ list to myenviron and then
  // point new environ to big area
  if (environ != myenviron) {
    for (i = 0; i < maxenviron &&  environ[i] != NULL; i++)
      myenviron[i] = environ[i];
    if (i < maxenviron) {
      environ = myenviron;
      myenviron[i] = NULL;
    }
  }
}

EnvironmentVariables::EnvironmentVariables(const String &namevalue) {
  register int i;

  // Change pointer of environ list to myenviron and then
  // point environ to big area
  if (environ != myenviron) {
    for (i = 0; i < maxenviron &&  environ[i] != NULL; i++)
      myenviron[i] = environ[i];
    if (i < maxenviron) {
      environ = myenviron;
      myenviron[i] = NULL;
    }
  }
  if(!EnvironmentVariables().set(namevalue))
    DebugAssert(!EnvironmentVariables().set(namevalue), AipsError);
}

EnvironmentVariables::EnvironmentVariables(const String &name, const String &value) {
  register int i;

  // Change pointer of environ list to myenviron and then
  // point environ to big area
  if (environ != myenviron) {
    for (i = 0; i < maxenviron &&  environ[i] != NULL; i++)
      myenviron[i] = environ[i];
    if (i < maxenviron) {
      environ = myenviron;
      myenviron[i] = NULL;
    }
  }
  if(!EnvironmentVariables().set(name, value))
    DebugAssert(!EnvironmentVariables().set(name, value), AipsError);
}

uInt EnvironmentVariables::number() {

  uInt i;
  for(i=0; environ[i]!=NULL; i++)
    ;
  return i;
}

String EnvironmentVariables::name(uInt number) {

  String s;

  if(number > EnvironmentVariables().number())
    return s;
  else {
    s=environ[number];
    s=s.before("=");
    return s;
  }
}

String EnvironmentVariables::value(const String &name) {

  String env;
  env=getenv(name);
  return env;
}

String EnvironmentVariables::value(uInt number) {

  String s;

  if(number > EnvironmentVariables().number())
    return s;
  else {
    s=environ[number];
    s=s.after("=");
    return s;
  }
}

Bool EnvironmentVariables::isSet(const String &name) {

  if(getenv(name) != NULL)
    return True;
  else
    return False;
}

Bool EnvironmentVariables::set(const String &name, const String &value) {

  if(name == "" || value == "")
    return False;
  if(!EnvironmentVariables().setenv(name, value)) {
    DebugAssert(!EnvironmentVariables().setenv(name, value), AipsError);
    return False;
  }
  return True;
}

Bool EnvironmentVariables::set(const String &nameAndValuePair ){

  String name=nameAndValuePair, value=nameAndValuePair;

  name=name.before("=");
  value=value.after("=");
  if(name == "" || value == "")
    return False;
  if(!EnvironmentVariables().setenv(name, value)) {
    DebugAssert(!EnvironmentVariables().setenv(name, value), AipsError);
    return False;
  }
  return True;
}

void EnvironmentVariables::unSet(const String &name) {

  // unsetenv function removes any definition of name. It is not
  // a error if such a definition doesn't exist.
  register char **cp;
  String env;

  for(int i=0; environ[i]!=NULL; i++) {
    env=environ[i];
    env=env.before("=");
    if(env==name) {
      for (cp = &environ[i];; ++cp)
        if (!(*cp = *(cp + 1)))
          break;
    }
  }
}

Bool EnvironmentVariables::setenv(const String &name, const String &value){

  char *ptr,*envnew;
  int i,found,need,lname,lvalue;
  String snew,ev;

  snew=name+"="+value;
  lname=name.length()+1;

  found = 0;
  for (i = 0; environ[i] != NULL; i++) {
  // If exist the definition
    ev=environ[i];
    if( name == ev.before("=") ) {
      found = 1;
      need=snew.length() - ev.length();
      if (need > 0) {
        // Create space for new name=value
	need=snew.length()+1;
	if ((ptr =(char *)malloc(need)) == NULL) {
	  DebugAssert((ptr = (char *)malloc(need)) == NULL, AipsError);
	  return False;
	}
	environ[i] = ptr;
      }
      // copy name=value into environment list
      envnew=(char *)snew.chars();
      strcpy (environ[i], envnew);
    }
  }
  // If doesn't exist the definition
  lvalue=value.length();
  if (found == 0 && lvalue >= 0) {
    if (i >= maxenviron) {
      // environment list is full
      DebugAssert(i >= maxenviron, AipsError);
      return False;
    }
    else {
      // Create a new slot
      need=snew.length()+1;
      if ((environ[i] =(char *)malloc(need)) == NULL) {
	DebugAssert((environ[i] = (char *)malloc(need)) == NULL, AipsError);
	return False;
      }
      else {
        // copy name=value into environment list
	envnew=(char *)snew.chars();
	strcpy (environ[i], envnew);
        // Environment list need NULL in last position
	environ[i+1] = NULL;
      }
    }
  }
  return True;
}
