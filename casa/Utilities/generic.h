//# generic.h:  some defines, from the GNU C++ Library
//# This may look like C code, but it is really -*- C++ -*-
//#
//# Copyright (C) 1988 Free Software Foundation
//#     written by Doug Lea (dl@rocky.oswego.edu)
//# 
//# This file is part of the GNU C++ Library.  This library is free
//# software; you can redistribute it and/or modify it under the terms of
//# the GNU Library General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your
//# option) any later version.  This library is distributed in the hope
//# that it will be useful, but WITHOUT ANY WARRANTY; without even the
//# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
//# PURPOSE.  See the GNU Library General Public License for more details.
//# You should have received a copy of the GNU Library General Public
//# License along with this library; if not, write to the Free Software
//# Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

//# $Id$

#ifndef CASA_GENERIC_H
#define CASA_GENERIC_H

namespace casacore { //# NAMESPACE CASACORE - BEGIN

/*
 *	See the CPP manual, argument prescan section for explanation
 */

// <summary>Generic gnu macros</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <group name=def>

#define g_name2(a,b) gEnErIc2(a,b)
#define gEnErIc2(a,b) a ## b

#define g_name3(a,b,c) gEnErIc3(a,b,c)
#define gEnErIc3(a,b,c) a ## b ## c

#define g_name4(a,b,c,d) gEnErIc4(a,b,c,d)
#define gEnErIc4(a,b,c,d) a ## b ## c ## d

#define GENERIC_STRING(a) gEnErIcStRiNg(a)
#define gEnErIcStRiNg(a) #a

#define g_declare(clas,t)        g_name2(clas,declare)(t)
#define g_declare2(clas,t1,t2)   g_name2(clas,declare2)(t1,t2)

#define g_implement(clas,t)      g_name2(clas,implement)(t)
#define g_implement2(clas,t1,t2) g_name2(clas,implement2)(t1,t2)

//extern genericerror(int,char*);
typedef int (*GPT)(int,char*);

#define g_set_handler(gen,type,x) g_name4(set_,type,gen,_handler)(x)

#define g_errorhandler(gen,type)  g_name3(type,gen,handler)

#define g_callerror(gen,type,a,b) (*g_errorhandler(gen,type))(a,b)

// </group>


} //# NAMESPACE CASACORE - END

#endif
