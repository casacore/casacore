//# aips.cc: Global initialization for namespace management, standard types, etc.
//# Copyright (C) 1993,1994,1996,1997
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
#include <stdio.h>
#include <stdlib.h>

#if defined(AIPS_DEBUG)
Bool aips_debug_on = True;
#else
Bool aips_debug_on = False;
#endif

static size_t total = 0;
const size_t &aipsalloc(total);

// Replace global operator new/delete unless the user says to do otherwise.
// We do this to keep track of memory use.
#if !defined(AIPS_STDNEW)
#include <new.h>

// Assume everything aligns on 8 byte boundaries. No portable way to determine
// this. We store the allocation size "before" the new'd pointer in an area this
// long
const int offset = 8;

// When we have modern compilers we should throw an exception if the new_handler
// fails.

void *operator new (size_t size)
{
    char *ptr;
    while ((ptr = (char *)malloc(size+offset)) == 0) {
	new_handler handler;
	handler = set_new_handler(0);
	set_new_handler(handler); // Put it back before trying it
	if (handler) {
	    (*handler)();
	} else {
	    return 0;
	}
    }
    *((size_t *)ptr) = size;
    if (sizeof(size_t)*2 <= offset) {
	// Write it twice if it will fit as a sanity check.
	*((size_t *)ptr+1) = size;
    }
    total += size;
    return ptr+offset;
}

void operator delete (void *ptr)
{
    if (ptr) {
	char *alias = (char *)ptr;
	alias -= offset;
	size_t size = *((size_t*)alias);
	if (sizeof(size_t)*2 <= offset) {
	    if (size != *((size_t*)alias+1)) {
		fprintf(stderr, "ERROR: delete'ing memory not allocated "
			"with new!\n");
		exit(999);
	    }
	}
	total -= size;
	free(alias);
    }
}

//------ Just copies of the above with [] added

void *operator new[] (size_t size)
{
    char *ptr;
    while ((ptr = (char *)malloc(size+offset)) == 0) {
	new_handler handler;
	handler = set_new_handler(0);
	set_new_handler(handler); // Put it back before trying it
	if (handler) {
	    (*handler)();
	} else {
	    return 0;
	}
    }
    *((size_t *)ptr) = size;
    if (sizeof(size_t)*2 <= offset) {
	// Write it twice if it will fit as a sanity check.
	*((size_t *)ptr+1) = size;
    }
    total += size;
    return ptr+offset;
}

void operator delete[] (void *ptr)
{
    if (ptr) {
	char *alias = (char *)ptr;
	alias -= offset;
	size_t size = *((size_t*)alias);
	if (sizeof(size_t)*2 <= offset) {
	    if (size != *((size_t*)alias+1)) {
		fprintf(stderr, "ERROR: delete'ing memory not allocated "
			"with new!\n");
		exit(999);
	    }
	}
	total -= size;
	free(alias);
    }
}

#endif
