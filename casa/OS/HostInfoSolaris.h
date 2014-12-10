//# HostInfo_solaris.h: Solaris specific memory, swap, and CPU code.
//# $Id$

 /*
 **  This is a greatly MODIFIED version of a "top" machine dependent file.
 **  The only resemblance it bears to the original is with respect to the
 **  mechanics of finding various system details. The copyright details
 **  follow.
 **
 **  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 **
 **  Top users/processes display for Unix
 **  Version 3
 **
 **  This program may be freely redistributed,
 **  but this entire comment MUST remain intact.
 **
 **  Copyright (c) 1984, 1989, William LeFebvre, Rice University
 **  Copyright (c) 1989 - 1994, William LeFebvre, Northwestern University
 **  Copyright (c) 1994, 1995, William LeFebvre, Argonne National Laboratory
 **  Copyright (c) 1996, William LeFebvre, Group sys Consulting
 **  Copyright (c) 2002, Associated Universities Inc.
 */

#ifndef CASA_HOSTINFOSOLARIS_H
#define CASA_HOSTINFOSOLARIS_H

# if defined(HOSTINFO_DO_IMPLEMENT)

/*
 *
 * LIBS: -lkstat
 *
 *          AUTHOR:       Darrell Schiebel  <drs@nrao.edu>
 *
 * ORIGINAL AUTHORS:      Torsten Kasch     <torsten@techfak.uni-bielefeld.de>
 *                        Robert Boucher    <boucher@sofkin.ca>
 * ORIGINAL CONTRIBUTORS: Marc Cohen        <marc@aai.com>
 *                        Charles Hedrick   <hedrick@geneva.rutgers.edu>
 *	                  William L. Jones  <jones@chpc>
 *                        Petri Kutvonen    <kutvonen@cs.helsinki.fi>
 *	                  Casper Dik        <casper.dik@sun.com>
 *                        Tim Pugh          <tpugh@oce.orst.edu>
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <kvm.h>
#include <sys/types.h>
#include <sys/sysinfo.h>
#include <sys/swap.h>

#include <kstat.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// HostInfo for Solaris machines.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=HostInfo>HostInfo</linkto>
// </prerequisite>

// <synopsis> 
// This file provides the Solaris specific functions for HostInfo.
// It is selectively included by HostInfo.cc.
// </synopsis>
//
// <group name="HostInfo">

/*
 * Some kstats are fixed at 32 bits, these will be specified as ui32; some
 * are "natural" size (32 bit on 32 bit Solaris, 64 on 64 bit Solaris
 * we'll make those unsigned long)
 * Older Solaris doesn't define KSTAT_DATA_UINT32, those are always 32 bit.
 */
# ifndef KSTAT_DATA_UINT32
#  define ui32 ul
# endif

#ifdef SC_AINFO
#undef USE_ANONINFO				/* Use swapctl() instead */
#endif

#define NO_NPROC

/* pagetok function is really a pointer to an appropriate function */
#define pagetok(size) ((*p_pagetok)(size))

#ifndef USE_ANONINFO
static void get_swapinfo(int *total, int *fr);
#endif

class HostMachineInfo {
friend class HostInfo;

    void kupdate( );
    HostMachineInfo( );
    ~HostMachineInfo( );
    void update_info( );

    static int pageshift;
    int (*p_pagetok) (int);

    static inline int pagetok_none(int size) { return(size); }
    static inline int pagetok_left(int size) { return(size << pageshift); }
    static inline int pagetok_right(int size) { return(size >> pageshift); }

    int valid;
    kstat_ctl_t *kc;

    int cpus;

    ptrdiff_t memory_total;
    ptrdiff_t memory_used;
    ptrdiff_t memory_free;

    ptrdiff_t swap_total;
    ptrdiff_t swap_used;
    ptrdiff_t swap_free;
};

// </group>


int HostMachineInfo::pageshift = 0;

HostMachineInfo::~HostMachineInfo( ) { if ( kc ) kstat_close(kc); }

HostMachineInfo::HostMachineInfo( ) : valid(1), kc(NULL)
{
    int i;

    /* calculate pageshift value */
    i = sysconf(_SC_PAGESIZE);
    pageshift = 0;
    while ((i >>= 1) > 0) pageshift++;

    /* calculate an amount to shift to K values */
    /* remember that log base 2 of 1024 is 10 (i.e.: 2^10 = 1024) */
    pageshift -= 10;

    /* now determine which pageshift function is appropriate for the 
       result (have to because x << y is undefined for y < 0) */
    if (pageshift > 0)
    {
	/* this is the most likely */
	p_pagetok = pagetok_left;
    }
    else if (pageshift == 0)
    {
	p_pagetok = pagetok_none;
    }
    else
    {
	p_pagetok = pagetok_right;
	pageshift = -pageshift;
    }

    long maxmem = sysconf(_SC_PHYS_PAGES);
    memory_total = pagetok (maxmem);

    /* use kstat to update all processor information */
    kupdate( );

    kstat_t *ks = kstat_lookup(kc, "unix", 0, "system_misc");
    if (kstat_read(kc, ks, 0) == -1) {
	perror("kstat_read");
	valid = 0;
    } 
    kstat_named_t *kn = (kstat_named_t*) kstat_data_lookup(ks, "ncpus");
    cpus = kn->value.ui32;
}

void HostMachineInfo::kupdate( )
{
    kid_t nkcid;
    int i;
    static kid_t kcid = 0;

    /*
     * 0. kstat_open
     */

    if (!kc)
    {
	kc = kstat_open();
	if (!kc)
	{
	    perror("kstat_open ");
	    valid = 0;
	}
	kcid = kc->kc_chain_id;
    }

    /* keep doing it until no more changes */
  kcid_changed:

    /*
     * 1.  kstat_chain_update
     */
    nkcid = kstat_chain_update(kc);
    if (nkcid)
    {
	/* UPDKCID will abort if nkcid is -1, so no need to check */
	kcid = nkcid;
    }
    if (nkcid == -1) {
        perror("kstat_read ");
	valid = 0;
    }
    if (nkcid != 0)
	goto kcid_changed;
}


void HostMachineInfo::update_info( )
{
  static long freemem;
  static int swaptotal;
  static int swapfree;
  kstat_t *ks;
  kstat_named_t *kn;

  ks = kstat_lookup(kc, "unix", 0, "system_pages");
  if (kstat_read(kc, ks, 0) == -1) {
      perror("kstat_read");
      valid = 0;
  }
  kn = (kstat_named_t*) kstat_data_lookup(ks, "freemem");
  if (kn)
      freemem = kn->value.ul;

  memory_free = pagetok (freemem);
  memory_used = memory_total - memory_free;

  get_swapinfo(&swaptotal, &swapfree);
  swap_total = pagetok(swaptotal);
  swap_used = pagetok(swaptotal - swapfree);
  swap_free = pagetok(swapfree);
}

#ifndef USE_ANONINFO
void get_swapinfo(int *total, int *fr)

{
#ifdef SC_AINFO
    struct anoninfo anon;

    if (swapctl(SC_AINFO, &anon) == -1) {
	*total = *fr = 0;
	return;
    }
    *total = anon.ani_max;
    *fr = anon.ani_max - anon.ani_resv;
#else
    int cnt, i;
    int t, f;
    struct swaptable *swt;
    struct swapent *ste;
    static char path[256];

    /* get total number of swap entries */
    cnt = swapctl(SC_GETNSWP, 0);

    /* allocate enough space to hold count + n swapents */
    swt = (struct swaptable *)malloc(sizeof(int) +
				     cnt * sizeof(struct swapent));
    if (swt == NULL)
    {
	*total = 0;
	*fr = 0;
	return;
    }
    swt->swt_n = cnt;

    /* fill in ste_path pointers: we don't care about the paths, so we point
       them all to the same buffer */
    ste = &(swt->swt_ent[0]);
    i = cnt;
    while (--i >= 0)
    {
	ste++->ste_path = path;
    }

    /* grab all swap info */
    swapctl(SC_LIST, swt);

    /* walk thru the structs and sum up the fields */
    t = f = 0;
    ste = &(swt->swt_ent[0]);
    i = cnt;
    while (--i >= 0)
    {
	/* dont count slots being deleted */
	if (!(ste->ste_flags & ST_INDEL) &&
	    !(ste->ste_flags & ST_DOINGDEL))
	{
	    t += ste->ste_pages;
	    f += ste->ste_free;
	}
	ste++;
    }

    /* fill in the results */
    *total = t;
    *fr = f;
    free(swt);
#endif /* SC_AINFO */
}
#endif /* USE_ANONINFO */


} //# NAMESPACE CASACORE - END

# endif
#endif
