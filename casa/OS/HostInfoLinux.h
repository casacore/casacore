//# HostInfo_linux.h: Linux specific memory, swap, and CPU code.
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

#ifndef CASA_HOSTINFOLINUX_H
#define CASA_HOSTINFOLINUX_H

# if defined(HOSTINFO_DO_IMPLEMENT)

/*
 *          AUTHOR:       Darrell Schiebel  <drs@nrao.edu>
 *
 * ORIGINAL AUTHORS:      Richard Henderson <rth@tamu.edu>
 *                        Alexey Klimkin    <kad@klon.tme.mcst.ru>
 *
 *
 */

#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/vfs.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

// <summary>
// HostInfo for Linux machines.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=HostInfo>HostInfo</linkto>
// </prerequisite>

// <synopsis> 
// This file provides the Linux specific functions for HostInfo.
// It is selectively included by HostInfo.cc.
// </synopsis>
//
// <group name="HostInfo">

#if 0
#include <linux/proc_fs.h>	/* for PROC_SUPER_MAGIC */
#else
#define PROC_SUPER_MAGIC 0x9fa0
#endif

#define PROCFS "/proc"
#define CPUINFO "/proc/cpuinfo"
#define MEMINFO "/proc/meminfo"

#define bytetok(x)	(((x) + 512) >> 10)

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class HostMachineInfo {
friend class HostInfo;
  
    HostMachineInfo( );
    void update_info( );

    int valid;
    int cpus;

    ptrdiff_t memory_total;
    ptrdiff_t memory_used;
    ptrdiff_t memory_free;

    ptrdiff_t swap_total;
    ptrdiff_t swap_used;
    ptrdiff_t swap_free;
};

// </group>


static inline char *
skip_ws(const char *p)
{
    while (isspace(*p)) p++;
    return (char *)p;
}
    
static inline char *
skip_token(const char *p)
{
    while (isspace(*p)) p++;
    while (*p && !isspace(*p)) p++;
    return (char *)p;
}

HostMachineInfo::HostMachineInfo( ) : valid(1)
{
    char buffer[4096+1];
    int fd, len;
    char *p;

#ifndef AIPS_CRAY_PGI
    /* make sure the proc filesystem is mounted */
    {
	struct statfs sb;
	if (statfs(PROCFS, &sb) < 0 || sb.f_type != PROC_SUPER_MAGIC)
	{
	    fprintf( stderr, "proc filesystem not mounted on " PROCFS "\n" );
	    valid = 0;
	    return;
	}
    }
#endif

    /* get number of CPUs */
    {
	cpus = 0;
	FILE *fptr = fopen(CPUINFO, "r");
	while ( (p = fgets( buffer, sizeof(buffer), fptr )) ) {
	    if ( ! strncmp( p, "processor", 9 ) ) ++cpus;
	}
	fclose(fptr);
    }

    /* get system total memory */
    /* See the sprintf() statements of the file
       fs/proc/proc_misc.c in the kernel source tree */
    {
	fd = open(MEMINFO, O_RDONLY);
	len = read(fd, buffer, sizeof(buffer)-1);
	close(fd);
	buffer[len] = '\0';

	int ret;
	unsigned long mem_total, swp_total;
	p = strstr(buffer, "MemTotal:");
	if ((ret = sscanf (p, "MemTotal: %lu kB\n", &mem_total)) != 1)
	  cerr << "Error parsing MemTotal in /proc/meminfo\n";
	memory_total = mem_total;
	p = strstr(buffer, "SwapTotal:");
	if ((ret = sscanf (p, "SwapTotal: %lu kB\n", &swp_total)) != 1)
	  cerr << "Error parsing SwapTotal in /proc/meminfo\n";
	swap_total = swp_total;
    }
}

void HostMachineInfo::update_info( )
{
    char buffer[4096+1];
    int fd, len;

    /* get system wide memory usage */
    {
	char *p;
	int ret;
	unsigned long mem_total, mem_free, mem_cached, swp_total, swp_free;

	fd = open(MEMINFO, O_RDONLY);
	len = read(fd, buffer, sizeof(buffer)-1);
	close(fd);
	buffer[len] = '\0';

	p = strstr(buffer, "MemTotal:");

	if ((ret = sscanf (p,"MemTotal: %lu kB\nMemFree: %lu kB\n",
			   &mem_total, &mem_free)) != 2)
	  cerr << "Error parsing MemTotal and MemFree in /proc/meminfo\n";

	p = strstr (buffer, "Cached:");
	if ((ret = sscanf (p,"Cached: %lu kB\n", &mem_cached)) != 1)
	  cerr << "Error parsing Cached in /proc/meminfo\n";

	memory_total = mem_total;
	memory_free = mem_free + mem_cached;
	memory_used = memory_total - memory_free;

	p = strstr (buffer, "SwapTotal:");
	if ((ret = sscanf (p, "SwapTotal: %lu kB\nSwapFree: %lu kB\n",
			   &swp_total, &swp_free)) != 2)
	  cerr << "Error parsing SwapTotal and SwapFree in /proc/meminfo\n";

	swap_total = swp_total;
	swap_free = swp_free;
	swap_used = swap_total-swap_free;
    }
}

} //# NAMESPACE CASACORE - END

# endif

#endif
