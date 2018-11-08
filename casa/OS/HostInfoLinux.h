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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <sched.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/vfs.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <limits>
#include <sys/time.h>
#include <sys/resource.h>


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

// get integer value from v1 cgroup hierarchy of current processes, if
// sub_value is set it returns the entry of a collection identified by value,
// e.g. total_rss from memory.stat
// returns std::numeric_limits<uInt64>::max() on error
// note unset cgroup limits usually have intptr_t.max()
// does not support v2 cgroups
static inline uInt64
get_cgroup_limit(std::string group, std::string value, std::string sub_value="")
{
    uInt64 result = std::numeric_limits<uInt64>::max();
    // assume common location, technically one needs to search for mounts
    const std::string cgroup = std::string("/sys/fs/cgroup/") + group + "/";

    // get hierarchy of current process, v1 format: id:controller:hierarchy
    std::string line;
    std::ifstream ifs("/proc/self/cgroup", std::ifstream::in);
    std::string hierarchy;
    while (getline(ifs, line)) {
	std::stringstream ss(line);
	std::string token;
	std::vector<std::string> fields;
	while (getline(ss, token, ':')) {
	    fields.push_back(token);
	}
	if (fields.size() % 3 != 0) {
	    return result;
	}
	for (std::vector<std::string>::size_type i=1; i < fields.size(); i+=3) {
	    if (fields[i].find(group) != std::string::npos) {
		hierarchy = fields[i + 1] + "/";
	    }
	}
    }
    if (hierarchy.size() == 0) {
	return result;
    }

    std::ifstream flimit((cgroup + hierarchy + value).c_str(), std::ifstream::in);
    // if hierarchy does not exist we might be in a namespace, use the root group
    if (!flimit.is_open()) {
	flimit.open((cgroup + value).c_str(), std::ifstream::in);
    }
    if (flimit.is_open()) {
	if (!sub_value.empty()) {
	    /* scan 'key value' entry like memory.stat for key == sub_value */
	    while (getline(flimit, line)) {
		std::stringstream ss(line);
		std::string token;
		ss >> token;
		if (token == sub_value) {
		    ss >> result;
		    break;
		}
	    }
	}
	else {
	    flimit >> result;
	}
    }
    return result;
}

HostMachineInfo::HostMachineInfo( ) : valid(1)
{
    char buffer[4096+1];
    char *p;

    /* get number of usable CPUs */
    cpu_set_t cpuset;
    if (sched_getaffinity(0, sizeof(cpuset), &cpuset) == 0) {
# ifdef CPU_COUNT /* glibc < 2.6 */
	cpus = CPU_COUNT(&cpuset);
# else
	for (int i = 0; i < CPU_SETSIZE; i++) {
	    if (CPU_ISSET(i, &cpuset)) {
		cpus++;
	    }
	}
# endif
    }
    else {
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
    }

    update_info();
}

void HostMachineInfo::update_info( )
{
    char buffer[4096+1];
    int fd, len;

    /* get system wide memory usage */
    {
	char *p;
	unsigned long sys_mem_total, sys_mem_free, sys_mem_cached,
		      sys_mem_avail, sys_swap_total, sys_swap_free;

	fd = open(MEMINFO, O_RDONLY);
	len = read(fd, buffer, sizeof(buffer)-1);
	close(fd);
	buffer[len] = '\0';

	p = strstr(buffer, "MemTotal:");

	if (sscanf (p,"MemTotal: %lu kB\nMemFree: %lu kB\n",
		    &sys_mem_total, &sys_mem_free) != 2)
	    cerr << "Error parsing MemTotal and MemFree in /proc/meminfo\n";

	p = strstr (buffer, "Cached:");
	if (sscanf (p,"Cached: %lu kB\n", &sys_mem_cached) != 1)
	    cerr << "Error parsing Cached in /proc/meminfo\n";

	/* available since 3.14, real available memory accounting for
	 * unreclaimable page cache and slab cache */
	p = strstr (buffer, "MemAvailable:");
	if (!p || sscanf (p,"MemAvailable: %lu kB\n", &sys_mem_avail) != 1) {
	    /* too old kernel, estimate via cache and free */
	    sys_mem_avail = sys_mem_cached + sys_mem_free;
	}

	/* check resource limits, note that these are not enforced by linux */
	struct rlimit rlim;
	if (getrlimit(RLIMIT_RSS, &rlim) == 0 && rlim.rlim_cur > 0) {
	    sys_mem_total = std::min(rlim.rlim_cur / 1024, (rlim_t)sys_mem_total);
	    /* without cgroups we cannot determine available memory within the limit */
	    sys_mem_avail = std::min(sys_mem_total, sys_mem_avail);
	}

	/* can't use more memory than allowed by cgroups, enforced */
	uInt64 proc_mem_max = get_cgroup_limit("memory", "memory.limit_in_bytes") / 1024;
	/* usage_in_bytes also includes cache so use memory.stat */
	uInt64 proc_mem_used = get_cgroup_limit("memory", "memory.stat", "total_rss") / 1024;

	/* set HostInfo memoryTotal() */
	memory_total = std::min((uInt64)sys_mem_total, proc_mem_max);

	/* if we have a valid cgroup limit we can determine memoryFree() exactly */
	if (proc_mem_max <= sys_mem_total && proc_mem_used <= proc_mem_max) {
	    memory_free = proc_mem_max - proc_mem_used;
	}
	else {
	    /* no cgroups so we have to assume all memory of host is available */
	    memory_free = std::min((uInt64)sys_mem_avail, (uInt64)memory_total);
	}
	memory_used = memory_total - memory_free;

	p = strstr (buffer, "SwapTotal:");
	if (sscanf (p, "SwapTotal: %lu kB\nSwapFree: %lu kB\n",
		    &sys_swap_total, &sys_swap_free) != 2)
	    cerr << "Error parsing SwapTotal and SwapFree in /proc/meminfo\n";

	/* can't use more swap than allowed by cgroups */
	uInt64 proc_swap_max = get_cgroup_limit("memory", "memory.memsw.limit_in_bytes") / 1024;
	uInt64 proc_swap_used = get_cgroup_limit("memory", "memory.stat", "total_swap") / 1024;
	/* limit is mem + swap */
	if (proc_mem_max <= sys_mem_total && proc_mem_max <= proc_swap_max) {
	    proc_swap_max = proc_swap_max - proc_mem_max;
	}

	/* set swapTotal() */
	swap_total = std::min((uInt64)sys_swap_total, proc_swap_max);

	if (proc_swap_max <= (uInt64)swap_total && proc_swap_used <= proc_swap_max) {
	    swap_free = proc_swap_max - proc_swap_used;
	}
	else {
	    swap_free = sys_swap_free;
	}
	swap_used = swap_total - swap_free;
    }
}

} //# NAMESPACE CASACORE - END

# endif

#endif
