#!/usr/bin/env python

import sys
import os
import re

deps = {'casa' : None,
	'tables': ['casa'],
	'mirlib': ['casa'],
	'scimath': ['casa'],
	'measures': ['tables', 'scimath'],
	'fits': ['measures'],
	'coordinates': ['fits'],
	'components': ['coordinates'],
	'lattices': ['tables', 'scimath'],
	'ms': ['measures'],
	'images': ['components', 'lattices', 'mirlib'],
	'msfits': ['ms', 'fits'],
	}

def get_libs(pkg):
    if pkg not in deps.keys():
	return
    pkgs = [pkg]
    def getpkg(pkg, pgs):
	plist =  deps.get(pkg)
	if plist is None: return
	for p in plist:
	    pgs.insert(0, p)
	    getpkg(p, pgs)
    getpkg(pkg, pkgs)
    outpkgs = []
    # strip off duplicates
    for i in pkgs:
	if i not in outpkgs:
	    outpkgs.append(i)
    return outpkgs

def run_scons(targets, args=[]):
    cwd = os.getcwd()
    for target in targets:
        os.chdir(target)
        command = "scons " #+ os.path.basename(target)
        # copy the command line args into the new command
	pfx = None
	tests = False
        for arg in args:
            command += " " + arg
	    if arg.startswith("prefix"):
		pfx = arg.split("=")[-1]
        print "Building package: " + target
        sys.stdout.flush()
	print command
	try:
	    failed = os.system(command)
	except KeyboardInterrupt:
	    sys.exit()
	if failed:
	    sys.exit(failed)
        sys.stdout.flush()
        os.chdir(cwd)

args = sys.argv[1:]
if "-h" not in args:
    if "doc" in args:
        os.system("doxygen doxygen.cfg")
        sys.exit(0)
    if "install" not in args:
        if "-c" not in args and "test" not in args:
            args.append("install")
            pth = "./stage"
            if not os.path.exists(pth):
                os.mkdir(pth)
            for a in args:
                if a.startswith("prefix="):
                    args.remove(a)
                if a.startswith("casacoreroot="):
                    args.remove(a)
            args.append("prefix=%s" % os.path.abspath(pth))
            args.append("casacoreroot=%s" % os.path.abspath(pth))
    else:
	hasprefix = False
	for a in args:
	    hasprefix = a.startswith("prefix=")
	    if hasprefix:
		args.append(a.replace("prefix", "casacoreroot"))
		break
	if not hasprefix:
	    args.append("casacoreroot=/usr/local")
# build all by default
tobuild = ['casa', 'tables', 'mirlib', 'scimath', 'measures','fits', 
	   'lattices', 'coordinates', 'components', 'images', 
	   'ms', 'msfits']
for k in deps.keys():
    k = k.rstrip("/")
    if k in args:
	tobuild = get_libs(k)
	args.remove(k)
if "-c" in args or "--clean" in args:
    # clean up highest level package first
    tobuild.reverse()
run_scons(tobuild, args)
