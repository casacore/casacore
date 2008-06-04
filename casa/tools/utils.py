import sys
import os
import glob
import re
import string
import platform

def generate(env):

    def SGlob(pattern, excludedirs=[], recursive=False):
	# always exclude .svn
	excludedirs.append(".svn")
        path = env.GetBuildPath('SConscript').replace('SConscript', '')	
	if recursive:
	    # remove '*' from pattern is accidentally specified
	    pattern=pattern.replace("*", "")
	    out = []
	    for d, ld, fls in os.walk(path):
		# remove directorys to be excluded 
		for exd in excludedirs:
		    if exd in ld:
			ld.remove(exd)
		for f in fls:		    
		    if f.endswith(pattern):
			drel=d.replace(path,"")
			out.append(os.path.join(drel,f))
	    return out
	else:
	    return [ i.replace(path, '') for i in  glob.glob(path + pattern) ]
    env.SGlob = SGlob

    def AddCustomPath(path=None):
        if path is None or not os.path.exists(path):
            env.Exit(1)
        env.PrependUnique(CPPPATH = [os.path.join(path, "include")])
        env.PrependUnique(LIBPATH = [os.path.join(path, "lib")])
    env.AddCustomPath = AddCustomPath

    def AddCustomPackage(pkgname=None):
        if pkgname is None:
	    return
	pkgroot = env.get("%sroot" % pkgname, None)
	pkgincd = env.get("%sincdir" % pkgname, None)
	pkglibd = env.get("%slibdir" % pkgname, None)
	incd = None
	libd = None
	if pkgroot is not None:
	    incd = os.path.join(pkgroot, "include")
	    libd = os.path.join(pkgroot, "lib")
	else:	    
	    if pkgincd is not None:
		incd = pkgincd
	    if pkglibd is not None:
		libd = pkglibd
	if incd is not None:
	    if not os.path.exists(incd):
		print "Custom %s include dir '%s' not found" % (pkgname, incd)
		env.Exit(1)
	    env.PrependUnique(CPPPATH = [incd])
	if libd is not None:
 	    if not os.path.exists(libd):
		print "Custom %s lib dir '%s' not found" % (pkgname, libd)
		env.Exit(1)
	    env.PrependUnique(LIBPATH = [libd])

    env.AddCustomPackage = AddCustomPackage

    def PlatformIdent():
	p = sys.platform
	# replace the trailing 2 in linux2
	p = re.sub(re.compile("2$"), "", p)
	return p + "_" + platform.machine()
    env.PlatformIdent = PlatformIdent

    def MergeFlags():
        def _to_list(xf):
            if xf.count(","):
                return xf.split(",")
            return xf.split()

        xf=env.get("extracppflags", None)
        if xf:
            env.AppendUnique(CPPFLAGS=_to_list(xf))
        xf=env.get("extralinkflags", None)
        if xf:
            env.AppendUnique(LINKFLAGS=_to_list(xf))
            env.AppendUnique(SHLINKFLAGS=_to_list(xf))
        xf=env.get("extracxxflags", None)
        if xf:
            env.AppendUnique(CXXFLAGS=_to_list(xf))
        xf=env.get("extrafflags", None)
        if xf:
            env.AppendUnique(FORTRANFLAGS=_to_list(xf))
            env.AppendUnique(SHFORTRANFLAGS=_to_list(xf))
        xf=env.get("extracflags", None)
        if xf:
            env.AppendUnique(CCFLAGS=_to_list(xf))
        xf=env.get("extralibpath", None)
        if xf:
            env.AppendUnique(LIBPATH=_to_list(xf))
        xf=env.get("extracpppath", None)
        if xf:
            env.AppendUnique(CPPPATH=_to_list(xf))
        xf=env.get("extraldlibrarypath", None)
        if xf:
            ldname = "LD_LIBRARY_PATH"
            if sys.platform == "darwin":
                ldname = "DY"+ldname
            env.AppendENVPath(ldname, _to_list(xf))
    # set the extra flags if available
    MergeFlags()
        
    def CheckFortran(conf):
        
        def getf2clib(fc):
            fdict = {'gfortran': 'gfortran', 'g77': 'g2c', 'f77': 'f2c'}
            return fdict[fc]

        
	if not conf.env.has_key("FORTRAN"):
	    # auto-detect fortran
	    detect_fortran = conf.env.Detect(['gfortran', 'g77', 'f77'])
	    conf.env["FORTRAN"] = detect_fortran
        f2clib = conf.env.get("f2clib", getf2clib(conf.env["FORTRAN"]))
        if not conf.CheckLib(f2clib):
            conf.env.Exit(1)

	if conf.env["FORTRAN"].startswith("g77"):
            fflags = ["-Wno-globals", "-fno-second-underscore"]
	    conf.env.AppendUnique(SHFORTRANFLAGS=fflags)
	    conf.env.AppendUnique(FORTRANFLAGS=fflags)
        conf.env.AppendUnique(SHFORTRANFLAGS=['-fPIC'])

    env.CheckFortran = CheckFortran

    def null_action(target, source, env): return 0

    def message(target, source, env):
        return "%s" % target[0]
    env.MessageAction = env.Action(null_action, message)

def exists(env):
    try:
        import os
        import glob
    except ImportError:
        return False
    else:
        return True
