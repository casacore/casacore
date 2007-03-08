import sys
import os
import glob
import re
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
            Exit(1)
        env.PrependUnique(CPPPATH = [os.path.join(path, "include")])
        env.PrependUnique(LIBPATH = [os.path.join(path, "lib")])
    env.AddCustomPath = AddCustomPath

    def PlatformIdent():
	p = sys.platform
	# replace the trailing 2 in linux2
	p = re.sub(re.compile("2$"), "", p)
	return p + "_" + platform.machine()
    env.PlatformIdent = PlatformIdent

    def CheckFortran(conf):
	    
	if not conf.env.has_key("FORTRAN"):
	    # auto-detect fortran
	    detect_fortran = conf.env.Detect(['gfortran', 'g77', 'f77'])
	    conf.env["FORTRAN"] = detect_fortran
	    fdict = {'gfortran': 'gfortran', 'g77': 'g2c', 'f77': 'f2c'}
	    f2clib = conf.env.get("f2clib", fdict[detect_fortran])
	    if not conf.CheckLib(f2clib):
		Exit(1)
	else:
	    if not conf.env.has_key("f2clib"):
		print "A custom fortran compiler also needs f2clib defined"
		Exit(1)
	    else:
		if not conf.CheckLib(env["f2clib"]):
		    Exit(1)
	if conf.env["FORTRAN"].startswith("g77"):
	    conf.env.Append(SHFORTRANFLAGS="-Wno-globals")
	    conf.env.Append(FORTRANFLAGS="-Wno-globals")
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
