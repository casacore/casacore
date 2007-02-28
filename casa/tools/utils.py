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
	    pattern=pattern.replace("*","")
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

    def AddCustomPath(path=""):
        if not len(path) or not os.path.exists(path):
            return
        env.PrependUnique(CPPPATH = [os.path.join(path, "include")])
        env.PrependUnique(LIBPATH = [os.path.join(path, "lib")])
    env.AddCustomPath = AddCustomPath

    def PlatformIdent():
	p = sys.platform
	# replace the trailing 2 in linux2
	p = re.sub(re.compile("2$"), "", p)
	return p + "_" + platform.machine()
    env.PlatformIdent = PlatformIdent


#    env.SetupTests(rootdir):
#	fp = Dir(rootdir).fullpath
	

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
