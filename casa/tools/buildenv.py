def generate(env):
    def BuildEnv(buildtype):
	lenv = env.Clone()
	lenv["build"] = buildtype
	if buildtype == "dbg":
	    lenv.AppendUnique(CPPFLAGS=["-g"])
	elif buildtype == "opt":
	    lenv.AppendUnique(CPPFLAGS=["-O2"])
	    lenv.AppendUnique(FORTRANFLAGS=["-O2"])	    
	return lenv
    env.BuildEnv = BuildEnv

def exists(env):
    return 1
