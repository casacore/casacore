def generate(env):
    def BuildEnv(buildtype):
	print buildtype
	lenv = env.Copy()
	if buildtype == "dbg":
	    lenv.Append(CPPFLAGS=["-g"])
	elif buildtype == "opt":
	    lenv.Append(CPPFLAGS=["-O2"])
	return lenv
    env.BuildEnv = BuildEnv

def exists(env):
    return 1
