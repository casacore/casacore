import platform

def get_libdir():
    if not platform.architecture()[0].startswith("64"):
        return "lib"
    dist = platform.dist()[0].lower()
    distdict = dict(suse='lib64', redhat='lib64')   
    return distdict.get(dist, 'lib')
