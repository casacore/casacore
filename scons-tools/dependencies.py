def generate(env):
    
    def get_deptree():
        return {'casa_casa' : None,
                'casa_tables': ['casa_casa'],
                'casa_mirlib': ['casa_casa'],
                'casa_scimath_f': ['casa_casa'] + env.get("LAPACK", []) 
                                  + env.get("BLAS", []) + env.get("F2CLIB", []),
                'casa_scimath': ['casa_scimath_f'],
                'casa_measures': ['casa_tables', 'casa_scimath'],
                'casa_fits': ['casa_measures'] + env.get("CFITSIO", []),
                'casa_coordinates': ['casa_fits'] + env.get("WCS", []),
                'casa_components': ['casa_coordinates'],
                'casa_lattices': ['casa_tables', 'casa_scimath'],
                'casa_ms': ['casa_measures'],
                'casa_derivedmscal': ['casa_ms'],
                'casa_images': ['casa_components', 'casa_lattices', 'casa_mirlib'],
                'casa_msfits': ['casa_ms', 'casa_fits'],
                }
    def get_deps(pkg):
        deps = get_deptree()
        if pkg not in deps.keys():
            return
        pkgs = [pkg]
    
        def getpkg(pkg, pgs):
            plist =  deps.get(pkg)
            if plist is None:
                return
            for p in plist:
                pgs.insert(0, p)
                getpkg(p, pgs)

        getpkg(pkg, pkgs)
        outpkgs = []
        # strip off duplicates
        for i in pkgs:
            if i not in outpkgs and i != pkg:
                outpkgs.append(i)
        return outpkgs[::-1]

    env.GetDependencies = get_deps

def exists(env):
    return 1

