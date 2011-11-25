# import root environment
Import( ["buildenv", "installer"])
# When adding a package, also add it to scons-tools/dependencies.py
buildenv.SConscript(dirs=["casa", "tables", "mirlib",
                          "scimath_f", "scimath",
                          "measures", "meas", "fits", "coordinates",
                          "components", "lattices", "images",
                          "ms", "derivedmscal", "msfits"], 
                    exports=["buildenv", "installer"])
